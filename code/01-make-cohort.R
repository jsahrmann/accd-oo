# Front matter -------------------------------------------------------
#
# Data management for the ACC&D analysis of the effect of
# sterilization on risk of overweight/obese status.
#
# John Sahrmann
# 20220725
#
# ;anti-join;data.table;data.table::.SD;data.table::fread
# ;data.table::rbindlist;data.table::setnames;data.table-anti-join
# ;data.table-rename;ggplot2;ggplot2::geom_density
# ;ggplot2::geom_histogram;ggplot2-legend;lubridate;readr;rename;zip


# Preface ------------------------------------------------------------

library(data.table)
library(dplyr)
library(ggconsort)
library(lubridate)
library(magrittr)
library(readxl)


# Constants ----------------------------------------------------------

data_dir <-
  "%HOME%\\Dropbox\\Banfield Dog Data\\Data Files from Banfield\\"

new_col_names <- c(
  "id", "sex", "birth_date", "breed", "mixed_breed", "neuter_date",
  "first_visit_date", "first_overweight_date", "state", "visit_date",
  "intact", "visit_reason", "wellness_plan", "bcs", "weight",
  "dx_ccl", "dx_hypothyroidism", "dx_hyperthyroidism")


# Input data ---------------------------------------------------------

# Read the full Banfield data set.
system.time(
visits_all <- data.table::fread(
  cmd = paste0("unzip -p \"", data_dir, "R2020_ACCD_DATA.zip\""),
  na.strings = "null")
)  # 6--21 s
data.table::setnames(visits_all, new_col_names)

# Recast columns with dates.
date_cols <- grep("date", colnames(visits_all), value = TRUE)
visits_all[, (date_cols) := lapply(.SD, mdy), .SDcols = date_cols]

# Read the breed sizes data set.
breedSizes <- readxl::read_excel(
  "../data/Banfield data breeds - final recommendations.xlsx",
  range = "A1:I506"
) %>%
  dplyr::select(
    `Banfield breed label`, `Recommended size category (DAP buckets)`
  ) %>%
  dplyr::rename(
    breed = `Banfield breed label`,
    size = `Recommended size category (DAP buckets)`
  ) %>%
  data.table::as.data.table()


# Data management ----------------------------------------------------

# Define an index date for each dog as `neuter_date` for dogs who were
# spayed/neutered in 2014 or the earliest visit date in 2014 for those
# who were not. Only consider dogs who were intact as of December 31,
# 2013. (Dogs who were overweight/obese in 2013 have already been
# excluded by Banfield.)
visits_2014ButNotIfSNIn2013 <- visits_all[
  lubridate::year(visit_date) == 2014
  & (is.na(neuter_date) | lubridate::year(neuter_date) > 2013)
][,
  `:=`(
    sn = data.table::fifelse(
      !is.na(neuter_date) & lubridate::year(neuter_date) == 2014,
      1, 0),
    index_date = data.table::fifelse(
      !is.na(neuter_date) & lubridate::year(neuter_date) == 2014,
      neuter_date, min(visit_date))
  ),
  by = id
]

# Make a data set of unique dogs in this sample for later merging.
dogs_a14 <- visits_2014ButNotIfSNIn2013[,
  lapply(.SD, first), by = id, .SDcols = c("sn", "index_date")
]

# Select all visits for dogs in this sample.
visits_a14_all <- merge(dogs_a14, visits_all, by = "id")

# Add breed size category to the 'all visits' data set.
visits_a14_all <- merge(
  visits_a14_all, breedSizes, by = "breed", all.x = TRUE)


# Exclusions ---------------------------------------------------------


# At or before index --------------
#
# - Overweight or obese any time at or before index
# - Hypothyroidism diagnosis any time at or before index
# - Hyperthyroidism diagnosis any time at or before index

# Select visits at or before index.
visits_a14_atIndexOrBefore <- visits_a14_all[visit_date <= index_date]

visits_a14_atIndexOrBefore[,
  `:=`(
    first_visit_date_ever = first_visit_date,
    first_visit_date = min(visit_date)
  ),
  by = id
][,
  dt2013VisitAndIndex := as.integer(index_date - first_visit_date + 1)
][,
  nVisits := dplyr::n_distinct(visit_date), by = id
][,
  visitsPerYear := nVisits / dt2013VisitAndIndex * 365.25
]

# Filter to visits with a BCS of 4 or 5.
visits_a14OO_atIndexOrBefore <- visits_a14_atIndexOrBefore[
  bcs %in% 4:5]

# Collapse to one record per dog.
dogs_a14ExclOO_atIndexOrBefore <- unique(
  visits_a14OO_atIndexOrBefore[, .(id, sn, index_date)])

# For each dog, check for a history of conditions that could be
# related to weight.
dogs_a14_atIndexOrBefore <- visits_a14_atIndexOrBefore[,
  .(dx_hypothy_preEver = max(dx_hypothyroidism),
    dx_hyperthy_preEver = max(dx_hyperthyroidism)),
  by = .(id, sn)]

# Make a data set of IDs of dogs with a weight-related exclusion
# diagnosis at or before index.
dogs_a14ExclDx_atIndexOrBefore <- dogs_a14_atIndexOrBefore[
    dx_hypothy_preEver == 1 | dx_hyperthy_preEver == 1, .(id, sn)]


# At index ------------------------

# - neuter date but no corresponding visit
# - neuter date missing but `intact == 0`
# - unrealistically high value for weight
# - BCS == 1
# - unresolved breed size
# - S/N at less than 90 days of age

# Subset to visits at index.
visits_a14_atIndex <- visits_a14_all[visit_date == index_date]

# Count the number of visit records on the index date for each dog.
dogs_a14VisitCounts_atIndex <- visits_a14_atIndex[,
  .(visits = .N), by = id]

# To simplify, we'll take the last record per dog. (Some hacking
# suggests that the difference between taking the first or last is
# minimal.)
dogs_a14_atIndex <- visits_a14_atIndex[, lapply(.SD, last), by = id]

# A small number of dogs don't appear in the data set at index. These
# dogs all have a neuter date (which is defined in the data dictionary
# as the spay/neuter date *at Banfield*) but no corresponding visit
# record.
dogs_a14ExclSNButNoVisit_atIndex <- dogs_a14[
  !dogs_a14_atIndex, on = "id"]

# Select dogs with missing BCS or BCS of 1 or with an abnormal weight.
dogs_a14ExclBCSMissing_atIndex <- dogs_a14_atIndex[
  is.na(bcs), .(id, sn)]
dogs_a14ExclBCS1_atIndex <- dogs_a14_atIndex[
  bcs == 1, .(id, sn)]
dogs_a14ExclWt_atIndex <- dogs_a14_atIndex[
  weight >= 250, .(id, sn)]

# Select dogs with missing neuter date but `intact == 0`, indicating
# spayed/neutered at the start of the index visit. (Perhaps these dogs
# were sterilized elsewhere? Curiously, `sex` does not suggest
# spayed/neutered.)
dogs_a14ExclNoNeuterDateButNotIntact_atIndex <- dogs_a14_atIndex[
  is.na(neuter_date) & intact == 0, .(id, sn)]

# Select dogs whose breed could not be categorized by size (or that
# are otherwise unusual).
dogs_a14ExclBreed_atIndex <- dogs_a14_atIndex[is.na(size), .(id, sn)]

# Select spayed/neutered dogs who were sterilized dogs at less than 90
# days of age.
dogs_a14ExclEarlySN_atIndex <- dogs_a14_atIndex[,
  age := as.integer(index_date - birth_date)
][
  sn == 1 & age < 90, .(id, sn)
]


# After index ---------------------

# Select dogs with no visits after index for 'exclusion', as they won't
# contribute any information to the survival analysis.
dogs_a14WithFoo <- unique(
  visits_a14_all[visit_date > index_date, .(id, sn)])
dogs_a14WithoutFoo <- dogs_a14[!dogs_a14WithFoo, .(id, sn), on = "id"]


# Final index data set -----------------------------------------------

# Apply exclusions in the order recommended by Jan in her 20220522
# revision of the Results.
#
# "Keep: No follow-up visits"
dogs_i1 <- dogs_a14[!dogs_a14WithoutFoo, .(id, sn), on = "id"]
dogs_x1 <- dogs_a14[!dogs_i1, .(id, sn), on = "id"]
# "Keep: No visit record on S/N date"
dogs_i2 <- dogs_i1[
  !dogs_a14ExclSNButNoVisit_atIndex, .(id, sn), on = "id"]
dogs_x2 <- dogs_i1[!dogs_i2, .(id, sn), on = "id"]
# "Combine: S/N before 90 days with neutered elsewhere"
dogs_i3 <- dogs_i2[
  !dogs_a14ExclEarlySN_atIndex, .(id, sn), on = "id"
][
  !dogs_a14ExclNoNeuterDateButNotIntact_atIndex, .(id, sn), on = "id"
]
dogs_x3 <- dogs_i2[!dogs_i3, .(id, sn), on = "id"]
# "Keep: BCS missing at index date"
dogs_i4 <- dogs_i3[
  !dogs_a14ExclBCSMissing_atIndex, .(id, sn), on = "id"]
dogs_x4 <- dogs_i3[!dogs_i4, .(id, sn), on = "id"]
# "Combine: BCS < 3 or >3 at or before index date or weight 250
# lbs. or more"
dogs_i5 <- dogs_i4[
  !dogs_a14ExclBCS1_atIndex, .(id, sn), on = "id"
][
  !dogs_a14ExclOO_atIndexOrBefore, .(id, sn), on = "id"
][
  !dogs_a14ExclWt_atIndex, .(id, sn), on = "id"
]
dogs_x5 <- dogs_i4[!dogs_i5, .(id, sn), on = "id"]
# "Keep: Hyperthyroid or hypothyroid at or before index date"
dogs_i6 <- dogs_i5[
  !dogs_a14ExclDx_atIndexOrBefore, .(id, sn), on = "id"]
dogs_x6 <- dogs_i5[!dogs_i6, .(id, sn), on = "id"]
# "Keep: Size category could not be assigned"
dogs_i7 <- dogs_i6[!dogs_a14ExclBreed_atIndex, .(id, sn), on = "id"]
dogs_x7 <- dogs_i6[!dogs_i7, .(id, sn), on = "id"]

# Print the sample size changes for a flow chart table.
purrr::map(
  list(
    dogs_a14, dogs_x1, dogs_i1, dogs_x2, dogs_i2, dogs_x3, dogs_i3,
    dogs_x4, dogs_i4, dogs_x5, dogs_i5, dogs_x6, dogs_i6, dogs_x7,
    dogs_i7
  ),
  ~ table(.x[["sn"]])
)

# Select the final sample for inclusion.
dogs_final <- dogs_a14[dogs_i7, on = c("id", "sn")]

# Add characteristics from index visits.
dogs_final <- dogs_a14_atIndex[dogs_final[, .(id)], on = "id"]

# Compute the rate of visits per year based on the pre-index and index
# periods.
visits_final <- visits_a14_all[dogs_final[, .(id)], on = "id"]
visits_final_atIndexOrBefore <- visits_final[visit_date <= index_date]

visits_final_atIndexOrBefore[,
  `:=`(
    first_visit_date_ever = first_visit_date,
    first_visit_date = min(visit_date)
  ),
  by = id
][,
  dt2013VisitAndIndex := as.integer(index_date - first_visit_date + 1)
][,
  nVisits := n_distinct(visit_date), by = id
][,
  visitsPerYear := nVisits / dt2013VisitAndIndex * 365.25
]

dogs_finalVisitsPerYear <- visits_final_atIndexOrBefore[,
  lapply(.SD, data.table::first), by = id, .SDcols = "visitsPerYear"
]

# Add `visitsPerYear` to the final inclusion data set.
dogs_final <- dogs_final[dogs_finalVisitsPerYear, on = "id"]

# Define new variables at index and discard unneeded ones.
dogs_final[,
  `:=`(
    ageDays = age,
    sex = fcase(
      sex == "Neutered Male", "Male",
      sex == "Spayed Female", "Female",
      sex == "Male", "Male",
      sex == "Female", "Female"),
    lenpre = as.integer(index_date - first_visit_date)
  )
][,
  c(
    "birth_date", "neuter_date", "first_visit_date",
    "first_overweight_date", "visit_date", "intact", "bcs", "dx_ccl",
    "dx_hypothyroidism", "dx_hyperthyroidism", "age")
  := NULL
]

readr::write_rds(
  dogs_final,
  paste0(
    "~/Dropbox/Banfield Dog Data/R Data Files (for analysis)/",
    "dogs_final.rds"
  ),
  compress = "gz"
)


# Outcomes and censoring ---------------------------------------------

# Make a data set of post-index visit records.
visits_final_postIndex <-
  visits_a14_all[dogs_final[, .(id)], on = "id"]

# Define dates for outcome and censoring events. (Note we're using the
# Date type-specific form of the missing value provided by the
# lubridate package, otherwise we get a type mismatch error.)
visits_final_postIndex[,
  `:=`(
    neuter_date_after2014 = fifelse(
      !is.na(neuter_date) & year(neuter_date) > 2014,
      neuter_date, NA_Date_),
    dx_hypothy_date = fifelse(
      dx_hypothyroidism == 1, visit_date, NA_Date_),
    dx_hyprthy_date = fifelse(
      dx_hyperthyroidism == 1, visit_date, NA_Date_),
    oo_date = fifelse(
      bcs == 4 | bcs == 5, visit_date, NA_Date_),
    obese_date = fifelse(
      bcs == 5, visit_date, NA_Date_))]

# Choose the earliest date of each outcome or censoring event per dog.
dogs_outcomeAndCensoringDates <- visits_final_postIndex[,
  .(last_visit_date = max(visit_date),
    neuter_date_after2014 = min(neuter_date_after2014, na.rm = TRUE),
    dx_hypothy_date_earliest = min(dx_hypothy_date, na.rm = TRUE),
    dx_hyprthy_date_earliest = min(dx_hyprthy_date, na.rm = TRUE),
    oo_date_earliest = min(oo_date, na.rm = TRUE),
    obese_date_earliest = min(obese_date, na.rm = TRUE)),
  by = .(id, index_date)]

# For dogs who never experience a certain type of outcome or censoring
# event (e.g., dogs never diagnosed with hypothyroidism), the
# aggregation functions return `Inf` rather than `NA`, so we need to
# manually set these to missing.
dogs_outcomeAndCensoringDates[,
  `:=`(
    dx_hypothy_date_earliest = fifelse(
      is.infinite(dx_hypothy_date_earliest),
      NA_Date_,
      dx_hypothy_date_earliest),
    dx_hyprthy_date_earliest = fifelse(
      is.infinite(dx_hyprthy_date_earliest),
      NA_Date_,
      dx_hyprthy_date_earliest),
    oo_date_earliest = fifelse(
      is.infinite(oo_date_earliest),
      NA_Date_,
      oo_date_earliest),
    obese_date_earliest = fifelse(
      is.infinite(obese_date_earliest),
      NA_Date_,
      obese_date_earliest))
][,  # Choose the earliest event per outcome.
  `:=`(
    oo_event_date = pmin(
      last_visit_date, neuter_date_after2014,
      dx_hypothy_date_earliest, dx_hyprthy_date_earliest,
      oo_date_earliest,
      na.rm = TRUE),
    obese_event_date = pmin(
      last_visit_date, neuter_date_after2014,
      dx_hypothy_date_earliest, dx_hyprthy_date_earliest,
      obese_date_earliest,
      na.rm = TRUE))
][,  # Define event indicator and time-to-event variables.
  `:=`(
    oo_event = fcase(
      is.na(oo_event_date), 0,
      oo_date_earliest == oo_event_date, 1,
      default = 0),
    obese_event = fcase(
      is.na(obese_event_date), 0,
      obese_date_earliest == obese_event_date, 1,
      default = 0),
    oo_t2e = as.integer(oo_event_date - index_date),
    obese_t2e = as.integer(obese_event_date - index_date))
]

readr::write_rds(
  dogs_outcomeAndCensoringDates,
  paste0(
    "~/Dropbox/Banfield Dog Data/R Data Files (for analysis)/",
    "dogs_outcomeAndCensoringDates.rds"
  ),
  compress = "gz"
)


## Consort diagram ---------------------------------------------------

# Apply exclusions in the order recommended by Jan in her 20220522
# revision of the Results.
dogs_a14[
  # "Keep: No follow-up visits"
  dogs_a14WithoutFoo, on = "id", exclWithoutFoo := !is.na(i.id)
][
  # "Keep: No visit record on S/N date"
  dogs_a14ExclSNButNoVisit_atIndex, on = "id",
  exclSNButNoVisit := !is.na(i.id)
][
  # "Combine: S/N before 90 days with neutered elsewhere"
  dogs_a14ExclEarlySN_atIndex, on = "id", exclEarlySN := !is.na(i.id)
][
  dogs_a14ExclNoNeuterDateButNotIntact_atIndex, on = "id",
  exclNoNeuterDateButNotIntact := !is.na(i.id)
][
  # "Keep: BCS missing at index date"
  dogs_a14ExclBCSMissing_atIndex, on = "id",
  exclBCSMissing := !is.na(i.id)
][
  # "Combine: BCS < 3 or >3 at or before index date or weight 250
  # lbs. or more"
  dogs_a14ExclBCS1_atIndex, on = "id", exclBCS1 := !is.na(i.id)
][
  dogs_a14ExclOO_atIndexOrBefore, on = "id", exclOO := !is.na(i.id)
][
  dogs_a14ExclWt_atIndex, on = "id", exclWt := !is.na(i.id)
][
  # "Keep: Hyperthyroid or hypothyroid at or before index date"
  dogs_a14ExclDx_atIndexOrBefore, on = "id", exclDx := !is.na(i.id)
][
  # "Keep: Size category could not be assigned"
  dogs_a14ExclBreed_atIndex, on = "id", exclBreed := !is.na(i.id)
]

t01 <- "Dogs examined in 2014 with a previous visit in 2013"
t02 <- "Excluded: no visits after enrollment<br>"
t03 <- "Dogs with available follow-up"
t04 <- "Excluded: no visit record on neuter<br>date"
t05 <- "Dogs with visit record on enrollment date"
t06 <- paste0(
  "Excluded: neutered before 90 days<br>of age or neutered at other ",
  "facility<br>"
)
t07 <- "Dogs neutered at appropriate age and at Banfield location"
t08 <- "Excluded: BCS missing at enrollment<br>"
t09 <- "Dogs with nonmissing BCS at enrollment"
t10 <- paste0(
  "Excluded: BCS indicating overweight/<br>obese before or at enrollment",
  ", or BCS<br>indicating underweight or recorded<br>weight of 250 lbs. or",
  " more at<br>enrollment"
)
t11 <- paste0(
  "Dogs with no history of overweight/obese and of normal weight at ",
  "enrollment"
)
t12 <- paste0(
  "Excluded: hyperthyroidism or<br>hypothyroidism diagnosis at or ",
  "<br>before enrollment"
)
t13 <- "Dogs with no history of hyperthyroidism or hypothyroidism"
t14 <- "Excluded: size category could not be<br>assigned"
t15 <- "Final sample"

cohorts <- dogs_a14 %>%
  ggconsort::cohort_start(t01) %>%
  ggconsort::cohort_define(
    exclWithoutFoo = .full %>% dplyr::filter(exclWithoutFoo),
    inWithFoo = .full %>% dplyr::filter(is.na(exclWithoutFoo)),
    exclSNButNoVisit = inWithFoo %>% dplyr::filter(exclSNButNoVisit),
    inSNVisit = inWithFoo %>% dplyr::filter(is.na(exclSNButNoVisit)),
    exclEarlySN = inSNVisit %>%
      dplyr::filter(exclEarlySN | exclNoNeuterDateButNotIntact),
    inEarlySN = inSNVisit %>%
      dplyr::filter(
        is.na(exclEarlySN), is.na(exclNoNeuterDateButNotIntact)),
    exclBCSMissing = inEarlySN %>% dplyr::filter(exclBCSMissing),
    inBCSMissing = inEarlySN %>% dplyr::filter(is.na(exclBCSMissing)),
    exclBCS1 = inBCSMissing %>%
      dplyr::filter(exclBCS1 | exclOO | exclWt),
    inBCS1 = inBCSMissing %>%
      dplyr::filter(is.na(exclBCS1), is.na(exclOO), is.na(exclWt)),
    exclDx = inBCS1 %>% dplyr::filter(exclDx),
    inDx = inBCS1 %>% dplyr::filter(is.na(exclDx)),
    exclBreed = inDx %>% dplyr::filter(exclBreed),
    inBreed = inDx %>% dplyr::filter(is.na(exclBreed))
  ) %>%
  ggconsort::cohort_label(
    exclWithoutFoo = t02,
    inWithFoo = t03,
    exclSNButNoVisit = t04,
    inSNVisit = t05,
    exclEarlySN = t06,
    inEarlySN = t07,
    exclBCSMissing = t08,
    inBCSMissing = t09,
    exclBCS1 = t10,
    inBCS1 = t11,
    exclDx = t12,
    inDx = t13,
    exclBreed = t14,
    inBreed = t15
  )

consort <- cohorts %>%
  ggconsort::consort_box_add(
    "full", 0, 90, ggconsort::cohort_count_adorn(cohorts, .full)
  ) %>%
  ggconsort::consort_box_add(
    "exclWithoutFoo", 30, 84,
    ggconsort::cohort_count_adorn(cohorts, exclWithoutFoo)
  ) %>%
  ggconsort::consort_box_add(
    "inWithFoo", 0, 80,
    ggconsort::cohort_count_adorn(cohorts, inWithFoo)
  ) %>%
  ggconsort::consort_arrow_add(
    start = "full", start_side = "bottom",
    end = "inWithFoo", end_side = "top",
    start_x = 0, start_y = 90
  ) %>%
  ggconsort::consort_arrow_add(
    end = "exclWithoutFoo", end_side = "left",
    start_x = 0, start_y = 84
  ) %>%
  ggconsort::consort_box_add(
    "exclSNButNoVisit", 30, 74,
    ggconsort::cohort_count_adorn(cohorts, exclSNButNoVisit)
  ) %>%
  ggconsort::consort_box_add(
    "inSNVisit", 0, 70,
    ggconsort::cohort_count_adorn(cohorts, inSNVisit)
  ) %>%
  ggconsort::consort_arrow_add(
    start = "inWithFoo", start_side = "bottom",
    end = "inSNVisit", end_side = "top",
    start_x = 0, start_y = 80
  ) %>%
  ggconsort::consort_arrow_add(
    end = "exclSNButNoVisit", end_side = "left",
    start_x = 0, start_y = 74
  ) %>%
  ggconsort::consort_box_add(
    "exclEarlySN", 30, 62,
    ggconsort::cohort_count_adorn(cohorts, exclEarlySN)
  ) %>%
  ggconsort::consort_box_add(
    "inEarlySN", 0, 57,
    ggconsort::cohort_count_adorn(cohorts, inEarlySN)
  ) %>%
  ggconsort::consort_arrow_add(
    start = "inSNVisit", start_side = "bottom",
    end = "inEarlySN", end_side = "top",
    start_x = 0, start_y = 70
  ) %>%
  ggconsort::consort_arrow_add(
    end = "exclEarlySN", end_side = "left",
    start_x = 0, start_y = 62
  ) %>%
  ggconsort::consort_box_add(
    "exclBCSMissing", 30, 49,
    ggconsort::cohort_count_adorn(cohorts, exclBCSMissing)
  ) %>%
  ggconsort::consort_box_add(
    "inBCSMissing", 0, 45,
    ggconsort::cohort_count_adorn(cohorts, inBCSMissing)
  ) %>%
  ggconsort::consort_arrow_add(
    start = "inEarlySN", start_side = "bottom",
    end = "inBCSMissing", end_side = "top",
    start_x = 0, start_y = 62
  ) %>%
  ggconsort::consort_arrow_add(
    end = "exclBCSMissing", end_side = "left",
    start_x = 0, start_y = 49
  ) %>%
  ggconsort::consort_box_add(
    "exclBCS1", 30, 35,
    ggconsort::cohort_count_adorn(cohorts, exclBCS1)
  ) %>%
  ggconsort::consort_box_add(
    "inBCS1", 0, 28,
    ggconsort::cohort_count_adorn(cohorts, inBCS1)
  ) %>%
  ggconsort::consort_arrow_add(
    start = "inBCSMissing", start_side = "bottom",
    end = "inBCS1", end_side = "top",
    start_x = 0, start_y = 45
  ) %>%
  ggconsort::consort_arrow_add(
    end = "exclBCS1", end_side = "left",
    start_x = 0, start_y = 35
  ) %>%
  ggconsort::consort_box_add(
    "exclDx", 30, 20,
    ggconsort::cohort_count_adorn(cohorts, exclDx)
  ) %>%
  ggconsort::consort_box_add(
    "inDx", 0, 15,
    ggconsort::cohort_count_adorn(cohorts, inDx)
  ) %>%
  ggconsort::consort_arrow_add(
    start = "inBCS1", start_side = "bottom",
    end = "inDx", end_side = "top",
    start_x = 0, start_y = 45
  ) %>%
  ggconsort::consort_arrow_add(
    end = "exclDx", end_side = "left",
    start_x = 0, start_y = 20
  ) %>%
  ggconsort::consort_box_add(
    "exclBreed", 30, 8,
    ggconsort::cohort_count_adorn(cohorts, exclBreed)
  ) %>%
  ggconsort::consort_box_add(
    "inBreed", 0, 4,
    ggconsort::cohort_count_adorn(cohorts, inBreed)
  ) %>%
  ggconsort::consort_arrow_add(
    start = "inDx", start_side = "bottom",
    end = "inBreed", end_side = "top",
    start_x = 0, start_y = 15
  ) %>%
  ggconsort::consort_arrow_add(
    end = "exclBreed", end_side = "left",
    start_x = 0, start_y = 8
  )

png("../output/fig/consort-diagram.png", width = 1100, height = 720)
consort %>%
  ggplot2::ggplot() +
  geom_consort() +
  theme_consort() +
  theme_consort(margin_h = 35, margin_v = 1)
dev.off()
