# Header -------------------------------------------------------------
#
# Produce output for manuscript tables.
#
# John Sahrmann
# 20220829


# Setup --------------------------------------------------------------

library(data.table)
library(lubridate)
library(openxlsx)
library(purrr)


# Constants ----------------------------------------------------------

data_dir <-
  "%HOME%\\Dropbox\\Banfield Dog Data\\Data Files from Banfield\\"

new_col_names <- c(
  "id", "sex", "birth_date", "breed", "mixed_breed", "neuter_date",
  "first_visit_date", "first_overweight_date", "state", "visit_date",
  "intact", "visit_reason", "wellness_plan", "bcs", "weight",
  "dx_ccl", "dx_hypothyroidism", "dx_hyperthyroidism")


# Function definitions -----------------------------------------------

n_percent <- function(x, y, digits = 0) {
  paste0(
    format(x, big.mark = ","),
    " (",
    format(round(x / y * 100, digits), nsmall = digits),
    ")")
}

median_iqr <- function(x, y, z, digits = 0) {
  paste0(
    format(round(x, digits), big.mark = ",", nsmall = digits),
    " (",
    format(round(y, digits), big.mark = ",", nsmall = digits),
    ", ",
    format(round(z, digits), big.mark = ",", nsmall = digits),
    ")")
}


# Input data ---------------------------------------------------------

source("./02-read-cohort.R")

visits_all <- data.table::fread(
  cmd = paste0("unzip -p \"", data_dir, "R2020_ACCD_DATA.zip\""),
  na.strings = "null")
data.table::setnames(visits_all, new_col_names)

date_cols <- grep("date", colnames(visits_all), value = TRUE)
visits_all[,
  (date_cols) := lapply(.SD, mdy), .SDcols = date_cols]


# Visits during follow-up --------------------------------------------

visits_fu <- merge(
  dat, visits_all, by = "id", all.x = TRUE
)[
  visit_date >= index_date & visit_date <= oo_event_date,
  .(n_visits_fu = .N),
  by = .(id, sn, oo_t2e)
][,
  visits_per_year_fu := n_visits_fu / oo_t2e * 365.25
]


# Table 1 - Cohort characteristics -----------------------------------

dat1 <- data.table::copy(dat)[,
  age_group := data.table::fcase(
    ageYearsX < 0.5, "3 mo to <6 mo",
    0.5 <= ageYearsX & ageYearsX < 1, "6 mo to <1 y",
    1 <= ageYearsX & ageYearsX < 2, "1 y to <2 y",
    2 <= ageYearsX, "2+ y"
  )
][,
  age_group := factor(
    age_group,
    levels = c(
      "3 mo to <6 mo", "6 mo to <1 y", "1 y to <2 y", "2+ y")
  )
]

sn_n <- table(dat1$sn)

sn_male <- with(dat1, table(sex, sn))["Male", ]
sn_female <- with(dat1, table(sex, sn))["Female", ]
sn_mixedy <- with(dat1, table(mixed_breed, sn))["Y", ]
sn_mixedn <- with(dat1, table(mixed_breed, sn))["N", ]
sn_age_q2 <- with(dat1, tapply(ageYearsX, sn, median))
sn_age_q1 <- with(dat1, tapply(ageYearsX, sn, quantile, .25))
sn_age_q3 <- with(dat1, tapply(ageYearsX, sn, quantile, .75))
sn_age_group1 <- with(dat1, table(age_group, sn))["3 mo to <6 mo", ]
sn_age_group2 <- with(dat1, table(age_group, sn))["6 mo to <1 y", ]
sn_age_group3 <- with(dat1, table(age_group, sn))["1 y to <2 y", ]
sn_age_group4 <- with(dat1, table(age_group, sn))["2+ y", ]
sn_weight_q2 <- with(dat1, tapply(weight, sn, median))
sn_weight_q1 <- with(dat1, tapply(weight, sn, quantile, .25))
sn_weight_q3 <- with(dat1, tapply(weight, sn, quantile, .75))
sn_size1 <- with(dat1, table(size, sn))["Toy and Small", ]
sn_size2 <- with(dat1, table(size, sn))["Medium", ]
sn_size3 <- with(dat1, table(size, sn))["Standard", ]
sn_size4 <- with(dat1, table(size, sn))["Large", ]
sn_size5 <- with(dat1, table(size, sn))["Giant", ]
sn_wellnessy <- with(dat1, table(wellness_plan, sn))["1", ]
sn_wellnessn <- with(dat1, table(wellness_plan, sn))["0", ]
oo_fu_q2 <- with(dat1, tapply(oo_t2e, sn, median))
oo_fu_q1 <- with(dat1, tapply(oo_t2e, sn, quantile, .25))
oo_fu_q3 <- with(dat1, tapply(oo_t2e, sn, quantile, .75))
ob_fu_q2 <- with(dat1, tapply(obese_t2e, sn, median))
ob_fu_q1 <- with(dat1, tapply(obese_t2e, sn, quantile, .25))
ob_fu_q3 <- with(dat1, tapply(obese_t2e, sn, quantile, .75))
sn_visits_q2 <- with(visits_fu, tapply(visits_per_year_fu, sn, median))
sn_visits_q1 <- with(visits_fu, tapply(visits_per_year_fu, sn, quantile, .25))
sn_visits_q3 <- with(visits_fu, tapply(visits_per_year_fu, sn, quantile, .75))

row_text <- list(); i <- 1
row_text[[i]] <- c(
  "", paste0(names(sn_n), ", n = ", format(sn_n, big.mark = ","))
); i <- i + 1
row_text[[i]] <- c(
  "Sex", rep("", length(sn_n))
); i <- i + 1
row_text[[i]] <- c(
  "  Male", purrr::map2_chr(sn_male, sn_n, n_percent)
); i <- i + 1
row_text[[i]] <- c(
  "  Female", purrr::map2_chr(sn_female, sn_n, n_percent)
); i <- i + 1
row_text[[i]] <- c(
  "Pure breed", rep("", length(sn_n))
); i <- i + 1
row_text[[i]] <- c(
  "  Yes", purrr::map2_chr(sn_mixedn, sn_n, n_percent)
); i <- i + 1
row_text[[i]] <- c(
  "  No", purrr::map2_chr(sn_mixedy, sn_n, n_percent)
); i <- i + 1
row_text[[i]] <- c(
  "Age in years (median, IQR)",
  purrr::pmap_chr(list(sn_age_q2, sn_age_q1, sn_age_q3), median_iqr)
); i <- i + 1
row_text[[i]] <- c(
  "Age", rep("", length(sn_n))
); i <- i + 1
row_text[[i]] <- c(
  "  3 mo to <6 mo", purrr::map2_chr(sn_age_group1, sn_n, n_percent)
); i <- i + 1
row_text[[i]] <- c(
  "  6 mo to <1 y", purrr::map2_chr(sn_age_group2, sn_n, n_percent)
); i <- i + 1
row_text[[i]] <- c(
  "  1 y to <2 y", purrr::map2_chr(sn_age_group3, sn_n, n_percent)
); i <- i + 1
row_text[[i]] <- c(
  "  2+ y", purrr::map2_chr(sn_age_group4, sn_n, n_percent)
); i <- i + 1
row_text[[i]] <- c(
  "Breed size", rep("", length(sn_n))
); i <- i + 1
row_text[[i]] <- c(
  "  Toy and Small", purrr::map2_chr(sn_size1, sn_n, n_percent)
); i <- i + 1
row_text[[i]] <- c(
  "  Medium", purrr::map2_chr(sn_size2, sn_n, n_percent)
); i <- i + 1
row_text[[i]] <- c(
  "  Standard", purrr::map2_chr(sn_size3, sn_n, n_percent)
); i <- i + 1
row_text[[i]] <- c(
  "  Large", purrr::map2_chr(sn_size4, sn_n, n_percent)
); i <- i + 1
row_text[[i]] <- c(
  "  Giant", purrr::map2_chr(sn_size5, sn_n, n_percent)
); i <- i + 1
row_text[[i]] <- c(
  "Enrolled in Wellness Plan", rep("", length(sn_n))
); i <- i + 1
row_text[[i]] <- c(
  "  Yes",
  purrr::map2_chr(sn_wellnessy, sn_n, n_percent)
); i <- i + 1
row_text[[i]] <- c(
  "  No",
  purrr::map2_chr(sn_wellnessn, sn_n, n_percent)
); i <- i + 1
row_text[[i]] <- c(
  "Follow-up in days for overweight/obese (median, IQR)",
  purrr::pmap_chr(list(oo_fu_q2, oo_fu_q1, oo_fu_q3), median_iqr)
); i <- i + 1
row_text[[i]] <- c(
  "Follow-up in days for obese (median, IQR)",
  purrr::pmap_chr(list(ob_fu_q2, ob_fu_q1, ob_fu_q3), median_iqr)
); i <- i + 1
row_text[[i]] <- c(
  "Visits per year during follow-up (median, IQR)",
  purrr::pmap_chr(
    list(sn_visits_q2, sn_visits_q1, sn_visits_q3), median_iqr)
); i <- i + 1

# Assemble the table.
table1 <- do.call(rbind, row_text) |>
  as.data.frame()

# Save the final output.
openxlsx::write.xlsx(
  table1, file = "../output/table1.xlsx",
  colNames = FALSE, colWidths = "auto"
)


# Exports ------------------------------------------------------------


## O/O ----------------------------

load(file = "../data/oo-results.Rdata")


## SN effect -----

openxlsx::write.xlsx(sn_results, "../output/s_table3a.xlsx")


## Age effect among SN

openxlsx::write.xlsx(
  age_among_sn_results, "../output/s_table4a.xlsx")


## Obesity ------------------------

load(file = "../data/ob-results.Rdata")


## SN effect -----

openxlsx::write.xlsx(sn_results, "../output/s_table3b.xlsx")


## Age effect among SN

openxlsx::write.xlsx(
  age_among_sn_results, "../output/s_table4b.xlsx")



# SN effect -----------------------

# Smallest difference in risk by SN
sn_results[wt_pctl == 50][which.min(hr)]
# Largest difference in risk by SN
sn_results[wt_pctl == 50][which.max(hr)]

round(sn_results[wt_pctl == 50][which.max(hr)]$hr, digits = 2)
round(sn_results[wt_pctl == 50][which.max(hr)]$hi, digits = 2)

# E-values
sn_results[lo > 1][which.min(e_val)]
sn_results[lo > 1][which.max(e_val)]

write.table(
  sn_results, file = "../output/table-oo-sn-effect-all-weights.csv",
  sep = ",", row.names = FALSE
)


# Age effect among SN -------------

# Smallest difference in risk by SN
age_among_sn_results[reference_age == 1.0][which.min(hr)]

# Largest difference in risk by SN
age_among_sn_results[wt_pctl == 50][which.max(hr)]

round(age_among_sn_results[wt_pctl == 50][which.max(hr)]$hr, digits = 2)
round(age_among_sn_results[wt_pctl == 50][which.max(hr)]$hi, digits = 2)

# E-values
age_among_sn_results[lo > 1][which.min(e_val)]
age_among_sn_results[lo > 1][which.max(e_val)]

write.table(
  age_among_sn_results,
  file = "../output/table-oo-age-effect-among-SN-all-years.csv",
  sep = ",", row.names = FALSE
)


# Exports ------------------------------------------------------------


# SN effect -----------------------

# Smallest difference in risk by SN
sn_results[wt_pctl == 50][which.min(hr)]
# Largest difference in risk by SN
sn_results[wt_pctl == 50][which.max(hr)]

round(sn_results[wt_pctl == 50][which.max(hr)]$hr, digits = 2)
round(sn_results[wt_pctl == 50][which.max(hr)]$hi, digits = 2)

# Age with largest hazard ratio within each breed size class.
sn_results[, max_hr := max(.SD), by = "size", .SDcols = "hr"]
max_hrs <- sn_results[hr == max_hr]
max_hrs
# Age with largest hazard ratio within each breed size class and sex.
sn_results[,
  max_hr := max(.SD), by = c("size", "sex"), .SDcols = "hr"]
max_hrs <- sn_results[hr == max_hr]
max_hrs

# E-values
sn_results[lo > 1][which.min(e_val)]
sn_results[lo > 1][which.max(e_val)]

write.table(
  sn_results, file = "../output/table-ob-sn-effect-all-weights.csv",
  sep = ",", row.names = FALSE
)


# Age effect among SN -------------

# Smallest difference in risk by SN
age_among_sn_results[reference_age == 1.0][which.min(hr)]

# Largest difference in risk by SN
age_among_sn_results[wt_pctl == 50][which.max(hr)]

round(age_among_sn_results[wt_pctl == 50][which.max(hr)]$hr, digits = 2)
round(age_among_sn_results[wt_pctl == 50][which.max(hr)]$hi, digits = 2)

# E-values
age_among_sn_results[lo > 1][which.min(e_val)]
age_among_sn_results[lo > 1][which.max(e_val)]

write.table(
  age_among_sn_results,
  file = "../output/table-ob-age-effect-among-SN-all-years.csv",
  sep = ",", row.names = FALSE
)
