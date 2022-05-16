# Header -------------------------------------------------------------
#
# Produce output for manuscript tables.
#
# John Sahrmann
# 20220516


# Setup --------------------------------------------------------------

library(purrr)


# Input data ---------------------------------------------------------

source("./02-read-cohort.R")


# Table 2 - Unadjusted SN effect estimates  --------------------------

# Get group-specific counts.
sn_n <- table(dat$sn)

# Compute total length of follow-up in years.
oo_total_fu <- with(dat, tapply(oo_t2e, sn, sum) / 365.25)
# Get outcome counts and percentages.
oo_outcome_ct <- with(dat, tapply(oo_event, sn, sum))
oo_outcome_pct <- oo_outcome_ct / sn_n * 100
# Compute crude incidence rates.
oo_outcome_rate <- oo_outcome_ct / oo_total_fu * 1000

# Repeat for obesity outcome.
ob_total_fu <- with(dat, tapply(obese_t2e, sn, sum) / 365.25)
ob_outcome_ct <- with(dat, tapply(obese_event, sn, sum))
ob_outcome_pct <- ob_outcome_ct / sn_n * 100
ob_outcome_rate <- ob_outcome_ct / ob_total_fu * 1000

row_text <- list()
row_text[[1]] <- c(
  "", paste0(names(sn_n), ", n = ", format(sn_n, big.mark = ","))
)
row_text[[2]] <- c("Overweight/obese", rep("", times = length(sn_n)))
row_text[[3]] <- c(
  "Total years of observation",
  purrr::map_chr(oo_total_fu, function(x) {
    format(round(x), big.mark = ",")
  })
)
row_text[[4]] <- c(
  "Crude outcome events, n (%)",
  purrr::map2_chr(oo_outcome_ct, oo_outcome_pct, function(x, y) {
    paste0(
      format(x, big.mark = ","),
      " (", format(round(y, 1), nsmall = 1), ")"
    )
  })
)
row_text[[5]] <- c(
  "Crude outcome incidence rates per 1,000 years of observation",
  purrr::map_chr(oo_outcome_rate, function(x) {
    format(round(x, 1), nsmall = 1)
  })
)
row_text[[6]] <- c("Obese", rep("", times = length(sn_n)))
row_text[[7]] <- c(
  "Total years of observation",
  purrr::map_chr(ob_total_fu, function(x) {
    format(round(x), big.mark = ",")
  })
)
row_text[[8]] <- c(
  "Crude outcome events, n (%)",
  purrr::map2_chr(ob_outcome_ct, ob_outcome_pct, function(x, y) {
    paste0(
      format(x, big.mark = ","),
      " (", format(round(y, 1), nsmall = 1), ")"
    )
  })
)
row_text[[9]] <- c(
  "Crude outcome incidence rates per 1,000 years of observation",
  purrr::map_chr(ob_outcome_rate, function(x) {
    format(round(x, 1), nsmall = 1)
  })
)

# Assemble the table.
table2 <- do.call(rbind, row_text)

# Save the final output.
write.table(
  table2, file = "../output/table2.csv",
  sep = ",", row.names = FALSE, col.names = FALSE
)


# Table 3 - Unadjusted age among SN effect estimates  ----------------

dat3 <- data.table::copy(dat)[
  sn == "Spayed/neutered",
  `:=`(
    age_group = data.table::fcase(
      ageYearsX <= 0.5, "Birth to 6 mo",
      0.5 < ageYearsX & ageYearsX <= 1, "6 mo to 1 y",
      1 < ageYearsX & ageYearsX <= 2, "1 y to 2 y",
      2 < ageYearsX, "> 2 y"
    )
  )
][,
  age_group := factor(
    age_group,
    levels = c("Birth to 6 mo", "6 mo to 1 y", "1 y to 2 y", "> 2 y"),
    labels = c("Birth to 6 mo", "6 mo to 1 y", "1 y to 2 y", "> 2 y")
  )
]


# Get group-specific counts.
age_n <- table(dat3$age_group)

# Compute total length of follow-up in years.
oo_total_fu <- with(dat3, tapply(oo_t2e, age_group, sum) / 365.25)
# Get outcome counts and percentages.
oo_outcome_ct <- with(dat3, tapply(oo_event, age_group, sum))
oo_outcome_pct <- oo_outcome_ct / age_n * 100
# Compute crude incidence rates.
oo_outcome_rate <- oo_outcome_ct / oo_total_fu * 1000

# Repeat for obesity outcome.
ob_total_fu <- with(dat3, tapply(obese_t2e, age_group, sum) / 365.25)
ob_outcome_ct <- with(dat3, tapply(obese_event, age_group, sum))
ob_outcome_pct <- ob_outcome_ct / age_n * 100
ob_outcome_rate <- ob_outcome_ct / ob_total_fu * 1000

row_text <- list()
row_text[[1]] <- c(
  "", paste0(names(age_n), ", n = ", format(age_n, big.mark = ","))
)
row_text[[2]] <- c("Overweight/obese", rep("", times = length(age_n)))
row_text[[3]] <- c(
  "Total years of observation",
  purrr::map_chr(oo_total_fu, function(x) {
    format(round(x), big.mark = ",")
  })
)
row_text[[4]] <- c(
  "Crude outcome events, n (%)",
  purrr::map2_chr(oo_outcome_ct, oo_outcome_pct, function(x, y) {
    paste0(
      format(x, big.mark = ","),
      " (", format(round(y, 1), nsmall = 1), ")"
    )
  })
)
row_text[[5]] <- c(
  "Crude outcome incidence rates per 1,000 years of observation",
  purrr::map_chr(oo_outcome_rate, function(x) {
    format(round(x, 1), nsmall = 1)
  })
)
row_text[[6]] <- c("Obese", rep("", times = length(age_n)))
row_text[[7]] <- c(
  "Total years of observation",
  purrr::map_chr(ob_total_fu, function(x) {
    format(round(x), big.mark = ",")
  })
)
row_text[[8]] <- c(
  "Crude outcome events, n (%)",
  purrr::map2_chr(ob_outcome_ct, ob_outcome_pct, function(x, y) {
    paste0(
      format(x, big.mark = ","),
      " (", format(round(y, 1), nsmall = 1), ")"
    )
  })
)
row_text[[9]] <- c(
  "Crude outcome incidence rates per 1,000 years of observation",
  purrr::map_chr(ob_outcome_rate, function(x) {
    format(round(x, 1), nsmall = 1)
  })
)

# Assemble the table.
table3 <- do.call(rbind, row_text)

# Save the final output.
write.table(
  table3, file = "../output/table3.csv",
  sep = ",", row.names = FALSE, col.names = FALSE
)
