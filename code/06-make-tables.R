# Header -------------------------------------------------------------
#
# Produce output for manuscript tables.
#
# John Sahrmann
# 20220516


# Setup --------------------------------------------------------------

library(purrr)


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


# Table 1 - Cohort characteristics -----------------------------------

dat1 <- data.table::copy(dat)[,
  age_group := data.table::fcase(
    ageYearsX <= 0.5, "3 mo to 6 mo",
    0.5 < ageYearsX & ageYearsX <= 1, "> 6 mo to 1 y",
    1 < ageYearsX & ageYearsX <= 2, "> 1 y to 2 y",
    2 < ageYearsX, "> 2 y"
  )
][,
  age_group := factor(
    age_group,
    levels = c(
      "3 mo to 6 mo", "> 6 mo to 1 y", "> 1 y to 2 y", "> 2 y")
  )
]

sn_n <- table(dat1$sn)

sn_female <- with(dat1, table(sex, sn))["Female", ]
sn_mixed <- with(dat1, table(mixed_breed, sn))["Y", ]
sn_age_q2 <- with(dat1, tapply(ageYearsX, sn, median))
sn_age_q1 <- with(dat1, tapply(ageYearsX, sn, quantile, .25))
sn_age_q3 <- with(dat1, tapply(ageYearsX, sn, quantile, .75))
sn_age_group1 <- with(dat1, table(age_group, sn))["3 mo to 6 mo", ]
sn_age_group2 <- with(dat1, table(age_group, sn))["> 6 mo to 1 y", ]
sn_age_group3 <- with(dat1, table(age_group, sn))["> 1 y to 2 y", ]
sn_age_group4 <- with(dat1, table(age_group, sn))["> 2 y", ]
sn_weight_q2 <- with(dat1, tapply(weight, sn, median))
sn_weight_q1 <- with(dat1, tapply(weight, sn, quantile, .25))
sn_weight_q3 <- with(dat1, tapply(weight, sn, quantile, .75))
sn_size1 <- with(dat1, table(size, sn))["Toy and Small", ]
sn_size2 <- with(dat1, table(size, sn))["Medium", ]
sn_size3 <- with(dat1, table(size, sn))["Standard", ]
sn_size4 <- with(dat1, table(size, sn))["Large", ]
sn_size5 <- with(dat1, table(size, sn))["Giant", ]
oo_fu_q2 <- with(dat1, tapply(oo_t2e, sn, median))
oo_fu_q1 <- with(dat1, tapply(oo_t2e, sn, quantile, .25))
oo_fu_q3 <- with(dat1, tapply(oo_t2e, sn, quantile, .75))
ob_fu_q2 <- with(dat1, tapply(obese_t2e, sn, median))
ob_fu_q1 <- with(dat1, tapply(obese_t2e, sn, quantile, .25))
ob_fu_q3 <- with(dat1, tapply(obese_t2e, sn, quantile, .75))

row_text <- list()
row_text[[1]] <- c(
  "", paste0(names(sn_n), ", n = ", format(sn_n, big.mark = ","))
)
row_text[[2]] <- c(
  "Female", purrr::map2_chr(sn_female, sn_n, n_percent)
)
row_text[[3]] <- c(
  "Mixed breed", purrr::map2_chr(sn_mixed, sn_n, n_percent)
)
row_text[[3]] <- c(
  "Mixed breed", purrr::map2_chr(sn_mixed, sn_n, n_percent)
)
row_text[[4]] <- c(
  "Age in years (median, IQR)",
  purrr::pmap_chr(list(sn_age_q2, sn_age_q1, sn_age_q3), median_iqr)
)
row_text[[5]] <- c("Age", rep("", length(sn_n)))
row_text[[6]] <- c(
  "  3 mo to 6 mo", purrr::map2_chr(sn_age_group1, sn_n, n_percent)
)
row_text[[7]] <- c(
  "  > 6 mo to 1 y", purrr::map2_chr(sn_age_group2, sn_n, n_percent)
)
row_text[[8]] <- c(
  "  > 1 y to 2 y", purrr::map2_chr(sn_age_group3, sn_n, n_percent)
)
row_text[[9]] <- c(
  "  > 2 y", purrr::map2_chr(sn_age_group4, sn_n, n_percent)
)
row_text[[10]] <- c("Breed size", rep("", length(sn_n)))
row_text[[11]] <- c(
  "  Toy and Small", purrr::map2_chr(sn_size1, sn_n, n_percent)
)
row_text[[12]] <- c(
  "  Medium", purrr::map2_chr(sn_size2, sn_n, n_percent)
)
row_text[[13]] <- c(
  "  Standard", purrr::map2_chr(sn_size3, sn_n, n_percent)
)
row_text[[14]] <- c(
  "  Large", purrr::map2_chr(sn_size4, sn_n, n_percent)
)
row_text[[15]] <- c(
  "  Giant", purrr::map2_chr(sn_size5, sn_n, n_percent)
)
row_text[[16]] <- c(
  "Follow-up in days for overweight/obese (median, IQR)",
  purrr::pmap_chr(list(oo_fu_q2, oo_fu_q1, oo_fu_q3), median_iqr)
)
row_text[[17]] <- c(
  "Follow-up in days for obese (median, IQR)",
  purrr::pmap_chr(list(ob_fu_q2, ob_fu_q1, ob_fu_q3), median_iqr)
)

# Assemble the table.
table1 <- do.call(rbind, row_text)

# Save the final output.
write.table(
  table1, file = "../output/table1.csv",
  sep = ",", row.names = FALSE, col.names = FALSE
)


# Table 2 - Unadjusted SN effect estimates  --------------------------

# Get group-specific counts.
sn_n <- table(dat$sn)

# Get outcome counts.
oo_ct <- with(dat, tapply(oo_event, sn, sum))
# Compute total length of follow-up in years.
oo_fu <- with(dat, tapply(oo_t2e, sn, sum) / 365.25)
# Compute crude incidence rates.
oo_rate <- oo_ct / oo_fu * 1000
# Compute the rate ratio and confidence limits. See Rothman et
# al. (2008, p 244).
oo_log_rate_ratio <- log(
  oo_rate[["Spayed/neutered"]] / oo_rate[["Intact"]])
oo_log_rate_ratio_se <- sqrt(sum(1 / oo_ct))
names(oo_log_rate_ratio_se) <- NULL
oo_rate_ratio <- exp(oo_log_rate_ratio)
oo_rate_ratio_ci <- exp(
  oo_log_rate_ratio + c(-1, 1) * qnorm(.975) * oo_log_rate_ratio_se)


# Repeat for obesity outcome.
ob_ct <- with(dat, tapply(obese_event, sn, sum))
ob_fu <- with(dat, tapply(obese_t2e, sn, sum) / 365.25)
ob_rate <- ob_ct / ob_fu * 1000
ob_log_rate_ratio <- log(
  ob_rate[["Spayed/neutered"]] / ob_rate[["Intact"]])
ob_log_rate_ratio_se <- sqrt(sum(1 / ob_ct))
names(ob_log_rate_ratio_se) <- NULL
ob_rate_ratio <- exp(ob_log_rate_ratio)
ob_rate_ratio_ci <- exp(
  ob_log_rate_ratio + c(-1, 1) * qnorm(.975) * ob_log_rate_ratio_se)

row_text <- list()
row_text[[1]] <- c(
  "", paste0(names(sn_n), ", n = ", format(sn_n, big.mark = ","))
)
row_text[[2]] <- c("Overweight/obese", rep("", length(sn_n)))
row_text[[3]] <- c(
  "Outcome events, n",
  purrr::map_chr(oo_ct, function(x) format(x, big.mark = ","))
)
row_text[[4]] <- c(
  "Total years of observation",
  purrr::map_chr(oo_fu, function(x) {
    format(round(x), big.mark = ",")
  })
)
row_text[[5]] <- c(
  "Crude incidence rates per 1,000 years of observation",
  purrr::map_chr(oo_rate, function(x) {
    format(round(x, 1), nsmall = 1)
  })
)
row_text[[6]] <- c(
  "Crude incidence rate ratio (95% CI)", "Ref",
  paste0(
    format(round(oo_rate_ratio, 2), nsmall = 2),
    " (",
    format(round(oo_rate_ratio_ci[[1]], 2), nsmall = 2),
    ", ",
    format(round(oo_rate_ratio_ci[[2]], 2), nsmall = 2),
    ")"
  )
)
row_text[[7]] <- c("Obese", rep("", length(sn_n)))
row_text[[8]] <- c(
  "Outcome events, n (%)",
  purrr::map_chr(ob_ct, function(x) format(x, big.mark = ","))
)
row_text[[9]] <- c(
  "Total years of observation",
  purrr::map_chr(ob_fu, function(x) {
    format(round(x), big.mark = ",")
  })
)
row_text[[10]] <- c(
  "Crude incidence rates per 1,000 years of observation",
  purrr::map_chr(ob_rate, function(x) {
    format(round(x, 1), nsmall = 1)
  })
)
row_text[[11]] <- c(
  "Crude incidence rate ratio (95% CI)", "Ref",
  paste0(
    format(round(ob_rate_ratio, 2), nsmall = 2),
    " (",
    format(round(ob_rate_ratio_ci[[1]], 2), nsmall = 2),
    ", ",
    format(round(ob_rate_ratio_ci[[2]], 2), nsmall = 2),
    ")"
  )
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
  age_group := data.table::fcase(
    ageYearsX <= 0.5, "3 mo to 6 mo",
    0.5 < ageYearsX & ageYearsX <= 1, "> 6 mo to 1 y",
    1 < ageYearsX & ageYearsX <= 2, "> 1 y to 2 y",
    2 < ageYearsX, "> 2 y"
  )
][,
  age_group := factor(
    age_group,
    levels = c(
      "3 mo to 6 mo", "> 6 mo to 1 y", "> 1 y to 2 y", "> 2 y")
  )
]

# Get group-specific counts.
age_n <- table(dat3$age_group)

# Get outcome counts.
oo_ct <- with(dat3, tapply(oo_event, age_group, sum))
# Compute total length of follow-up in years.
oo_fu <- with(dat3, tapply(oo_t2e, age_group, sum) / 365.25)
# Compute crude incidence rates.
oo_rate <- oo_ct / oo_fu * 1000
# Compute the rate ratio and confidence limits. See Rothman et
# al. (2008, p 244).
oo_log_rate_ratio <- c(
  log(oo_rate[["3 mo to 6 mo"]] / oo_rate[["> 6 mo to 1 y"]]),
  log(oo_rate[["> 1 y to 2 y"]] / oo_rate[["> 6 mo to 1 y"]]),
  log(oo_rate[["> 2 y"]] / oo_rate[["> 6 mo to 1 y"]])
)
oo_log_rate_ratio_se <- sqrt(sum(1 / oo_ct))
names(oo_log_rate_ratio_se) <- NULL
oo_rate_ratio <- exp(oo_log_rate_ratio)
oo_rate_ratio_lo <- exp(
  oo_log_rate_ratio - qnorm(.975) * oo_log_rate_ratio_se)
oo_rate_ratio_hi <- exp(
  oo_log_rate_ratio + qnorm(.975) * oo_log_rate_ratio_se)

# Repeat for obesity outcome.
ob_ct <- with(dat3, tapply(obese_event, age_group, sum))
ob_fu <- with(dat3, tapply(obese_t2e, age_group, sum) / 365.25)
ob_rate <- ob_ct / ob_fu * 1000
ob_log_rate_ratio <- c(
  log(ob_rate[["3 mo to 6 mo"]] / ob_rate[["> 6 mo to 1 y"]]),
  log(ob_rate[["> 1 y to 2 y"]] / ob_rate[["> 6 mo to 1 y"]]),
  log(ob_rate[["> 2 y"]] / ob_rate[["> 6 mo to 1 y"]])
)
ob_log_rate_ratio_se <- sqrt(sum(1 / ob_ct))
names(ob_log_rate_ratio_se) <- NULL
ob_rate_ratio <- exp(ob_log_rate_ratio)
ob_rate_ratio_lo <- exp(
  ob_log_rate_ratio - qnorm(.975) * ob_log_rate_ratio_se)
ob_rate_ratio_hi <- exp(
  ob_log_rate_ratio + qnorm(.975) * ob_log_rate_ratio_se)

row_text <- list()
row_text[[1]] <- c(
  "", paste0(names(age_n), ", n = ", format(age_n, big.mark = ","))
)
row_text[[2]] <- c("Overweight/obese", rep("", length(age_n)))
row_text[[3]] <- c(
  "Outcome events, n",
  purrr::map_chr(oo_ct, function(x) format(x, big.mark = ","))
)
row_text[[4]] <- c(
  "Total years of observation",
  purrr::map_chr(oo_fu, function(x) {
    format(round(x), big.mark = ",")
  })
)
row_text[[5]] <- c(
  "Crude incidence rates per 1,000 years of observation",
  purrr::map_chr(oo_rate, function(x) {
    format(round(x, 1), nsmall = 1)
  })
)
row_text[[6]] <- c(
  "Crude incidence rate ratio (95% CI)",
  paste0(
    format(round(oo_rate_ratio[[1]], 2), nsmall = 2),
    " (",
    format(round(oo_rate_ratio_lo[[1]], 2), nsmall = 2),
    ", ",
    format(round(oo_rate_ratio_hi[[1]], 2), nsmall = 2),
    ")"
  ),
  "Ref",
  paste0(
    format(round(oo_rate_ratio[[2]], 2), nsmall = 2),
    " (",
    format(round(oo_rate_ratio_lo[[2]], 2), nsmall = 2),
    ", ",
    format(round(oo_rate_ratio_hi[[2]], 2), nsmall = 2),
    ")"
  ),
  paste0(
    format(round(oo_rate_ratio[[3]], 2), nsmall = 2),
    " (",
    format(round(oo_rate_ratio_lo[[3]], 2), nsmall = 2),
    ", ",
    format(round(oo_rate_ratio_hi[[3]], 2), nsmall = 2),
    ")"
  )
)
row_text[[7]] <- c("Obese", rep("", length(age_n)))
row_text[[8]] <- c(
  "Outcome events, n",
  purrr::map_chr(ob_ct, function(x) format(x, big.mark = ","))
)
row_text[[9]] <- c(
  "Total years of observation",
  purrr::map_chr(ob_fu, function(x) {
    format(round(x), big.mark = ",")
  })
)
row_text[[10]] <- c(
  "Crude incidence rates per 1,000 years of observation",
  purrr::map_chr(ob_rate, function(x) {
    format(round(x, 1), nsmall = 1)
  })
)
row_text[[11]] <- c(
  "Crude incidence rate ratio (95% CI)",
  paste0(
    format(round(ob_rate_ratio[[1]], 2), nsmall = 2),
    " (",
    format(round(ob_rate_ratio_lo[[1]], 2), nsmall = 2),
    ", ",
    format(round(ob_rate_ratio_hi[[1]], 2), nsmall = 2),
    ")"
  ),
  "Ref",
  paste0(
    format(round(ob_rate_ratio[[2]], 2), nsmall = 2),
    " (",
    format(round(ob_rate_ratio_lo[[2]], 2), nsmall = 2),
    ", ",
    format(round(ob_rate_ratio_hi[[2]], 2), nsmall = 2),
    ")"
  ),
  paste0(
    format(round(ob_rate_ratio[[3]], 2), nsmall = 2),
    " (",
    format(round(ob_rate_ratio_lo[[3]], 2), nsmall = 2),
    ", ",
    format(round(ob_rate_ratio_hi[[3]], 2), nsmall = 2),
    ")"
  )
)

# Assemble the table.
table3 <- do.call(rbind, row_text)

# Save the final output.
write.table(
  table3, file = "../output/table3.csv",
  sep = ",", row.names = FALSE, col.names = FALSE
)
