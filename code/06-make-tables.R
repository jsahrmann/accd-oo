# Header -------------------------------------------------------------
#
# Produce output for manuscript tables.
#
# John Sahrmann
# 20220731


# Setup --------------------------------------------------------------

library(openxlsx)
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
sn_visits_q2 <- with(dat1, tapply(visitsPerYear, sn, median))
sn_visits_q1 <- with(dat1, tapply(visitsPerYear, sn, quantile, .25))
sn_visits_q3 <- with(dat1, tapply(visitsPerYear, sn, quantile, .75))
oo_fu_q2 <- with(dat1, tapply(oo_t2e, sn, median))
oo_fu_q1 <- with(dat1, tapply(oo_t2e, sn, quantile, .25))
oo_fu_q3 <- with(dat1, tapply(oo_t2e, sn, quantile, .75))
ob_fu_q2 <- with(dat1, tapply(obese_t2e, sn, median))
ob_fu_q1 <- with(dat1, tapply(obese_t2e, sn, quantile, .25))
ob_fu_q3 <- with(dat1, tapply(obese_t2e, sn, quantile, .75))

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
  "Visits per year (median, IQR)",
  purrr::pmap_chr(
    list(sn_visits_q2, sn_visits_q1, sn_visits_q3), median_iqr)
); i <- i + 1
row_text[[i]] <- c(
  "Follow-up in days for overweight/obese (median, IQR)",
  purrr::pmap_chr(list(oo_fu_q2, oo_fu_q1, oo_fu_q3), median_iqr)
); i <- i + 1
row_text[[i]] <- c(
  "Follow-up in days for obese (median, IQR)",
  purrr::pmap_chr(list(ob_fu_q2, ob_fu_q1, ob_fu_q3), median_iqr)
); i <- i + 1

# Assemble the table.
table1 <- do.call(rbind, row_text) |>
  as.data.frame()

# Save the final output.
openxlsx::write.xlsx(
  table1, file = "../output/table1.xlsx",
  colNames = FALSE, colWidths = "auto"
)


# sTable 1 - Unadjusted SN effect estimates  -------------------------

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

row_text <- list(); i <- 1
row_text[[i]] <- c(
  "", paste0(names(sn_n), ", n = ", format(sn_n, big.mark = ","))
); i <- i + 1
row_text[[i]] <- c(
  "Overweight/obese", rep("", length(sn_n))
); i <- i + 1
row_text[[i]] <- c(
  "Outcome events, n",
  purrr::map_chr(oo_ct, function(x) format(x, big.mark = ","))
); i <- i + 1
row_text[[i]] <- c(
  "Total years of observation",
  purrr::map_chr(oo_fu, function(x) {
    format(round(x), big.mark = ",")
  })
); i <- i + 1
row_text[[i]] <- c(
  "Crude incidence rates per 1,000 years of observation",
  purrr::map_chr(oo_rate, function(x) {
    format(round(x, 1), nsmall = 1)
  })
); i <- i + 1
row_text[[i]] <- c(
  "Crude incidence rate ratio (95% CI)", "Ref",
  paste0(
    format(round(oo_rate_ratio, 2), nsmall = 2),
    " (",
    format(round(oo_rate_ratio_ci[[1]], 2), nsmall = 2),
    ", ",
    format(round(oo_rate_ratio_ci[[2]], 2), nsmall = 2),
    ")"
  )
); i <- i + 1
row_text[[i]] <- c(
  "Obese", rep("", length(sn_n))
); i <- i + 1
row_text[[i]] <- c(
  "Outcome events, n (%)",
  purrr::map_chr(ob_ct, function(x) format(x, big.mark = ","))
); i <- i + 1
row_text[[i]] <- c(
  "Total years of observation",
  purrr::map_chr(ob_fu, function(x) {
    format(round(x), big.mark = ",")
  })
); i <- i + 1
row_text[[i]] <- c(
  "Crude incidence rates per 1,000 years of observation",
  purrr::map_chr(ob_rate, function(x) {
    format(round(x, 1), nsmall = 1)
  })
); i <- i + 1
row_text[[i]] <- c(
  "Crude incidence rate ratio (95% CI)", "Ref",
  paste0(
    format(round(ob_rate_ratio, 2), nsmall = 2),
    " (",
    format(round(ob_rate_ratio_ci[[1]], 2), nsmall = 2),
    ", ",
    format(round(ob_rate_ratio_ci[[2]], 2), nsmall = 2),
    ")"
  )
); i <- i + 1

# Assemble the table.
stable1 <- do.call(rbind, row_text) |>
  as.data.frame()

# Save the final output.
openxlsx::write.xlsx(
  stable1, file = "../output/s_table1.xlsx",
  colNames = FALSE, colWidths = "auto"
)


# sTable 2 - Unadjusted age among SN effect estimates  ---------------

dat3 <- data.table::copy(dat)[
 sn == "Spayed/neutered",
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
  log(oo_rate[["3 mo to <6 mo"]] / oo_rate[["6 mo to <1 y"]]),
  log(oo_rate[["1 y to <2 y"]] / oo_rate[["6 mo to <1 y"]]),
  log(oo_rate[["2+ y"]] / oo_rate[["6 mo to <1 y"]])
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
  log(ob_rate[["3 mo to <6 mo"]] / ob_rate[["6 mo to <1 y"]]),
  log(ob_rate[["1 y to <2 y"]] / ob_rate[["6 mo to <1 y"]]),
  log(ob_rate[["2+ y"]] / ob_rate[["6 mo to <1 y"]])
)
ob_log_rate_ratio_se <- sqrt(sum(1 / ob_ct))
names(ob_log_rate_ratio_se) <- NULL
ob_rate_ratio <- exp(ob_log_rate_ratio)
ob_rate_ratio_lo <- exp(
  ob_log_rate_ratio - qnorm(.975) * ob_log_rate_ratio_se)
ob_rate_ratio_hi <- exp(
  ob_log_rate_ratio + qnorm(.975) * ob_log_rate_ratio_se)

row_text <- list(); i <- 1
row_text[[i]] <- c(
  "", paste0(names(age_n), ", n = ", format(age_n, big.mark = ","))
); i <- i + 1
row_text[[i]] <- c(
  "Overweight/obese", rep("", length(age_n))
); i <- i + 1
row_text[[i]] <- c(
  "Outcome events, n",
  purrr::map_chr(oo_ct, function(x) format(x, big.mark = ","))
); i <- i + 1
row_text[[i]] <- c(
  "Total years of observation",
  purrr::map_chr(oo_fu, function(x) {
    format(round(x), big.mark = ",")
  })
); i <- i + 1
row_text[[i]] <- c(
  "Crude incidence rates per 1,000 years of observation",
  purrr::map_chr(oo_rate, function(x) {
    format(round(x, 1), nsmall = 1)
  })
); i <- i + 1
row_text[[i]] <- c(
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
); i <- i + 1
row_text[[i]] <- c(
  "Obese", rep("", length(age_n))
); i <- i + 1
row_text[[i]] <- c(
  "Outcome events, n",
  purrr::map_chr(ob_ct, function(x) format(x, big.mark = ","))
); i <- i + 1
row_text[[i]] <- c(
  "Total years of observation",
  purrr::map_chr(ob_fu, function(x) {
    format(round(x), big.mark = ",")
  })
); i <- i + 1
row_text[[i]] <- c(
  "Crude incidence rates per 1,000 years of observation",
  purrr::map_chr(ob_rate, function(x) {
    format(round(x, 1), nsmall = 1)
  })
); i <- i + 1
row_text[[i]] <- c(
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
); i <- i + 1

# Assemble the table.
stable2 <- do.call(rbind, row_text) |>
  as.data.frame()

# Save the final output.
openxlsx::write.xlsx(
  stable2, file = "../output/s_table2.xlsx",
  colNames = FALSE, colWidths = "auto"
)


## Table X - Unadjusted SN effect estimates, large breeds ------------

large <- subset(dat, size == "Large")

sn_n <- table(large$sn)

oo_ct <- with(large, tapply(oo_event, sn, sum))
oo_fu <- with(large, tapply(oo_t2e, sn, sum) / 365.25)
oo_rate <- oo_ct / oo_fu * 1000
oo_log_rate_ratio <- log(
  oo_rate[["Spayed/neutered"]] / oo_rate[["Intact"]])
oo_log_rate_ratio_se <- sqrt(sum(1 / oo_ct))
names(oo_log_rate_ratio_se) <- NULL
oo_rate_ratio <- exp(oo_log_rate_ratio)
oo_rate_ratio_ci <- exp(
  oo_log_rate_ratio + c(-1, 1) * qnorm(.975) * oo_log_rate_ratio_se)

ob_ct <- with(large, tapply(obese_event, sn, sum))
ob_fu <- with(large, tapply(obese_t2e, sn, sum) / 365.25)
ob_rate <- ob_ct / ob_fu * 1000
ob_log_rate_ratio <- log(
  ob_rate[["Spayed/neutered"]] / ob_rate[["Intact"]])
ob_log_rate_ratio_se <- sqrt(sum(1 / ob_ct))
names(ob_log_rate_ratio_se) <- NULL
ob_rate_ratio <- exp(ob_log_rate_ratio)
ob_rate_ratio_ci <- exp(
  ob_log_rate_ratio + c(-1, 1) * qnorm(.975) * ob_log_rate_ratio_se)

row_text <- list()
row_text[[1]] <- c("Large Breed Dogs", rep("", length(sn_n)))
row_text[[2]] <- c(
  "", paste0(names(sn_n), ", n = ", format(sn_n, big.mark = ","))
)
row_text[[3]] <- c("Overweight/obese", rep("", length(sn_n)))
row_text[[4]] <- c(
  "Outcome events, n",
  purrr::map_chr(oo_ct, function(x) format(x, big.mark = ","))
)
row_text[[5]] <- c(
  "Total years of observation",
  purrr::map_chr(oo_fu, function(x) {
    format(round(x), big.mark = ",")
  })
)
row_text[[6]] <- c(
  "Crude incidence rates per 1,000 years of observation",
  purrr::map_chr(oo_rate, function(x) {
    format(round(x, 1), nsmall = 1)
  })
)
row_text[[7]] <- c(
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
row_text[[8]] <- c("Obese", rep("", length(sn_n)))
row_text[[9]] <- c(
  "Outcome events, n (%)",
  purrr::map_chr(ob_ct, function(x) format(x, big.mark = ","))
)
row_text[[10]] <- c(
  "Total years of observation",
  purrr::map_chr(ob_fu, function(x) {
    format(round(x), big.mark = ",")
  })
)
row_text[[11]] <- c(
  "Crude incidence rates per 1,000 years of observation",
  purrr::map_chr(ob_rate, function(x) {
    format(round(x, 1), nsmall = 1)
  })
)
row_text[[12]] <- c(
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
table <- do.call(rbind, row_text)

# Save the final output.
write.table(
  table, file = "../output/table_crude_large_breeds.csv",
  sep = ",", row.names = FALSE, col.names = FALSE
)


## Table Y - Unadjusted SN effect estimates, Golden Retrievers -------

golden <- subset(dat, breed == "Golden Retriever")

sn_n <- table(golden$sn)

oo_ct <- with(golden, tapply(oo_event, sn, sum))
oo_fu <- with(golden, tapply(oo_t2e, sn, sum) / 365.25)
oo_rate <- oo_ct / oo_fu * 1000
oo_log_rate_ratio <- log(
  oo_rate[["Spayed/neutered"]] / oo_rate[["Intact"]])
oo_log_rate_ratio_se <- sqrt(sum(1 / oo_ct))
names(oo_log_rate_ratio_se) <- NULL
oo_rate_ratio <- exp(oo_log_rate_ratio)
oo_rate_ratio_ci <- exp(
  oo_log_rate_ratio + c(-1, 1) * qnorm(.975) * oo_log_rate_ratio_se)

ob_ct <- with(golden, tapply(obese_event, sn, sum))
ob_fu <- with(golden, tapply(obese_t2e, sn, sum) / 365.25)
ob_rate <- ob_ct / ob_fu * 1000
ob_log_rate_ratio <- log(
  ob_rate[["Spayed/neutered"]] / ob_rate[["Intact"]])
ob_log_rate_ratio_se <- sqrt(sum(1 / ob_ct))
names(ob_log_rate_ratio_se) <- NULL
ob_rate_ratio <- exp(ob_log_rate_ratio)
ob_rate_ratio_ci <- exp(
  ob_log_rate_ratio + c(-1, 1) * qnorm(.975) * ob_log_rate_ratio_se)

row_text <- list()
row_text[[1]] <- c("Golden Retrievers", rep("", length(sn_n)))
row_text[[2]] <- c(
  "", paste0(names(sn_n), ", n = ", format(sn_n, big.mark = ","))
)
row_text[[3]] <- c("Overweight/obese", rep("", length(sn_n)))
row_text[[4]] <- c(
  "Outcome events, n",
  purrr::map_chr(oo_ct, function(x) format(x, big.mark = ","))
)
row_text[[5]] <- c(
  "Total years of observation",
  purrr::map_chr(oo_fu, function(x) {
    format(round(x), big.mark = ",")
  })
)
row_text[[6]] <- c(
  "Crude incidence rates per 1,000 years of observation",
  purrr::map_chr(oo_rate, function(x) {
    format(round(x, 1), nsmall = 1)
  })
)
row_text[[7]] <- c(
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
row_text[[8]] <- c("Obese", rep("", length(sn_n)))
row_text[[9]] <- c(
  "Outcome events, n (%)",
  purrr::map_chr(ob_ct, function(x) format(x, big.mark = ","))
)
row_text[[10]] <- c(
  "Total years of observation",
  purrr::map_chr(ob_fu, function(x) {
    format(round(x), big.mark = ",")
  })
)
row_text[[11]] <- c(
  "Crude incidence rates per 1,000 years of observation",
  purrr::map_chr(ob_rate, function(x) {
    format(round(x, 1), nsmall = 1)
  })
)
row_text[[12]] <- c(
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
table <- do.call(rbind, row_text)

# Save the final output.
write.table(
  table, file = "../output/table_crude_golden_retrievers.csv",
  sep = ",", row.names = FALSE, col.names = FALSE
)


## Table Z - Unadjusted SN effect estimates, large breeds sans Golden
## Retrievers --------------------------------------------------------

large_sans_golden <- subset(
  dat, size == "Large" & breed != "Golden Retriever")

sn_n <- table(large_sans_golden$sn)

oo_ct <- with(large_sans_golden, tapply(oo_event, sn, sum))
oo_fu <- with(large_sans_golden, tapply(oo_t2e, sn, sum) / 365.25)
oo_rate <- oo_ct / oo_fu * 1000
oo_log_rate_ratio <- log(
  oo_rate[["Spayed/neutered"]] / oo_rate[["Intact"]])
oo_log_rate_ratio_se <- sqrt(sum(1 / oo_ct))
names(oo_log_rate_ratio_se) <- NULL
oo_rate_ratio <- exp(oo_log_rate_ratio)
oo_rate_ratio_ci <- exp(
  oo_log_rate_ratio + c(-1, 1) * qnorm(.975) * oo_log_rate_ratio_se)


ob_ct <- with(large_san_golden, tapply(obese_event, sn, sum))
ob_fu <- with(large_sans_golden, tapply(obese_t2e, sn, sum) / 365.25)
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
  "Large Breed Dogs, Excluding Golden Retrievers",
  rep("", length(sn_n)))
row_text[[2]] <- c(
  "", paste0(names(sn_n), ", n = ", format(sn_n, big.mark = ","))
)
row_text[[3]] <- c("Overweight/obese", rep("", length(sn_n)))
row_text[[4]] <- c(
  "Outcome events, n",
  purrr::map_chr(oo_ct, function(x) format(x, big.mark = ","))
)
row_text[[5]] <- c(
  "Total years of observation",
  purrr::map_chr(oo_fu, function(x) {
    format(round(x), big.mark = ",")
  })
)
row_text[[6]] <- c(
  "Crude incidence rates per 1,000 years of observation",
  purrr::map_chr(oo_rate, function(x) {
    format(round(x, 1), nsmall = 1)
  })
)
row_text[[7]] <- c(
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
row_text[[8]] <- c("Obese", rep("", length(sn_n)))
row_text[[9]] <- c(
  "Outcome events, n (%)",
  purrr::map_chr(ob_ct, function(x) format(x, big.mark = ","))
)
row_text[[10]] <- c(
  "Total years of observation",
  purrr::map_chr(ob_fu, function(x) {
    format(round(x), big.mark = ",")
  })
)
row_text[[11]] <- c(
  "Crude incidence rates per 1,000 years of observation",
  purrr::map_chr(ob_rate, function(x) {
    format(round(x, 1), nsmall = 1)
  })
)
row_text[[12]] <- c(
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
table <- do.call(rbind, row_text)

# Save the final output.
write.table(
  table,
  file = paste0(
    "../output/",
    "table_crude_large_breeds_excluding_golden_retrievers.csv"),
  sep = ",", row.names = FALSE, col.names = FALSE
)
