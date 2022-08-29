# Header -------------------------------------------------------------
#
# Fit a Cox proportional hazards model as part of the sensitivity
# analysis of obesity for the ACC&D overweight/obesity analysis.
#
# John Sahrmann
# 20220829


# Setup --------------------------------------------------------------

library(data.table)
library(Hmisc)
library(magrittr)
library(rms)
library(survival)


# Input data ---------------------------------------------------------

source("./00-const-fn.R")
source("./02-read-cohort.R")


# Modeling -----------------------------------------------------------

# Set options for Hmisc/rms analysis functions.
Hmisc::units(dat$t2e_obese, "day")
dd <- datadist(dat)
options(datadist = "dd")

# Create the survival outcome object.
surv_ob <- dat %$% Surv(obese_t2e, obese_event)

# Fit the primary model.
model1 <- cph(
  surv_ob ~
    sn + rcs(ageYearsX, 5) + size + sex + mixed_breed +
    wellness_plan + rcs(weight, 3) + rcs(visitsPerYear, 3) +
    sn : rcs(ageYearsX, 5) + sn : size + sn : sex + sn : rcs(weight, 3) +
    size : rcs(ageYearsX, 5) + size : rcs(weight, 3) + size : sex +
    sex : rcs(ageYearsX, 5) + sex : rcs(weight, 3),
  data = dat, x = TRUE, y = TRUE, surv = TRUE)

anova(model1)
summary(model1)


# SN effect -----------------------

sn_ref_pts <- as.data.table(
  define_sn_reference_points(ages = c(0.25, seq(0.5, 6, by = 0.5))))

sn_est <- evaluate_sn_reference_points(sn_ref_pts, model1)

sn_results <- cbind(sn_ref_pts, sn_est)


# Age effect among SN -------------

age_among_sn_ref_pts <- data.table::as.data.table(
  define_age_reference_points(ages = c(0.25, seq(0.5, 6, by = 0.5))))

age_among_sn_est <- evaluate_age_among_sn_reference_points(
  age_among_sn_ref_pts, model1)

age_among_sn_results <- cbind(age_among_sn_ref_pts, age_among_sn_est)


# Exports ------------------------------------------------------------

save(
  sn_results, age_among_sn_results,
  file = "../data/ob-results.Rdata"
)


# Exports ------------------------------------------------------------

save(
  sn_results, age_among_sn_results,
  file = "../data/ob-results.Rdata"
)


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
# Age with largest hazard ratio within each breed size class and sex.
sn_results[,
  max_hr := max(.SD), by = c("size", "sex"), .SDcols = "hr"]
max_hrs <- sn_results[hr == max_hr]


# Age effect among SN -------------

# Smallest difference in risk by SN
age_among_sn_results[reference_age == 1.0][which.min(hr)]

# Largest difference in risk by SN
age_among_sn_results[wt_pctl == 50][which.max(hr)]

round(age_among_sn_results[wt_pctl == 50][which.max(hr)]$hr, digits = 2)
round(age_among_sn_results[wt_pctl == 50][which.max(hr)]$hi, digits = 2)
