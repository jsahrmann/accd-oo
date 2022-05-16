# Header -------------------------------------------------------------
#
# Fit a Cox proportional hazards model as part of the sensitivity
# analysis of obesity for the ACC&D overweight/obesity analysis.
#
# John Sahrmann
# 20220516


# Setup --------------------------------------------------------------

library(data.table)
library(EValue)
library(Hmisc)
library(magrittr)
library(rms)
library(survival)


# Input data ---------------------------------------------------------

source("./0w-const-fn.R")
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
  define_sn_reference_points(ages = seq(0.5, 6, by = 0.5)))

sn_est <- evaluate_sn_reference_points(sn_ref_pts, model1)
sn_e_val <- compute_e_values(sn_est)

sn_results <- cbind(sn_ref_pts, sn_est, sn_e_val)


# Age effect among SN -------------

age_among_sn_ref_pts <- data.table::as.data.table(
  define_age_reference_points(ages = seq(0.5, 6, by = 0.5)))

age_among_sn_est <- evaluate_age_among_sn_reference_points(
  age_among_sn_ref_pts, model1)
age_among_sn_e_val <- compute_e_values(age_among_sn_est)

age_among_sn_results <- cbind(
  age_among_sn_ref_pts, age_among_sn_est, age_among_sn_e_val)


# Exports ------------------------------------------------------------

save(
  sn_results, age_among_sn_results,
  file = "../data/ob-results.Rdata"
)
