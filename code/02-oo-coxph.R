# Front matter -------------------------------------------------------
#
# Primary Cox proportional hazards modeling for the ACC&D analysis of
# the effect of sterilization on risk of overweight/obese status.
#
# John Sahrmann
# 20220430


# Preface ------------------------------------------------------------

library(data.table)
library(Hmisc)
library(magrittr)
library(rms)
library(survival)


# Input data ---------------------------------------------------------

basedat <- readr::read_rds(
  paste0(
    "~/Dropbox/Banfield Dog Data/R Data Files (for analysis)/",
    "dogs_final.rds")
)
survdat <- readr::read_rds(
  paste0(
    "~/Dropbox/Banfield Dog Data/R Data Files (for analysis)/",
    "dogs_outcomeAndCensoringDates.rds")
)
dat <- basedat[survdat, on = c("id", "index_date")]
rm(basedat, survdat)


# Data management ----------------------------------------------------

dat[,
    `:=`(
    ageYearsX = ageDays / 365.25,
    ageYearsT = floor(ageDays / 365.25),
    ageYearsR = round(ageDays / 365.25),
    sex = factor(sex, levels = c("Male", "Female")),
    mixed_breed = factor(mixed_breed, levels = c("N", "Y")),
    size = factor(
      size, levels = c(
        "Standard", "Toy and Small", "Medium", "Large", "Giant")),
    sn = factor(
      sn, levels = 0:1, labels = c("Intact", "Spayed/neutered"))    
  )
]

# Cap follow-up at five years.
dat[,
  `:=`(
    oo_event = fifelse(oo_t2e > 1825, 0, oo_event),
    oo_t2e = fifelse(oo_t2e > 1825, 1825, oo_t2e)
  )
]

dat %$%
  table(sn, oo_event)
dat %$%
  tapply(oo_t2e, sn, summary)


# Modeling -----------------------------------------------------------

# Set options for Hmisc/rms analysis functions.
Hmisc::units(dat$t2e_oo, "day")
dd <- datadist(dat)
options(datadist = "dd")

# Create the survival outcome object for the O/O outcome.
ooSurv <- dat %$% Surv(oo_t2e, oo_event)

# Fit the primary model.
mod1 <- cph(
  ooSurv ~
    sn + rcs(ageYearsX, 5) + size + sex + mixed_breed +
    wellness_plan + rcs(weight, 4) + rcs(visitsPerYear, 3) +
    sn : rcs(ageYearsX, 5) + sn : size + sn : sex + sn : rcs(weight, 4) +
    rcs(ageYearsX, 5) : size + rcs(weight, 4) : size +
    sex : size + sex : rcs(weight, 4),
  data = dat, x = TRUE, y = TRUE, surv = TRUE)

anova(mod1)
summary(mod1)

# Define reference points at which to generate hazard ratios for
# effect plots.
ref_ageYearsX <- c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6)
ref_size <- levels(dat$size)
ref_sex <- levels(dat$sex)

# Initialize a list to hold the reference point values and hazard
# ratios. We'll convert this to a tibble shortly.
n <- length(ref_ageYearsX)*length(ref_size)*length(ref_sex)*3
modelSummary <- list(
  ageYears = numeric(n),
  size = character(n),
  sex = character(n),
  wtPntl = character(n),
  weight = numeric(n),
  hr = numeric(n),
  lb = numeric(n),
  ub = numeric(n)
)

# Evaluate the model at the reference points.
i <- 1
for (thisAge in ref_ageYearsX) {
  for (thisSize in ref_size) {
    for (thisSex in ref_sex) {
      for (pntl in c(.25, .5, .75)) {
        if (i %% 10 == 0) print(i)
        modelSummary$ageYears[[i]] <- thisAge
        modelSummary$size[[i]] <- thisSize
        modelSummary$sex[[i]] <- thisSex
        modelSummary$wtPntl[[i]] <- as.character(pntl)
        # We still need to get actual values for `weight`. For integer
        # reference values of age, use dogs of this size and sex and
        # where rounded continuous age (`ageYearsX`) equals the
        # reference value. For noninteger reference values of age, use
        # dogs of this size and sex and where rounded continuous age
        # equals the floor or ceiling of the reference value; then
        # average the quartiles of the two ages.
        if (thisAge %% 1 == 0) {
          modelSummary$weight[[i]] <- dat[
            ageYearsT == thisAge & size == thisSize & sex == thisSex,
            quantile(weight, pntl)]
        } else {
          modelSummary$weight[[i]] <- mean(
            c(dat[
                ageYearsT == floor(thisAge) & size == thisSize &
                sex == thisSex,
                quantile(weight, pntl)],
              dat[
                ageYearsT == ceiling(thisAge) & size == thisSize &
                sex == thisSex,
                quantile(weight, pntl)]
              )
          )
        }
        est <- summary(
          mod1,
          ageYearsX = modelSummary$ageYears[[i]],
          size = modelSummary$size[[i]],
          sex = modelSummary$sex[[i]],
          weight = modelSummary$weight[[i]]
        )
        snRow <- which(rownames(est) == "sn - Spayed/neutered:Intact")
        modelSummary$hr[[i]] <- est[snRow+1, "Effect"]
        modelSummary$lb[[i]] <- est[snRow+1, "Lower 0.95"]
        modelSummary$ub[[i]] <- est[snRow+1, "Upper 0.95"]
        i <- i + 1
      }
    }
  }
}

modelSummaryTable <- as.data.table(modelSummary)
modelSummaryTable

modelSummaryTable[wtPntl == 0.5] %>%
  ggplot(
    aes(x = ageYears, y = hr, ymin = lb, ymax = ub, colour = size)
  ) +
  geom_pointrange() +
  geom_line() +
  scale_colour_discrete("Breed Size") +
  facet_wrap(vars(sex))


modelSummaryTable %>%
  dplyr::filter(sex == "Male") %>%
  ggplot(
    aes(x = ageYears, y = hr, ymin = lb, ymax = ub, colour = wtPntl)
  ) +
  geom_pointrange() +
  geom_line() +
  facet_wrap(vars(size))

modelSummaryTable %>%
  dplyr::filter(sex == "Female") %>%
  ggplot(
    aes(x = ageYears, y = hr, ymin = lb, ymax = ub, colour = wtPntl)
  ) +
  geom_pointrange() +
  geom_line() +
  facet_wrap(vars(size))


i <- 1
for (thisAge in ref_ageYearsX) {
  for (thisSize in ref_size) {
    for (thisSex in ref_sex) {
      for (pntl in c(.25, .5, .75)) {
        ## if (i %% 10 == 0) print(i)
        print(paste("###", i , "###"))
        lwt <- dat[
          (thisAge-0.25) <= ageYearsX & ageYearsX < (thisAge+0.25) &
          size == thisSize & sex == thisSex,
          quantile(weight, pntl-.125)]
        uwt <- dat[
          (thisAge-0.25) <= ageYearsX & ageYearsX < (thisAge+0.25) &
          size == thisSize & sex == thisSex,
          quantile(weight, pntl+.125)]
        temp <- dat[
          (thisAge-0.25) <= ageYearsX & ageYearsX < (thisAge+0.25) &
          size == thisSize & sex == thisSex &
          lwt <= weight & weight < uwt
        ]
        temp[,
          `:=`(
            reflage = thisAge-0.25,
            refuage = thisAge+0.25,
            refsize = thisSize,
            refsex = thisSex,
            refwtpct = pntl,
            reflwt = lwt,
            refuwt = uwt
          )
        ]
        print(
          paste(
            paste("reflage =", thisAge-0.25),
            paste("refuage =", thisAge+0.25),
            paste("refsize =", thisSize),
            paste("refsex =", thisSex),
            paste("refwtpct =", pntl),
            paste("reflwt =", lwt),
            paste("refuwt =", uwt),
            sep = "; ")
        )
        assign(
          stringr::str_c("dat", stringr::str_pad(i, 3, "left", 0)),
          temp
        )
        i <- i + 1
      }
    }
  }
}

dat001
m001 <- cph(Surv(oo_t2e, oo_event) ~ sn, data = dat001)
summary(m001)
modelSummaryTable[1, ]
dat001 %$% proportions(table(sn, oo_event), 1)

dat072
m072 <- cph(Surv(oo_t2e, oo_event) ~ sn, data = dat072)
summary(m072)
modelSummaryTable[72, ]
dat072 %$% proportions(table(sn, oo_event), 1)

unadjustedModels <- list(
  unadjhr = numeric(n),
  unadjlb = numeric(n),
  unadjub = numeric(n)
)
for (i in seq_len(n)) {
  unadjustedModel <- cph(
    Surv(oo_t2e, oo_event) ~ sn,
    data = get(stringr::str_c("dat", stringr::str_pad(i, 3, "left", 0))))
  if (length(unadjustedModel) > 1) {
    unadjustedModelSummary <- summary(unadjustedModel)
    unadjustedModels$unadjhr[[i]] <- unadjustedModelSummary[2, "Effect"]
    unadjustedModels$unadjlb[[i]] <- unadjustedModelSummary[2, "Lower 0.95"]
    unadjustedModels$unadjub[[i]] <- unadjustedModelSummary[2, "Upper 0.95"]
  } else {
    unadjustedModels$unadjhr[[i]] <- NA
    unadjustedModels$unadjlb[[i]] <- NA
    unadjustedModels$unadjub[[i]] <- NA
  }
}
unadjustedModels <- as.data.table(unadjustedModels)
unadjustedModels[,
  ne := fifelse(is.na(unadjhr) | is.infinite(unadjub) | unadjub > 20, 1, 0)
]

modelComparison <- cbind(modelSummaryTable, unadjustedModels)
xx <- tibble::as_tibble(modelComparison)
xx %>%
  dplyr::filter(ne != 1) %>%
  ggplot(aes(x = hr, y = unadjhr)) +
  geom_point(aes(colour = size, shape = sex)) +
  geom_smooth() +
  geom_abline(slope = 1, intercept = 0)
