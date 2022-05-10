# Header -------------------------------------------------------------
#
# Fit a Cox proportional hazards model as part of the primary analysis
# for the ACC&D overweight/obesity analysis.
#
# John Sahrmann
# 20220510


# Setup --------------------------------------------------------------

library(data.table)
library(EValue)
library(Hmisc)
library(magrittr)
library(rms)
library(survival)


# Input data ---------------------------------------------------------

source("./02-read-cohort.R")


# Modeling -----------------------------------------------------------

# Set options for Hmisc/rms analysis functions.
Hmisc::units(dat$t2e_oo, "day")
dd <- datadist(dat)
options(datadist = "dd")

# Create the survival outcome object.
surv_oo <- dat %$% Surv(oo_t2e, oo_event)

# Fit the primary model.
model1 <- cph(
  surv_oo ~
    sn + rcs(ageYearsX, 5) + size + sex + mixed_breed +
    wellness_plan + rcs(weight, 3) + rcs(visitsPerYear, 3) +
    sn : rcs(ageYearsX, 5) + sn : size + sn : sex + sn : rcs(weight, 3) +
    size : rcs(ageYearsX, 5) + size : rcs(weight, 3) + size : sex +
    sex : rcs(ageYearsX, 5) + sex : rcs(weight, 3),
  data = dat, x = TRUE, y = TRUE, surv = TRUE)

anova(model1)
summary(model1)

# Define reference points at which to generate hazard ratios for
# effect plots.
ref_ageYearsX <- c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6)
ref_size <- c("Toy and Small", "Standard", "Medium", "Large", "Giant")
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
          model1,
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

modelSummaryTable <- as.data.table(modelSummary)[,
  size := factor(
    size,
    levels = c(
      "Toy and Small", "Standard", "Medium", "Large", "Giant")
  )
]
modelSummaryTable

# Get e-values for the HR estimates and lower 95% confidence
# limits. Unfortunately, `EValue::evalues.HR` isn't vectorized, so we
# instead wrap it in an ugly anonymous function. The output of
# `EValue::evalues.HR` is a data.frame, with the first row being the
# estimated risk ratio and the second being the e-values. We select
# the row of e-values, prevent `[` from dropping a dimension, then
# send the output to `data.table::as.data.table` and
# `data.table::rbindlist` to produce a single data set.
eVals <- modelSummaryTable[, .(hr, lb, ub)] %>%
  apply(
    MARGIN = 1,
    FUN = function(x) {
      EValue::evalues.HR(
        est = x[["hr"]], lo = x[["lb"]], hi = x[["ub"]], rare = FALSE
      )["E-values", c("point", "lower"), drop = FALSE] %>%
        data.table::as.data.table()
    }
  ) %>%
  data.table::rbindlist()

# Add the e-values to the model summary table. Recall that `..` tells
# data.table to look for the object in the global environment.
modelSummaryTable[,
  `:=`(
    eVal = ..eVals$point,
    eValLo = ..eVals$lower
  )
]

# Output a full version of the model summary table.
modelSummaryTable %>%
  readr::write_csv("../output/sTableA-oo-modelSummary.csv")

# Output a condensed version of the model summary table using just the
# median weight rows.
modelSummaryTable[wtPntl == 0.5, !"wtPntl"] %>%
  readr::write_csv("../output/sTableB-oo-modelSummaryMedWt.csv")


# Plotting -----------------------------------------------------------

cairo_pdf(
  "../output/fig/figA-oo-femaleAllWt.pdf", width = 7, height = 7
)
modelSummaryTable[sex == "Female"] %>%
  ggplot(
    aes(x = ageYears, y = hr, ymin = lb, ymax = ub, colour = wtPntl)
  ) +
  geom_pointrange() +
  geom_line() +
  scale_colour_discrete("Weight Quartile") +
  labs(title = "Female Dogs") +
  xlab("\nAge at Index") +
  ylab("Hazard Ratio of Spayed/Neutered vs. Intact\n") +
  facet_wrap(vars(size)) +
  theme_gray(base_size = 12) +
  theme(
    legend.position = "bottom"
  )
dev.off()

cairo_pdf(
  "../output/fig/figA-oo-maleAllWt.pdf", width = 7, height = 7
)
modelSummaryTable[sex == "Male"] %>%
  ggplot(
    aes(x = ageYears, y = hr, ymin = lb, ymax = ub, colour = wtPntl)
  ) +
  geom_pointrange() +
  geom_line() +
  scale_colour_discrete("Weight Quartile") +
  labs(title = "Male Dogs") +
  xlab("\nAge at Index") +
  ylab("Hazard Ratio of Spayed/Neutered vs. Intact\n") +
  facet_wrap(vars(size)) +
  theme_gray(base_size = 12) +
  theme(
    legend.position = "bottom"
  )
dev.off()

pdftools::pdf_combine(
  c("../output/fig/figA-oo-femaleAllWt.pdf",
    "../output/fig/figA-oo-maleAllWt.pdf"),
  "../output/fig/figA-oo-allWt.pdf"
)
file.remove(
  c("../output/fig/figA-oo-femaleAllWt.pdf",
    "../output/fig/figA-oo-maleAllWt.pdf")
)


cairo_pdf(
  "../output/fig/figB-oo-medWt.pdf", width = 7, height = 5
)
modelSummaryTable[wtPntl == 0.5] %>%
  ggplot(
    aes(x = ageYears, y = hr, ymin = lb, ymax = ub, colour = size)
  ) +
  geom_pointrange() +
  geom_line() +
  labs(title = "Dogs at Median Weight") +
  xlab("\nAge at Index") +
  ylab("Hazard Ratio of Spayed/Neutered vs. Intact\n") +
  scale_colour_ordinal("Breed Size") +
  facet_wrap(vars(sex)) +
  theme_gray(base_size = 11) +
  theme(
    legend.position = "bottom"
  )
dev.off()


# Age effect among SN ------------------------------------------------

ggplot(
  Predict(
    model1,
    ageYearsX, size = "Toy and Small", sex = "Male", weight = 9.8,
    sn = "Spayed/neutered"
  )
)

n2 <- length(ref_size) * length(ref_ageYearsX) * length(ref_ageYearsX)
ageEffectAmongSN <- list(
  ageYears1 = numeric(n2), ageYears2 = numeric(n2),
  size = numeric(n2)
)

# Evaluate the model at the reference points.
i2 <- 1
for (thisSize in ref_size) {
  for (thisAge in ref_ageYearsX) {
    for (thatAge in ref_ageYearsX) {
      ageEffectAmongSN$size[[i2]] <- thisSize
      ageEffectAmongSN$ageYears1[[i2]] <- thisAge
      ageEffectAmongSN$ageYears2[[i2]] <- thatAge
      est <- summary(
        model1,
        size = ageEffectAmongSN$size[[i2]],
        ageYearsX = c(
          ageEffectAmongSN$ageYears1[[i2]],
          ageEffectAmongSN$ageYears2[[i2]]
        )
      )
      ageRow <- which(rownames(est) == "ageYearsX")
      ageEffectAmongSN$hr[i2] <- est[ageRow+1, "Effect"]
      ageEffectAmongSN$lo[i2] <- est[ageRow+1, "Lower 0.95"]
      ageEffectAmongSN$hi[i2] <- est[ageRow+1, "Upper 0.95"]
      i2 <- i2 + 1
    }
  }
}

agePts <- c(0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 5.0, 6.0)
ageEffectAmongSN <- as.data.table(ageEffectAmongSN)[
  ageYears1 %in% agePts & ageYears2 %in% agePts
][,
  `:=`(
    size = factor(
      size,
      levels = c(
        "Toy and Small", "Standard", "Medium", "Large", "Giant")
    ),
    ageYears1 = factor(
      ageYears1,
      levels = agePts,
      labels = paste("Spayed/neutered at", agePts, "years")
    )
  )
]

ageEffectAmongSN


# Plotting ------------------------

cairo_pdf(
  "../output/fig/fig-oo-age-effect-among-SN.pdf",
  width = 10, height = 7
)
ageEffectAmongSN %>%
  ggplot(
    aes(x = ageYears2, y = hr, ymin = lo, ymax = hi, colour = size)
  ) +
  geom_pointrange(size = 0.25) +
  geom_line() +
  xlab("\nAge at Spay/neuter") +
  ylab("Hazard Ratio of Age\n") +
  scale_y_log10() +
  scale_colour_ordinal("Breed Size") +
  facet_wrap(vars(ageYears1), nrow = 2) +
  theme_gray(base_size = 11) +
  theme(
    legend.position = "bottom"
  )
dev.off()

ageEffectAmongSN[, .(size, ageYears1, ageYears2, hr, lo, hi)] %>%
readr::write_csv("../output/table-oo-age-effect-among-SN.csv")


# Crude estimates ----------------------------------------------------

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
