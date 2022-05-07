# Front matter -------------------------------------------------------
#
# Cox proportional hazards modeling for the ACC&D sensitivity analysis
# of the effect of sterilization on risk of overweight/obese status.
#
# John Sahrmann
# 20220503


# Preface ------------------------------------------------------------

library(data.table)
library(EValue)
library(Hmisc)
library(magrittr)
library(rms)
library(survival)


# Input data ---------------------------------------------------------

# Read and join the covariates and outcomes data sets.
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

# Define additional variables for analysis, and declar factor
# variables.
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
    oo_t2e = fifelse(oo_t2e > 1825, 1825, oo_t2e),
    obese_event = fifelse(obese_t2e > 1825, 0, obese_event),
    obese_t2e = fifelse(obese_t2e > 1825, 1825, obese_t2e)
  )
]

# Table 2 counts (obese)
tab2obese_ct <- dat %$%
  table(sn, obese_event)
# Table 2 row percentages (obese)
tab2obese_rpct <- dat %$%
  round(proportions(table(sn, obese_event), margin = 1)*100)

# Combine and format the text for each outcome.
tab2obese <- paste0(
  format(tab2obese_ct, big.mark = ","), " (", tab2obese_rpct, ")"
)
# Assemble into a matrix with two rows and four columns.
table2 <- matrix(
  tab2obese,
  nrow = 2,
  dimnames = list(
    c("Intact", "Spayed/neutered"),
    c("CensoredObese", "OutcomeObese")
  )
) %>%
  as.data.frame()

# Save the final output.
write.csv(
  table2, file = "../output/table2obese.csv", row.names = TRUE
)

dat %$%
  tapply(oo_t2e, sn, summary)
dat %$%
  tapply(obese_t2e, sn, summary)


# Modeling -----------------------------------------------------------

# Set options for Hmisc/rms analysis functions.
Hmisc::units(dat$t2e_obese, "day")
dd <- datadist(dat)
options(datadist = "dd")

# Create the survival outcome object for the O/O outcome.
obeseSurv <- dat %$% Surv(obese_t2e, obese_event)

# Fit the primary model.
mod1 <- cph(
  obeseSurv ~
    sn + rcs(ageYearsX, 5) + size + sex + mixed_breed +
    wellness_plan + rcs(weight, 3) + rcs(visitsPerYear, 3) +
    sn : rcs(ageYearsX, 5) + sn : size + sn : sex + sn : rcs(weight, 3) +
    rcs(ageYearsX, 5) : size + rcs(weight, 3) : size +
    sex : size + sex : rcs(weight, 3),
  data = dat, x = TRUE, y = TRUE, surv = TRUE)

anova(mod1)
summary(mod1)

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
  readr::write_csv("../output/sTableC-obese-modelSummary.csv")

# Output a condensed version of the model summary table using just the
# median weight rows.
modelSummaryTable[wtPntl == 0.5, !"wtPntl"] %>%
  readr::write_csv("../output/sTableD-obese-modelSummaryMedWt.csv")


# Plotting -----------------------------------------------------------

cairo_pdf(
  "../output/fig/figC-obese-femaleAllWt.pdf", width = 7, height = 7
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
  "../output/fig/figC-obese-maleAllWt.pdf", width = 7, height = 7
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
  c("../output/fig/figC-obese-femaleAllWt.pdf",
    "../output/fig/figC-obese-maleAllWt.pdf"),
  "../output/fig/figC-obese-allWt.pdf"
)
file.remove(
  c("../output/fig/figC-obese-femaleAllWt.pdf",
    "../output/fig/figC-obese-maleAllWt.pdf")
)


cairo_pdf(
  "../output/fig/figD-obese-medWt.pdf", width = 7, height = 5
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
