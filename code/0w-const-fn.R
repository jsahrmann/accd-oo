# Header -------------------------------------------------------------
#
# Define constants and helper functions.
#
# John Sahrmann
# 20220510


# Setup --------------------------------------------------------------

source("./02-read-cohort.R")


# Function definitions -----------------------------------------------


age_ref_pts <- seq(0.5, max(dat$ageYearsX), by = 0.5)
size_ref_pts <- levels(dat$size)
sex_ref_pts <- levels(dat$sex)
wt_pctl <- c(.25, .5, .75)

x <- expand.grid(age = age_ref_pts, size = size_ref_pts, sex = sex_ref_pts, wt_pctl = wt_pctl)


get_wt_qntl <- function(x) {
  age <- as.double(x[["age"]])
  size <- x[["size"]]
  sex <- x[["sex"]]
  wt_pctl <- as.double(x[["wt_pctl"]])
  print(paste(age, size, sex, wt_pctl))
  if (age %% 1 == 0) {
    print(dat[ageYearsR == age & size == size & sex == sex])
    dat[ageYearsR == age & size == size & sex == sex, quantile(weight, wt_pctl)]
  } else {
    mean(
      c(dat[ageYearsR == floor(age) & size == size & sex == sex, quantile(weight, wt_pctl)],
        dat[ageYearsR == ceiling(age) & size == size & sex == sex, quantile(weight, wt_pctl)]))
  }
}

get_wt_qntl <- function(x) {
  this_age <- as.double(x[["age"]])
  this_size <- x[["size"]]
  this_sex <- x[["sex"]]
  this_wt_pctl <- as.double(x[["wt_pctl"]])
  print(paste(this_age, this_size, this_sex, this_wt_pctl))
  if (this_age %% 1 == 0) {
    dat[ageYearsT == this_age & size == this_size & sex == this_sex, quantile(weight, this_wt_pctl)]
  } else {
    mean(
      c(dat[ageYearsT == floor(this_age) & size == this_size & sex == this_sex, quantile(weight, this_wt_pctl)],
        dat[ageYearsT == ceiling(this_age) & size == this_size & sex == this_sex, quantile(weight, this_wt_pctl)]))
  }
}


xx <- head(x, n = 20)
xx$weight <- apply(xx, 1, get_wt_qntl)



define_sn_reference_points <- function(
  sizes = NULL, sexes = NULL, ages = NULL, weight_pctls = NULL
) {
  # Define a helper function to get the actual values of weight for
  # the requested percentile.
  get_wt_qntl <- function(x) {
    if (as.double(x[["age"]]) %% 1 == 0) {
      dat[
        ageYearsR == as.double(x[["age"]]) & size == x[["size"]]
        & sex == x[["sex"]],
        quantile(weight, as.double(x[["wt_pctl"]]) / 100)]
    } else {
      mean(
        c(dat[
            ageYearsR == floor(as.double(x[["age"]]))
            & size == x[["size"]] & sex == x[["sex"]],
            quantile(weight, as.double(x[["wt_pctl"]]) / 100)],
          dat[
            ageYearsR == ceiling(as.double(x[["age"]]))
            & size == x[["size"]] & sex == x[["sex"]],
            quantile(weight, as.double(x[["wt_pctl"]]) / 100)]))
    }
  }

  if (!is.null(sizes)) {
    size_ref_pts <- sizes
  } else {
    size_ref_pts <- levels(dat$size)
  }
  if (!is.null(sexes)) {
    sex_ref_pts <- sexes
  } else {
    sex_ref_pts <- levels(dat$sex)
  }
  if (!is.null(ages)) {
    age_ref_pts <- ages
  } else {
    age_ref_pts <- seq(0.5, max(dat$ageYearsX), by = 0.5)
  }
  if (!is.null(weight_pctls)) {
    wt_pctl <- weight_pctls
  } else {
    wt_pctl <- c(25, 50, 75)
  }

  ref_ds <- expand.grid(
    size = size_ref_pts, sex = sex_ref_pts, age = age_ref_pts,
    wt_pctl = wt_pctl)
  ref_ds$weight <- apply(ref_ds, 1, get_wt_qntl)
  ref_ds
}



ref <- data.table::as.data.table(
  define_sn_reference_points(ages = seq(0.5, 6, by = 0.5)))

ref <- data.table::as.data.table(
  define_sn_reference_points(ages = seq(0.5, 6, by = 0.5), weight_pctls = 50))


define_age_reference_points <- function(
  sizes = NULL, sexes = NULL, ages = NULL
) {
  if (!is.null(sizes)) {
    size_ref_pts <- sizes
  } else {
    size_ref_pts <- levels(dat$size)
  }
  if (!is.null(sexes)) {
    sex_ref_pts <- sexes
  } else {
    sex_ref_pts <- levels(dat$sex)
  }
  if (!is.null(ages)) {
    age_ref_pts <- ages
  } else {
    age_ref_pts <- seq(0.5, max(dat$ageYearsX), by = 0.5)
  }

  ## ref_ds <- expand.grid(
  ##   size = size_ref_pts, sex = sex_ref_pts,
  ##   reference_age = age_ref_pts, comparator_age = age_ref_pts)
  ref_ds <- expand.grid(
    comparator_age = age_ref_pts, reference_age = age_ref_pts,
    sex = sex_ref_pts, size = size_ref_pts)
  ref_ds[, c("size", "sex", "reference_age", "comparator_age")]
}

ref_age_among_sn <- data.table::as.data.table(
  define_age_reference_points(ages = seq(0.5, 3, by = 0.5)))

