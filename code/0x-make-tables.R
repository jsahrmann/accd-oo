# Header -------------------------------------------------------------
#
# Produce output for manuscript tables.
#
# John Sahrmann
# 20220510


# Input data ---------------------------------------------------------

source("./02-read-cohort.R")


# Table 2 - Unadjusted outcome ---------------------------------------

# Get outcome counts and row percentages.
table2_oo_ct <- table(dat$sn, dat$oo_event)
table2_oo_rpct <- round(proportions(table2_oo_ct), margin = 1) * 100

table2_ob_ct <- table(dat$sn, dat$ob_event)
table2_ob_rpct <- round(proportions(table2_ob_ct), margin = 1) * 100

# Combine and format the text.
table2_oo <- paste0(
  format(table2_oo_ct, big.mark = ","), " (", table2_oo_rpct, ")")
table2_ob <- paste0(
  format(table2_ob_ct, big.mark = ","), " (", table2_ob_rpct, ")")

# Assemble into a matrix.
table2 <- matrix(
  c(table2_oo, table2_ob), nrow = 4, byrow = TRUE,
  dimnames = list(
    c("intact_oo", "sn_oo", "intact_ob", "sn_ob"),
    c("event", "censored"))

# Save the final output.
write.csv(table2, file = "../output/table2.csv", row.names = TRUE)
