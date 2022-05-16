# Header -------------------------------------------------------------
#
# Plot results for the ACC&D overweight/obesity analysis.
#
# John Sahrmann
# 20220515


# Setup --------------------------------------------------------------

library(data.table)
library(ggplot2)
library(magrittr)
library(pdftools)


# Constant definitions -----------------------------------------------

gray3a <- "#f0f0f0"
gray3b <- "#bdbdbd"
gray3c <- "#636363"

gray5a <- "#f7f7f7"
gray5b <- "#cccccc"
gray5c <- "#969696"
gray5d <- "#636363"
gray5e <- "#252525"


# Plotting  ----------------------------------------------------------

load("../data/oo-results.Rdata")


# O/O, SN effect ------------------

# Colored by weight, facetted by breed size, separated by sex

## cairo_pdf(
##   "../output/fig/fig-oo-sn-effect-female-all-weights.pdf",
##   width = 7, height = 7
## )
## ds[sex == "Female"] %>%
##   ggplot(
##     aes(x = age, y = hr, ymin = lo, ymax = hi, colour = wt_pctl_char)
##   ) +
##   geom_pointrange() +
##   geom_line() +
##   scale_colour_discrete("Weight Quartile") +
##   labs(title = "Female Dogs") +
##   xlab("\nAge at Index") +
##   ylab("Hazard Ratio of Spayed/Neutered vs. Intact\n") +
##   facet_wrap(vars(size)) +
##   theme_gray(base_size = 12) +
##   theme(
##     legend.position = "bottom"
##   )
## dev.off()


ds <- data.table::copy(sn_results)
ds[,
  `:=`(
    wt_pctl_char = as.character(wt_pctl),
    age = data.table::fcase(
      wt_pctl == 25, age - 0.1,
      wt_pctl == 50, age,
      wt_pctl == 75, age + 0.1
    )
  )
]

cairo_pdf(
  "../output/fig/fig-oo-sn-effect-female-all-weights-bw.pdf",
  width = 7, height = 7
)
ds[sex == "Female"] %>%
  ggplot(
    aes(x = age, y = hr, ymin = lo, ymax = hi, fill = wt_pctl_char)
  ) +
  geom_pointrange(shape = 21, colour = "black") +
  geom_line() +
  scale_y_log10() +
  scale_fill_manual("Weight Quartile", values = c(gray3a, gray3b, gray3c)) +
  labs(title = "Female Dogs") +
  xlab("\nAge at Index") +
  ylab("Hazard Ratio of Spayed/Neutered vs. Intact\n") +
  facet_wrap(vars(size)) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom"
  )
dev.off()

cairo_pdf(
  "../output/fig/fig-oo-sn-effect-male-all-weights-bw.pdf",
  width = 7, height = 7
)
ds[sex == "Male"] %>%
  ggplot(
    aes(x = age, y = hr, ymin = lo, ymax = hi, fill = wt_pctl_char)
  ) +
  geom_pointrange(shape = 21, colour = "black") +
  geom_line() +
  scale_y_log10() +
  scale_fill_manual("Weight Quartile", values = c(gray3a, gray3b, gray3c)) +
  labs(title = "Male Dogs") +
  xlab("\nAge at Index (Years)") +
  ylab("Hazard Ratio of Spayed/Neutered vs. Intact\n") +
  facet_wrap(vars(size)) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom"
  )
dev.off()


pdftools::pdf_combine(
  c("../output/fig/fig-oo-sn-effect-female-all-weights-bw.pdf",
    "../output/fig/fig-oo-sn-effect-male-all-weights-bw.pdf"),
  "../output/fig/fig-oo-sn-effect-all-weights.pdf"
)
file.remove(
  c("../output/fig/fig-oo-sn-effect-female-all-weights-bw.pdf",
    "../output/fig/fig-oo-sn-effect-male-all-weights-bw.pdf")
)


# Colored by breed size, facetted by sex

ds <- data.table::copy(sn_results)
ds[,
  `:=`(
    wt_pctl_char = as.character(wt_pctl),
    age = data.table::fcase(
      size == "Toy and Small", age - 0.12,
      size == "Medium", age - 0.06,
      size == "Standard", age,
      size == "Large", age + 0.06,
      size == "Giant", age + 0.12
    )
  )
]

cairo_pdf(
  "../output/fig/fig-oo-sn-effect-median-weight-bw.pdf",
  width = 8, height = 6
)
ds[wt_pctl == 50] %>%
  ggplot(
    aes(x = age, y = hr, ymin = lo, ymax = hi, fill = size)
  ) +
  geom_pointrange(shape = 21, colour = "black") +
  geom_line() +
  scale_y_log10() +
  labs(title = "Dogs at Median Weight") +
  xlab("\nAge at Index (Years)") +
  ylab("Hazard Ratio of Spayed/Neutered vs. Intact\n") +
  scale_fill_manual(
    "Breed Size", values = c(gray5a, gray5b, gray5c, gray5d, gray5e)
  ) +
  facet_wrap(vars(sex)) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom"
  )
dev.off()


# O/O, age effect among SN --------

ds <- data.table::copy(age_among_sn_results)
ds[,
  `:=`(
    comparator_age = data.table::fcase(
      size == "Toy and Small", comparator_age - 0.12,
      size == "Medium", comparator_age - 0.06,
      size == "Standard", comparator_age,
      size == "Large", comparator_age + 0.06,
      size == "Giant", comparator_age + 0.12
    ),
    reference_age = factor(
      reference_age,
      levels = unique(reference_age),
      labels = paste(
        rep(
          "Spayed/Neutered at",
          times = length(unique(reference_age))),
        unique(reference_age),
        c("Years", "Year",
          rep("Years", times = length(unique(reference_age)) - 2))
      )
    )
  )
]

cairo_pdf(
  "../output/fig/fig-oo-age-effect-among-SN-all-years-female-bw.pdf",
  width = 13, height = 8.5
)
ds[sex == "Female"] %>%
  ggplot(
    aes(
      x = comparator_age, y = hr, ymin = lo, ymax = hi, fill = size
    )
  ) +
  geom_pointrange(shape = 21, colour = "black") +
  geom_line() +
  labs(title = "Female Dogs") +
  xlab("\nAge at Spay/Neuter (Years)") +
  ylab("Hazard Ratio for Age\n") +
  scale_y_log10() +
  scale_fill_manual(
    "Breed Size",
    values = c(gray5a, gray5b, gray5c, gray5d, gray5e)
  ) +
  facet_wrap(vars(reference_age), nrow = 2) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom"
  )
dev.off()

cairo_pdf(
  "../output/fig/fig-oo-age-effect-among-SN-all-years-male-bw.pdf",
  width = 13, height = 8.5
)
ds[sex == "Male"] %>%
  ggplot(
    aes(
      x = comparator_age, y = hr, ymin = lo, ymax = hi, fill = size
    )
  ) +
  geom_pointrange(shape = 21, colour = "black") +
  geom_line() +
  labs(title = "Male Dogs") +
  xlab("\nAge at Spay/Neuter (Years)") +
  ylab("Hazard Ratio for Age\n") +
  scale_y_log10() +
  scale_fill_manual(
    "Breed Size",
    values = c(gray5a, gray5b, gray5c, gray5d, gray5e)
  ) +
  facet_wrap(vars(reference_age), nrow = 2) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom"
  )
dev.off()

pdftools::pdf_combine(
  c("../output/fig/fig-oo-age-effect-among-SN-all-years-female-bw.pdf",
    "../output/fig/fig-oo-age-effect-among-SN-all-years-male-bw.pdf"),
  "../output/fig/fig-oo-age-effect-among-SN-all-years-bw.pdf"
)
file.remove(
  c("../output/fig/fig-oo-age-effect-among-SN-all-years-female-bw.pdf",
    "../output/fig/fig-oo-age-effect-among-SN-all-years-male-bw.pdf")
)


cairo_pdf(
  "../output/fig/fig-oo-age-effect-among-SN-1-year-bw.pdf",
  width = 7.5, height = 6.5
)
ds[reference_age == "Spayed/Neutered at 1 Year"] %>%
  ggplot(
    aes(x = comparator_age, y = hr, ymin = lo, ymax = hi, fill = size)
  ) +
  geom_pointrange(shape = 21, colour = "black") +
  geom_line() +
  scale_y_log10() +
  labs(title = "Compared to Dogs Gonadectomized at 1 Year") +
  xlab("\nAge at Spay/Neuter (Years)") +
  ylab("Hazard Ratio for Age\n") +
  scale_fill_manual(
    "Breed Size", values = c(gray5a, gray5b, gray5c, gray5d, gray5e)
  ) +
  facet_wrap(vars(sex)) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom"
  )
dev.off()


# Obese, SN effect ----------------

load("../data/ob-results.Rdata")


# Colored by weight, facetted by breed size, separated by sex

ds <- data.table::copy(sn_results)[,
  `:=`(
    wt_pctl_char = as.character(wt_pctl),
    age = data.table::fcase(
      wt_pctl == 25, age - 0.1,
      wt_pctl == 50, age,
      wt_pctl == 75, age + 0.1
    )
  )
]

cairo_pdf(
  "../output/fig/fig-ob-sn-effect-female-all-weights-bw.pdf",
  width = 7, height = 7
)
ds[sex == "Female"] %>%
  ggplot(
    aes(x = age, y = hr, ymin = lo, ymax = hi, fill = wt_pctl_char)
  ) +
  geom_pointrange(shape = 21, colour = "black") +
  geom_line() +
  scale_y_log10() +
  scale_fill_manual("Weight Quartile", values = c(gray3a, gray3b, gray3c)) +
  labs(title = "Female Dogs") +
  xlab("\nAge at Index") +
  ylab("Hazard Ratio of Spayed/Neutered vs. Intact\n") +
  facet_wrap(vars(size)) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom"
  )
dev.off()

cairo_pdf(
  "../output/fig/fig-ob-sn-effect-male-all-weights-bw.pdf",
  width = 7, height = 7
)
ds[sex == "Male"] %>%
  ggplot(
    aes(x = age, y = hr, ymin = lo, ymax = hi, fill = wt_pctl_char)
  ) +
  geom_pointrange(shape = 21, colour = "black") +
  geom_line() +
  scale_y_log10() +
  scale_fill_manual("Weight Quartile", values = c(gray3a, gray3b, gray3c)) +
  labs(title = "Male Dogs") +
  xlab("\nAge at Index (Years)") +
  ylab("Hazard Ratio of Spayed/Neutered vs. Intact\n") +
  facet_wrap(vars(size)) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom"
  )
dev.off()


pdftools::pdf_combine(
  c("../output/fig/fig-ob-sn-effect-female-all-weights-bw.pdf",
    "../output/fig/fig-ob-sn-effect-male-all-weights-bw.pdf"),
  "../output/fig/fig-ob-sn-effect-all-weights-bw.pdf"
)
file.remove(
  c("../output/fig/fig-ob-sn-effect-female-all-weights-bw.pdf",
    "../output/fig/fig-ob-sn-effect-male-all-weights-bw.pdf")
)


# Colored by breed size, facetted by sex

ds <- data.table::copy(sn_results)
ds[,
  `:=`(
    wt_pctl_char = as.character(wt_pctl),
    age = data.table::fcase(
      size == "Toy and Small", age - 0.12,
      size == "Medium", age - 0.06,
      size == "Standard", age,
      size == "Large", age + 0.06,
      size == "Giant", age + 0.12
    )
  )
]

cairo_pdf(
  "../output/fig/fig-ob-sn-effect-median-weight-bw.pdf",
  width = 8, height = 6
)
ds[wt_pctl == 50] %>%
  ggplot(
    aes(x = age, y = hr, ymin = lo, ymax = hi, fill = size)
  ) +
  geom_pointrange(shape = 21, colour = "black") +
  geom_line() +
  scale_y_log10() +
  labs(title = "Dogs at Median Weight") +
  xlab("\nAge at Index (Years)") +
  ylab("Hazard Ratio of Spayed/Neutered vs. Intact\n") +
  scale_fill_manual(
    "Breed Size", values = c(gray5a, gray5b, gray5c, gray5d, gray5e)
  ) +
  facet_wrap(vars(sex)) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom"
  )
dev.off()


# Obese, age effect among SN ------

ds <- data.table::copy(age_among_sn_results)
ds[,
  `:=`(
    comparator_age = data.table::fcase(
      size == "Toy and Small", comparator_age - 0.12,
      size == "Medium", comparator_age - 0.06,
      size == "Standard", comparator_age,
      size == "Large", comparator_age + 0.06,
      size == "Giant", comparator_age + 0.12
    ),
    reference_age = factor(
      reference_age,
      levels = unique(reference_age),
      labels = paste(
        rep(
          "Spayed/Neutered at",
          times = length(unique(reference_age))),
        unique(reference_age),
        c("Years", "Year",
          rep("Years", times = length(unique(reference_age)) - 2))
      )
    )
  )
]

cairo_pdf(
  "../output/fig/fig-ob-age-effect-among-SN-all-years-female-bw.pdf",
  width = 13, height = 8.5
)
ds[sex == "Female"] %>%
  ggplot(
    aes(
      x = comparator_age, y = hr, ymin = lo, ymax = hi, fill = size
    )
  ) +
  geom_pointrange(shape = 21, colour = "black") +
  geom_line() +
  labs(title = "Female Dogs") +
  xlab("\nAge at Spay/Neuter (Years)") +
  ylab("Hazard Ratio for Age\n") +
  scale_y_log10() +
  scale_fill_manual(
    "Breed Size",
    values = c(gray5a, gray5b, gray5c, gray5d, gray5e)
  ) +
  facet_wrap(vars(reference_age), nrow = 2) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom"
  )
dev.off()

cairo_pdf(
  "../output/fig/fig-ob-age-effect-among-SN-all-years-male-bw.pdf",
  width = 13, height = 8.5
)
ds[sex == "Male"] %>%
  ggplot(
    aes(
      x = comparator_age, y = hr, ymin = lo, ymax = hi, fill = size
    )
  ) +
  geom_pointrange(shape = 21, colour = "black") +
  geom_line() +
  labs(title = "Male Dogs") +
  xlab("\nAge at Spay/Neuter (Years)") +
  ylab("Hazard Ratio for Age\n") +
  scale_y_log10() +
  scale_fill_manual(
    "Breed Size",
    values = c(gray5a, gray5b, gray5c, gray5d, gray5e)
  ) +
  facet_wrap(vars(reference_age), nrow = 2) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom"
  )
dev.off()

pdftools::pdf_combine(
  c("../output/fig/fig-ob-age-effect-among-SN-all-years-female-bw.pdf",
    "../output/fig/fig-ob-age-effect-among-SN-all-years-male-bw.pdf"),
  "../output/fig/fig-ob-age-effect-among-SN-all-years-bw.pdf"
)
file.remove(
  c("../output/fig/fig-ob-age-effect-among-SN-all-years-female-bw.pdf",
    "../output/fig/fig-ob-age-effect-among-SN-all-years-male-bw.pdf")
)


cairo_pdf(
  "../output/fig/fig-ob-age-effect-among-SN-1-year-bw.pdf",
  width = 7.5, height = 6.5
)
ds[reference_age == "Spayed/Neutered at 1 Year"] %>%
  ggplot(
    aes(x = comparator_age, y = hr, ymin = lo, ymax = hi, fill = size)
  ) +
  geom_pointrange(shape = 21, colour = "black") +
  geom_line() +
  scale_y_log10() +
  labs(title = "Compared to Dogs Gonadectomized at 1 Year") +
  xlab("\nAge at Spay/Neuter (Years)") +
  ylab("Hazard Ratio for Age\n") +
  scale_fill_manual(
    "Breed Size", values = c(gray5a, gray5b, gray5c, gray5d, gray5e)
  ) +
  facet_wrap(vars(sex)) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom"
  )
dev.off()
