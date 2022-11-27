# Header -------------------------------------------------------------
#
# Plot results for the ACC&D overweight/obesity analysis.
#
# John Sahrmann
# 20221127


# Setup --------------------------------------------------------------

library(data.table)
library(ggplot2)
library(magrittr)
library(paletteer)
library(pdftools)


# Constant definitions -----------------------------------------------

color3 <- rev(paletteer::paletteer_c("viridis::viridis", 3))
color5 <- rev(paletteer::paletteer_c("viridis::viridis", 5))

line_size <- 0.4
rel_point_size <- 1.5


# Plotting  ----------------------------------------------------------

load("../data/oo-results.Rdata")


# O/O, SN effect ------------------

ds <- data.table::copy(sn_results)
ds[,
  `:=`(
    wt_pctl_char = as.character(wt_pctl),
    age = data.table::fcase(
      wt_pctl == 25, age - 0.05,
      wt_pctl == 50, age,
      wt_pctl == 75, age + 0.05
    )
  )
]

cairo_pdf(
  "../output/fig/fig-oo-sn-effect-female-all-weights-col.pdf",
  width = 7, height = 7
)
ds[sex == "Female"] %>%
  ggplot(
    aes(x = age, y = hr, ymin = lo, ymax = hi, colour = wt_pctl_char)
  ) +
  geom_pointrange(size = line_size, fatten = rel_point_size) +
  geom_line() +
  scale_y_log10() +
  scale_colour_manual("Weight Quartile", values = color3) +
  labs(title = "Female Dogs") +
  xlab("\nAge at Index (Years)") +
  ylab("Hazard Ratio for Gonadectomy\n") +
  facet_wrap(vars(size)) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom"
  )
dev.off()

cairo_pdf(
  "../output/fig/fig-oo-sn-effect-male-all-weights-col.pdf",
  width = 7, height = 7
)
ds[sex == "Male"] %>%
  ggplot(
    aes(x = age, y = hr, ymin = lo, ymax = hi, colour = wt_pctl_char)
  ) +
  geom_pointrange(size = line_size, fatten = rel_point_size) +
  geom_line() +
  scale_y_log10() +
  scale_colour_manual("Weight Quartile", values = color3) +
  labs(title = "Male Dogs") +
  xlab("\nAge at Index (Years)") +
  ylab("Hazard Ratio for Gonadectomy\n") +
  facet_wrap(vars(size)) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom"
  )
dev.off()

pdftools::pdf_combine(
  c("../output/fig/fig-oo-sn-effect-female-all-weights-col.pdf",
    "../output/fig/fig-oo-sn-effect-male-all-weights-col.pdf"),
  "../output/fig/sFigure 1 oo-sn-effect-all-weights-col.pdf"
)
file.remove(
  c("../output/fig/fig-oo-sn-effect-female-all-weights-col.pdf",
    "../output/fig/fig-oo-sn-effect-male-all-weights-col.pdf")
)


# Colored by breed size, facetted by sex

ds <- data.table::copy(sn_results)
ds[,
  `:=`(
    wt_pctl_char = as.character(wt_pctl),
    age = data.table::fcase(
      size == "Toy and Small", age - 0.06,
      size == "Medium", age - 0.03,
      size == "Standard", age,
      size == "Large", age + 0.03,
      size == "Giant", age + 0.06
    )
  )
]

cairo_pdf(
  "../output/fig/Figure2_20221127.pdf",
  width = 8, height = 6
)
ds[wt_pctl == 50] %>%
  ggplot(
    aes(x = age, y = hr, ymin = lo, ymax = hi, colour = size)
  ) +
  geom_pointrange(size = line_size, fatten = rel_point_size) +
  geom_line() +
  scale_y_continuous(trans = "log", breaks = c(1, 3, 5)) +
  xlab("\nAge at Index (Years)") +
  ylab("Hazard Ratio for Gonadectomy\n") +
  scale_colour_manual("Breed Size", values = color5) +
  facet_wrap(vars(sex)) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom"
  )
dev.off()

cairo_pdf(
  "../output/fig/Figure2_commonY_20221127.pdf",
  width = 8, height = 6
)
ds[wt_pctl == 50] %>%
  ggplot(
    aes(x = age, y = hr, ymin = lo, ymax = hi, colour = size)
  ) +
  geom_pointrange(size = line_size, fatten = rel_point_size) +
  geom_line() +
  scale_y_continuous(
    trans = "log", limits = c(0.6, 9.5), breaks = c(1, 3, 5)
  ) +
  xlab("\nAge at Index (Years)") +
  ylab("Hazard Ratio for Gonadectomy\n") +
  scale_colour_manual("Breed Size", values = color5) +
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
      size == "Toy and Small", comparator_age - 0.06,
      size == "Medium", comparator_age - 0.03,
      size == "Standard", comparator_age,
      size == "Large", comparator_age + 0.03,
      size == "Giant", comparator_age + 0.06
    ),
    reference_age = factor(
      reference_age,
      levels = unique(reference_age),
      labels = paste(
        rep(
          "Gonadectomized at",
          times = length(unique(reference_age))),
        unique(reference_age),
        c("Years", "Years", "Year",
          rep("Years", times = length(unique(reference_age)) - 3))
      )
    )
  )
]

cairo_pdf(
  "../output/fig/fig-oo-age-effect-among-SN-all-years-female-col.pdf",
  width = 13, height = 8.5
)
ds[
  sex == "Female" & reference_age != "Gonadectomized at 5.5 Years"
] %>%
  ggplot(
    aes(
      x = comparator_age, y = hr, ymin = lo, ymax = hi, colour = size
    )
  ) +
  geom_pointrange(size = line_size, fatten = rel_point_size) +
  geom_line() +
  labs(title = "Female Dogs") +
  xlab("\nAge at Gonadectomy (Years)") +
  ylab("Hazard Ratio for Age\n") +
  scale_y_log10() +
  scale_colour_manual(
    "Breed Size",
    values = color5
  ) +
  facet_wrap(vars(reference_age), nrow = 2) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom"
  )
dev.off()

cairo_pdf(
  "../output/fig/fig-oo-age-effect-among-SN-all-years-male-col.pdf",
  width = 13, height = 8.5
)
ds[
  sex == "Male" & reference_age != "Gonadectomized at 5.5 Years"
] %>%
  ggplot(
    aes(
      x = comparator_age, y = hr, ymin = lo, ymax = hi, colour = size
    )
  ) +
  geom_pointrange(size = line_size, fatten = rel_point_size) +
  geom_line() +
  labs(title = "Male Dogs") +
  xlab("\nAge at Gonadectomy (Years)") +
  ylab("Hazard Ratio for Age\n") +
  scale_y_log10() +
  scale_colour_manual(
    "Breed Size",
    values = color5
  ) +
  facet_wrap(vars(reference_age), nrow = 2) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom"
  )
dev.off()

pdftools::pdf_combine(
  c("../output/fig/fig-oo-age-effect-among-SN-all-years-female-col.pdf",
    "../output/fig/fig-oo-age-effect-among-SN-all-years-male-col.pdf"),
  "../output/fig/sFigure 2 oo-age-effect-among-SN-all-years-col.pdf"
)
file.remove(
  c("../output/fig/fig-oo-age-effect-among-SN-all-years-female-col.pdf",
    "../output/fig/fig-oo-age-effect-among-SN-all-years-male-col.pdf")
)

cairo_pdf(
  "../output/fig/Figure3_20221127.pdf",
  width = 7.5, height = 6.5
)
ds[reference_age == "Gonadectomized at 1 Year"] %>%
  ggplot(
    aes(
      x = comparator_age, y = hr, ymin = lo, ymax = hi, colour = size)
  ) +
  geom_pointrange(size = line_size, fatten = rel_point_size) +
  geom_line() +
  scale_y_continuous(trans = "log", breaks = c(0.7, 1, 2)) +
  xlab("\nAge at Gonadectomy (Years)") +
  ylab("Hazard Ratio for Age\n") +
  scale_colour_manual("Breed Size", values = color5) +
  facet_wrap(vars(sex)) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom"
  )
dev.off()

cairo_pdf(
  "../output/fig/Figure3_commonY_20221127.pdf",
  width = 7.5, height = 6.5
)
ds[reference_age == "Gonadectomized at 1 Year"] %>%
  ggplot(
    aes(
      x = comparator_age, y = hr, ymin = lo, ymax = hi, colour = size)
  ) +
  geom_pointrange(size = line_size, fatten = rel_point_size) +
  geom_line() +
  scale_y_continuous(
    trans = "log", breaks = c(0.3, 1, 3), limits = c(0.3, 6.5)
  ) +
  xlab("\nAge at Gonadectomy (Years)") +
  ylab("Hazard Ratio for Age\n") +
  scale_colour_manual("Breed Size", values = color5) +
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
      wt_pctl == 25, age - 0.05,
      wt_pctl == 50, age,
      wt_pctl == 75, age + 0.05
    )
  )
]

cairo_pdf(
  "../output/fig/fig-ob-sn-effect-female-all-weights-col.pdf",
  width = 7, height = 7
)
ds[sex == "Female"] %>%
  ggplot(
    aes(x = age, y = hr, ymin = lo, ymax = hi, colour = wt_pctl_char)
  ) +
  geom_pointrange(size = line_size, fatten = rel_point_size) +
  geom_line() +
  scale_y_log10() +
  scale_colour_manual("Weight Quartile", values = color3) +
  labs(title = "Female Dogs") +
  xlab("\nAge at Index (Years)") +
  ylab("Hazard Ratio for Gonadectomy\n") +
  facet_wrap(vars(size)) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom"
  )
dev.off()

cairo_pdf(
  "../output/fig/fig-ob-sn-effect-male-all-weights-col.pdf",
  width = 7, height = 7
)
ds[sex == "Male"] %>%
  ggplot(
    aes(x = age, y = hr, ymin = lo, ymax = hi, colour = wt_pctl_char)
  ) +
  geom_pointrange(size = line_size, fatten = rel_point_size) +
  geom_line() +
  scale_y_log10() +
  scale_colour_manual("Weight Quartile", values = color3) +
  labs(title = "Male Dogs") +
  xlab("\nAge at Index (Years)") +
  ylab("Hazard Ratio for Gonadectomy\n") +
  facet_wrap(vars(size)) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom"
  )
dev.off()

pdftools::pdf_combine(
  c("../output/fig/fig-ob-sn-effect-female-all-weights-col.pdf",
    "../output/fig/fig-ob-sn-effect-male-all-weights-col.pdf"),
  "../output/fig/sFigure 3 ob-sn-effect-all-weights-col.pdf"
)
file.remove(
  c("../output/fig/fig-ob-sn-effect-female-all-weights-col.pdf",
    "../output/fig/fig-ob-sn-effect-male-all-weights-col.pdf")
)


# Colored by breed size, facetted by sex

ds <- data.table::copy(sn_results)
ds[,
  `:=`(
    wt_pctl_char = as.character(wt_pctl),
    age = data.table::fcase(
      size == "Toy and Small", age - 0.06,
      size == "Medium", age - 0.03,
      size == "Standard", age,
      size == "Large", age + 0.03,
      size == "Giant", age + 0.06
    )
  )
]

cairo_pdf(
  "../output/fig/Figure4_20221127.pdf",
  width = 8, height = 6
)
ds[wt_pctl == 50] %>%
  ggplot(
    aes(x = age, y = hr, ymin = lo, ymax = hi, colour = size)
  ) +
  geom_pointrange(size = line_size, fatten = rel_point_size) +
  geom_line() +
  scale_y_continuous(trans = "log", breaks = c(1, 3, 5)) +
  xlab("\nAge at Index (Years)") +
  ylab("Hazard Ratio for Gonadectomy\n") +
  scale_colour_manual("Breed Size", values = color5) +
  facet_wrap(vars(sex)) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom"
  )
dev.off()

cairo_pdf(
  "../output/fig/Figure4_commonY_20221127.pdf",
  width = 8, height = 6
)
ds[wt_pctl == 50] %>%
  ggplot(
    aes(x = age, y = hr, ymin = lo, ymax = hi, colour = size)
  ) +
  geom_pointrange(size = line_size, fatten = rel_point_size) +
  geom_line() +
  scale_y_continuous(
    trans = "log", breaks = c(1, 3, 5), limits = c(0.6, 9.5)
  ) +
  xlab("\nAge at Index (Years)") +
  ylab("Hazard Ratio for Gonadectomy\n") +
  scale_colour_manual("Breed Size", values = color5) +
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
      size == "Toy and Small", comparator_age - 0.06,
      size == "Medium", comparator_age - 0.03,
      size == "Standard", comparator_age,
      size == "Large", comparator_age + 0.03,
      size == "Giant", comparator_age + 0.06
    ),
    reference_age = factor(
      reference_age,
      levels = unique(reference_age),
      labels = paste(
        rep(
          "Gonadectomized at",
          times = length(unique(reference_age))),
        unique(reference_age),
        c("Years", "Years", "Year",
          rep("Years", times = length(unique(reference_age)) - 3))
      )
    )
  )
]

cairo_pdf(
  "../output/fig/fig-ob-age-effect-among-SN-all-years-female-col.pdf",
  width = 13, height = 8.5
)
ds[
  sex == "Female" & reference_age != "Gonadectomized at 5.5 Years"
] %>%
  ggplot(
    aes(
      x = comparator_age, y = hr, ymin = lo, ymax = hi, colour = size
    )
  ) +
  geom_pointrange(size = line_size, fatten = rel_point_size) +
  geom_line() +
  labs(title = "Female Dogs") +
  xlab("\nAge at Gonadectomy (Years)") +
  ylab("Hazard Ratio for Age\n") +
  scale_y_log10() +
  scale_colour_manual("Breed Size", values = color5) +
  facet_wrap(vars(reference_age), nrow = 2) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom"
  )
dev.off()

cairo_pdf(
  "../output/fig/fig-ob-age-effect-among-SN-all-years-male-col.pdf",
  width = 13, height = 8.5
)
ds[
  sex == "Male" & reference_age != "Gonadectomized at 5.5 Years"
] %>%
  ggplot(
    aes(
      x = comparator_age, y = hr, ymin = lo, ymax = hi, colour = size
    )
  ) +
  geom_pointrange(size = line_size, fatten = rel_point_size) +
  geom_line() +
  labs(title = "Male Dogs") +
  xlab("\nAge at Gonadectomy (Years)") +
  ylab("Hazard Ratio for Age\n") +
  scale_y_log10() +
  scale_colour_manual("Breed Size", values = color5) +
  facet_wrap(vars(reference_age), nrow = 2) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom"
  )
dev.off()

pdftools::pdf_combine(
  c("../output/fig/fig-ob-age-effect-among-SN-all-years-female-col.pdf",
    "../output/fig/fig-ob-age-effect-among-SN-all-years-male-col.pdf"),
  "../output/fig/sFigure 4 ob-age-effect-among-SN-all-years-col.pdf"
)
file.remove(
  c("../output/fig/fig-ob-age-effect-among-SN-all-years-female-col.pdf",
    "../output/fig/fig-ob-age-effect-among-SN-all-years-male-col.pdf")
)

cairo_pdf(
  "../output/fig/Figure5_20221127.pdf",
  width = 7.5, height = 6.5
)
ds[reference_age == "Gonadectomized at 1 Year"] %>%
  ggplot(
    aes(
      x = comparator_age, y = hr, ymin = lo, ymax = hi, colour = size)
  ) +
  geom_pointrange(size = line_size, fatten = rel_point_size) +
  geom_line() +
  scale_y_continuous(trans = "log", breaks = c(0.3, 1, 3)) +
  xlab("\nAge at Gonadectomy (Years)") +
  ylab("Hazard Ratio for Age\n") +
  scale_colour_manual("Breed Size", values = color5) +
  facet_wrap(vars(sex)) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom"
  )
dev.off()

cairo_pdf(
  "../output/fig/Figure5_commonY_20221127.pdf",
  width = 7.5, height = 6.5
)
ds[reference_age == "Gonadectomized at 1 Year"] %>%
  ggplot(
    aes(
      x = comparator_age, y = hr, ymin = lo, ymax = hi, colour = size)
  ) +
  geom_pointrange(size = line_size, fatten = rel_point_size) +
  geom_line() +
  scale_y_continuous(
    trans = "log", breaks = c(0.3, 1, 3), limits = c(0.3, 6.5)
  ) +
  xlab("\nAge at Gonadectomy (Years)") +
  ylab("Hazard Ratio for Age\n") +
  scale_colour_manual("Breed Size", values = color5) +
  facet_wrap(vars(sex)) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom"
  )
dev.off()
