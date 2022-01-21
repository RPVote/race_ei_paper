#!/usr/bin/env Rscript
#' Generates Figure 2 for "Comparing BISG to CVAP Estimates in Racially
#' Polarized Voting Analysis" by Collingwood et al.
#'
#' This script calculates and produces the scatter comparisons between BISG and
#' self-reported race across racial groups.

# Import relevant libraries
suppressWarnings(suppressMessages({
  library(readr)
  library(tidyverse)
  library(patchwork)
}))

# Turn verbosity on or off
verbose <- TRUE
# Set the base path: where all data files are located
base_path <- "../../data"
agg_path <- file.path(base_path, "ga_2018_agg.csv")
agg <- readr::read_csv(
  agg_path,
  col_types = readr::cols(
    .default = readr::col_double(),
    precinct_id_2018 = readr::col_character()))

# Plot theme
alpha <- 0.25
plot_theme <-
  theme_bw() +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 23, face = "bold"),
        axis.title.x = element_text(margin = margin(t = 20)),
        axis.title.y = element_text(margin = margin(r = 20)),
        plot.title = element_text(size = 27, face = "bold", hjust = 0.5),
        legend.position = "none")

# Create Figure 2: individual subplots
plot_whi <-
  ggplot(data = agg, aes(x = whi_true, y = whi_bisg)) +
  geom_point(color = "black", alpha = alpha) +
  geom_abline(intercept = 0,
              slope = 1,
              color = "grey",
              lwd = 1,
              linetype = "dashed") +
  xlim(0, 1) + ylim(0, 1) +
  xlab("Observed Fraction (Self-Reported)") +
  ylab("Estimated Fraction (BISG)") +
  ggtitle("White") +
  plot_theme +
  coord_fixed()

plot_bla <-
  ggplot(data = agg, aes(x = bla_true, y = bla_bisg)) +
  geom_point(color = "black", alpha = alpha) +
  geom_abline(intercept = 0,
              slope = 1,
              color = "grey",
              lwd = 1,
              linetype = "dashed") +
  xlim(0, 1) + ylim(0, 1) +
  xlab("Observed Fraction (Self-Reported)") +
  ylab("Estimated Fraction (BISG)") +
  ggtitle("Black") +
  plot_theme +
  coord_fixed()

plot_his <-
  ggplot(data = agg, aes(x = his_true, y = his_bisg)) +
  geom_point(color = "black", alpha = alpha) +
  geom_abline(intercept = 0,
              slope = 1,
              color = "grey",
              lwd = 1,
              linetype = "dashed") +
  xlim(0, 0.5) + ylim(0, 0.5) +
  xlab("Observed Fraction (Self-Reported)") +
  ylab("Estimated Fraction (BISG)") +
  ggtitle("Hispanic/Latino") +
  plot_theme +
  coord_fixed()

plot_asi <-
  ggplot(data = agg, aes(x = asi_true, y = asi_bisg)) +
  geom_point(color = "black", alpha = alpha) +
  geom_abline(intercept = 0,
              slope = 1,
              color = "grey",
              lwd = 1,
              linetype = "dashed") +
  xlim(0, 1) + ylim(0, 1) +
  xlab("Observed Fraction (Self-Reported)") +
  ylab("Estimated Fraction (BISG)") +
  ggtitle("Asian") +
  plot_theme +
  coord_fixed()

plot_oth <-
  ggplot(data = agg, aes(x = oth_true, y = oth_bisg)) +
  geom_point(color = "black", alpha = alpha) +
  geom_abline(intercept = 0,
              slope = 1,
              color = "grey",
              lwd = 1,
              linetype = "dashed") +
  xlim(0, 1) + ylim(0, 1) +
  xlab("Observed Fraction (Self-Reported)") +
  ylab("Estimated Fraction (BISG)") +
  ggtitle("Other") +
  plot_theme +
  coord_fixed()

# Create Figure 2: overall figure
plot <-
  plot_whi +
  plot_bla +
  plot_his +
  plot_asi +
  plot_oth +
  plot_layout(nrow = 2, byrow = TRUE) +
  plot_annotation(tag_levels = "a") & theme(plot.tag = element_text(size = 27))
ggsave(filename = "Figure2.pdf", plot = plot, width = 21, height = 14)
