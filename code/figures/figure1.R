#!/usr/bin/env Rscript
#' Generates Figure 2 for "Comparing BISG to CVAP Estimates in Racially
#' Polarized Voting Analysis" by Collingwood et al.
#'
#' This script calculates and produces the scatter comparisons between BISG,
#' CVAP, and self-reported race across racial groups.

# Import relevant libraries
suppressWarnings(suppressMessages({
  library(tidyverse)
  library(patchwork)
}))

# Set working directory to folder in which this file is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Turn verbosity on or off
verbose <- TRUE
# Set the base path: where all data files are located
base_path <- "../../data"
agg_path <- file.path(base_path, "ga_2018_agg_all.rds")
agg <- readRDS(agg_path)

# Plot theme
alpha <- 0.25
plot_theme <-
  theme_bw() +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 22, face = "bold"),
        axis.title.x = element_text(margin = margin(t = 20)),
        axis.title.y = element_text(margin = margin(r = 20)),
        plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
        plot.margin = unit(c(0, 20, 40, 0), "pt"),
        legend.position = "none")

# White, BISG vs. CVAP
plot_whi_bisg <-
  ggplot(data = agg, aes(x = whi_true_prop, y = whi_bisg_prop)) +
  geom_point(color = "black", alpha = alpha) +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red",
              lwd = 1,
              linetype = "dashed") +
  xlim(0, 1) + ylim(0, 1) +
  xlab("Observed Fraction (Self-Reported)") +
  ylab("Estimated Fraction (BISG)") +
  ggtitle("White, BISG") +
  plot_theme +
  coord_fixed()

plot_whi_cvap <-
  ggplot(data = agg, aes(x = whi_true_prop, y = whi_2018_cvap_ext_prop)) +
  geom_point(color = "black", alpha = alpha) +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red",
              lwd = 1,
              linetype = "dashed") +
  xlim(0, 1) + ylim(0, 1) +
  xlab("Observed Fraction (Self-Reported)") +
  ylab("Estimated Fraction (CVAP)") +
  ggtitle("White, CVAP") +
  plot_theme +
  coord_fixed()

# Black, BISG vs. CVAP
plot_bla_bisg <-
  ggplot(data = agg, aes(x = bla_true_prop, y = bla_bisg_prop)) +
  geom_point(color = "black", alpha = alpha) +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red",
              lwd = 1,
              linetype = "dashed") +
  xlim(0, 1) + ylim(0, 1) +
  xlab("Observed Fraction (Self-Reported)") +
  ylab("Estimated Fraction (BISG)") +
  ggtitle("Black, BISG") +
  plot_theme +
  coord_fixed()

plot_bla_cvap <-
  ggplot(data = agg, aes(x = bla_true_prop, y = bla_2018_cvap_ext_prop)) +
  geom_point(color = "black", alpha = alpha) +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red",
              lwd = 1,
              linetype = "dashed") +
  xlim(0, 1) + ylim(0, 1) +
  xlab("Observed Fraction (Self-Reported)") +
  ylab("Estimated Fraction (CVAP)") +
  ggtitle("Black, CVAP") +
  plot_theme +
  coord_fixed()

# Hispanic/Latino, BISG vs. CVAP
plot_his_bisg <-
  ggplot(data = agg, aes(x = his_true_prop, y = his_bisg_prop)) +
  geom_point(color = "black", alpha = alpha) +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red",
              lwd = 1,
              linetype = "dashed") +
  xlim(0, 0.5) + ylim(0, 0.5) +
  xlab("Observed Fraction (Self-Reported)") +
  ylab("Estimated Fraction (BISG)") +
  ggtitle("Hispanic/Latino, BISG") +
  plot_theme +
  coord_fixed()

plot_his_cvap <-
  ggplot(data = agg, aes(x = his_true_prop, y = his_2018_cvap_ext_prop)) +
  geom_point(color = "black", alpha = alpha) +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red",
              lwd = 1,
              linetype = "dashed") +
  xlim(0, 0.5) + ylim(0, 0.5) +
  xlab("Observed Fraction (Self-Reported)") +
  ylab("Estimated Fraction (CVAP)") +
  ggtitle("Hispanic/Latino, CVAP") +
  plot_theme +
  coord_fixed()

# Asian, BISG vs. CVAP
plot_asi_bisg <-
  ggplot(data = agg, aes(x = asi_true_prop, y = asi_bisg_prop)) +
  geom_point(color = "black", alpha = alpha) +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red",
              lwd = 1,
              linetype = "dashed") +
  xlim(0, 0.3) + ylim(0, 0.3) +
  xlab("Observed Fraction (Self-Reported)") +
  ylab("Estimated Fraction (BISG)") +
  ggtitle("Asian/Pacific Islander, BISG") +
  plot_theme +
  coord_fixed()

plot_asi_cvap <-
  ggplot(data = agg, aes(x = asi_true_prop, y = asi_2018_cvap_ext_prop)) +
  geom_point(color = "black", alpha = alpha) +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red",
              lwd = 1,
              linetype = "dashed") +
  xlim(0, 0.3) + ylim(0, 0.3) +
  xlab("Observed Fraction (Self-Reported)") +
  ylab("Estimated Fraction (CVAP)") +
  ggtitle("Asian/Pacific Islander, CVAP") +
  plot_theme +
  coord_fixed()

# Other, BISG vs. CVAP
plot_oth_bisg <-
  ggplot(data = agg, aes(x = oth_true_prop, y = oth_bisg_prop)) +
  geom_point(color = "black", alpha = alpha) +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red",
              lwd = 1,
              linetype = "dashed") +
  xlim(0, 0.1) + ylim(0, 0.1) +
  xlab("Observed Fraction (Self-Reported)") +
  ylab("Estimated Fraction (BISG)") +
  ggtitle("Other, BISG") +
  plot_theme +
  coord_fixed()

plot_oth_cvap <-
  ggplot(data = agg, aes(x = oth_true_prop, y = oth_2018_cvap_ext_prop)) +
  geom_point(color = "black", alpha = alpha) +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red",
              lwd = 1,
              linetype = "dashed") +
  xlim(0, 0.1) + ylim(0, 0.1) +
  xlab("Observed Fraction (Self-Reported)") +
  ylab("Estimated Fraction (CVAP)") +
  ggtitle("Other, CVAP") +
  plot_theme +
  coord_fixed()

# Create Figure 2: overall figure
plot <-
  plot_whi_bisg + plot_whi_cvap + theme(plot.margin = unit(c(0, 100, 0, 0), "pt")) +
  plot_bla_bisg + plot_bla_cvap +
  plot_his_bisg + plot_his_cvap + theme(plot.margin = unit(c(0, 100, 0, 0), "pt")) +
  plot_asi_bisg + plot_asi_cvap +
  plot_oth_bisg + plot_oth_cvap +
  plot_layout(ncol = 4) +
  plot_annotation(tag_levels = "a") & theme(plot.tag = element_text(size = 28, face = "bold"))
ggsave(filename = "Figure1.pdf", plot = plot, width = 28, height = 21)
