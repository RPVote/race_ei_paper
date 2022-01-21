#!/usr/bin/env Rscript
#' Generates Figure B1 for "Comparing BISG to CVAP Estimates in Racially
#' Polarized Voting Analysis" by Collingwood et al.
#'
#' This script calculates and produces the scatter comparisons between VAP and
#' self-reported race across racial groups.

# Import relevant libraries
suppressWarnings(suppressMessages({
  library(readr)
  library(tidyverse)
  library(patchwork)
}))

# Turn verbosity on or off
verbose <- TRUE
# Set working directory to folder in which this file is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Set the base path: where all data files are located
base_path <- "../../data"
agg_path <- file.path(base_path, "ga_2018_agg_all.rds")
race_ests <- readRDS(agg_path)

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


plot_whi <-
  ggplot(race_ests, aes(x = whi_true, y = whi_2010_vap_ext_prop)) +
  geom_point(color = 'black', alpha = alpha) +
  geom_abline(slope = 1, color = 'red', lwd = 2, alpha = 0.2) +
  xlab("Observed Fraction (Self-Reported)") +
  ylab("Estimated Fraction (VAP)") +
  coord_fixed() +
  xlim(0, 1) +
  ylim(0, 1) +
  ggtitle("White") +
  plot_theme

plot_bla <-
  ggplot(race_ests, aes(x = bla_true, y = bla_2010_vap_ext_prop)) +
  geom_point(color = 'black', alpha = alpha) +
  geom_abline(intercept = 0,
              slope = 1,
              color = "grey",
              lwd = 1,
              linetype = "dashed") +
  xlab("Observed Fraction (Self-Reported)") +
  ylab("Estimated Fraction (VAP)") +
  coord_fixed() +
  xlim(0, 1) +
  ylim(0, 1) +
  ggtitle("Black") +
  plot_theme

plot_his <-
  ggplot(race_ests, aes(x = his_true, y = his_2010_vap_ext_prop)) +
  geom_point(color = 'black', alpha = alpha) +
  geom_abline(intercept = 0,
              slope = 1,
              color = "grey",
              lwd = 1,
              linetype = "dashed") +
  xlab("Observed Fraction (Self-Reported)") +
  ylab("Estimated Fraction (VAP)") +
  coord_fixed() +
  xlim(0, 1) +
  ylim(0, 1) +
  ggtitle("Hispanic") +
  plot_theme

plot_asi <-
  ggplot(race_ests, aes(x = asi_true, y = asi_2010_vap_ext_prop)) +
  geom_point(color = 'black', alpha = alpha) +
  geom_abline(intercept = 0,
              slope = 1,
              color = "grey",
              lwd = 1,
              linetype = "dashed") +
  xlab("Observed Fraction (Self-Reported)") +
  ylab("Estimated Fraction (VAP)") +
  coord_fixed() +
  xlim(0, 1) +
  ylim(0, 1) +
  ggtitle("Asian") +
  plot_theme

plot_oth <-
  ggplot(race_ests, aes(x = oth_true, y = oth_2010_vap_ext_prop)) +
  geom_point(color = 'black', alpha = alpha) +
  geom_abline(intercept = 0,
              slope = 1,
              color = "grey",
              lwd = 1,
              linetype = "dashed") +
  xlab("Observed Fraction (Self-Reported)") +
  ylab("Estimated Fraction (VAP)") +
  coord_fixed() +
  xlim(0, 1) +
  ylim(0, 1) +
  ggtitle("Other") +
  plot_theme

# Create Figure B1: overall figure
plot <-
  plot_whi +
  plot_bla +
  plot_his +
  plot_asi +
  plot_oth +
  plot_layout(nrow = 2, byrow = TRUE) +
  plot_annotation(tag_levels = "a") & theme(plot.tag = element_text(size = 27))
ggsave(filename = "FigureB1.pdf", plot = plot, width = 21, height = 14)
