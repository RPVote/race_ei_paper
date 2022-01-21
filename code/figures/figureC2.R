#!/usr/bin/env Rscript
#' Generates Figure C1 for "Comparing BISG to vap Estimates in Racially
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
agg_path <- file.path(base_path, "ga_2010_agg_all.rds")
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
  ggplot(race_ests, aes(x = whi_true_total, y = whi_2010_vap_total)) +
  geom_point(color = 'black', alpha = alpha) +
  geom_abline(slope = 1, color = 'red', lwd = 2, alpha = 0.2) +
  xlab("Observed Total (Self-Reported)") +
  ylab("Estimated Total (vap)") +
  coord_fixed() +
  xlim(0, max(race_ests$whi_true_total, race_ests$whi_2010_vap_total)) +
  ylim(0, max(race_ests$whi_true_total, race_ests$whi_2010_vap_total)) +
  ggtitle("White") +
  plot_theme

plot_bla <-
  ggplot(race_ests, aes(x = bla_true_total, y = bla_2010_vap_total)) +
  geom_point(color = 'black', alpha = alpha) +
  geom_abline(intercept = 0,
              slope = 1,
              color = "grey",
              lwd = 1,
              linetype = "dashed") +
  xlab("Observed Total (Self-Reported)") +
  ylab("Estimated Total (vap)") +
  coord_fixed() +
  xlim(0, max(race_ests$bla_true_total, race_ests$bla_2010_vap_total)) +
  ylim(0, max(race_ests$bla_true_total, race_ests$bla_2010_vap_total)) +
  ggtitle("Black") +
  plot_theme

plot_his <-
  ggplot(race_ests, aes(x = his_true_total, y = his_2010_vap_total)) +
  geom_point(color = 'black', alpha = alpha) +
  geom_abline(intercept = 0,
              slope = 1,
              color = "grey",
              lwd = 1,
              linetype = "dashed") +
  xlab("Observed Total (Self-Reported)") +
  ylab("Estimated Total (vap)") +
  coord_fixed() +
  xlim(0, max(race_ests$his_true_total, race_ests$his_2010_vap_total)) +
  ylim(0, max(race_ests$his_true_total, race_ests$his_2010_vap_total)) +
  ggtitle("Hispanic") +
  plot_theme

plot_asi <-
  ggplot(race_ests, aes(x = asi_true_total, y = asi_2010_vap_total)) +
  geom_point(color = 'black', alpha = alpha) +
  geom_abline(intercept = 0,
              slope = 1,
              color = "grey",
              lwd = 1,
              linetype = "dashed") +
  xlab("Observed Total (Self-Reported)") +
  ylab("Estimated Total (vap)") +
  coord_fixed() +
  xlim(0, max(race_ests$asi_true_total, race_ests$asi_2010_vap_total)) +
  ylim(0, max(race_ests$asi_true_total, race_ests$asi_2010_vap_total)) +
  ggtitle("Asian") +
  plot_theme

plot_oth <-
  ggplot(race_ests, aes(x = oth_true_total, y = oth_2010_vap_total)) +
  geom_point(color = 'black', alpha = alpha) +
  geom_abline(intercept = 0,
              slope = 1,
              color = "grey",
              lwd = 1,
              linetype = "dashed") +
  xlab("Observed Total (Self-Reported)") +
  ylab("Estimated Total (vap)") +
  coord_fixed() +
  xlim(0, max(race_ests$oth_true_total, race_ests$oth_2010_vap_total)) +
  ylim(0, max(race_ests$oth_true_total, race_ests$oth_2010_vap_total)) +
  ggtitle("Other") +
  plot_theme

# Create Figure C1: overall figure
plot <-
  plot_whi +
  plot_bla +
  plot_his +
  plot_asi +
  plot_oth +
  plot_layout(nrow = 2, byrow = TRUE) +
  plot_annotation(tag_levels = "a") & theme(plot.tag = element_text(size = 27))
ggsave(filename = "FigureC2.pdf", plot = plot, width = 21, height = 14)
