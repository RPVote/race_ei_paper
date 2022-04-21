#!/usr/bin/env Rscript
#' Generates Figure 1 for "Comparing BISG to CVAP Estimates in Racially
#' Polarized Voting Analysis" by Collingwood et al.
#'
#' This script calculates and produces the ROC curves depicted in Figure 1
#' of the paper.

# Import packages
suppressWarnings(suppressMessages({
  library(patchwork)
  library(readr)
  library(ROCR)
  library(tidyverse)
}))

# Set working directory to folder in which this file is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Base path: enter the path to the folder where the voter file can be found
base_path <- "../../data"
vf_path <- file.path(base_path, "ga_voter_file_2018_final.csv")

# Import voter file (filter out one precinct which is empty)
vf <- readr::read_csv(
  vf_path,
  col_types = readr::cols(
    .default = readr::col_character(),
    registration_number = readr::col_double(),
    birthyear = readr::col_double(),
    date_registration = readr::col_date(),
    date_added = readr::col_date(),
    date_changed = readr::col_date(),
    date_last_contact = readr::col_date(),
    whi = readr::col_double(),
    bla = readr::col_double(),
    his = readr::col_double(),
    asi = readr::col_double(),
    oth = readr::col_double())) %>%
  dplyr::filter(precinct_id_2018 != "Fulton,Sc17B")
# Filter out unknown voters
vf <- dplyr::filter(vf, race != "U")
# Select probability and race columns, and add classification labels
vf <- vf %>%
  dplyr::select(race, whi, bla) %>%
  dplyr::rename(
    whi_pred = whi,
    bla_pred = bla) %>%
  dplyr::mutate(
    whi = as.integer(race == "WH"),
    bla = as.integer(race == "BH"))

# Calculate FPR, TPR, and AUC for white predictions
predictions_whi <- ROCR::prediction(vf$whi_pred, vf$whi)
performance_whi <- ROCR::performance(predictions_whi, "tpr", "fpr")
roc_whi <- tibble::tibble(
  fpr = attributes(performance_whi)$x.values[[1]],
  tpr = attributes(performance_whi)$y.values[[1]])
auc_whi <- ROCR::performance(predictions_whi, measure = "auc")
auc_whi <- attributes(auc_whi)$y.values[[1]]
# Calculate FPR, TPR, and AUC for black predictions
predictions_bla <- ROCR::prediction(vf$bla_pred, vf$bla)
performance_bla <- ROCR::performance(predictions_bla, "tpr", "fpr")
roc_bla <- tibble::tibble(
  fpr = attributes(performance_bla)$x.values[[1]],
  tpr = attributes(performance_bla)$y.values[[1]])
auc_bla <- ROCR::performance(predictions_bla, measure = "auc")
auc_bla <- attributes(auc_bla)$y.values[[1]]

### Begin making Figure 1

# Annotation for AUC in white voter plot
whi_annotation <- tibble::tibble(
  x = 0.83,
  y = 0,
  label = paste("AUC = ", round(auc_whi, 3)))
# Annotation for AUC in black voter plot
bla_annotation <- tibble::tibble(
  x = 0.83,
  y = 0,
  label = paste("AUC = ", round(auc_bla, 3)))

# Plot theme
plot_theme <-
  theme_bw() +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 23, face = "bold"),
        axis.title.x = element_text(margin = margin(t = 20)),
        axis.title.y = element_text(margin = margin(r = 20)),
        plot.title = element_text(size = 27, face = "bold", hjust = 0.5),
        legend.position = "none")

# Figure 1a: ROC curve for white voters
whi_roc <- ggplot2::ggplot(
  data = roc_whi, aes(x = fpr, y = tpr)) +
  geom_line(color = "black", size = 2) +
  geom_abline(intercept = 0,
              slope = 1,
              color = "grey",
              lwd = 1,
              linetype = "dashed") +
  geom_label(data = whi_annotation,
             aes(x = x, y = y, label = label),
             color = "black",
             size = 8,
             fontface = "bold") +
  xlim(0, 1) + ylim(0, 1) +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  ggtitle("White Voters") +
  plot_theme +
  coord_fixed()

# Figure 1b: ROC curve for black voters
bla_roc <- ggplot2::ggplot(
  data = roc_bla, aes(x = fpr, y = tpr)) +
  geom_line(color = "black", size = 2) +
  geom_abline(intercept = 0,
              slope = 1,
              color = "grey",
              lwd = 1,
              linetype = "dashed") +
  geom_label(data = bla_annotation,
             aes(x = x, y = y, label = label),
             color = "black",
             size = 8,
             fontface = "bold") +
  xlim(0, 1) + ylim(0, 1) +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  ggtitle("Black Voters") +
  plot_theme +
  coord_fixed()

# Final plot
plot <-
  whi_roc +
  bla_roc +
  plot_annotation(tag_levels = "a") & theme(plot.tag = element_text(size = 27))
ggsave(filename = "FigureA1.pdf", plot = plot, width = 16, height = 8)
