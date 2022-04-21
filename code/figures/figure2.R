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
  library(ggExtra)
  library(ggpubr)
}))

# Turn verbosity on or off
verbose <- TRUE
# Set working directory to folder in which this file is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Set the base path: where all data files are located
base_path <- "../../data"
agg_path <- file.path(base_path, "ga_2018_agg_all.rds")
race_ests <- readRDS(agg_path) %>%
  select(-geometry)

# Plot theme
alpha <- 0.25
plot_theme <-
  theme_bw() +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 23, face = "bold"),
        axis.title.x = element_text(margin = margin(t = 20)),
        axis.title.y = element_text(margin = margin(r = 20)),
        plot.title = element_text(size = 27, face = "bold"),
        legend.position = "none")

# Compute Theil's entropy index and Brier score
race_ests <- race_ests %>%
  mutate(
    # Here, we force the entropy equation to set p*log(p) = 0 when p = 0.
    # This ensures a completely absent group maximally indicates segregation.
    # If this adjustment is not made, all precincts with a zero proportion
    # will end up as NAs

    # Discussion w/ Stephen Jenkins:
    # https://www.statalist.org/forums/forum/general-stata-discussion/
    # general/1329390-decompose-theil-index-for-data-including-zero-values-no-income-earnings

    # Refs:
    # Liao., T (2016). Evaluating Distributional Differences in Income Inequality with Sampling Weights
    # Morrisson, C. and Murtin, F. (2013). The Kuznets curve of human capital inequality: 1870–2010.
    #     Journal of Economic Inequality, 11(3), 283–301.

    # Note, we use VAP to estimate segregation because it reflects neighborhood
    # segregation, rather than the turned out or voting-eligible population
    whi_theil = whi_2018_cvap_ext_prop*log(whi_2018_cvap_ext_prop),
    bla_theil = bla_2018_cvap_ext_prop*log(bla_2018_cvap_ext_prop),
    his_theil = his_2018_cvap_ext_prop*log(his_2018_cvap_ext_prop),
    asi_theil = asi_2018_cvap_ext_prop*log(asi_2018_cvap_ext_prop),
    oth_theil = oth_2018_cvap_ext_prop*log(oth_2018_cvap_ext_prop),

    whi_theil = ifelse(is.na(whi_theil) | whi_theil == -Inf, 0, whi_theil),
    bla_theil = ifelse(is.na(bla_theil) | bla_theil == -Inf, 0, bla_theil),
    his_theil = ifelse(is.na(his_theil) | his_theil == -Inf, 0, his_theil),
    asi_theil = ifelse(is.na(asi_theil) | asi_theil == -Inf, 0, asi_theil),
    oth_theil = ifelse(is.na(oth_theil) | oth_theil == -Inf, 0, oth_theil),

    theil = -1 * (whi_theil + bla_theil + his_theil + asi_theil + oth_theil),

    # BISG Deviations
    whi_err_bisg = whi_bisg_prop - whi_true_prop,
    bla_err_bisg = bla_bisg_prop - bla_true_prop,
    his_err_bisg = his_bisg_prop - his_true_prop,
    asi_err_bisg = asi_bisg_prop - asi_true_prop,
    oth_err_bisg = oth_bisg_prop - oth_true_prop,

    # CVAP Deviations
    whi_err_cvap = whi_2018_cvap_ext_prop - whi_true_prop,
    bla_err_cvap = bla_2018_cvap_ext_prop - bla_true_prop,
    his_err_cvap = his_2018_cvap_ext_prop - his_true_prop,
    asi_err_cvap = asi_2018_cvap_ext_prop - asi_true_prop,
    oth_err_cvap = oth_2018_cvap_ext_prop - oth_true_prop,

    # # VAP Deviations
    # whi_err_vap = whi_2010_vap_ext_prop - whi_true_prop,
    # bla_err_vap = bla_2010_vap_ext_prop - bla_true_prop,
    # his_err_vap = his_2010_vap_ext_prop - his_true_prop,
    # asi_err_vap = asi_2010_vap_ext_prop - asi_true_prop,
    # oth_err_vap = oth_2010_vap_ext_prop - oth_true_prop,

    # Brier Scores
    brier_score_bisg = (whi_err_bisg^2 + bla_err_bisg^2 + his_err_bisg^2 + asi_err_bisg^2 + oth_err_bisg^2)/5,
    brier_score_cvap = (whi_err_cvap^2 + bla_err_cvap^2 + his_err_cvap^2 + asi_err_cvap^2 + oth_err_cvap^2)/5,
    # brier_score_vap = (whi_err_vap^2 + bla_err_vap^2 + his_err_vap^2 + asi_err_vap^2 + oth_err_vap^2)/5,

    # BISG fractional errors
    whi_frac_err_bisg = whi_err_bisg / whi_bisg_prop,
    bla_frac_err_bisg = bla_err_bisg / bla_bisg_prop,
    his_frac_err_bisg = his_err_bisg / his_bisg_prop,
    asi_frac_err_bisg = asi_err_bisg / asi_bisg_prop,
    oth_frac_err_bisg = oth_err_bisg / oth_bisg_prop,

    # CVAP fractional errors
    whi_frac_err_cvap = whi_err_cvap / whi_2018_cvap_ext_prop,
    bla_frac_err_cvap = bla_err_cvap / bla_2018_cvap_ext_prop,
    his_frac_err_cvap = his_err_cvap / his_2018_cvap_ext_prop,
    asi_frac_err_cvap = asi_err_cvap / asi_2018_cvap_ext_prop,
    oth_frac_err_cvap = oth_err_cvap / oth_2018_cvap_ext_prop,

    # VAP fractional errors
    # whi_frac_err_vap = whi_err_vap / whi_2010_vap_ext_prop,
    # bla_frac_err_vap = bla_err_vap / bla_2010_vap_ext_prop,
    # his_frac_err_vap = his_err_vap / his_2010_vap_ext_prop,
    # asi_frac_err_vap = asi_err_vap / asi_2010_vap_ext_prop,
    # oth_frac_err_vap = oth_err_vap / oth_2010_vap_ext_prop

  )

race_ests %>%
  dplyr::rename(
    "BISG" = brier_score_bisg,
    "CVAP" = brier_score_cvap,
  ) %>%
  pivot_longer(cols = one_of("BISG", "CVAP"), names_to = "Estimate:", values_to = "brier")  %>%
  ggplot(aes(x = theil, y = brier)) +
  geom_point(alpha = 0) +
  geom_smooth(method = 'loess', size = 2, aes(linetype = `Estimate:`), color = 'black') +
  geom_hline(yintercept = .01, size = .5, linetype = "dashed") +
  geom_hline(yintercept = .003, size = .5, linetype = "dashed") +
  geom_vline(xintercept = 0.9, size = .5, linetype = "dashed") +
  ggtitle("a") +
  xlab("Theil's Entropy Index") +
  ylab("Brier Score") +
  scale_linetype_manual(values = c("solid", "dotted")) +
  scale_y_log10(
    expand = c(0.01,0.01),
    breaks = c(.00001, .0001, .001, .003, .01),
    labels = c("1e-5", "1-e4", ".001", ".003", ".01")
  ) +
  scale_x_continuous(expand = c(0.01,0.01), breaks = c(0, .5, .9, 1.0)) +
  coord_cartesian(ylim = c(.000005, .013)) +
  plot_theme +
  theme(legend.position = c(.85,.2),
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.key.size = unit(1, "in"),
        legend.background = element_blank())
ggsave(filename = "Figure2_left.pdf", width = 10, height = 10)

plot_left <- race_ests %>%
  ggplot(aes(x = theil)) +
  stat_bin(
    aes(y = cumsum(..density..)/40),
    geom = "smooth",
    color = 'black', 
    binwidth = .025
  ) +
  scale_x_continuous(expand = c(0.01,0.01), breaks = c(0, .5, .9, 1.0, 1.5)) +
  scale_y_continuous(expand = c(0.01,0.01)) +
  geom_vline(xintercept = 0.9, size = .5, linetype = "dashed") +
  ggtitle("b") +
  xlab("Theil's Entropy Index") +
  ylab("Cumulative Density") +
  plot_theme 
plot_left
ggsave(filename = "Figure2_right.pdf",  width = 10, height = 10)
