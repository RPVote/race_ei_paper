#!/usr/bin/env Rscript
#' Analysis script 7 for "Comparing BISG to CVAP Estimates in Racially Polarized
#' Voting Analysis" by Collingwood et al.
#'
#' Run this script once you have obtained the final results file from the
#' previous six scripts.
#'
#' This script performs the EI analysis on the Georgia data using ground truth,
#' BISG, and CVAP inputs.

# Import relevant libraries
suppressWarnings(suppressMessages({
  library(eiCompare)
  library(readr)
  library(tidyverse)
}))

# Preamble: Adjust these settings according to your use case
# Turn verbosity on or off
verbose <- TRUE
# Set the base path: where all data files are located

# Set working directory to folder in which this file is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

base_path <- "../../data"
ga_agg_path <- file.path(base_path, "ga_2018_agg_all.rds")
ei_results_path <- file.path(base_path, "ga_2018_ei_results.csv")
# Turn on for parallel computing
par_compute <- TRUE

if (verbose) {
  message("======================================================")
  message("Beginning script. Reading in precinct results...")
}

# Import election results
results <- readRDS(ga_agg_path) %>%
  # Filter out precincts with no votes
  dplyr::filter(total_votes > 0)

if (verbose) {
  message("Precinct results read in.")
  message("======================================================")
  message("Performing Iterative EI using ground truth...")
}

# Race vectors
race_true <- c("whi_true_prop",
               "bla_true_prop",
               "his_true_prop",
               "asi_true_prop",
               "oth_true_prop")
race_bisg <- c("whi_bisg_prop",
               "bla_bisg_prop",
               "his_bisg_prop",
               "asi_bisg_prop",
               "oth_bisg_prop")
race_cvap <- c("whi_2018_cvap_int_prop",
               "bla_2018_cvap_int_prop",
               "his_2018_cvap_int_prop",
               "asi_2018_cvap_int_prop",
               "oth_2018_cvap_int_prop")

# Candidate vectors
cand_cols <- c("abrams_prop", "kemp_prop", "metz_prop")

# Ground truth: Iterative EI
ei_iter_true <- eiCompare::ei_iter(
  data = results,
  cand_cols = cand_cols[1:2],
  race_cols = race_true[1:2],
  totals_col = "total_votes",
  verbose = TRUE,
  plots = FALSE,
  par_compute = par_compute,
  name = "",
  seed = 784392)
ei_iter_true_summary <- summary(ei_iter_true)
rm(ei_iter_true)
# # Store results
# mean <- c(ei_iter_true_summary$whi_true_prop$mean,
#           ei_iter_true_summary$bla_true_prop$mean)
# sd <- c(ei_iter_true_summary$whi_true_prop$sd,
#         ei_iter_true_summary$bla_true_prop$sd)
# ci_95_lower <- c(ei_iter_true_summary$whi_true_prop$ci_95_lower,
#                  ei_iter_true_summary$bla_true_prop$ci_95_lower)
# ci_95_upper <- c(ei_iter_true_summary$whi_true_prop$ci_95_upper,
#                  ei_iter_true_summary$bla_true_prop$ci_95_upper)

if (verbose) {
  message("Iterative EI on ground truth complete.")
  message("Performing RxC EI using ground truth...")
}

# Ground truth: RxC EI
ei_rxc_true <- eiCompare::ei_rxc(
  data = results,
  cand_cols = cand_cols,
  race_cols = race_true,
  totals_col = "total_votes",
  verbose = TRUE,
  ntunes = 10,
  samples = 50000,
  thin = 5,
  n_chains = 1,
  name = "rxc_true",
  seed = 76689115)
ei_rxc_true_summary <- summary(ei_rxc_true)
rm(ei_rxc_true)
# Store results
# mean <- c(mean,
#           ei_rxc_true_summary$whi_true_prop$mean[1:2],
#           ei_rxc_true_summary$bla_true_prop$mean[1:2])
# sd <- c(sd,
#         ei_rxc_true_summary$whi_true_prop$sd[1:2],
#         ei_rxc_true_summary$bla_true_prop$sd[1:2])
# ci_95_lower <- c(ci_95_lower,
#                  ei_rxc_true_summary$whi_true_prop$ci_95_lower[1:2],
#                  ei_rxc_true_summary$bla_true_prop$ci_95_lower[1:2])
# ci_95_upper <- c(ci_95_upper,
#                  ei_rxc_true_summary$whi_true_prop$ci_95_upper[1:2],
#                  ei_rxc_true_summary$bla_true_prop$ci_95_upper[1:2])

if (verbose) {
  message("RxC EI on ground truth complete.")
  message("======================================================")
  message("Iterative EI using BISG input...")
}

# BISG: Iterative EI
ei_iter_bisg <- eiCompare::ei_iter(
  data = results,
  cand_cols = cand_cols[1:2],
  race_cols = race_bisg[1:2],
  totals_col = "total_votes",
  verbose = TRUE,
  plots = FALSE,
  par_compute = par_compute,
  name = "iter_bisg",
  seed = 12289115)
ei_iter_bisg_summary <- summary(ei_iter_bisg)
# rm(ei_iter_bisg)
# mean <- c(mean,
#           ei_iter_bisg_summary$whi_true_prop$mean,
#           ei_iter_bisg_summary$bla_true_prop$mean)
# sd <- c(sd,
#         ei_iter_bisg_summary$whi_true_prop$sd,
#         ei_iter_bisg_summary$bla_true_prop$sd)
# ci_95_lower <- c(ci_95_lower,
#                  ei_iter_bisg_summary$whi_true_prop$ci_95_lower,
#                  ei_iter_bisg_summary$bla_true_prop$ci_95_lower)
# ci_95_upper <- c(ci_95_upper,
#                  ei_iter_bisg_summary$whi_true_prop$ci_95_upper,
#                  ei_iter_bisg_summary$bla_true_prop$ci_95_upper)

if (verbose) {
  message("Iterative EI on BISG input complete.")
  message("Performing RxC EI using BISG input...")
}

# BISG: RxC EI
ei_rxc_bisg <- eiCompare::ei_rxc(
  data = results,
  cand_cols = cand_cols,
  race_cols = race_bisg,
  totals_col = "total_votes",
  verbose = TRUE,
  ntunes = 10,
  samples = 50000,
  thin = 5,
  n_chains = 1,
  name = "rxc_true",
  seed = 435575087)
ei_rxc_bisg_summary <- summary(ei_rxc_bisg)
# rm(ei_rxc_bisg)
# mean <- c(mean,
#           ei_rxc_bisg_summary$whi_true_prop$mean[1:2],
#           ei_rxc_bisg_summary$bla_true_prop$mean[1:2])
# sd <- c(sd,
#         ei_rxc_bisg_summary$whi_true_prop$sd[1:2],
#         ei_rxc_bisg_summary$bla_true_prop$sd[1:2])
# ci_95_lower <- c(ci_95_lower,
#                  ei_rxc_bisg_summary$whi_true_prop$ci_95_lower[1:2],
#                  ei_rxc_bisg_summary$bla_true_prop$ci_95_lower[1:2])
# ci_95_upper <- c(ci_95_upper,
#                  ei_rxc_bisg_summary$whi_true_prop$ci_95_upper[1:2],
#                  ei_rxc_bisg_summary$bla_true_prop$ci_95_upper[1:2])

if (verbose) {
  message("RxC EI on BISG input complete.")
  message("======================================================")
  message("Iterative EI using CVAP input...")
}

# CVAP: Iterative EI
ei_iter_cvap <- eiCompare::ei_iter(
  data = results,
  cand_cols = cand_cols[1:2],
  race_cols = race_cvap[1:2],
  totals_col = "total_votes",
  verbose = TRUE,
  plots = FALSE,
  par_compute = par_compute,
  name = "iter_bisg",
  seed = 12289115)
ei_iter_bisg_summary <- summary(ei_iter_bisg)

# CVAP: RxC EI
ei_rxc_cvap <- eiCompare::ei_rxc(
  data = results,
  cand_cols = cand_cols,
  race_cols = race_cvap,
  totals_col = "total_votes",
  verbose = TRUE,
  ntunes = 10,
  samples = 50000,
  thin = 5,
  n_chains = 1,
  name = "rxc_true",
  seed = 435575087)
ei_rxc_bisg_summary <- summary(ei_rxc_bisg)

# Extra preprocessing on the CVAP data
# results <-
#   results %>%
#   dplyr::filter(precinct_id_2018 != "Glynn,Jekyll Island") %>%
#   dplyr::mutate(
#     abrams_prop = abrams_total / cvap_total,
#     kemp_prop = kemp_total / cvap_total,
#     metz_prop = metz_total / cvap_total,
#     other_prop = (cvap_total - total_votes) / cvap_total) %>%
#   dplyr::filter(other_prop >= 0)

#  <- with(prec_res_s, abrams_total / cvap_total)
# prec_res_s$kemp_prop <- with(prec_res_s, kemp_total / cvap_total)
# prec_res_s$metz_prop <- with(prec_res_s, metz_total / cvap_total)
# prec_res_s$pct_novote <- with(prec_res_s, (cvap_total-totalvotes)/cvap_total)

# Cut out precincts that have negative pct_no_vote #
# Due to quasi-shitty spatial join                 #


