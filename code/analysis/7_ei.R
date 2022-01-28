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
  dplyr::filter(total_votes > 0) %>%
  as_tibble() %>%
  select(-geometry)

# Combine Asian and Other to match up with exit poll
results <- results %>%
  mutate(
    oth_true_prop = oth_true_prop + asi_true_prop,
    oth_bisg_prop = oth_bisg_prop + asi_bisg_prop,
    oth_2018_cvap_ext_prop = oth_2018_cvap_ext_prop + asi_2018_cvap_ext_prop,
    oth_2018_cvap_int_prop = oth_2018_cvap_int_prop + asi_2018_cvap_int_prop
  )

if (verbose) {
  message("Precinct results read in.")
  message("======================================================")
  message("Performing Iterative EI using ground truth...")
}

# Race vectors
race_true <- c("whi_true_prop",
               "bla_true_prop",
               "his_true_prop",
               "oth_true_prop")
race_bisg <- c("whi_bisg_prop",
               "bla_bisg_prop",
               "his_bisg_prop",
               "oth_bisg_prop")
race_cvap <- c("whi_2018_cvap_int_prop",
               "bla_2018_cvap_int_prop",
               "his_2018_cvap_int_prop",
               "oth_2018_cvap_int_prop")

# Candidate vectors
cand_cols <- c("abrams_prop", "kemp_prop")

# Make abrams + kemp == 1 
results <- results %>%
  mutate(
    total = abrams_prop + kemp_prop,
    abrams_prop = abrams_prop / total,
    kemp_prop = kemp_prop / total
  )

# Confirm all proportions are valid
results %>%
  select(any_of(race_true)) %>%
  rowSums()

results %>%
  select(any_of(race_bisg)) %>%
  rowSums()

results %>%
  select(any_of(race_cvap)) %>%
  rowSums()

results %>%
  select(any_of(race_true)) %>%
  rowSums()

results %>%
  select(any_of(cand_cols)) %>%
  rowSums()

# Ground truth: Iterative EI
ei_iter_true <- eiCompare::ei_iter(
  data = results,
  cand_cols = cand_cols,
  race_cols = race_true,
  totals_col = "total_votes",
  verbose = TRUE,
  plots = FALSE,
  par_compute = FALSE,
  name = "",
  seed = 784392)

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

if (verbose) {
  message("RxC EI on ground truth complete.")
  message("======================================================")
  message("Iterative EI using BISG input...")
}

# BISG: Iterative EI
ei_iter_bisg <- eiCompare::ei_iter(
  data = results,
  cand_cols = cand_cols,
  race_cols = race_bisg,
  totals_col = "total_votes",
  verbose = TRUE,
  plots = FALSE,
  par_compute = par_compute,
  name = "iter_bisg",
  seed = 12289115)

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

if (verbose) {
  message("RxC EI on BISG input complete.")
  message("======================================================")
  message("Iterative EI using CVAP input...")
}

# CVAP: Iterative EI
ei_iter_cvap <- eiCompare::ei_iter(
  data = results,
  cand_cols = cand_cols,
  race_cols = race_cvap,
  totals_col = "total_votes",
  verbose = TRUE,
  plots = FALSE,
  par_compute = FALSE,
  name = "iter_cvap",
  seed = 12289115)

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


ei_iter_bisg$estimates$ei_type <- "iter"
ei_iter_bisg$estimates$race_type <- "bisg"
ei_iter_cvap$estimates$ei_type <- "iter"
ei_iter_cvap$estimates$race_type <- "cvap"
ei_iter_true$estimates$ei_type <- "iter"
ei_iter_true$estimates$race_type <- "true"

ei_rxc_bisg$estimates$ei_type <- "rxc"
ei_rxc_bisg$estimates$race_type <- "bisg"
ei_rxc_cvap$estimates$ei_type <- "rxc"
ei_rxc_cvap$estimates$race_type <- "cvap"
ei_rxc_true$estimates$ei_type <- "rxc"
ei_rxc_true$estimates$race_type <- "true"

# Exit polling data brought in from WAPO website:
# https://www.washingtonpost.com/graphics/2018/politics/voter-polls/georgia.html#methodology
# Last accessed Jan 27, 2022
exit_poll_abrams_whi <- data.frame(
  "cand" = "abrams_prop",
  "race" = "whi",
  "mean" = .25,
  "sd" = NA,
  "ci_95_lower" = .23,
  "ci_95_upper" = .27,
  "ei_type" = "exit_polls",
  "race_type" = "exit_polls"
)

exit_poll_abrams_bla <- data.frame(
  "cand" = "abrams_prop",
  "race" = "bla",
  "mean" = .94,
  "sd" = NA,
  "ci_95_lower" = .92,
  "ci_95_upper" = .96,
  "ei_type" = "exit_polls",
  "race_type" = "exit_polls"
)

exit_poll_abrams_his <- data.frame(
  "cand" = "abrams_prop",
  "race" = "his",
  "mean" = .56,
  "sd" = NA,
  "ci_95_lower" = .54,
  "ci_95_upper" = .58,
  "ei_type" = "exit_polls",
  "race_type" = "exit_polls"
)

exit_poll_abrams_oth <- data.frame(
  "cand" = "abrams_prop",
  "race" = "oth",
  "mean" = .61,
  "sd" = NA,
  "ci_95_lower" = .59,
  "ci_95_upper" = .63,
  "ei_type" = "exit_polls",
  "race_type" = "exit_polls"
)

exit_poll_kemp_whi <- data.frame(
  "cand" = "kemp_prop",
  "race" = "whi",
  "mean" = .73,
  "sd" = NA,
  "ci_95_lower" = .71,
  "ci_95_upper" = .75,
  "ei_type" = "exit_polls",
  "race_type" = "exit_polls"
)

exit_poll_kemp_bla <- data.frame(
  "cand" = "kemp_prop",
  "race" = "bla",
  "mean" = .95,
  "sd" = NA,
  "ci_95_lower" = .93,
  "ci_95_upper" = .97,
  "ei_type" = "exit_polls",
  "race_type" = "exit_polls"
)

exit_poll_kemp_his <- data.frame(
  "cand" = "kemp_prop",
  "race" = "his",
  "mean" = .39,
  "sd" = NA,
  "ci_95_lower" = .37,
  "ci_95_upper" = .41,
  "ei_type" = "exit_polls",
  "race_type" = "exit_polls"
)

exit_poll_kemp_oth <- data.frame(
  "cand" = "kemp_prop",
  "race" = "oth",
  "mean" = .35,
  "sd" = NA,
  "ci_95_lower" = .33,
  "ci_95_upper" = .37,
  "ei_type" = "exit_polls",
  "race_type" = "exit_polls"
)

all_ei_results <- as_tibble(rbind(
  ei_iter_bisg$estimates,
  ei_iter_cvap$estimates,
  ei_iter_true$estimates,
  ei_rxc_bisg$estimates,
  ei_rxc_cvap$estimates,
  ei_rxc_true$estimates,
  exit_poll_abrams_whi,
  exit_poll_abrams_bla,
  exit_poll_abrams_his,
  exit_poll_abrams_oth,
  exit_poll_kemp_whi,
  exit_poll_kemp_bla,
  exit_poll_kemp_his,
  exit_poll_kemp_oth
))

saveRDS(all_ei_results, "../../data/ei_results_all.rds")
