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

base_path <- "~/data/voting"
results_2016_path <- file.path(base_path, "ersd_2016_results.rds")

if (verbose) {
  message("======================================================")
  message("Beginning script. Reading in precinct results...")
}

results <- readRDS(results_2016_path)
cand_cols <- c("morales_prop", "weissmandl_prop")
race_cvap_cols <- c("whi_cvap", "bla_cvap", "oth_cvap")
race_bisg_cols <- c("whi_bisg", "bla_bisg", "oth_bisg")
totals_col <- "total"

if (verbose) {
  message("Precinct results read in.")
  message("======================================================")
  message("Performing Iterative EI using CVAP...")
}

ei_iter_cvap <- eiCompare::ei_iter(
  data = results,
  cand_cols = cand_cols,
  race_cols = race_cvap_cols,
  totals_col = totals_col,
  verbose = TRUE,
  plots = FALSE,
  par_compute = FALSE,
  name = "",
  erho = 1,
  seed = 7439)

if (verbose) {
  message("Iterative EI using CVAP complete.")
  message("Performing RxC EI using CVAP...")
}

ei_rxc_cvap <- eiCompare::ei_rxc(
  data = results,
  cand_cols = cand_cols,
  race_cols = race_cvap_cols,
  totals_col = totals_col,
  verbose = TRUE,
  ntunes = 10,
  samples = 100000,
  thin = 5,
  n_chains = 10,
  name = "rxc_true",
  seed = 76689115)

if (verbose) {
  message("RxC EI on CVAP complete.")
  message("======================================================")
  message("Iterative EI using BISG input...")
}

ei_iter_bisg <- eiCompare::ei_iter(
  data = results,
  cand_cols = cand_cols,
  race_cols = race_bisg_cols,
  totals_col = totals_col,
  verbose = TRUE,
  plots = FALSE,
  par_compute = FALSE,
  name = "",
  erho = 0.5,
  seed = 8432)

if (verbose) {
  message("Iterative EI using BISG complete.")
  message("Performing RxC EI using BISG...")
}

ei_rxc_bisg <- eiCompare::ei_rxc(
  data = results,
  cand_cols = cand_cols,
  race_cols = race_bisg_cols,
  totals_col = totals_col,
  verbose = TRUE,
  ntunes = 10,
  samples = 100000,
  thin = 5,
  n_chains = 10,
  name = "rxc_true",
  seed = 76689115)

ei_iter_bisg$estimates$ei_type <- "iter"
ei_iter_bisg$estimates$race_type <- "bisg"
ei_iter_cvap$estimates$ei_type <- "iter"
ei_iter_cvap$estimates$race_type <- "cvap"

ei_rxc_bisg$estimates$ei_type <- "rxc"
ei_rxc_bisg$estimates$race_type <- "bisg"
ei_rxc_cvap$estimates$ei_type <- "rxc"
ei_rxc_cvap$estimates$race_type <- "cvap"

all_ei_results <- as_tibble(rbind(
  ei_iter_bisg$estimates,
  ei_iter_cvap$estimates,
  ei_rxc_bisg$estimates,
  ei_rxc_cvap$estimates
))

saveRDS(all_ei_results, "~/data/voting/ersd_ei_results.rds")
