#!/usr/bin/env Rscript
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
results_2016_path <- file.path(base_path, "ersd_all.rds")

if (verbose) {
  message("======================================================")
  message("Beginning script. Reading in precinct results...")
}

results <- readRDS(results_2016_path)

results <- results %>%
  mutate(
    oth_cvap = 1 - whi_2016_cvap_int_prop - bla_2016_cvap_int_prop,
    oth_bisg_prop = 1 - whi_bisg_prop - bla_bisg_prop,
    oth_fbisg_prop = 1 - whi_fbisg_prop - bla_fbisg_prop,
    oth_fbisgf_prop = 1 - whi_fbisgf_prop - bla_fbisgf_prop
  )

cand_cols <- c("morales_prop", "weissmandl_prop")
race_cvap_cols <- c("whi_2016_cvap_int_prop", "bla_2016_cvap_int_prop", "oth_cvap")
race_bisg_cols <- c("whi_bisg_prop", "bla_bisg_prop", "oth_bisg_prop")
race_fbisg_cols <- c("whi_fbisg_prop", 'bla_fbisg_prop', 'oth_fbisg_prop')
race_fbisgf_cols <- c("whi_fbisgf_prop", 'bla_fbisgf_prop', 'oth_fbisgf_prop')
totals_col <- "total"

results %>%
  select(any_of(race_cvap_cols)) %>%
  rowSums()

results %>%
  select(any_of(race_bisg_cols)) %>%
  rowSums()

results %>%
  select(any_of(race_fbisg_cols)) %>%
  rowSums()

results %>%
  select(any_of(race_fbisg_cols)) %>%
  rowSums()

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
  erho = seq(100),
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

if (verbose) {
  message("RxC EI using BISG complete.")
  message("Performing Iterative EI using FBISG...")
}

ei_iter_fbisg <- eiCompare::ei_iter(
  data = results,
  cand_cols = cand_cols,
  race_cols = race_fbisg_cols,
  totals_col = totals_col,
  verbose = TRUE,
  plots = FALSE,
  par_compute = FALSE,
  name = "",
  erho = seq(100),
  seed = 8432)

if (verbose) {
  message("Iterative EI using fBISG complete.")
  message("Performing RxC EI using FBISG...")
}

ei_rxc_fbisg <- eiCompare::ei_rxc(
  data = results,
  cand_cols = cand_cols,
  race_cols = race_fbisg_cols,
  totals_col = totals_col,
  verbose = TRUE,
  ntunes = 10,
  samples = 100000,
  thin = 5,
  n_chains = 10,
  name = "rxc_true",
  seed = 76689115)

if (verbose) {
  message("RxC EI using FBISG complete.")
  message("Performing Iterative EI using FBISG with first names...")
}

ei_iter_fbisgf <- eiCompare::ei_iter(
  data = results,
  cand_cols = cand_cols,
  race_cols = race_fbisgf_cols,
  totals_col = totals_col,
  verbose = TRUE,
  plots = FALSE,
  par_compute = FALSE,
  name = "",
  erho = seq(100),
  seed = 8432)

if (verbose) {
  message("Iterative EI using FBISG with first names complete.")
  message("Performing RxC EI using FBISG with first names...")
}

ei_rxc_fbisgf <- eiCompare::ei_rxc(
  data = results,
  cand_cols = cand_cols,
  race_cols = race_fbisgf_cols,
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
ei_iter_fbisg$estimates$ei_type <- "iter"
ei_iter_fbisg$estimates$race_type <- "fbisg"
ei_iter_fbisgf$estimates$ei_type <- "iter"
ei_iter_fbisgf$estimates$race_type <- "fbisgf"

ei_rxc_bisg$estimates$ei_type <- "rxc"
ei_rxc_bisg$estimates$race_type <- "bisg"
ei_rxc_cvap$estimates$ei_type <- "rxc"
ei_rxc_cvap$estimates$race_type <- "cvap"
ei_rxc_fbisg$estimates$ei_type <- "rxc"
ei_rxc_fbisg$estimates$race_type <- "fbisg"
ei_rxc_fbisgf$estimates$ei_type <- "rxc"
ei_rxc_fbisgf$estimates$race_type <- "fbisgf"

all_ei_results <- as_tibble(rbind(
  ei_iter_bisg$estimates,
  ei_iter_cvap$estimates,
  ei_iter_fbisg$estimates,
  ei_iter_fbisgf$estimates,
  ei_rxc_bisg$estimates,
  ei_rxc_cvap$estimates,
  ei_rxc_fbisg$estimates,
  ei_rxc_fbisgf$estimates
))

saveRDS(all_ei_results, "../../data/ersd_ei_results.rds")
