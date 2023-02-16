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

ga_agg_path <- file.path("ga_2018_agg_inc_unk.csv")
ei_results_path <- file.path("ga_2018_ei_unk.csv")
# Turn on for parallel computing
par_compute <- TRUE

if (verbose) {
  message("======================================================")
  message("Beginning script. Reading in precinct results...")
}

# Import election results
results <- read_csv(ga_agg_path) %>%
  # Filter out precincts with no votes
  dplyr::filter(total_votes > 0) 

# Combine Asian and Other to match up with exit poll
results <- results %>%
  mutate(
    oth_true_prop = oth_true_prop + asi_true_prop,
    oth_bisg_prop = oth_bisg_prop + asi_bisg_prop,
    oth_fbisg_prop = oth_fbisg_prop + asi_fbisg_prop,
    oth_fbisgf_prop = oth_fbisgf_prop + asi_fbisgf_prop,
    total_votes = whi_true_total + bla_true_total + his_true_total + asi_true_total + oth_true_total
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
race_fbisg <- c("whi_fbisg_prop",
               "bla_fbisg_prop",
               "his_fbisg_prop",
               "oth_fbisg_prop")
race_fbisgf <- c("whi_fbisgf_prop",
               "bla_fbisgf_prop",
               "his_fbisgf_prop",
               "oth_fbisgf_prop")

# Candidate vectors
cand_cols <- c("abrams_prop", "kemp_prop")

# Make abrams + kemp == 1 
results <- results %>%
  mutate(
    total = abrams_prop + kemp_prop,
    abrams_prop = abrams_prop / total,
    kemp_prop = kemp_prop / total,
    tt_true = whi_true_prop + bla_true_prop + his_true_prop + oth_true_prop,
    whi_true_prop = whi_true_prop / tt_true,
    bla_true_prop = bla_true_prop / tt_true,
    his_true_prop = his_true_prop / tt_true,
    oth_true_prop = oth_true_prop / tt_true,
    tt_bisg = whi_bisg_prop + bla_bisg_prop + his_bisg_prop + oth_bisg_prop,
    whi_bisg_prop = whi_bisg_prop / tt_bisg,
    bla_bisg_prop = bla_bisg_prop / tt_bisg,
    his_bisg_prop = his_bisg_prop / tt_bisg,
    oth_bisg_prop = oth_bisg_prop / tt_bisg,
    tt_fbisg = whi_fbisg_prop + bla_fbisg_prop + his_fbisg_prop + oth_fbisg_prop,
    whi_fbisg_prop = whi_fbisg_prop / tt_fbisg,
    bla_fbisg_prop = bla_fbisg_prop / tt_fbisg,
    his_fbisg_prop = his_fbisg_prop / tt_fbisg,
    oth_fbisg_prop = oth_fbisg_prop / tt_fbisg,
    tt_fbisgf = whi_fbisgf_prop + bla_fbisgf_prop + his_fbisgf_prop + oth_fbisgf_prop,
    whi_fbisgf_prop = whi_fbisgf_prop / tt_fbisgf,
    bla_fbisgf_prop = bla_fbisgf_prop / tt_fbisgf,
    his_fbisgf_prop = his_fbisgf_prop / tt_fbisgf,
    oth_fbisgf_prop = oth_fbisgf_prop / tt_fbisgf
  ) 

list.files("ei_results")

# Confirm all proportions are valid
results %>%
  select(any_of(race_true)) %>%
  rowSums()

results %>%
  select(any_of(race_bisg)) %>%
  rowSums()

results %>%
  select(any_of(race_fbisg)) %>%
  rowSums()

results %>%
  select(any_of(race_fbisgf)) %>%
  rowSums()

results %>%
  select(any_of(cand_cols)) %>%
  rowSums()

if (verbose) {
  message("Precinct results read in.")
  message("======================================================")
  message("Performing Iterative EI using ground truth...")
}

# Ground truth: Iterative EI
ei_iter_true <- eiCompare::ei_iter(
  data = results,
  cand_cols = cand_cols,
  race_cols = race_true,
  totals_col = "total_votes",
  verbose = TRUE,
  erho = c(100, 75, 50, 25, 1, .5, .25, .1, .01),
  plots = FALSE,
  par_compute = par_compute,
  name = "ei_iter_true",
  seed = 2748934)
ei_iter_true$estimates$ei_type <- "iter"
ei_iter_true$estimates$race_type <- "true"
saveRDS(ei_iter_true$estimates, "ei_results/ei_iter_true.rds")

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
ei_rxc_true$estimates$ei_type <- "rxc"
ei_rxc_true$estimates$race_type <- "true"
saveRDS(ei_rxc_true$estimates, "ei_results/ei_rxc_true.rds")

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
ei_iter_bisg$estimates$ei_type <- "iter"
ei_iter_bisg$estimates$race_type <- "bisg"
saveRDS(ei_iter_bisg$estimates, "ei_results/ei_iter_bisg.rds")

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
ei_rxc_bisg$estimates$ei_type <- "rxc"
ei_rxc_bisg$estimates$race_type <- "bisg"
saveRDS(ei_rxc_bisg$estimates, "ei_results/ei_rxc_bisg.rds")

if (verbose) {
  message("======================================================")
  message("Iterative EI using fBISG input...")
}

# fBISG: Iterative EI
ei_iter_fbisg <- eiCompare::ei_iter(
  data = results,
  cand_cols = cand_cols,
  race_cols = race_fbisg,
  totals_col = "total_votes",
  erho = c(100, 75, 50, 25, 1, .5, .25, .1, .01),
  verbose = TRUE,
  plots = FALSE,
  par_compute = TRUE,
  name = "iter_fbisg",
  seed = 435575087)
ei_iter_fbisg$estimates$ei_type <- "iter"
ei_iter_fbisg$estimates$race_type <- "fbisg"
saveRDS(ei_iter_fbisg$estimates, "ei_results/ei_iter_fbisg.rds")

# fBISG: RxC EI
ei_rxc_fbisg <- eiCompare::ei_rxc(
  data = results,
  cand_cols = cand_cols,
  race_cols = race_fbisg,
  totals_col = "total_votes",
  verbose = TRUE,
  ntunes = 10,
  samples = 50000,
  thin = 5,
  n_chains = 1,
  name = "rxc_fbisg",
  seed = 435575087)
ei_rxc_fbisg$estimates$ei_type <- "rxc"
ei_rxc_fbisg$estimates$race_type <- "fbisg"
saveRDS(ei_rxc_fbisg$estimates, "ei_results/ei_rxc_fbisg.rds")

if (verbose) {
  message("RxC EI on fBISG input complete.")
  message("======================================================")
  message("Iterative EI using fBISGf input...")
}

# fbisgf: Iterative EI
ei_iter_fbisgf <- eiCompare::ei_iter(
  data = results,
  cand_cols = cand_cols,
  race_cols = race_fbisgf,
  totals_col = "total_votes",
  erho = c(100, 75, 50, 25, 1, .5, .25, .1, .01),
  verbose = TRUE,
  plots = FALSE,
  par_compute = TRUE,
  name = "iter_fbisgf",
  seed = 435575087)
ei_iter_fbisgf$estimates$ei_type <- "iter"
ei_iter_fbisgf$estimates$race_type <- "fbisgf"
saveRDS(ei_iter_fbisgf$estimates, "ei_results/ei_iter_fbisgf.rds")

# CVAP: RxC EI
ei_rxc_fbisgf <- eiCompare::ei_rxc(
  data = results,
  cand_cols = cand_cols,
  race_cols = race_fbisgf,
  totals_col = "total_votes",
  verbose = TRUE,
  ntunes = 10,
  samples = 50000,
  thin = 5,
  n_chains = 1,
  name = "rxc_fbisgf",
  seed = 435575087)
ei_rxc_fbisgf$estimates$ei_type <- "rxc"
ei_rxc_fbisgf$estimates$race_type <- "fbisgf"
saveRDS(ei_rxc_fbisgf$estimates, "ei_results/ei_rxc_fbisgf.rds")

results_path <- "ei_results"
ei_results <- map_dfr(
  list.files(results_path),
  ~ readRDS(file.path(results_path, .)) %>%
    select(-any_of("se"))
)

ei_results <- ei_results %>%
  mutate(race_type = paste0(race_type, "_unk"))

saveRDS(ei_results, "ei_results_unk.rds")
