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
    oth_fbisg_prop = oth_fbisg_prop + asi_fbisg_prop,
    oth_fbisgf_prop = oth_fbisgf_prop + asi_fbisgf_prop,
    oth_2018_cvap_ext_prop = oth_2018_cvap_ext_prop + asi_2018_cvap_ext_prop,
    oth_2018_cvap_int_prop = oth_2018_cvap_int_prop + asi_2018_cvap_int_prop,
    total_votes = whi_true_total + bla_true_total + his_true_total + asi_true_total + oth_true_total
  )

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
    kemp_prop = kemp_prop / total,
    tt = whi_bisg_prop + bla_bisg_prop + his_bisg_prop + oth_bisg_prop,
    whi_bisg_prop = whi_bisg_prop / tt,
    bla_bisg_prop = bla_bisg_prop / tt,
    his_bisg_prop = his_bisg_prop / tt,
    oth_bisg_prop = oth_bisg_prop / tt
  ) 
  

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
  select(any_of(race_cvap)) %>%
  rowSums()

results %>%
  select(any_of(race_true)) %>%
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
saveRDS(ei_iter_true$estimates, "../../data/ei_results/ei_iter_true.rds")

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
saveRDS(ei_rxc_true$estimates, "../../data/ei_results/ei_rxc_true.rds")

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
saveRDS(ei_iter_bisg$estimates, "../../data/ei_results/ei_iter_bisg.rds")

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
saveRDS(ei_rxc_bisg$estimates, "../../data/ei_results/ei_rxc_bisg.rds")

if (verbose) {
  message("RxC EI on BISG input complete.")
  message("======================================================")
  message("Iterative EI using CVAP input...")
}

# CVAP: Iterative EI
ei_iter_cvap <- eiCompare::ei_iter(
  data = results %>% filter(!is.na(whi_2018_cvap_int_prop)),
  cand_cols = cand_cols,
  race_cols = race_cvap,
  totals_col = "total_votes",
  verbose = TRUE,
  plots = FALSE,
  par_compute = par_compute,
  name = "iter_cvap",
  seed = 435575087)
ei_iter_cvap$estimates$ei_type <- "iter"
ei_iter_cvap$estimates$race_type <- "cvap"
saveRDS(ei_iter_cvap$estimates, "../../data/ei_results/ei_iter_cvap.rds")

ei_rxc_cvap <- eiCompare::ei_rxc(
  data = results %>% filter(!is.na(whi_2018_cvap_int_prop)),
  cand_cols = cand_cols,
  race_cols = race_cvap,
  totals_col = "total_votes",
  verbose = TRUE,
  ntunes = 10,
  samples = 50000,
  thin = 5,
  n_chains = 1,
  name = "rxc_cvap",
  seed = 435575087)
ei_rxc_cvap$estimates$ei_type <- "rxc"
ei_rxc_cvap$estimates$race_type <- "cvap"
saveRDS(ei_rxc_cvap$estimates, "../../data/ei_results/ei_rxc_cvap.rds")



# For CVAP, we add a third group to cand_cols
results_cvap <- results %>%
  mutate(abrams_prop = abrams_prop*total_votes/cvap_total,
         kemp_prop = kemp_prop*total_votes/cvap_total,
         novote_prop = (cvap_total-total_votes)/cvap_total) %>%
  filter(novote_prop > 0)
cand_cols_cvap <- c(cand_cols, "novote_prop")
results %>%
  select(any_of(cand_cols_cvap)) %>%
  rowSums()

# CVAP: Iterative EI
ei_iter_cvap_novote <- eiCompare::ei_iter(
  data = results_cvap  %>% filter(!is.na(whi_2018_cvap_int_prop)),
  cand_cols = cand_cols_cvap,
  race_cols = race_cvap,
  totals_col = "total_votes",
  verbose = TRUE,
  plots = FALSE,
  par_compute = par_compute,
  name = "iter_cvap",
  seed = 435575087)
ei_iter_cvap_novote$estimates <- ei_iter_cvap_novote$estimates %>%
  filter(cand != 'novote_prop') %>%
  group_by(race) %>%
  mutate(
    se = (ci_95_lower + mean)/1.96,
    se = se/sum(mean),
    mean = mean/sum(mean),
    ci_95_lower = mean - 1.96*se,
    ci_95_upper = mean + 1.96*se
  )
ei_iter_cvap_novote$estimates$ei_type <- "iter"
ei_iter_cvap_novote$estimates$race_type <- "cvap_novote"
saveRDS(ei_iter_cvap_novote$estimates, "../../data/ei_results/ei_iter_cvap_novote.rds")


# CVAP: RxC EI
ei_rxc_cvap_novote <- eiCompare::ei_rxc(
  data = results_cvap %>% filter(!is.na(whi_2018_cvap_int_prop)),
  cand_cols = cand_cols_cvap,
  race_cols = race_cvap,
  totals_col = "total_votes",
  verbose = TRUE,
  ntunes = 10,
  samples = 50000,
  thin = 5,
  n_chains = 1,
  name = "rxc_cvap",
  seed = 435575087)
ei_rxc_cvap_novote$estimates <- ei_rxc_cvap_novote$estimates %>%
  filter(cand != 'novote_prop') %>%
  group_by(race) %>%
  mutate(    
    se = (ci_95_lower + mean)/1.96,
    se = se/sum(mean),
    mean = mean/sum(mean),
    ci_95_lower = mean - 1.96*se,
    ci_95_upper = mean + 1.96*se
  )
ei_rxc_cvap_novote$estimates$ei_type <- "rxc"
ei_rxc_cvap_novote$estimates$race_type <- "cvap_novote"
saveRDS(ei_rxc_cvap_novote$estimates, "../../data/ei_results/ei_rxc_cvap_novote.rds")

if (verbose) {
  message("RxC EI on CVAP input complete.")
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
saveRDS(ei_iter_fbisg$estimates, "../../data/ei_results/ei_iter_fbisg.rds")

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
saveRDS(ei_rxc_fbisg$estimates, "../../data/ei_results/ei_rxc_fbisg.rds")

if (verbose) {
  message("RxC EI on fBISG input complete.")
  message("======================================================")
  message("Iterative EI using fBISGf input...")
}

# CVAP: Iterative EI
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
saveRDS(ei_iter_fbisgf$estimates, "../../data/ei_results/ei_iter_fbisgf.rds")

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
saveRDS(ei_rxc_fbisgf$estimates, "../../data/ei_results/ei_rxc_fbisgf.rds")

results_path <- "../../data/ei_results"
ei_results <- map_dfr(
  list.files(results_path),
  ~ readRDS(file.path(results_path, .)) %>%
    select(-any_of("se"))
)

# Exit polling data brought in from WAPO website:
# https://www.washingtonpost.com/graphics/2018/politics/voter-polls/georgia.html#methodology
# Last accessed Jan 27, 2022
exit_poll_abrams_whi <- data.frame(
  "cand" = "abrams_prop",
  "race" = "whi",
  "mean" = .25,
  "sd" = NA,
  "ci_95_lower" = .25 - sqrt(.25*(1-.25)/(3984*.63))*1.96,
  "ci_95_upper" = .25 + sqrt(.25*(1-.25)/(3984*.63))*1.96,
  "ei_type" = "exit_polls",
  "race_type" = "exit_polls"
)

exit_poll_abrams_bla <- data.frame(
  "cand" = "abrams_prop",
  "race" = "bla",
  "mean" = .94,
  "sd" = NA,
  "ci_95_lower" = .94 - sqrt(.94*(1-.94)/(3984*.31))*1.96,
  "ci_95_upper" = .94 + sqrt(.94*(1-.94)/(3984*.31))*1.96,
  "ei_type" = "exit_polls",
  "race_type" = "exit_polls"
)

exit_poll_abrams_his <- data.frame(
  "cand" = "abrams_prop",
  "race" = "his",
  "mean" = .56,
  "sd" = NA,
  "ci_95_lower" = .56 - sqrt(.56*(1-.56)/(3984*.03))*1.96,
  "ci_95_upper" = .56 + sqrt(.56*(1-.56)/(3984*.03))*1.96,
  "ei_type" = "exit_polls",
  "race_type" = "exit_polls"
)

exit_poll_abrams_oth <- data.frame(
  "cand" = "abrams_prop",
  "race" = "oth",
  "mean" = .61,
  "sd" = NA,
  "ci_95_lower" = .61 - sqrt(.61*(1-.61)/(3984*.04))*1.96,
  "ci_95_upper" = .61 + sqrt(.61*(1-.61)/(3984*.04))*1.96,
  "ei_type" = "exit_polls",
  "race_type" = "exit_polls"
)

# exit_poll_kemp_whi <- data.frame(
#   "cand" = "kemp_prop",
#   "race" = "whi",
#   "mean" = .73,
#   "sd" = NA,
#   "ci_95_lower" = .73,
#   "ci_95_upper" = .73,
#   "ei_type" = "exit_polls",
#   "race_type" = "exit_polls"
# )
# 
# exit_poll_kemp_bla <- data.frame(
#   "cand" = "kemp_prop",
#   "race" = "bla",
#   "mean" = .95,
#   "sd" = NA,
#   "ci_95_lower" = .93,
#   "ci_95_upper" = .97,
#   "ei_type" = "exit_polls",
#   "race_type" = "exit_polls"
# )
# 
# exit_poll_kemp_his <- data.frame(
#   "cand" = "kemp_prop",
#   "race" = "his",
#   "mean" = .39,
#   "sd" = NA,
#   "ci_95_lower" = .37,
#   "ci_95_upper" = .41,
#   "ei_type" = "exit_polls",
#   "race_type" = "exit_polls"
# )
# 
# exit_poll_kemp_oth <- data.frame(
#   "cand" = "kemp_prop",
#   "race" = "oth",
#   "mean" = .35,
#   "sd" = NA,
#   "ci_95_lower" = .33,
#   "ci_95_upper" = .37,
#   "ei_type" = "exit_polls",
#   "race_type" = "exit_polls"
# )

all_ei_results <- as_tibble(rbind(
  ei_results,
  exit_poll_abrams_whi,
  exit_poll_abrams_bla,
  exit_poll_abrams_his,
  exit_poll_abrams_oth#,
  #exit_poll_kemp_whi,
  #exit_poll_kemp_bla,
  #exit_poll_kemp_his,
  #exit_poll_kemp_oth
))

saveRDS(all_ei_results, "../../data/ei_results_all.rds")
