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

base_path <- "../../../data"
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

if (verbose) {
  message("Precinct results read in.")
  message("======================================================")
  message("Performing Iterative EI using ground truth...")
}

# Combine minority groups
results <- results %>%
  mutate(nwhi_true_prop = bla_true_prop + his_true_prop + oth_true_prop,
         nwhi_bisg_prop = bla_bisg_prop + his_bisg_prop + oth_bisg_prop,
         nwhi_fbisg_prop = bla_fbisg_prop + his_fbisg_prop + oth_fbisg_prop,
         nwhi_fbisgf_prop = bla_fbisgf_prop + his_fbisgf_prop + oth_fbisgf_prop,
         nwhi_2018_cvap_int_prop = bla_2018_cvap_int_prop + 
                                   his_2018_cvap_int_prop +
                                   oth_2018_cvap_int_prop)

# Race vectors
race_true <- c("whi_true_prop",
               "nwhi_true_prop")
race_bisg <- c("whi_bisg_prop",
               "nwhi_bisg_prop")
race_fbisg <- c("whi_fbisg_prop",
               "nwhi_fbisg_prop")
race_fbisgf <- c("whi_fbisgf_prop",
                "nwhi_fbisgf_prop")
race_cvap <- c("whi_2018_cvap_int_prop",
               "nwhi_2018_cvap_int_prop")

# Candidate vectors
cand_cols <- c("abrams_prop", "kemp_prop")

# Make abrams + kemp == 1 
results <- results %>%
  mutate(
    total = abrams_prop + kemp_prop,
    abrams_prop = abrams_prop / total,
    kemp_prop = kemp_prop / total,
    tt_bisg = whi_bisg_prop + nwhi_bisg_prop,
    whi_bisg_prop = whi_bisg_prop / tt_bisg,
    nwhi_bisg_prop = nwhi_bisg_prop / tt_bisg,
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

races <- list("true" = race_true, 'cvap' = race_cvap, 'bisg' = race_bisg,
           'fbisg' = race_fbisg, 'fbisgf' = race_fbisgf)

out_path = "ei_results"

walk(
  1:length(races),
  function(ii) {
    files <- list.files(out_path)
    outfile <- paste0("ei_", names(races)[[ii]], '_iter.rds')
    print(outfile)
    if (!(outfile %in% files)) {
      ei_iter = eiCompare::ei_iter(
        data = results,
        cand_cols = cand_cols,
        race_cols = races[[ii]],
        totals_col = 'total_votes',
        verbose = TRUE,
        plots = FALSE,
        par_compute = TRUE,
        name = names(races)[[ii]],
        seed = 1010101
      )
      ei_iter$estimates$ei_type <- "iter"
      ei_iter$estimates$race_type <- names(races)[[ii]]
      saveRDS(ei_iter, file.path(out_path, outfile)) 
    } else {
      print("Already run!")
    }
  }
)

walk(
  1:length(races),
  function(ii) {
    files <- list.files(out_path)
    outfile <- paste0("ei_", names(races)[[ii]], '_rxc.rds')
    print(outfile)
    if (!(outfile %in% files)) {
      ei_iter = eiCompare::ei_rxc(
        data = results,
        cand_cols = cand_cols,
        race_cols = races[[ii]],
        totals_col = 'total_votes',
        verbose = TRUE,
        par_compute = TRUE,
        ntunes = 10,
        samples = 50000,
        thin = 5,
        n_chains = 1,
        name = names(races)[[ii]],
        seed = 1010101
      )
      ei_iter$estimates$ei_type <- "rxc"
      ei_iter$estimates$race_type <- names(races)[[ii]]
      saveRDS(ei_iter, file.path(out_path, outfile)) 
    } else {
      print("Already run!")
    }
  }
)

ei_results <- map_dfr(
  list.files(out_path),
  function(x) {
    print(x)
    data <- readRDS(file.path(out_path, x)) 
    return(data$estimates %>% select(-any_of("se")))
  }
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

# nwhi = (.94*.31 + .56*.03 + .61*.04) / .38 = 0.875
exit_poll_abrams_nwhi <- data.frame(
  "cand" = "abrams_prop",
  "race" = "nwhi",
  "mean" = .875,
  "sd" = NA,
  "ci_95_lower" = .875 - sqrt(.875*(1-.875)/(3984*.38))*1.96,
  "ci_95_upper" = .875 + sqrt(.875*(1-.875)/(3984*.38))*1.96,
  "ei_type" = "exit_polls",
  "race_type" = "exit_polls"
)

# exit_poll_kemp_whi <- data.frame(
#   "cand" = "kemp_prop",
#   "race" = "whi",
#   "mean" = .73,
#   "sd" = NA,
#   "ci_95_lower" = .71,
#   "ci_95_upper" = .75,
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
  exit_poll_abrams_nwhi
))

saveRDS(all_ei_results, "ei_results_all_combined_minority.rds")
