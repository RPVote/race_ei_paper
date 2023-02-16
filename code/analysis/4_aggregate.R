#!/usr/bin/env Rscript
#' Analysis script 4 for "Comparing BISG to CVAP Estimates in Racially Polarized
#' Voting Analysis" by Collingwood et al.
#'
#' Run this script once you have extracted the voters from 2018 using the
#' 3_voter_history.R script.
#'
#' This script aggregates the voters in the voter file at the precinct level,
#' using both the self-reported race and BISG predicted race probabilities.
#' Additionally, this script adds election data to the aggregated results.

# Import relevant libraries
suppressWarnings(suppressMessages({
  library(eiCompare)
  library(readr)
  library(sf)
  library(tidyverse)
}))

# Preamble: Adjust these settings according to your use case
# Turn verbosity on or off
verbose <- TRUE
# Set the base path: where all data files are located
base_path <- "../../data"
ga_precincts_2018_path <- file.path(base_path, "ga_2018_election_precincts")
vf_path <- file.path(base_path, "ga_voter_file_2018_final.csv")
agg_path <- file.path(base_path, "ga_2018_agg.csv")

if (verbose) {
  message("======================================================")
  message("Beginning script. Reading in voter file...")
}

# Import voter file and filter out a specific precinct, since it only has one
# voter
vf <-
  readr::read_csv(
    vf_path,
    col_types = readr::cols(
      .default = readr::col_character(),
      registration_number = readr::col_double(),
      birthyear = readr::col_double(),
      date_registration = readr::col_date(),
      date_added = readr::col_date(),
      date_changed = readr::col_date(),
      date_last_contact = readr::col_date(),
      date_last_voted = readr::col_date(),
      whi_bisg = readr::col_double(),
      bla_bisg = readr::col_double(),
      his_bisg = readr::col_double(),
      asi_bisg = readr::col_double(),
      oth_bisg = readr::col_double(),
      whi_fbisg = readr::col_double(),
      bla_fbisg = readr::col_double(),
      his_fbisg = readr::col_double(),
      asi_fbisg = readr::col_double(),
      oth_fbisg = readr::col_double(),
      whi_fbisgf = readr::col_double(),
      bla_fbisgf = readr::col_double(),
      his_fbisgf = readr::col_double(),
      asi_fbisgf = readr::col_double(),
      oth_fbisgf = readr::col_double())) %>%
  dplyr::filter(precinct_id_2018 != "Fulton,Sc17B")  

rows <- nrow(vf)
vf <- vf %>%
  dplyr::filter(race != "U")
(rows - nrow(vf))/rows

if (verbose) {
  message("Voter file read in.")
  message("======================================================")
  message("Aggregating voters...")
}

# Aggregate voters by self-reported race
self_reported <-
  eiCompare::precinct_agg_combine(
    voter_file = vf,
    group_col = "precinct_id_2018",
    true_race_col = "race",
    true_race_keys = list("whi" = "WH",
                          "bla" = "BH",
                          "his" = "HP",
                          "asi" = "AP",
                          "oth" = c("OT", "AI")),
    include_total = TRUE) %>%
  dplyr::rename(
    whi_true_prop = whi_prop,
    bla_true_prop = bla_prop,
    his_true_prop = his_prop,
    asi_true_prop = asi_prop,
    oth_true_prop = oth_prop,
    whi_true_total = whi_total,
    bla_true_total = bla_total,
    his_true_total = his_total,
    asi_true_total = asi_total,
    oth_true_total = oth_total) %>%
  dplyr::select(
    precinct_id_2018,
    whi_true_prop,
    bla_true_prop,
    his_true_prop,
    asi_true_prop,
    oth_true_prop,
    whi_true_total,
    bla_true_total,
    his_true_total,
    asi_true_total,
    oth_true_total)

# Aggregate voters by BISG predicted race
bisg <-
  eiCompare::precinct_agg_combine(
    voter_file = vf,
    group_col = "precinct_id_2018",
    race_cols = c("whi_bisg", "bla_bisg", "his_bisg", "asi_bisg", "oth_bisg"),
    include_total = TRUE) %>%
  dplyr::select(
    precinct_id_2018,
    whi_bisg_prop,
    bla_bisg_prop,
    his_bisg_prop,
    asi_bisg_prop,
    oth_bisg_prop,
    whi_bisg_total,
    bla_bisg_total,
    his_bisg_total,
    asi_bisg_total,
    oth_bisg_total)

fbisg <-
  eiCompare::precinct_agg_combine(
    voter_file = vf,
    group_col = "precinct_id_2018",
    race_cols = c("whi_fbisg", "bla_fbisg", "his_fbisg", "asi_fbisg", "oth_fbisg"),
    include_total = TRUE) %>%
  dplyr::select(
    precinct_id_2018,
    whi_fbisg_prop,
    bla_fbisg_prop,
    his_fbisg_prop,
    asi_fbisg_prop,
    oth_fbisg_prop,
    whi_fbisg_total,
    bla_fbisg_total,
    his_fbisg_total,
    asi_fbisg_total,
    oth_fbisg_total)

fbisgf <-
  eiCompare::precinct_agg_combine(
    voter_file = vf,
    group_col = "precinct_id_2018",
    race_cols = c("whi_fbisgf", "bla_fbisgf", "his_fbisgf", "asi_fbisgf", "oth_fbisgf"),
    include_total = TRUE) %>%
  dplyr::select(
    precinct_id_2018,
    whi_fbisgf_prop,
    bla_fbisgf_prop,
    his_fbisgf_prop,
    asi_fbisgf_prop,
    oth_fbisgf_prop,
    whi_fbisgf_total,
    bla_fbisgf_total,
    his_fbisgf_total,
    asi_fbisgf_total,
    oth_fbisgf_total)


# Bind all self-reported/BISG results together
agg <- dplyr::bind_cols(self_reported, bisg[, -1], fbisg[,-1], fbisgf[,-1])

if (verbose) {
  message("Aggregation complete.")
  message("======================================================")
}

if (verbose) {
  message("======================================================")
  message("Writing aggregation file...")
}

readr::write_csv(agg, agg_path)

if (verbose) {
  message("Aggregation file written. Script complete.")
}
