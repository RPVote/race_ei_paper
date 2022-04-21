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

# Set working directory to folder in which this file is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Preamble: Adjust these settings according to your use case
# Turn verbosity on or off
verbose <- TRUE
# Set the base path: where all data files are located
base_path <- "../../../data"
ga_precincts_2018_path <- file.path(base_path, "ga_2018_election_precincts")
vf_path <- file.path(base_path, "ga_voter_file_2018_final.csv")

out_path <- file.path("ga_2018_agg_inc_unk.csv")

if (verbose) {
  message("======================================================")
  message("Beginning script. Reading in voter file...")
}

# Import voter file and filter out a specific precinct, since it only has one
# voter
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
    date_last_voted = readr::col_date(),
    whi = readr::col_double(),
    bla = readr::col_double(),
    his = readr::col_double(),
    asi = readr::col_double(),
    oth = readr::col_double())) %>%
  dplyr::filter(precinct_id_2018 != "Fulton,Sc17B")  

# HERE IS WHERE UNK IS FILTERED IN MAIN ANALYSIS

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
    race_cols = c("whi", "bla", "his", "asi", "oth"),
    include_total = TRUE) %>%
  dplyr::rename(
    whi_bisg_prop = whi_prop,
    bla_bisg_prop = bla_prop,
    his_bisg_prop = his_prop,
    asi_bisg_prop = asi_prop,
    oth_bisg_prop = oth_prop,
    whi_bisg_total = whi_total,
    bla_bisg_total = bla_total,
    his_bisg_total = his_total,
    asi_bisg_total = asi_total,
    oth_bisg_total = oth_total) %>%
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

# Bind all self-reported/BISG results together
agg <- dplyr::bind_cols(self_reported, bisg[, -1])

if (verbose){
  message("Now add in election results (This code comes from the CVAP script")
}

sf_precinct_path <- file.path(
  base_path,
  "ga_precinct_shapefile/2018Precincts.shp"
)

sf_precincts <- sf::read_sf(sf_precinct_path) %>%
  dplyr::select(loc_prec,
                G18DGOV,
                G18RGOV,
                G18LGOV) %>%
  dplyr::mutate(total_votes = G18DGOV + G18RGOV + G18LGOV) %>%
  dplyr::mutate(
    abrams_prop = G18DGOV / total_votes,
    kemp_prop = G18RGOV / total_votes,
    metz_prop = G18LGOV / total_votes) %>%
  dplyr::rename(precinct_id_2018 = loc_prec) %>%
  dplyr::select(precinct_id_2018,
                abrams_prop,
                kemp_prop,
                metz_prop,
                total_votes)

#' Patriots park appears split in two in the shapefile
#' sf_precincts %>%
#'   mutate("is_duped" = precinct_id_2018 == "Columbia,Patriots Park") %>%
#'   ggplot() +
#'     geom_sf(aes(fill = is_duped)) +
#'     xlim(-82.4, -82) +
#'     ylim(33.5, 33.6)
#'

# This code combines them
patpark_stats <- sf_precincts %>%
  dplyr::filter(precinct_id_2018 == "Columbia,Patriots Park") %>%
  dplyr::as_tibble() %>%
  select(-geometry) %>%
  distinct()

suppressMessages({
  sf_patpark_combined <- sf_precincts %>%
    dplyr::filter(precinct_id_2018 == "Columbia,Patriots Park") %>%
    dplyr::group_by(precinct_id_2018) %>%
    dplyr::summarize(geometry = sf::st_union(geometry)) %>%
    inner_join(patpark_stats)
})
sf_precincts <- sf_precincts %>%
  dplyr::filter(precinct_id_2018 != "Columbia,Patriots Park") %>%
  rbind(sf_patpark_combined) %>%
  as_tibble() %>%
  select(-geometry)

agg <- agg %>%
  dplyr::left_join(sf_precincts, by = "precinct_id_2018")

if (verbose) {
  message("Aggregation complete.")
  message("======================================================")
}

if (verbose) {
  message("======================================================")
  message("Writing aggregation file...")
}

readr::write_csv(agg, out_path)

if (verbose) {
  message("Aggregation file written. Script complete.")
}
