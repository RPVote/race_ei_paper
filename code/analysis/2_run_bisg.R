#!/usr/bin/env Rscript
#' Analysis script 2 for "Comparing BISG to CVAP Estimates in Racially Polarized
#' Voting Analysis" by Collingwood et al.#
#'
#' Run this script once you have processed the voter file using the
#' 1_process_voterfile.R script.
#'
#' This script reads in the voter file and uses the BISG package to run BISG
#' on the voters, storing the predictions for race as additional columns in the
#' voter file. It then saves the voter file.

# Import relevant libraries
suppressWarnings(suppressMessages({
  library(readr)
  library(wru)
}))

# Preamble: Adjust these settings according to your use case
# Turn verbosity on or off
verbose <- TRUE

# Set working directory to folder in which this file is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Set the base path: where all data files are located
base_path <- "../../data"
# Path to processed voter file
vf_path <- file.path(base_path, "ga_voter_file_2018_processed.csv")
# Path to voter file with BISG
vf_bisg_path <- file.path(base_path, "ga_voter_file_2018_bisg.csv")

if (verbose) {
  message("==============================================================")
  message("Beginning script. Reading in (processed) Georgia voter file...")
}

# Import voter file
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
    date_last_voted = readr::col_date()))

if (verbose) {
  message("Processed voter file successfully read in.")
  message(paste("Number of voter records:", nrow(vf)))
  message(paste("Number of columns:", ncol(vf)))
  message("==============================================================")
  message("Obtaining block counts from Census...")
}

# Extract the block counts from the Census. To run this, you must have a Census
# API key for use in tidycensus
future::plan(future::multisession)
census <- get_census_data(states="GA")

if (verbose) {
  message("Block counts obtained.")
  message(paste("Number of blocks:", nrow(block_counts)))
  message("==============================================================")
  message("Performing BISG...")
}

# Apply BISG using the block counts and voter file
vf_bisg <- vf %>%
  rename('block' = 'fips_block_2010',
         'county' = 'fips_county_2010',
         'tract' = 'fips_tract_2010',
         'surname' = 'last_name',
         'first' = 'first_name') %>%
  mutate("state" = 'GA') %>%
  select(registration_number, surname, first, block, tract, county, state) %>%
  filter(!is.na(surname)) %>%
  wru::predict_race(
    census.data = census,
    census.geo = "block",
    model = 'BISG',
    names.to.use = 'surname'
  ) %>%
  select(registration_number, contains("pred"))

vf_fbisg <- vf %>%
  rename('block' = 'fips_block_2010',
         'county' = 'fips_county_2010',
         'tract' = 'fips_tract_2010',
         'surname' = 'last_name',
         'first' = 'first_name') %>%
  mutate("state" = 'GA') %>%
  select(registration_number, surname, first, block, tract, county, state) %>%
  filter(!is.na(surname)) %>%
  wru::predict_race(
    census.data = census,
    census.geo = "block",
    model = 'fBISG',
    names.to.use = 'surname'
  ) %>%
  select(registration_number, contains("pred"))

vf_fbisg_names <- vf %>%
  rename('block' = 'fips_block_2010',
         'county' = 'fips_county_2010',
         'tract' = 'fips_tract_2010',
         'surname' = 'last_name',
         'first' = 'first_name') %>%
  mutate("state" = 'GA') %>%
  select(registration_number, surname, first, block, tract, county, state) %>%
  filter(!is.na(surname)) %>%
  wru::predict_race(
    census.data = census,
    census.geo = "block",
    model = 'fBISG',
    names.to.use = 'surname, first'
  ) %>%
  select(registration_number, contains("pred"))

names(vf_bisg) <- c("registration_number", 'whi_bisg', 'bla_bisg', 'his_bisg', 'asi_bisg', 'oth_bisg')
names(vf_fbisg) <- c("registration_number", 'whi_fbisg', 'bla_fbisg', 'his_fbisg', 'asi_fbisg', 'oth_fbisg')
names(vf_fbisg_names) <- c("registration_number", 'whi_fbisgf', 'bla_fbisgf', 'his_fbisgf', 'asi_fbisgf', 'oth_fbisgf')

vf <- vf %>% 
  inner_join(vf_bisg) %>%
  inner_join(vf_fbisg) %>%
  inner_join(vf_fbisg_names)


if (verbose) {
  message("==============================================================")
  message("Saving voter file...")
}

# Save voter file with BISG columns.
readr::write_csv(tibble::as_tibble(vf), vf_bisg_path)

if (verbose) {
  message("Voter file saved. Script complete.")
}
