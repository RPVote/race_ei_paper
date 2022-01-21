#!/usr/bin/env Rscript
#' Analysis script 1 for "Comparing BISG to CVAP Estimates in Racially Polarized
#' Voting Analysis" by Collingwood et al.
#'
#' This script processes the raw Georgia voter file, obtained in June 2020. The
#' voter file has already been geocoded using tools available in the eiCompare
#' package.
#'
#' This script reads the voter file and two sets of shapefiles. It extracts
#' and modifies a subset of the voter file columns. Then, it joins the voter
#' file to the shape files. In the end, it produces a processed voter file which
#' has information about each voter's location and precinct.

# Import relevant libraries
suppressWarnings(suppressMessages({
  library(eiCompare)
  library(lubridate)
  library(readr)
  library(sf)
  library(tidyverse)
}))

# Preamble: Adjust these settings according to your use case
# Turn verbosity on or off
verbose <- TRUE

# Set working directory to folder in which this file is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Set the base path: where all data files are located
base_path <- "../../data"
# Path to the raw voter file: adjust name of voter file, if needed
vf_path <- file.path(base_path, "ga_voter_file_2018_raw.csv")
# Paths for the shapefiles
ga_blocks_2010_path <- file.path(base_path, "ga_blocks_2010.rds")
ga_precincts_2018_path <- file.path(base_path, "ga_2018_election_precincts")
# Path with which to save processed voter file
vf_processed_path <- file.path(base_path, "ga_voter_file_2018_processed.csv")

if (verbose) {
  message("======================================================")
  message("Beginning script. Reading in Georgia voter file...")
}

# Read in the voter file: we include a subset of the columns
vf <- readr::read_csv(
  file = vf_path,
  col_names = TRUE,
  col_types = readr::cols(
    .default = readr::col_character(),
    county_code = readr::col_double(),
    registration_number = readr::col_double(),
    voter_status = readr::col_character(),
    last_name = readr::col_character(),
    first_name = readr::col_character(),
    middle_maiden_name = readr::col_character(),
    name_suffix = readr::col_character(),
    name_title = readr::col_character(),
    residence_house_number = readr::col_double(),
    residence_street_name = readr::col_character(),
    residence_street_suffix = readr::col_character(),
    residence_apt_unit_nbr = readr::col_character(),
    residence_city = readr::col_character(),
    residence_zipcode = readr::col_character(),
    birthdate = readr::col_double(),
    registration_date = readr::col_date(),
    race = readr::col_character(),
    gender = readr::col_character(),
    congressional_district = readr::col_double(),
    senate_district = readr::col_double(),
    house_district = readr::col_double(),
    date_last_voted = readr::col_date(),
    party_last_voted = readr::col_character(),
    date_added = readr::col_date(),
    date_changed = readr::col_date(),
    district_combo = readr::col_double(),
    race_desc = readr::col_character(),
    last_contact_date = readr::col_date(),
    state = readr::col_character(),
    street_address = readr::col_character(),
    final_address = readr::col_character(),
    lat = readr::col_double(),
    lon = readr::col_double()))

if (verbose) {
  message("Raw voter file successfully read in.")
  message(paste("Number of voter records:", nrow(vf)))
  message(paste("Number of columns:", ncol(vf)))
  message("======================================================")
  message("Reading in shapefiles...")
}

# Rename certain columns and select a subset of columns
vf <- vf %>%
  dplyr::rename(
    birthyear = birthdate,
    address = final_address,
    date_last_contact = last_contact_date,
    date_registration = registration_date) %>%
  dplyr::select(
    registration_number,
    last_name,
    first_name,
    middle_maiden_name,
    name_suffix,
    address,
    address,
    voter_status,
    race,
    gender,
    party_last_voted,
    birthyear,
    date_registration,
    date_added,
    date_changed,
    date_last_contact,
    date_last_voted,
    lat,
    lon)

# Shape 1: Read in the Georgia 2010 block data, obtained from the US Census
ga_blocks_2010 <- readr::read_rds(ga_blocks_2010_path) %>%
  dplyr::rename(
    fips_2010 = GEOID10,
    fips_state_2010 = STATEFP10,
    fips_county_2010 = COUNTYFP10,
    fips_tract_2010 = TRACTCE10,
    fips_block_2010 = BLOCKCE10) %>%
  dplyr::select(
    fips_2010,
    fips_state_2010,
    fips_county_2010,
    fips_tract_2010,
    fips_block_2010,
    geometry)

# Shape 2: Read in precincts across Georgia, obtained from OpenPrecints
ga_precincts_2018 <- sf::st_read(dsn = ga_precincts_2018_path, quiet = TRUE) %>%
  sf::st_transform(4326) %>%
  dplyr::select(loc_prec, geometry) %>%
  dplyr::rename(precinct_id_2018 = loc_prec)

if (verbose) {
  message("Shapefiles successfully read in.")
  message(paste("Number of blocks (2010):", nrow(ga_blocks_2010)))
  message(paste("Number of precincts (2018):", nrow(ga_precincts_2018)))
  message("======================================================")
  message("Merging voter file with shapefiles.")
}

# Merge voter file with shape files
vf <- eiCompare::merge_voter_file_to_shape(
  voter_file = vf,
  shape_file = ga_blocks_2010,
  crs = 4326,
  coords = c("lon", "lat"),
  voter_id = "registration_number")
vf <- eiCompare::merge_voter_file_to_shape(
  voter_file = vf,
  shape_file = ga_precincts_2018,
  crs = 4326,
  coords = c("lon", "lat"),
  voter_id = "registration_number")

if (verbose) {
  message("Merge complete.")
  message("======================================================")
  message("Removing voters that did not match...")
}

# Extract matches and save voter file
vf_processed <- dplyr::filter(vf, !is.na(fips_2010) & !is.na(precinct_id_2018))

if (verbose) {
  n_removed <- nrow(vf) - nrow(vf_processed)
  message(paste("Number of removed voters:", n_removed))
  message("======================================================")
  message("Writing processed voter file...")
}

# Save processed voter file
readr::write_csv(tibble::as_tibble(vf_processed), vf_processed_path)

if (verbose) {
  message("Voter file saved. Script complete.")
}
