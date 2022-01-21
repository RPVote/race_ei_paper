#!/usr/bin/env Rscript
#' Analysis script 3 for "Comparing BISG to CVAP Estimates in Racially Polarized
#' Voting Analysis" by Collingwood et al.
#'
#' Run this script once you have applied BISG to the voter file using the
#' 2_process_voterfile.R script.
#'
#' This script extracts the voters from the voter file that participated in the
#' 2018 Georgia general election. It uses the voter history file provided by
#' the Georgia Secretary of State.

# Import relevant libraries
suppressWarnings(suppressMessages({
  library(dplyr)
  library(readr)
  library(tidyverse)
}))

# Preamble: Adjust these settings according to your use case
# Turn verbosity on or off
verbose <- TRUE
# Set the base path: where all data files are located
base_path <- "../../data"
vh_path <- file.path(base_path, "ga_voter_file_history_11_06_2018.txt")
vf_path <- file.path(base_path, "ga_voter_file_2018_bisg.csv")
vf_2018_path <- file.path(base_path, "ga_voter_file_2018_final.csv")

if (verbose) {
  message("======================================================")
  message("Beginning script. Reading in voter history...")
}

vh <-
  readr::read_delim(
    file = vh_path,
    delim = "\n",
    col_names = FALSE,
    col_types = c(readr::col_character())) %>%
  tidyr::separate(
    col = "X1",
    into = c("county_number",
              "registration_number",
              "election_date",
              "election_type",
              "party",
              "absentee",
              "provisional",
              "supplemental"),
    sep = c(3, 11, 19, 22, 24, 25, 26)) %>%
  readr::type_convert(
    col_types = readr::cols(
      registration_number = readr::col_double(),
      election_date = readr::col_date(format = "%Y%m%d")))

if (verbose) {
  message("Voter history read in.")
  message("Reading in voter file with BISG...")
}

# Filter out precinct because it only contains one voter with unknown race
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
      whi = readr::col_double(),
      bla = readr::col_double(),
      his = readr::col_double(),
      asi = readr::col_double(),
      oth = readr::col_double()))

if (verbose) {
  message("Voter file read in.")
  message("======================================================")
  message("Sub-selecting voter file by history...")
}

# Select voters that participated in 2018 election
voters_in_2018 <- vf$registration_number %in% vh$registration_number
vf_2018 <- vf[voters_in_2018, ]

if (verbose) {
  message("Voter file merged to history.")
  message(paste("Number of voters matched:", nrow(vf_2018)))
  message("======================================================")
  message("Saving voter file...")
}

# Save voter files as CSV files
readr::write_csv(tibble::as_tibble(vf_2018), vf_2018_path)

if (verbose) {
  message("Voter file written. Script complete.")
}
