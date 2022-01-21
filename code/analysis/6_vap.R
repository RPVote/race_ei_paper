#!/usr/bin/env Rscript
#' Analysis script 6 for "Comparing BISG to CVAP Estimates in Racially Polarized
#' Voting Analysis" by Collingwood et al.
#'
#' Run this script once you have extracted the voters from 2018 using the
#' 5_cvap.R script.
#''
#' This script uses spatial interpolation of block level Voting
#' Age Population data to estimate the raical composition of Georgia
#' election precincts
#'
suppressWarnings(suppressMessages({
  library(readr)
  library(tidycensus)
  library(tidyverse)
  library(areal)
}))

# Preamble: Adjust these settings according to your use case
# Turn verbosity on or off
verbose <- TRUE
# Set working directory to folder in which this file is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Set the base path: where all data files are located
base_path <- "../../data"
# Set all other paths
agg_path <- file.path(base_path, "ga_2018_agg_cvap.rds")
out_path <- file.path(base_path, "ga_2018_agg_all.rds")

if (verbose) {
  message("======================================================")
  message("Reading in data from previous script...")
}

sf_precincts <- readRDS(agg_path)

if (verbose) {
  message("======================================================")
  message("Using tidycensus to get VAP data...")
}

georgia_counties <- c("001","003","005","007","009","011","013","015","017",
                      "019","021","023","025","027","029","031","033","035",
                      "037","039","043", "045","047","049","051","053","055",
                      "057","059","061","063","065","067","069","071","073",
                      "075","077","079","081","083", "085","087","089","091",
                      "093","095","097","099","101","103","105","107","109",
                      "111","113","115", "117","119","121","123", "125","127",
                      "129","131","133","135","137","139","141","143","145",
                      "147","149","151","153","155","157","159","161","163",
                      "165","167","169","171","173","175","177","179","181",
                      "183","185","187","189","191","193","195","197","199",
                      "201","205","207","209","211","213","215","217","219",
                      "221","223","225","227","229","231","233","235","237",
                      "239","241","243","245","247","249","251","253","255",
                      "257","259","261","263","265","267","269","271","273",
                      "275","277","279","281","283", "285","287","289","291",
                      "293","295","297","299","301","303","305","307","309",
                      "311","313","315","317","319","321")

sf_vap <- purrr::map_dfr(
  georgia_counties,
  ~ tidycensus::get_decennial(
    geography = "block",
    variables = c(
      "P011002", # total hispanic, 18+
      "P011005", # total white, 18+
      "P011006", # total black, 18+
      "P011008"  # total asian, 18+
    ),
    year = 2010,
    summary_var = "P011001", # total population, 18+,
    state = "GA",
    county = .,
    geometry = TRUE,
    cache = TRUE,
    output = "wide"
  )
)

if (verbose) {
  message("======================================================")
  message("Doing interpolation...")
}

# Get on the same CRS
sf_vap <- sf::st_transform(sf_vap, sf::st_crs(sf_precincts))

# Compute proportions for intensive interpolation
sf_vap <- sf_vap %>%
  dplyr::mutate(
    "whi_2010_vap_int_prop" = ifelse(summary_value == 0, 0, P011005 / summary_value),
    "bla_2010_vap_int_prop" = ifelse(summary_value == 0, 0, P011006 / summary_value),
    "his_2010_vap_int_prop" = ifelse(summary_value == 0, 0, P011002 / summary_value),
    "asi_2010_vap_int_prop" = ifelse(summary_value == 0, 0, P011008 / summary_value),
    "oth_2010_vap_int_prop" = ifelse(
      summary_value == 0,
      0,
      (summary_value - P011002 - P011005 - P011006 - P011008)/summary_value
    )
  )

sf_all_combined <- areal::aw_interpolate(
  sf_precincts,
  tid = precinct_id_2018,
  source = sf_vap,
  sid = GEOID,
  output = "sf",
  weight = "sum",
  extensive = c(
    "P011002", "P011005",
    "P011006", "P011008", "summary_value"
  ),
  intensive = c(
    "whi_2010_vap_int_prop",
    "bla_2010_vap_int_prop",
    "his_2010_vap_int_prop",
    "asi_2010_vap_int_prop",
    "oth_2010_vap_int_prop"
  )
)

sf_all_combined <- sf_all_combined %>%
  dplyr::rename(
    "whi_2010_vap_total" = "P011005",
    "bla_2010_vap_total" = "P011006",
    "his_2010_vap_total" = "P011002",
    "asi_2010_vap_total" = "P011008",
    "vap_total" = "summary_value"
  )
sf_all_combined$oth_2010_vap_total <- sf_all_combined$vap_total -
  sf_all_combined$whi_2010_vap_total -
  sf_all_combined$bla_2010_vap_total -
  sf_all_combined$his_2010_vap_total -
  sf_all_combined$asi_2010_vap_total

sf_all_combined <- sf_all_combined %>%
  dplyr::mutate(oth_2010_vap_block_total = ifelse(
    oth_2010_vap_total < 0,
    0,
    oth_2010_vap_total
  )) %>%
  dplyr::mutate(
    "whi_2010_vap_ext_prop" = ifelse(vap_total == 0, 0, whi_2010_vap_total / vap_total),
    "bla_2010_vap_ext_prop" = ifelse(vap_total == 0, 0, bla_2010_vap_total / vap_total),
    "his_2010_vap_ext_prop" = ifelse(vap_total == 0, 0, his_2010_vap_total / vap_total),
    "asi_2010_vap_ext_prop" = ifelse(vap_total == 0, 0, asi_2010_vap_total / vap_total),
    "oth_2010_vap_ext_prop" = ifelse(vap_total == 0, 0, oth_2010_vap_total / vap_total)
  )

if (verbose) {
  message("======================================================")
  message("Saving out final dataset...")
}

saveRDS(sf_all_combined, out_path)
