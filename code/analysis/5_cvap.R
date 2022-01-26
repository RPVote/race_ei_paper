#!/usr/bin/env Rscript
#' Analysis script 5 for "Comparing BISG to CVAP Estimates in Racially Polarized
#' Voting Analysis" by Collingwood et al.
#'
#' Run this script once you have extracted the voters from 2018 using the
#' 4_aggregate.R script.
#''
#' This script uses spatial interpolation of block-group level Citizen Voting
#' Age Population (CVAP) data to estimate the raical composition of Georgia
#' election precincts
#'
#' CVAP data comes from the Census Bureau:
#' https://www.census.gov/programs-surveys/decennial-census/about/voting-rights/cvap.html

suppressWarnings(suppressMessages({
  library(readr)
  library(tigris)
  library(sf)
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
agg_path <- file.path(base_path, "ga_2018_agg.csv")
sf_precinct_path <- file.path(
  base_path,
  "ga_precinct_shapefile/2018Precincts.shp"
)
cvap_data_path <- file.path(base_path, "CVAP_BlockGr.csv")

if (verbose) {
  message("======================================================")
  message("Beginning script. Reading in precinct racial composition...")
}

df_turnout <- readr::read_csv(
  file = agg_path,
  col_names = TRUE,
  col_types = cols(
    precinct_id_2018 = col_character(),
    whi_true_prop = col_double(),
    bla_true_prop = col_double(),
    his_true_prop = col_double(),
    asi_true_prop = col_double(),
    oth_true_prop = col_double(),
    whi_true_total = col_double(),
    bla_true_total = col_double(),
    his_true_total = col_double(),
    asi_true_total = col_double(),
    oth_true_total = col_double(),
    whi_bisg_prop = col_double(),
    bla_bisg_prop = col_double(),
    his_bisg_prop = col_double(),
    asi_bisg_prop = col_double(),
    oth_bisg_prop = col_double(),
    whi_bisg_total = col_double(),
    bla_bisg_total = col_double(),
    his_bisg_total = col_double(),
    asi_bisg_total = col_double(),
    oth_bisg_total = col_double()
  ))


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
  rbind(sf_patpark_combined)

if (verbose) {
  message("======================================================")
  message("Converting precinct composition file into an sf object...")
}

sf_turnout <- sf_precincts %>%
  dplyr::right_join(df_turnout, by = "precinct_id_2018")

if (verbose) {
  message("======================================================")
  message("Reading in CVAP data and cleaning it...")
}

df_cvap <- readr::read_csv(cvap_data_path,
                           col_types = cols(
                             geoname = col_character(),
                             lntitle = col_character(),
                             geoid = col_character(),
                             lnnumber = col_double(),
                             cit_est = col_double(),
                             cit_moe = col_double(),
                             cvap_est = col_double(),
                             cvap_moe = col_double()
                           ))

cvap_names <- c("American Indian or Alaska Native Alone",
                "Asian Alone",
                "Black or African American Alone",
                "Native Hawaiian or Other Pacific Islander Alone",
                "White Alone",
                "American Indian or Alaska Native and White",
                "Asian and White",
                "Black or African American and White",
                "American Indian or Alaska Native and Black or African American",
                "Remainder of Two or More Race Responses",
                "Hispanic or Latino")
cvap_codes <- c("AIAN",
                "AS",
                "BL",
                "NHOPI",
                "WH",
                "AIANWH",
                "ASWH",
                "BLWH",
                "AIANBL",
                "MULTI",
                "HL")


# Filter out Georgia from CVAP file, and tidy up into the standard above
df_ga_cvap <- df_cvap %>%
  dplyr::mutate(GEOID = substr(geoid, 8, 1000)) %>%
  tidyr::separate(geoid,
                  into = c("GEOID_PREFIX", "STATE", "COUNTY"),
                  sep = c(7, 9)) %>%
  dplyr::filter(STATE == "13") %>%
  tidyr::pivot_wider(id_cols = all_of(c("GEOID", "COUNTY")),
                     names_from = lntitle,
                     values_from = cvap_est) %>%
  dplyr::select(GEOID, COUNTY, all_of(cvap_names)) %>%
  dplyr::rename_with(~cvap_codes, .cols = all_of(cvap_names))

df_ga_cvap$MULTI <- df_ga_cvap$AIANWH +
  df_ga_cvap$ASWH +
  df_ga_cvap$BLWH +
  df_ga_cvap$AIANBL +
  df_ga_cvap$MULTI
df_ga_cvap <- df_ga_cvap %>%
  dplyr::select(COUNTY, GEOID, WH, BL, AIAN, AS, NHOPI, MULTI, HL)

if (verbose) {
  message("======================================================")
  message("Get block-group shapes from using tigris...")
  message("Then joining block group shapes to CVAP data...")
}

census_bg_shapes <- tigris::block_groups("Georgia", year = 2018)
sf_cvap <- dplyr::inner_join(census_bg_shapes, df_ga_cvap)


if (verbose) {
  message("======================================================")
  message("Getting all CVAP estimates via spatial interpolation...")
  message("This may take some time...")
}

# NAD83(2011) Georgia East
sf_cvap <- sf::st_transform(sf_cvap, 6444)
sf_turnout <- sf::st_transform(sf_turnout, 6444)

# Compute proportions for intensive AW-I.
# Combine the groups so they match up with wh, bl, hs, asi, oth
# wh = WH
# bl = BL
# hs = HL
# asi = AS
# oth = AIAN, NHOPI, MULTI
sf_cvap <- sf_cvap %>%
  dplyr::mutate(
    ttl = WH + BL + AIAN + AS + NHOPI + MULTI + HL,
    whi_2018_cvap_int_prop = WH / ttl,
    bla_2018_cvap_int_prop = BL / ttl,
    his_2018_cvap_int_prop = HL / ttl,
    asi_2018_cvap_int_prop = AS / ttl,
    oth_2018_cvap_int_prop = (AIAN + NHOPI + MULTI) / ttl
  )

sf_all_combined <- areal::aw_interpolate(
  sf_turnout,
  tid = precinct_id_2018,
  source = sf_cvap,
  sid = GEOID,
  output = "sf",
  weight = "sum",
  extensive = c("WH", "BL", "AIAN", "AS", "NHOPI", "MULTI", "HL"),
  intensive = c(
    "whi_2018_cvap_int_prop",
    "bla_2018_cvap_int_prop",
    "his_2018_cvap_int_prop",
    "asi_2018_cvap_int_prop",
    "oth_2018_cvap_int_prop"
  )
)

sf_all_combined <- sf_all_combined %>%
  dplyr::rename(
    "whi_2018_cvap_total" = WH,
    "bla_2018_cvap_total" = BL,
    "his_2018_cvap_total" = HL,
    "asi_2018_cvap_total" = AS
  ) %>%
  dplyr::mutate("oth_2018_cvap_total" = AIAN + NHOPI + MULTI) %>%
  dplyr::select(-AIAN, -NHOPI, -MULTI)

# Get 2018 cvap proportions ###########################
sf_all_combined <- sf_all_combined %>%
  dplyr::mutate(
    cvap_total = whi_2018_cvap_total +
      bla_2018_cvap_total +
      his_2018_cvap_total +
      asi_2018_cvap_total +
      oth_2018_cvap_total,
    whi_2018_cvap_ext_prop = whi_2018_cvap_total / cvap_total,
    bla_2018_cvap_ext_prop = bla_2018_cvap_total / cvap_total,
    his_2018_cvap_ext_prop = his_2018_cvap_total / cvap_total,
    asi_2018_cvap_ext_prop = asi_2018_cvap_total / cvap_total,
    oth_2018_cvap_ext_prop = oth_2018_cvap_total / cvap_total
  )

# mean(sf_all_combined$cvap_total)
# sd(sf_all_combined$cvap_total)

if (verbose) {
  message("======================================================")
  message("Saving out data with CVAP estimates included...")
  message("Saving as .rds instead of .csv to preserve sf class...")
}

agg_w_cvap_path <- file.path(base_path, "ga_2018_agg_cvap.rds")
saveRDS(sf_all_combined, agg_w_cvap_path)
