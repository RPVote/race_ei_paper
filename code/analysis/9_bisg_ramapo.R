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
  library(tidyverse)
  library(sf)
  library(wru)
  library(tidycensus)
}))

# Preamble: Adjust these settings according to your use case
# Turn verbosity on or off
verbose <- TRUE

# Set working directory to folder in which this file is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Set the base path: where all data files are located
base_path <- "../../data"

# Load in geocoded voter file
vf <- read_csv(file.path(base_path, "ersd_2016_vf_geocoded.csv")) %>%
  rename('surname' = 'last') 

# Get block racial composition data.
rockland_blocks <- tidycensus::get_decennial(
  geography = "block",
  sumfile = 'sf1',
  variables = c(
    "P011002", # total hispanic, 18+
    "P011005", # total white, 18+
    "P011006", # total black, 18+
    "P011008"  # total asian, 18+
  ),
  year = 2010,
  summary_var = "P011001", # total population, 18+,
  state = "NY",
  county = 'Rockland',
  geometry = TRUE,
  cache = TRUE,
  output = "wide"
)

# Clean up vf 
vf <- vf %>%
  st_as_sf(coords = c('lon', 'lat')) %>%
  st_set_crs(4236)

rockland_blocks <- rockland_blocks %>%
  st_transform(4236)

ggplot() +
  geom_sf(data = rockland_blocks, fill = 'grey', color = 'white') +
  geom_sf(data = vf %>% sample_n(1000), color = 'black') 

vf <- st_join(vf, rockland_blocks)

vf <- vf %>% 
  filter(voted.er.2016 == 'YES') %>%
  mutate(county = str_sub(GEOID, 3, 5),
         tract = str_sub(GEOID, 6, 11),
         block = str_sub(GEOID, 12, 17)) %>%
  mutate(state == 'NY')

future::plan(future::multisession())  
census <- get_census_data(states="NY", county.list = list('NY' = '087'), year = 2010)

vf_bisg <- vf %>%
  filter(!is.na(surname), !is.na(GEOID)) %>%
  wru::predict_race(
    census.data = census,
    census.geo = "block",
    model = 'BISG',
    names.to.use = 'surname'
  ) %>%
  select(id, contains("pred"))

vf_fbisg <- vf %>%
  mutate(state == 'NY') %>%
  filter(!is.na(surname), !is.na(GEOID)) %>%
  as_tibble() %>% 
  wru::predict_race(
    census.data = census,
    census.geo = "block",
    model = 'fBISG',
    names.to.use = 'surname',
    use.counties = TRUE
  ) %>%
  select(id, contains("pred"))

vf_fbisgf <- vf %>%
  filter(!is.na(surname), !is.na(GEOID)) %>%
  as_tibble() %>%
  wru::predict_race(
    census.data = census,
    census.geo = "block",
    model = 'fBISG',
    names.to.use = 'surname, first',
    use.counties = TRUE
  ) %>%
  select(id, contains("pred"))

names(vf_bisg) <- c("id", 'whi_bisg', 'bla_bisg', 'his_bisg', 'asi_bisg', 'oth_bisg')
names(vf_fbisg) <- c("id", 'whi_fbisg', 'bla_fbisg', 'his_fbisg', 'asi_fbisg', 'oth_fbisg')
names(vf_fbisgf) <- c("id", 'whi_fbisgf', 'bla_fbisgf', 'his_fbisgf', 'asi_fbisgf', 'oth_fbisgf')

vf <- vf %>%
  as_tibble() %>%
  inner_join(vf_bisg) %>%
  inner_join(vf_fbisg) %>%
  inner_join(vf_fbisgf)

bisg <-
  eiCompare::precinct_agg_combine(
    voter_file = vf,
    group_col = "ed_school",
    race_cols = c("whi_bisg", "bla_bisg", "his_bisg", "asi_bisg", "oth_bisg"),
    include_total = TRUE) %>%
  dplyr::select(
    ed_school,
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
    group_col = "ed_school",
    race_cols = c("whi_fbisg", "bla_fbisg", "his_fbisg", "asi_fbisg", "oth_fbisg"),
    include_total = TRUE) %>%
  dplyr::select(
    ed_school,
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
    group_col = "ed_school",
    race_cols = c("whi_fbisgf", "bla_fbisgf", "his_fbisgf", "asi_fbisgf", "oth_fbisgf"),
    include_total = TRUE) %>%
  dplyr::select(
    ed_school,
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

agg <- dplyr::bind_cols(bisg, fbisg[,-1], fbisgf[,-1])

# Read in CVAP
cvap <- readRDS("../../data/ersd_cvap_and_vap.rds")

cvap <- cvap %>% 
  mutate("ed_school" = as.numeric(str_sub(DISTRICT, 2, 3))) %>%
  select(ed_school, contains("2016_cvap_int_prop"))

agg <- agg %>%
  inner_join(cvap)

# Add in results
results <- readRDS("../../data/ersd_2016_results.rds") %>%
  select(precinct, total, morales_prop, weissmandl_prop, total) %>%
  rename('ed_school' = 'precinct')

agg <- results %>%
  inner_join(agg)

if (verbose) {
  message("==============================================================")
  message("Saving voter file...")
}

# Save voter file with BISG columns.
saveRDS(agg, "../../data/ersd_all.rds")

 