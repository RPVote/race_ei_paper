
#' This script combines CVAP data, voter turnout data, and county-level EI 
#' results to enable analyses of divergent EI results across counties
#'

suppressPackageStartupMessages({
  library(tidyverse)
  library(tidycensus)
  library(ggtext)
  library(tidymodels)
  library(sf)
})

# Set working directory to folder in which this file is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

base_path <- "../../data"

# -----------------------
# Read in CVAP and aggregate to county level
# -----------------------
cvap_data_path <- file.path(base_path, "CVAP_BlockGr.csv")
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
df_ga_cvap <- df_cvap %>%
  dplyr::mutate(GEOID = substr(geoid, 8, 1000)) %>%
  tidyr::separate(GEOID,
                  into = c("STATE", "COUNTY", "BG"),
                  sep = c(2, 5)) %>%
  dplyr::filter(STATE == "13") %>%
  tidyr::pivot_wider(id_cols = all_of(c("STATE", "COUNTY", "BG")),
                     names_from = lntitle,
                     values_from = cvap_est) %>%
  dplyr::select(COUNTY, all_of(cvap_names)) %>%
  dplyr::rename_with(~cvap_codes, .cols = all_of(cvap_names))

df_ga_cvap$MULTI <- df_ga_cvap$AIANWH +
  df_ga_cvap$ASWH +
  df_ga_cvap$BLWH +
  df_ga_cvap$AIANBL +
  df_ga_cvap$MULTI
df_ga_cvap <- df_ga_cvap %>%
  dplyr::select(COUNTY, WH, BL, AIAN, AS, NHOPI, MULTI, HL)

# Aggregation step
df_ga_cvap <- df_ga_cvap %>%
  dplyr::group_by(COUNTY) %>%
  dplyr::summarize(
    tot_cvap_total = sum(WH + BL + HL + AS + AIAN + NHOPI + MULTI),
    whi_cvap_total = sum(WH),
    bla_cvap_total = sum(BL),
    his_cvap_total = sum(HL),
    oth_cvap_total = sum(AS + AIAN + NHOPI + MULTI)
  )

# Now finally convert county fips to county name for matching
county_fips <- read_csv("../../data/county_fips_matches.csv", col_types = cols(
  fips = col_character(),
  county = col_character()
)) %>%
  mutate(fips = str_sub(fips, 3, 5))
df_ga_cvap <- df_ga_cvap %>%
  left_join(county_fips, by = c("COUNTY" = "fips")) %>%
  select(-COUNTY)


# -----------------------
# Get number of turned out voters by race
# -----------------------

base_path <- "../../data"
ga_agg_path <- file.path(base_path, "ga_2018_agg_all.rds")
county_turnout <- readRDS(ga_agg_path) %>%
  as_tibble() %>%
  select(-geometry) %>%
  # Filter out precincts with no votes
  dplyr::filter(total_votes > 0) %>%
  mutate(county = gsub("^(.*?),.*", "\\1", precinct_id_2018)) %>%
  group_by(county) %>%
  summarize(
    tot_turnout = sum(
      whi_true_total + 
        bla_true_total + 
        his_true_total + 
        asi_true_total +
        oth_true_total
    ),
    whi_turnout = sum(whi_true_total),
    bla_turnout = sum(bla_true_total),
    his_turnout = sum(his_true_total),
    oth_turnout = sum(oth_true_total + asi_true_total),
    n_prec = n()
  )

# -----------------------
# Merge voter and CVAP totals to compute turnout
# -----------------------
county_turnout <- county_turnout %>%
  left_join(df_ga_cvap) %>%
  mutate(
    whi_cvap_prp = whi_cvap_total / tot_cvap_total,
    bla_cvap_prp = bla_cvap_total / tot_cvap_total,
    his_cvap_prp = his_cvap_total / tot_cvap_total,
    oth_cvap_prp = oth_cvap_total / tot_cvap_total,
    
    tot_turnout_prp = tot_turnout / tot_cvap_total,
    
    whi_turnout_prp = ifelse(whi_cvap_total == 0, 0, whi_turnout/whi_cvap_total),
    bla_turnout_prp = ifelse(bla_cvap_total == 0, 0, bla_turnout/bla_cvap_total),
    his_turnout_prp = ifelse(his_cvap_total == 0, 0, his_turnout/his_cvap_total),
    oth_turnout_prp = ifelse(oth_cvap_total == 0, 0, oth_turnout/oth_cvap_total),
    
    whi_turnout_prp = ifelse(whi_cvap_total == 0, 0, whi_turnout/whi_cvap_total),
    bla_turnout_prp = ifelse(bla_cvap_total == 0, 0, bla_turnout/bla_cvap_total),
    his_turnout_prp = ifelse(his_cvap_total == 0, 0, his_turnout/his_cvap_total),
    oth_turnout_prp = ifelse(oth_cvap_total == 0, 0, oth_turnout/oth_cvap_total),
    
    # Censor turnouts -- they cannot be greater than 1
    whi_turnout_prp = pmin(whi_turnout_prp, 1),
    bla_turnout_prp = pmin(bla_turnout_prp, 1),
    his_turnout_prp = pmin(his_turnout_prp, 1),
    oth_turnout_prp = pmin(oth_turnout_prp, 1),
    
    whi_turnout_diff = whi_turnout_prp-whi_cvap_prp,
    bla_turnout_diff = bla_turnout_prp-bla_cvap_prp,
    his_turnout_diff = his_turnout_prp-his_cvap_prp,
    oth_turnout_diff = oth_turnout_prp-oth_cvap_prp
  ) %>%
  select(county, ends_with("turnout_prp"), ends_with("diff"), ends_with("turnout"), ends_with("total"), n_prec)


# -----------------------
# Merge in county-level EI results
# -----------------------
path <- "../../data/county_ei"
files <- list.files(path)

# First, get long county turnout data
county_turnout_long <- county_turnout %>%
  select(county, tot_turnout, n_prec, ends_with("prp"), ends_with("diff")) %>%
  pivot_longer(
    ends_with("diff"),
    names_to = c("race", "t"),
    names_sep = "_",
    values_to = "turnout_difference"
  ) %>%
  select(county, tot_turnout, n_prec, race, turnout_difference, ends_with("prp"))

# Read in and combine all county-level ei data
all_ei_county <- map_dfr(
  files,
  function(f) { readRDS(file.path(path, f)) }
) %>%
  as_tibble() %>% 
  mutate(cand = gsub("_prop", "", cand),
         race = str_sub(race, 1, 3))

all_ei_county <- all_ei_county %>%
  left_join(county_turnout_long) %>%
  filter(cand == "abrams", race_type %in% c("bisg", "cvap"))

all_ei_county <- all_ei_county %>%
  #select(race, county, mean, race_type, type, n_prec)  %>%
  pivot_wider(id_cols = c("race", "county", "type", "n_prec", "turnout_difference"), 
              names_from = "race_type", values_from = "mean", values_fill = NA) %>%
  mutate(diff = bisg-cvap) %>%
  right_join(all_ei_county)

saveRDS(all_ei_county, "../../data/county_ei_turnout.rds")
