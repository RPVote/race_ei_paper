#'  This script constructs a figure that describes county-level variation in 
#'  ei results using RxC and three different race-estimation methods
#'  
 
suppressPackageStartupMessages({
  library(tidyverse)
  library(sf)
})

# Set working directory to folder in which this file is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Plot theme
alpha <- 0.25
plot_theme <-
  theme_bw() +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 23, face = "bold"),
        axis.title.x = element_text(margin = margin(t = 20)),
        axis.title.y = element_text(margin = margin(r = 20)),
        plot.title = element_text(size = 27, face = "bold"),
        legend.position = "bottom")

path <- "../../data/county_ei"
files <- list.files(path)

# Read in and combine all county-level ei data
all_ei_county <- map_dfr(
  files,
  function(f) { readRDS(file.path(path, f)) }
) %>%
  as_tibble() %>% 
  mutate(county = str_pad(county, 3, "left", "0"),
         cand = gsub("_prop", "", cand),
         race = str_sub(race, 1, 3))

# Get turnout by county
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
    cvap = sum(cvap_total), 
    votes = sum(total_votes),
    prp_bla = mean(bla_true_prop),
    prp_whi = mean(whi_true_prop)
  ) %>%
  mutate(turnout = votes/cvap)

all_ei_county <- all_ei_county %>%
  left_join(county_turnout) %>%
  filter(cand == "abrams", race_type %in% c("bisg", "cvap"), type == "rxc")

all_ei_county <- all_ei_county %>%
  select(race, county, mean, race_type)  %>%
  pivot_wider(id_cols = c("race", "county"), names_from = "race_type", values_from = "mean") %>%
  mutate(diff = bisg-cvap) %>%
  select(-bisg, -cvap) %>%
  right_join(all_ei_county)

all_ei_county %>%
  filter(race_type == "cvap") %>%
  ggplot(aes(x = prp_whi, y = diff)) +
    geom_point() + 
    geom_hline(yintercept = 0) +
    facet_grid(. ~ race) 

rankdata <- all_ei_county %>%
  filter(cand == "abrams", type == "rxc") %>%
  select(-cand) %>%
  pivot_wider(
    id_cols = c("race", "county", "type"),
    names_from = "race_type",
    values_from = c("mean", "ci_95_lower", "ci_95_upper")
  ) %>%
  group_by(type, race) %>%
  arrange(mean_bisg, .by_group = TRUE) %>%
  filter(!(is.na(mean_bisg))) %>%
  mutate(rank = n():1) %>%
  ungroup() %>%
  select(race, county, type, rank)

plotdata <- all_ei_county %>%
  filter(cand == "abrams", type == "rxc") %>%
  select(-cand) %>%
  left_join(rankdata) %>%
  filter(!(is.na(mean)))

plotdata %>%
  filter(race_type != "true") %>%
  mutate(line = ifelse(race_type == "bisg", mean, NA)) %>%
  mutate(race = case_when(
    race == "whi" ~ "White",
    race == "bla" ~ "Black",
    race == "his" ~ "Hisp.",
    race == "oth" ~ "Other"
  )) %>%
  mutate(race = ordered(race, levels = c("White", "Black", "Hisp.", "Other"))) %>%
  ggplot(aes(x = rank, y = mean, shape = race_type)) +
    geom_line(aes(y = ci_95_lower, linetype = race_type)) +
    geom_line(aes(y = ci_95_upper, linetype = race_type)) +
   # geom_line(aes(y = line)) +
  #  geom_point() +
    scale_y_continuous(
      limits = c(0, 1)
    ) +
    scale_x_reverse() +
    facet_grid(race ~ .) +
    plot_theme +
    theme(
      strip.text = element_text(size = 20),
      strip.background = element_rect(fill = "white"),
      axis.text.x = element_text(size = 16),
      legend.title = element_blank(),
      legend.text = element_text(size = 20)
    )


