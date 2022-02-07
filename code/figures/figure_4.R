#'  This script constructs a figure that describes county-level variation in 
#'  ei results using RxC and three different race-estimation methods
#'  
 
suppressPackageStartupMessages({
  library(tidyverse)
  library(ggtext)
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
    prp_whi = mean(whi_true_prop),
    n_prec = n()
  ) %>%
  mutate(turnout = votes/cvap)

all_ei_county <- all_ei_county %>%
  left_join(county_turnout) %>%
  filter(cand == "abrams", race_type %in% c("bisg", "cvap"))

all_ei_county <- all_ei_county %>%
  select(race, county, mean, race_type, type)  %>%
  pivot_wider(id_cols = c("race", "county", "type"), names_from = "race_type", values_from = "mean") %>%
  mutate(diff = bisg-cvap) %>%
  select(-bisg, -cvap) %>%
  right_join(all_ei_county)

all_ei_county %>%
  filter(race_type == "cvap") %>%
  mutate(
    race = case_when(
      race == "whi" ~ "White",
      race == "bla" ~ "Black",
      race == "his" ~ "Hispanic",
      race == "oth" ~ "Other"
    ), 
    race = ordered(race, levels = c("White", "Black", "Hispanic", "Other")),
    type = ifelse(type == "iter", "Iterative EI", "RxC EI")
  ) %>%
  ggplot(aes(x = turnout, y = diff)) +
    geom_point(aes(size = votes/100), alpha = alpha) + 
    geom_smooth(method = 'lm', color = 'black') +
    geom_hline(yintercept = 0, color = 'red', linetype = "dashed") +
    facet_grid(type ~ race) +
    ylab("Difference in predicted share voting for Abrams<br>(BISG - CVAP)") + 
    xlab("Turnout as proportion of CVAP") +
    coord_cartesian(ylim = c(-.5, .5)) +
    scale_size_continuous(name = "Number of voters (100s)") + 
    plot_theme +
    theme(
      strip.text = element_text(size = 20),
      strip.background = element_rect(fill = "white"),
      axis.text.x = element_text(size = 16),
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 20),
      axis.title.y = element_markdown(),
      panel.spacing = unit(.15, "in")
    )
ggsave("figure4.pdf", height = 10, width = 16)
