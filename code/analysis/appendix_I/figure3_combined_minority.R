# This script produces figure 3 from the main text. 

# Import relevant libraries
suppressWarnings(suppressMessages({
  library(eiCompare)
  library(readr)
  library(tidyverse)
}))

# Preamble: Adjust these settings according to your use case
# Turn verbosity on or off
verbose <- TRUE
# Set the base path: where all data files are located

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


# Set working directory to folder in which this file is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

base_path <- "../../../data"
agg_path <- file.path(base_path, "ga_2018_agg_all.rds")
county_cvap_total <- readRDS(agg_path) %>%
  select(-geometry)

all_ei <- readRDS("ei_results_all_combined_minority.rds")

all_ei <- all_ei %>%
  mutate(cand = gsub("_prop", "", cand),
         race = str_sub(race, 1, 3),
         jitter = case_when(
           ei_type == "iter" ~ -.5,
           ei_type == "rxc" ~ .5,
           ei_type == "exit_polls" ~ 0
         )) 

all_ei %>%
  filter(cand == "abrams", race_type != 'cvap_novote') %>%
  mutate(
    race = case_when(
      race == "whi" ~ "White",
      race == "nwh" ~ "Non-white"
    ),
    race = ordered(race, levels = c("White", "Non-white")),
    race_type = ordered(race_type, levels = c("exit_polls", "true", "cvap", "bisg", "fbisg", "fbisgf"))
  ) %>%
  ggplot(aes(x = race_type, y = mean, shape = ei_type, fill = race_type)) +
  geom_errorbar(
    aes(ymin = ci_95_lower, ymax = ci_95_upper),
    width = 0,
    size = 1,
    position = position_dodge(.5)
  ) +
  geom_point(size = 5, position = position_dodge(.5)) +
  scale_y_continuous(
    limits = c(0, 1),
    name = "Proportion voting for Abrams"
  ) +
  scale_x_discrete(
    labels = c("Exit polls", "Known", "CVAP", "BISG", "FBISG", "FBISG with first name"),
    name = "Source of turnout by race"
  ) +
  scale_shape_manual(
    values = c(22, 24, 21),
    labels = c("Exit polls", "Iterative EI", "RxC EI")
  ) +
  scale_fill_brewer(type = "qual") +
  guides(fill = "none") +
  facet_grid(. ~ race) +
  plot_theme +
  theme(
    strip.text = element_text(size = 20),
    strip.background = element_rect(fill = "white"),
    axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
    legend.title = element_blank(),
    legend.text = element_text(size = 20)
  )
ggsave("figure3_combined_minority.pdf", units = "in", height = 10, width = 16)
