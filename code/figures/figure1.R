#!/usr/bin/env Rscript
#' Generates Figure 1
#'
#' This script calculates and produces the scatter comparisons between BISG,
#' CVAP, and self-reported race across racial groups.

# Import relevant libraries
suppressWarnings(suppressMessages({
  library(ggh4x)
  library(tidyverse)
  library(sf)
  library(patchwork)
}))

# Set working directory to folder in which this file is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Turn verbosity on or off
verbose <- TRUE
# Set the base path: where all data files are located
base_path <- "../../data"
agg_path <- file.path(base_path, "ga_2018_agg_cvap.rds")
agg <- readRDS(agg_path)

# Plot theme
alpha <- 0.25
plot_theme <-
  theme_bw() +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 22, face = "bold"),
        axis.title.x = element_text(margin = margin(t = 20)),
        axis.title.y = element_text(margin = margin(r = 20)),
        plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
        plot.margin = unit(c(0, 20, 40, 0), "pt"),
        legend.position = "none",
        strip.background = element_rect(fill = 'white'),
        strip.text = element_text(size = 22))

agg %>%
  as_tibble() %>%
  select(contains("true"), contains("bisg"), contains("cvap_ext"), total_votes) %>%
  select(contains("prop"), total_votes) %>%
  rename(
    "whi_cvap_prop" = "whi_2018_cvap_ext_prop",
    "bla_cvap_prop" = "bla_2018_cvap_ext_prop",
    "his_cvap_prop" = "his_2018_cvap_ext_prop",
    "asi_cvap_prop" = "asi_2018_cvap_ext_prop",
    "oth_cvap_prop" = "oth_2018_cvap_ext_prop",
  ) %>%
  pivot_longer(
    -any_of(c('whi_true_prop', 'bla_true_prop', 'his_true_prop', 
            'asi_true_prop', 'oth_true_prop', 'total_votes')),
    names_to = c("race", "type"),
    names_sep = '_',
    values_to = 'prp'
  ) %>%
  pivot_longer(contains("true"),
               names_to = c("true_race", "_"),
               names_sep = "_",
               values_to = "true_prp") %>%
  filter(race == true_race) %>%
  mutate(
    type = case_when(
      type == "cvap" ~ "CVAP",
      type == "bisg" ~ "BISG",
      type == "fbisg" ~ "FBISG",
      type == "fbisgf" ~ "FN-FBISG"
    ),
    type = ordered(type, levels = c('CVAP', "BISG", "FBISG", "FN-FBISG")),
    race = case_when(
      race == 'whi' ~ "White",
      race == 'bla' ~ "Black",
      race == 'his' ~ "Hispanic",
      race == 'asi' ~ "Asian",
      race == 'oth' ~ "Other",
    ),
    race = ordered(race, levels = c("White", "Black", "Hispanic", "Asian", "Other")) 
  ) %>%
  ggplot(aes(x = true_prp, y = prp, size = total_votes)) +
  geom_point(color = "black", alpha = alpha) +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red",
              lwd = 1,
              linetype = "dashed") +
  ylim(0, 1) +
  scale_x_continuous(
   # breaks = c(0, .25, .5, .75), 
    name = "Observed Fraction (Self-Reported)",
    limits = c(0, 1)) +
  ylab("Estimated Fraction") +
  ggh4x::facet_grid2(race ~ type, scales = 'free') +
  plot_theme #+
  #  coord_fixed()
ggsave(filename = "figure1.pdf",  width = 21, height = 24)
