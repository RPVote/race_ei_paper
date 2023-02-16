# This script produces figure X from the main text. 

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

all_ei <- readRDS("../../../data/ei_results_all.rds")

all_ei_unk <- readRDS("ei_results_unk.rds")

all_ei_bisg <- all_ei %>%
  filter(race_type %in% c('bisg', 'fbisg', 'fbisgf')) %>%
  rbind(all_ei_unk)

all_ei_bisg <- all_ei_bisg %>%
  mutate(cand = gsub("_prop", "", cand),
         race = str_sub(race, 1, 3),
         jitter = case_when(
           ei_type == "iter" ~ -.5,
           ei_type == "rxc" ~ .5,
           ei_type == "exit_polls" ~ 0
         )) 

all_ei_bisg %>%
  filter(cand == "abrams", !grepl('true', race_type)) %>%
  mutate(
    race = case_when(
      race == "whi" ~ "White",
      race == "bla" ~ "Black",
      race == "his" ~ "Hispanic",
      race == "oth" ~ "Other"
    ),
    race = ordered(race, levels = c("White", "Black", "Hispanic", "Other")),
    type = ifelse(grepl("_", race_type), "With Unknown", 'Without Unknown'),
    race_type = sub("_unk", "", race_type),
    race_type = case_when(
      race_type == 'bisg' ~ "BISG",
      race_type == 'fbisg' ~ "FBISG",
      race_type == 'fbisgf' ~ "FBISG + First Name"
    )
  ) %>%
  ggplot(aes(x = type, y = mean, shape = ei_type, fill = type)) +
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
    scale_shape_manual(
      values = c(22, 24, 21),
      labels = c("Iterative EI", "RxC EI")
    ) +
    scale_fill_manual(values = c("black", "white")) +
    guides(fill = "none") +
    facet_grid(race_type ~ race) +
    plot_theme +
    theme(
      strip.text = element_text(size = 20),
      strip.background = element_rect(fill = "white"),
      axis.text.x = element_text(size = 16, angle = 90, hjust = .5, vjust = 0),
      legend.title = element_blank(),
      legend.text = element_text(size = 20),
      axis.title.x = element_blank(),
    )
ggsave("ei_results_unk.pdf", units = "in", height = 10, width = 16)
