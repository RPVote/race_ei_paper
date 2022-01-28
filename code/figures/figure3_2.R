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

in_path <- "../../data/ei_results_all.rds"
all_ei <- readRDS(in_path)

all_ei <- all_ei %>%
  mutate(cand = gsub("_prop", "", cand),
         race = str_sub(race, 1, 3),
         jitter = case_when(
           ei_type == "iter" ~ -.5,
           ei_type == "rxc" ~ .5,
           ei_type == "exit_polls" ~ 0
         )) 

all_ei %>%
  filter(cand == "abrams") %>%
  mutate(
    race = case_when(
      race == "whi" ~ "White",
      race == "bla" ~ "Black",
      race == "his" ~ "Hispanic",
      race == "oth" ~ "Other"
    ),
    race = ordered(race, levels = c("White", "Black", "Hispanic", "Other")),
    race_type = ordered(race_type, levels = c("exit_polls", "true", "bisg", "cvap"))
  ) %>%
  ggplot(aes(x = race_type, y = mean, shape = ei_type)) +
    geom_errorbar(
      aes(ymin = ci_95_lower, ymax = ci_95_upper),
      width = 0,
      position = position_dodge(.5)
    ) +
    geom_point(size = 5, fill = "white", position = position_dodge(.5)) +
    scale_y_continuous(
      limits = c(0, 1),
      name = "Proportion voting for Abrams"
    ) +
    scale_x_discrete(
      labels = c("Exit Polls", "Known", "BISG", "CVAP"),
      name = "Source of turnout by race"
    ) +
    scale_shape_manual(
      values = c(22, 4, 21),
      labels = c("Exit polls", "Iterative EI", "RxC EI")
    ) +
    facet_grid(. ~ race) +
    plot_theme +
    theme(
      strip.text = element_text(size = 20),
      strip.background = element_rect(fill = "white"),
      axis.text.x = element_text(size = 16),
      legend.title = element_blank(),
      legend.text = element_text(size = 20)
    )
ggsave("ei_results.pdf", units = "in", height = 13, width = 16)
