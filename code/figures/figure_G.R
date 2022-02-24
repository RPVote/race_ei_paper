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

# Get turnout by county
base_path <- "../../data"
ga_agg_path <- file.path(base_path, "ga_2018_agg_all.rds")
results <- readRDS(ga_agg_path) %>%
  as_tibble() %>%
  select(-geometry) %>%
  # Filter out precincts with no votes
  dplyr::filter(total_votes > 0)

# Combine Asian and Other to match up with other analyses
results <- results %>%
  mutate(
    oth_true_prop = oth_true_prop + asi_true_prop,
    oth_bisg_prop = oth_bisg_prop + asi_bisg_prop,
    oth_2018_cvap_ext_prop = oth_2018_cvap_ext_prop + asi_2018_cvap_ext_prop,
    oth_2018_cvap_int_prop = oth_2018_cvap_int_prop + asi_2018_cvap_int_prop
  )

# Make abrams + kemp == 1 
results <- results %>%
  mutate(
    total = abrams_prop + kemp_prop,
    abrams_prop = abrams_prop / total,
    kemp_prop = kemp_prop / total
  )

precincts %>%
  select(abrams_prop, whi_true_prop, bla_true_prop, his_true_prop, oth_true_prop) %>%
  cor() %>%
  corrplot::corrplot(method = "number")

