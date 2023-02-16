suppressPackageStartupMessages({
  library(tidyverse)
})

# Preamble: Adjust these settings according to your use case
# Turn verbosity on or off
verbose <- TRUE
# Set the base path: where all data files are located

plot_theme <-
  theme_bw() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(margin = margin(t = 20)),
        axis.title.y = element_text(margin = margin(r = 20)),
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "bottom")

# Set working directory to folder in which this file is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

base_path <- "../../data"
results_2016_path <- file.path(base_path, "ersd_all.rds")

results <- readRDS(results_2016_path)

results %>%
  select(-contains("_total"), -morales_prop, - weissmandl_prop) %>%
  pivot_longer(
    -any_of(c("ed_school", "total")),
    names_to = c('race', 'method'),
    names_sep = '_',
    values_to = 'prop'
  ) %>%
  mutate(
    method = case_when(
      method == '2016' ~ "CVAP",
      method == 'bisg' ~ "BISG",
      method == 'fbisg' ~ "FBISG",
      method == 'fbisgf' ~ "FBISG + First"
    ),
    method = ordered(method, levels = c('FBISG + First', 'FBISG', 'BISG', 'CVAP'))
  ) %>%
  filter(race %in% c('whi')) %>%
  ggplot(aes(y = method, x = prop, fill = method)) +
    facet_wrap(ed_school ~ ., ncol = 5) +
    geom_label(aes(label = round(prop, 3)), size = 2) + 
    scale_x_continuous(limits = c(0, 1.1), name = "Estimated proportion of white voters",
                       breaks = c(0, .25, .5, .75, 1)) + 
    scale_fill_brewer(type = 'qual') +
    plot_theme +
    theme(axis.text.x = element_text(size = 7),
          axis.title.y = element_blank(),
          axis.text.y = element_text(angle = 45, size = 7),
          strip.background = element_rect(fill = 'white'),
          strip.text = element_text(size = 8),
          legend.position = 'None')

ggsave("figure4.pdf", height = 4, width = 8)
