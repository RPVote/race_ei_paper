#'
#' This script generate appendix figure X from the main text
#'
suppressPackageStartupMessages({
  library(tidyverse)
  library(tidycensus)
  library(ggtext)
  library(tidymodels)
  library(sf)
})

base_path <- "../../data"

# Set working directory to folder in which this file is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Plot theme
alpha <- 0.25
plot_theme <-
  theme_bw() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 23, face = "bold"),
        axis.title.x = element_text(margin = margin(t = 20)),
        axis.title.y = element_text(margin = margin(r = 20)),
        plot.title = element_text(size = 27, face = "bold"),
        legend.position = "bottom")

all_ei_county <- readRDS(file.path(base_path, "county_ei_turnout.rds"))


fit_model <- function(df) {
  lm(diff ~ turnout_difference, df)
}

all_ei_county_appendix <- all_ei_county %>%
  filter(race_type == "cvap", race %in% c("whi", "bla")) %>%
  group_by(race, type) %>%
  nest() %>%
  mutate(
    model = map(data, fit_model),
    tidied = map(model, ~ tidy(., conf.int = TRUE))
  ) %>%
  unnest(data) %>%
  unnest(tidied) %>%
  filter(term == "turnout_difference") %>%
  group_by(type, race) %>%
  mutate(n_county = sum(!is.na(diff))) %>%
  ungroup() %>%
  mutate(
    text = paste0(
      "Beta = ",
      round(estimate, 3), 
      " [", 
      round(conf.low, 3), 
      ", ", 
      round(conf.high, 3), 
      "], N = ",
      n_county
    )
  )

all_ei_county_appendix %>%
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
  ggplot(aes(x = turnout_difference, y = diff)) +
  geom_point(aes(size = n_prec), alpha = alpha) + 
  geom_smooth(method = 'lm', color = 'black') +
  geom_hline(yintercept = 0, color = 'red', linetype = "dashed") +
  geom_label(
    aes(x = -Inf, y = -Inf, vjust = 0, hjust = 0, label = text),
    size = 4,
    label.r = unit(0, "pt")
  ) +
  facet_grid(type ~ race, scales = "free") +
  ylab("Difference in predicted share voting for Abrams<br>(BISG - CVAP)") + 
  xlab("Difference between race-specific turnout<br> and CVAP population share") +
  coord_cartesian(ylim = c(-.5, .5)) +
  scale_size_continuous(name = "Number of precincts") + 
  plot_theme +
  theme(
    strip.text = element_text(size = 20),
    strip.background = element_rect(fill = "white"),
    axis.text.x = element_text(size = 16),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown(),
    panel.spacing = unit(.15, "in")
  )
ggsave("figure_i1.pdf", height = 10, width = 8)
