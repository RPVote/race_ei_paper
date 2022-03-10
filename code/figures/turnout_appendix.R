#'
#' Do a version of figure 4 where the x-axis is race-specific turnout
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

# Read in CVAP data to get county
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

# Now get turnout numbers
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
    
    whi_turnout_rat = ifelse(tot_turnout_prp == 0, 0, whi_turnout_prp/whi_cvap_prp),
    bla_turnout_rat = ifelse(tot_turnout_prp == 0, 0, bla_turnout_prp/bla_cvap_prp),
    his_turnout_rat = ifelse(tot_turnout_prp == 0, 0, his_turnout_prp/his_cvap_prp),
    oth_turnout_rat = ifelse(tot_turnout_prp == 0, 0, oth_turnout_prp/oth_cvap_prp),
    
    whi_turnout_rat = whi_turnout_prp-whi_cvap_prp,
    bla_turnout_rat = bla_turnout_prp-bla_cvap_prp,
    his_turnout_rat = his_turnout_prp-his_cvap_prp,
    oth_turnout_rat = oth_turnout_prp-oth_cvap_prp
  ) %>%
  select(county, ends_with("turnout_prp"), ends_with("rat"), ends_with("turnout"), ends_with("total"), n_prec)

# Now read in all ei county results and merge with turnout
path <- "../../data/county_ei"
files <- list.files(path)

# First, get long county turnout data
county_turnout_long <- county_turnout %>%
  select(county, tot_turnout, n_prec, ends_with("prp"), ends_with("rat")) %>%
  pivot_longer(
    ends_with("rat"),
    names_to = c("race", "t"),
    names_sep = "_",
    values_to = "turnout_ratio"
  ) %>%
  select(county, tot_turnout, n_prec, race, turnout_ratio, ends_with("prp"))

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
  pivot_wider(id_cols = c("race", "county", "type", "n_prec", "turnout_ratio"), 
              names_from = "race_type", values_from = "mean", values_fill = NA) %>%
  mutate(diff = bisg-cvap) %>%
  right_join(all_ei_county)

fit_model <- function(df) {
  lm(diff ~ tot_turnout_prp, df)
}

all_ei_county_f4 <- all_ei_county %>%
  filter(race_type == "cvap", n_prec > 3) %>%
  group_by(race, type) %>%
  nest() %>%
  mutate(
    model = map(data, fit_model),
    tidied = map(model, ~ tidy(., conf.int = TRUE))
  ) %>%
  unnest(data) %>%
  unnest(tidied) %>%
  filter(term == "tot_turnout_prp") %>%
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

all_ei_county_f4 %>%
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
  ggplot(aes(x = tot_turnout_prp, y = diff)) +
  geom_point(aes(size = n_prec), alpha = alpha) + 
  geom_smooth(method = 'lm', color = 'black') +
  geom_hline(yintercept = 0, color = 'red', linetype = "dashed") +
  geom_label(
    aes(x = -Inf, y = -Inf, vjust = 0, hjust = 0, label = text),
    size = 4,
    label.r = unit(0, "pt")
  ) +
  facet_grid(type ~ race) +
  ylab("Difference in predicted share voting for Abrams<br>(BISG - CVAP)") + 
  xlab("Turnout as proportion of CVAP") +
  coord_cartesian(ylim = c(-.5, .5)) +
  scale_size_continuous(name = "Number of precincts") + 
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


fit_model <- function(df) {
  lm(diff ~ turnout_ratio, df)
}

all_ei_county_appendix <- all_ei_county %>%
  filter(race_type == "cvap", n_prec > 3) %>%
  group_by(race, type) %>%
  nest() %>%
  mutate(
    model = map(data, fit_model),
    tidied = map(model, ~ tidy(., conf.int = TRUE))
  ) %>%
  unnest(data) %>%
  unnest(tidied) %>%
  filter(term == "turnout_ratio") %>%
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
  ggplot(aes(x = turnout_ratio, y = diff)) +
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
  xlab("Difference between race-specific turnout and CVAP population share") +
  coord_cartesian(ylim = c(-.5, .5)) +
  scale_size_continuous(name = "Number of precincts") + 
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
ggsave("figure4_appendix.pdf", height = 10, width = 16)


## Try with diversity?
base_path <- "../../data"
agg_path <- file.path(base_path, "ga_2018_agg_all.rds")
race_ests <- readRDS(agg_path) %>%
  as_tibble() %>%
  select(-geometry)

# Compute Theil's entropy index and Brier score
race_ests <- race_ests %>%
  mutate(
    # Here, we force the entropy equation to set p*log(p) = 0 when p = 0.
    # This ensures a completely absent group maximally indicates segregation.
    # If this adjustment is not made, all precincts with a zero proportion
    # will end up as NAs
    
    # Discussion w/ Stephen Jenkins:
    # https://www.statalist.org/forums/forum/general-stata-discussion/
    # general/1329390-decompose-theil-index-for-data-including-zero-values-no-income-earnings
    
    # Refs:
    # Liao., T (2016). Evaluating Distributional Differences in Income Inequality with Sampling Weights
    # Morrisson, C. and Murtin, F. (2013). The Kuznets curve of human capital inequality: 1870–2010.
    #     Journal of Economic Inequality, 11(3), 283–301.
    
    # Note, we use VAP to estimate segregation because it reflects neighborhood
    # segregation, rather than the turned out or voting-eligible population
    whi_theil = whi_2018_cvap_ext_prop*log(whi_2018_cvap_ext_prop),
    bla_theil = bla_2018_cvap_ext_prop*log(bla_2018_cvap_ext_prop),
    his_theil = his_2018_cvap_ext_prop*log(his_2018_cvap_ext_prop),
    asi_theil = asi_2018_cvap_ext_prop*log(asi_2018_cvap_ext_prop),
    oth_theil = oth_2018_cvap_ext_prop*log(oth_2018_cvap_ext_prop),
    
    whi_theil = ifelse(is.na(whi_theil) | whi_theil == -Inf, 0, whi_theil),
    bla_theil = ifelse(is.na(bla_theil) | bla_theil == -Inf, 0, bla_theil),
    his_theil = ifelse(is.na(his_theil) | his_theil == -Inf, 0, his_theil),
    asi_theil = ifelse(is.na(asi_theil) | asi_theil == -Inf, 0, asi_theil),
    oth_theil = ifelse(is.na(oth_theil) | oth_theil == -Inf, 0, oth_theil),
    
    theil = -1 * (whi_theil + bla_theil + his_theil + asi_theil + oth_theil),
    
    # BISG Deviations
    whi_err_bisg = whi_bisg_prop - whi_true_prop,
    bla_err_bisg = bla_bisg_prop - bla_true_prop,
    his_err_bisg = his_bisg_prop - his_true_prop,
    asi_err_bisg = asi_bisg_prop - asi_true_prop,
    oth_err_bisg = oth_bisg_prop - oth_true_prop,
    
    # CVAP Deviations
    whi_err_cvap = whi_2018_cvap_ext_prop - whi_true_prop,
    bla_err_cvap = bla_2018_cvap_ext_prop - bla_true_prop,
    his_err_cvap = his_2018_cvap_ext_prop - his_true_prop,
    asi_err_cvap = asi_2018_cvap_ext_prop - asi_true_prop,
    oth_err_cvap = oth_2018_cvap_ext_prop - oth_true_prop,
    
    # # VAP Deviations
    # whi_err_vap = whi_2010_vap_ext_prop - whi_true_prop,
    # bla_err_vap = bla_2010_vap_ext_prop - bla_true_prop,
    # his_err_vap = his_2010_vap_ext_prop - his_true_prop,
    # asi_err_vap = asi_2010_vap_ext_prop - asi_true_prop,
    # oth_err_vap = oth_2010_vap_ext_prop - oth_true_prop,
    
    # Brier Scores
    brier_score_bisg = (whi_err_bisg^2 + bla_err_bisg^2 + his_err_bisg^2 + asi_err_bisg^2 + oth_err_bisg^2)/5,
    brier_score_cvap = (whi_err_cvap^2 + bla_err_cvap^2 + his_err_cvap^2 + asi_err_cvap^2 + oth_err_cvap^2)/5,
    # brier_score_vap = (whi_err_vap^2 + bla_err_vap^2 + his_err_vap^2 + asi_err_vap^2 + oth_err_vap^2)/5,
    
    # BISG fractional errors
    whi_frac_err_bisg = whi_err_bisg / whi_bisg_prop,
    bla_frac_err_bisg = bla_err_bisg / bla_bisg_prop,
    his_frac_err_bisg = his_err_bisg / his_bisg_prop,
    asi_frac_err_bisg = asi_err_bisg / asi_bisg_prop,
    oth_frac_err_bisg = oth_err_bisg / oth_bisg_prop,
    
    # CVAP fractional errors
    whi_frac_err_cvap = whi_err_cvap / whi_2018_cvap_ext_prop,
    bla_frac_err_cvap = bla_err_cvap / bla_2018_cvap_ext_prop,
    his_frac_err_cvap = his_err_cvap / his_2018_cvap_ext_prop,
    asi_frac_err_cvap = asi_err_cvap / asi_2018_cvap_ext_prop,
    oth_frac_err_cvap = oth_err_cvap / oth_2018_cvap_ext_prop,
    
    # VAP fractional errors
    # whi_frac_err_vap = whi_err_vap / whi_2010_vap_ext_prop,
    # bla_frac_err_vap = bla_err_vap / bla_2010_vap_ext_prop,
    # his_frac_err_vap = his_err_vap / his_2010_vap_ext_prop,
    # asi_frac_err_vap = asi_err_vap / asi_2010_vap_ext_prop,
    # oth_frac_err_vap = oth_err_vap / oth_2010_vap_ext_prop
    
    county = gsub("^(.*?),.*", "\\1", precinct_id_2018)
  )
county_diversity <- race_ests %>%
  group_by(county) %>%
  summarize(entropy = mean(theil))

fit_model <- function(df) {
  lm(diff ~ entropy, df)
}

all_ei_county_div <- all_ei_county %>%
  left_join(county_diversity) %>%
  filter(race_type == "cvap", n_prec > 3) %>%
  group_by(race, type) %>%
  nest() %>%
  mutate(
    model = map(data, fit_model),
    tidied = map(model, ~ tidy(., conf.int = TRUE))
  ) %>%
  unnest(data) %>%
  unnest(tidied) %>%
  filter(term == "entropy") %>%
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

all_ei_county_div %>%
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
  ggplot(aes(x = entropy, y = diff)) +
  geom_point(aes(size = n_prec), alpha = alpha) + 
  geom_smooth(method = 'lm', color = 'black') +
  geom_hline(yintercept = 0, color = 'red', linetype = "dashed") +
  geom_label(
    aes(x = -Inf, y = -Inf, vjust = 0, hjust = 0, label = text),
    size = 5,
    label.r = unit(0, "pt")
  ) +
  facet_grid(type ~ race, scales = "free") +
  ylab("Difference in predicted share voting for Abrams<br>(BISG - CVAP)") + 
  xlab("Theil's Entropy") +
  coord_cartesian(ylim = c(-.25, .25)) +
  scale_size_continuous(name = "Number of precincts") + 
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
ggsave("figure4_div.pdf", height = 10, width = 16)


# What about n precincts
fit_model <- function(df) {
  lm(diff ~ log(n_prec), df)
}

all_ei_county_prec <- all_ei_county %>%
  left_join(county_diversity) %>%
  filter(race_type == "cvap", n_prec > 1) %>%
  group_by(race, type) %>%
  nest() %>%
  mutate(
    model = map(data, fit_model),
    tidied = map(model, ~ tidy(., conf.int = TRUE))
  ) %>%
  unnest(data) %>%
  unnest(tidied) %>%
  filter(term == "log(n_prec)") %>%
  group_by(type, race) %>%
  mutate(n_county = sum(!is.na(diff))) %>%
  ungroup() %>%
  mutate(
    text = paste0(
      "Beta = ",
      round(estimate, 3), 
      " (p = ", 
      round(p.value, 3),
      "), N = ",
      n_county
    )
  )

all_ei_county_prec %>%
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
  ggplot(aes(x = log(n_prec), y = diff)) +
  geom_point(aes(size = n_prec), alpha = alpha) + 
  geom_smooth(method = 'lm', color = 'black') +
  geom_hline(yintercept = 0, color = 'red', linetype = "dashed") +
  geom_label(
    aes(x = -Inf, y = -Inf, vjust = 0, hjust = 0, label = text),
    size = 5,
    label.r = unit(0, "pt")
  ) +
  facet_grid(type ~ race, scales = "free") +
  ylab("Difference in predicted share voting for Abrams<br>(BISG - CVAP)") + 
  xlab("Log(Number of precincts)") +
  coord_cartesian(ylim = c(-.25, .25)) +
  scale_size_continuous(name = "Number of precincts") + 
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
ggsave("figure4_nprec.pdf", height = 10, width = 16)


# Check difference between the full and the successfully estimated datasets
full_precinct_counts <- race_ests %>% 
  mutate(county = gsub("^(.*?),.*", "\\1", precinct_id_2018)) %>% 
  count(county)

res <- all_ei_county %>%
  filter(race == "whi", type == "rxc", race_type == "bisg") %>%
  select(county, n_prec) %>%
  rename("n_bisg_rxc" = "n_prec") %>%
  right_join(full_precinct_counts) %>%
  mutate(n_bisg_rxc = ifelse(is.na(n_bisg_rxc), 0, n_bisg_rxc))

res <- all_ei_county %>%
  filter(race == "whi", type == "iter", race_type == "bisg") %>%
  select(county, n_prec) %>%
  rename("n_bisg_iter" = "n_prec") %>%
  right_join(res) %>%
  mutate(n_bisg_iter = ifelse(is.na(n_bisg_iter), 0, n_bisg_iter))

res <- all_ei_county %>%
  filter(race == "whi", type == "rxc", race_type == "cvap") %>%
  select(county, n_prec) %>%
  rename("n_cvap_rxc" = "n_prec") %>%
  right_join(res) %>%
  mutate(n_cvap_rxc = ifelse(is.na(n_cvap_rxc), 0, n_cvap_rxc))

res <- all_ei_county %>%
  filter(race == "whi", type == "iter", race_type == "cvap") %>%
  select(county, n_prec) %>%
  rename("n_cvap_iter" = "n_prec") %>%
  right_join(res) %>%
  mutate(n_cvap_iter = ifelse(is.na(n_cvap_iter), 0, n_cvap_iter))


