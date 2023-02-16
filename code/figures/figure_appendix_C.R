# Import relevant libraries
suppressWarnings(suppressMessages({
  library(eiCompare)
  library(readr)
  library(sf)
  library(tidyverse)
}))

# Preamble: Adjust these settings according to your use case
# Turn verbosity on or off
verbose <- TRUE
# Set the base path: where all data files are located
base_path <- "../../data"
ga_precincts_2018_path <- file.path(base_path, "ga_2018_election_precincts")
vf_path <- file.path(base_path, "ga_voter_file_2018_final.csv")
agg_path <- file.path(base_path, "ga_2018_agg.csv")

plot_theme <-
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(margin = margin(t = 20)),
        axis.title.y = element_text(margin = margin(r = w)),
        plot.title = element_text(size = 12, face = "bold"),
        legend.position = "bottom")

vf <-
  readr::read_csv(
    vf_path,
    col_types = readr::cols(
      .default = readr::col_character(),
      registration_number = readr::col_double(),
      birthyear = readr::col_double(),
      date_registration = readr::col_date(),
      date_added = readr::col_date(),
      date_changed = readr::col_date(),
      date_last_contact = readr::col_date(),
      date_last_voted = readr::col_date(),
      whi_bisg = readr::col_double(),
      bla_bisg = readr::col_double(),
      his_bisg = readr::col_double(),
      asi_bisg = readr::col_double(),
      oth_bisg = readr::col_double(),
      whi_fbisg = readr::col_double(),
      bla_fbisg = readr::col_double(),
      his_fbisg = readr::col_double(),
      asi_fbisg = readr::col_double(),
      oth_fbisg = readr::col_double(),
      whi_fbisgf = readr::col_double(),
      bla_fbisgf = readr::col_double(),
      his_fbisgf = readr::col_double(),
      asi_fbisgf = readr::col_double(),
      oth_fbisgf = readr::col_double())) %>%
  dplyr::filter(precinct_id_2018 != "Fulton,Sc17B") 

vf <- vf %>%
  mutate(race = case_when(
    race == 'WH' ~ 'whi',
    race == 'BH' ~ 'bla',
    race == 'HP' ~ 'his',
    race == 'AP' ~ 'asi',
    race %in% c('AI', 'OT') ~ 'oth'
  )) %>%
  filter(!is.na(race))

vf <- vf %>%
  mutate(
    whi_binary = as.factor(ifelse(race == 'whi', 1, 0)),
    bla_binary = as.factor(ifelse(race == 'bla', 1, 0)),
    his_binary = as.factor(ifelse(race == 'his', 1, 0)),
    asi_binary = as.factor(ifelse(race == 'asi', 1, 0)),
    oth_binary = as.factor(ifelse(race == 'oth', 1, 0))
  ) 

whi_bisg_auc <- data.frame('auc' = yardstick::roc_auc_vec(vf$whi_binary, vf$whi_bisg, event_level = 'second'), 'race' = 'White', 'method' = 'BISG')
whi_fbisg_auc <- data.frame('auc' = yardstick::roc_auc_vec(vf$whi_binary, vf$whi_fbisg, event_level = 'second'), 'race' = 'White', 'method' = 'FBISG')
whi_fbisgf_auc <- data.frame('auc' = yardstick::roc_auc_vec(vf$whi_binary, vf$whi_fbisgf, event_level = 'second'), 'race' = 'White', 'method' = 'FBISG + First Name')

bla_bisg_auc <- data.frame('auc' = yardstick::roc_auc_vec(vf$bla_binary, vf$bla_bisg, event_level = 'second'), 'race' = 'Black', 'method' = 'BISG')
bla_fbisg_auc <- data.frame('auc' = yardstick::roc_auc_vec(vf$bla_binary, vf$bla_fbisg, event_level = 'second'), 'race' = 'Black', 'method' = 'FBISG')
bla_fbisgf_auc <- data.frame('auc' = yardstick::roc_auc_vec(vf$bla_binary, vf$bla_fbisgf, event_level = 'second'), 'race' = 'Black', 'method' = 'FBISG + First Name')

his_bisg_auc <- data.frame('auc' = yardstick::roc_auc_vec(vf$his_binary, vf$his_bisg, event_level = 'second'), 'race' = 'Hispanic', 'method' = 'BISG')
his_fbisg_auc <- data.frame('auc' = yardstick::roc_auc_vec(vf$his_binary, vf$his_fbisg, event_level = 'second'), 'race' = 'Hispanic', 'method' = 'FBISG')
his_fbisgf_auc <- data.frame('auc' = yardstick::roc_auc_vec(vf$his_binary, vf$his_fbisgf, event_level = 'second'), 'race' = 'Hispanic', 'method' = 'FBISG + First Name')

asi_bisg_auc <- data.frame('auc' = yardstick::roc_auc_vec(vf$asi_binary, vf$asi_bisg, event_level = 'second'), 'race' = 'Asian', 'method' = 'BISG')
asi_fbisg_auc <- data.frame('auc' = yardstick::roc_auc_vec(vf$asi_binary, vf$asi_fbisg, event_level = 'second'), 'race' = 'Asian', 'method' = 'FBISG')
asi_fbisgf_auc <- data.frame('auc' = yardstick::roc_auc_vec(vf$asi_binary, vf$asi_fbisgf, event_level = 'second'), 'race' = 'Asian', 'method' = 'FBISG + First Name')

oth_bisg_auc <- data.frame('auc' = yardstick::roc_auc_vec(vf$oth_binary, vf$oth_bisg, event_level = 'second'), 'race' = 'Other', 'method' = 'BISG')
oth_fbisg_auc <- data.frame('auc' = yardstick::roc_auc_vec(vf$oth_binary, vf$oth_fbisg, event_level = 'second'), 'race' = 'Other', 'method' = 'FBISG')
oth_fbisgf_auc <- data.frame('auc' = yardstick::roc_auc_vec(vf$oth_binary, vf$oth_fbisgf, event_level = 'second'), 'race' = 'Other', 'method' = 'FBISG + First Name')

aucs <- rbind(
  whi_bisg_auc,
  whi_fbisg_auc,
  whi_fbisgf_auc,
  bla_bisg_auc,
  bla_fbisg_auc,
  bla_fbisgf_auc,
  his_bisg_auc,
  his_fbisg_auc,
  his_fbisgf_auc,
  asi_bisg_auc,
  asi_fbisg_auc,
  asi_fbisgf_auc,
  oth_bisg_auc,
  oth_fbisg_auc,
  oth_fbisgf_auc
)

aucs %>%
  mutate(race = ordered(race, levels = c("White", "Black", "Hispanic", "Asian", "Other")),
         method = ordered(method, levels = c("FBISG + First Name", "FBISG", "BISG"))) %>%
  ggplot(aes(y = method, x = auc)) +
    geom_point(shape = 22, aes(fill = method), size = 3) +
    geom_text(aes(label = round(auc, 3)), nudge_x = .04, size = 3) +
    facet_grid(race ~ .) +
    scale_x_continuous(limits = c(0, 1), name = 'AUC') +
    plot_theme +
    theme(axis.title.y = element_blank(),
          legend.position = 'None')
ggsave("auc.pdf", height = 6, width = 8)

# Now by precinct
precinct_rocs <- vf %>%
  group_by(precinct_id_2018) %>%
  summarize(white_auc_bisg = yardstick::roc_auc_vec(whi_binary, whi_bisg, event_level = 'second'),
            white_auc_fbisg = yardstick::roc_auc_vec(whi_binary, whi_fbisg, event_level = 'second'),
            white_auc_fbisgf = yardstick::roc_auc_vec(whi_binary, whi_fbisgf, event_level = 'second'),
            prp_white = sum(race == 'whi')/n())

precinct_rocs %>%
  pivot_longer(-any_of(c("precinct_id_2018","prp_white")),
               names_to = 'type', 
               values_to = 'auc') %>%
  ggplot(aes(x = prp_white, y = auc)) +
    geom_point(alpha = .05) +
    geom_smooth() +
    facet_grid(.~type)
