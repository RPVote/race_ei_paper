#!/usr/bin/env Rscript
#' Generates the numbers for table 1
#'
#' This script calculates the RMSE and MAD for BISG and CVAP predictions of
#' racial composition

# Import relevant libraries
suppressWarnings(suppressMessages({
  library(tidyverse)
}))

# Turn verbosity on or off
verbose <- TRUE
# Set working directory to folder in which this file is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Set the base path: where all data files are located
base_path <- "../../data"
agg_path <- file.path(base_path, "ga_2018_agg_all.rds")
race_ests <- readRDS(agg_path) %>%
  select(-geometry)

n_voters <- race_ests$whi_true_total + race_ests$bla_true_total +
            race_ests$his_true_total + race_ests$asi_true_total +
            race_ests$oth_true_total

# Function to get MAD and RMSE for a vector of ground-truth and predicted
# values
get_errors <- function(y, yhat, weights = n_voters) {

  missing_y <- sum(is.na(y))
  missing_yhat <- sum(is.na(yhat))
  # if(missing_y > 0 | missing_yhat > 0) {
  #   warning(removing )
  # }

  RMSE <- sqrt(1/sum(log(weights)) * sum(log(weights)*((yhat - y)^2)))
  MAD <- 1/sum(log(weights)) * sum(log(weights)*(abs(yhat - y)))
  BIAS <- 1/sum(log(weights)) * sum(log(weights)*(yhat - y))
  return(data.frame("RMSE" = RMSE, "MAD" = MAD, "BIAS" = BIAS))
}

get_errors(race_ests$whi_true_prop, race_ests$whi_2018_cvap_ext_prop)
get_errors(race_ests$bla_true_prop, race_ests$bla_2018_cvap_ext_prop)
get_errors(race_ests$his_true_prop, race_ests$his_2018_cvap_ext_prop)
get_errors(race_ests$asi_true_prop, race_ests$asi_2018_cvap_ext_prop)
get_errors(race_ests$oth_true_prop, race_ests$oth_2018_cvap_ext_prop)

get_errors(race_ests$whi_true_prop, race_ests$whi_bisg_prop)
get_errors(race_ests$bla_true_prop, race_ests$bla_bisg_prop)
get_errors(race_ests$his_true_prop, race_ests$his_bisg_prop)
get_errors(race_ests$asi_true_prop, race_ests$asi_bisg_prop)
get_errors(race_ests$oth_true_prop, race_ests$oth_bisg_prop)

# Get Brier Score
briers <- race_ests %>%
  as_tibble() %>%
  mutate(
    whi_dev2_bisg = (whi_true_prop - whi_bisg_prop)^2,
    bla_dev2_bisg = (bla_true_prop - bla_bisg_prop)^2,
    his_dev2_bisg = (his_true_prop - his_bisg_prop)^2,
    asi_dev2_bisg = (asi_true_prop - asi_bisg_prop)^2,
    oth_dev2_bisg = (oth_true_prop - oth_bisg_prop)^2,
    
    whi_dev2_cvap = (whi_true_prop - whi_2018_cvap_ext_prop)^2,
    bla_dev2_cvap = (bla_true_prop - bla_2018_cvap_ext_prop)^2,
    his_dev2_cvap = (his_true_prop - his_2018_cvap_ext_prop)^2,
    asi_dev2_cvap = (asi_true_prop - asi_2018_cvap_ext_prop)^2,
    oth_dev2_cvap = (oth_true_prop - oth_2018_cvap_ext_prop)^2,
    
    brier_bisg = whi_dev2_bisg + bla_dev2_bisg + his_dev2_bisg + asi_dev2_bisg + oth_dev2_bisg,
    brier_cvap = whi_dev2_cvap + bla_dev2_cvap + his_dev2_cvap + asi_dev2_cvap + oth_dev2_cvap,
    
    brier_bisg_w = log(n_voters)*brier_bisg,
    brier_cvap_w = log(n_voters)*brier_cvap
    
  ) %>%
  summarise(
    brier_bisg = mean(brier_bisg),
    brier_cvap = mean(brier_cvap),
    
    brier_bisg_w = 1/sum(log(n_voters))*sum(brier_bisg_w),
    brier_cvap_w = 1/sum(log(n_voters))*sum(brier_cvap_w)
  )

briers
