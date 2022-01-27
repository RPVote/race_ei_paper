#!/usr/bin/env Rscript
#' Generates the numbers for table 1 of "Comparing BISG to CVAP Estimates in
#' Racially Polarized Voting Analysis" by Collingwood et al.
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
  if(missing_y > 0 | missing_yhat > 0) {
    warning(removing )
  }

  RMSE <- sqrt(1/sum(log(weights)) * sum(log(weights)*((yhat - y)^2)))
  MAD <- 1/sum(log(weights)) * sum(log(weights)*(abs(yhat - y)))
  BIAS <- 1/sum(log(weights)) * sum(log(weights)*(yhat - y))
  return(data.frame("RMSE" = RMSE, "MAD" = MAD, "BIAS" = BIAS))
}

get_errors(race_ests$whi_true_prop, race_ests$whi_2018_cvap_ext_prop)
get_errors(race_ests$bla_true_prop, race_ests$bla_2018_cvap_ext_prop)
get_errors(race_ests$his_true_prop, race_ests$asi_2018_cvap_ext_prop)
get_errors(race_ests$asi_true_prop, race_ests$asi_2018_cvap_ext_prop)
get_errors(race_ests$oth_true_prop, race_ests$oth_2018_cvap_ext_prop)

get_errors(race_ests$whi_true_prop, race_ests$whi_bisg_prop)
get_errors(race_ests$bla_true_prop, race_ests$bla_bisg_prop)
get_errors(race_ests$his_true_prop, race_ests$his_bisg_prop)
get_errors(race_ests$asi_true_prop, race_ests$asi_bisg_prop)
get_errors(race_ests$oth_true_prop, race_ests$oth_bisg_prop)
