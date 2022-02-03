suppressWarnings(suppressMessages({
  library(eiCompare)
  library(readr)
  library(tidyverse)
}))

# Preamble: Adjust these settings according to your use case
# Turn verbosity on or off
verbose <- TRUE
# Set the base path: where all data files are located

# Set working directory to folder in which this file is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

base_path <- "../../data"
ga_agg_path <- file.path(base_path, "ga_2018_agg_all.rds")
ei_results_path <- file.path(base_path, "ga_2018_ei_results.csv")
# Turn on for parallel computing
par_compute <- TRUE

if (verbose) {
  message("======================================================")
  message("Beginning script. Reading in precinct results...")
}

# Import election results
results <- readRDS(ga_agg_path) %>%
  # Filter out precincts with no votes
  dplyr::filter(total_votes > 0) %>%
  mutate(county = gsub("^(.*?),.*", "\\1", precinct_id_2018))

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

# Race vectors
race_true <- c("whi_true_prop",
               "bla_true_prop",
               "his_true_prop",
               "oth_true_prop")
race_bisg <- c("whi_bisg_prop",
               "bla_bisg_prop",
               "his_bisg_prop",
               "oth_bisg_prop")
race_cvap <- c("whi_2018_cvap_int_prop",
               "bla_2018_cvap_int_prop",
               "his_2018_cvap_int_prop",
               "oth_2018_cvap_int_prop")

# Candidate vector
cand_cols <- c("abrams_prop", "kemp_prop")

# Race types to loop over
race_cols <- list("true" = race_true, "bisg" = race_bisg, "cvap" = race_cvap)

# Counties to loop over
counties <- sort(unique(results$county))

#' run_eis
#'
#' Function to run both types of EI on a subset of the data, using a 
#' dynamic set of turnoutXrace columns
#' 
#' @param data A subset of precincts from a state
#' @param race_cols A character vector of column names
run_eis <- function(data, race_colnames, par_compute, seed) {

  tryCatch({
    ei_rxc <- eiCompare::ei_rxc(
      data = data,
      cand_cols = cand_cols,
      race_cols = race_colnames,
      totals_col = "total_votes",
      verbose = FALSE,
      ntunes = 4,
      samples = 5000,
      thin = 2,
      n_chains = 2,
      par_compute = par_compute,
      seed = seed
    )
    ei_rxc$estimates$type <- "rxc"  
  }, error = function(err) {
    message("RxC EI Failed!")
    return ("0")
  })

  tryCatch({
    ei_iter <- eiCompare::ei_iter(
      data = data,
      cand_cols = cand_cols,
      race_cols = race_colnames,
      totals_col = "total_votes",
      verbose = FALSE,
      plots = FALSE,
      par_compute = par_compute,
      name = "",
      seed = seed
    )
    ei_iter$estimates$type <- "iter"
    
  }, error = function(err) {
    message("Iter EI Failed!")
    return("0")
  })
  
  if (ei_iter != "0" & ei_rxc != "0") {
    res <- rbind(ei_iter$estimates, ei_rxc$estimates)
    return(res)
  } else if (ei_iter == "0" & ei_rxc == "0") {
    return()
  } else if (ei_iter == "0") {
    return(ei_rxc$estimates)
  } else {
    return(ei_iter$estimates)
  }
}

county_out_path <- "../../data/county_ei"

purrr::walk(
  44:length(counties),
  function(cc) {
    message(cc)
    c <- counties[cc]
    message(c)
    county_results <- results %>% dplyr::filter(county == c)
    county_eis <- purrr::map_dfr(
      1:length(race_cols),
      function(rr) {
        message(names(race_cols)[rr])
        tryCatch({
          ei_res <- run_eis(
            data = county_results,
            race_colnames = race_cols[[rr]],
            par_compute = FALSE,
            seed = 1010101
          )
          ei_res$county <- c
          ei_res$race_type <- names(race_cols)[rr]
          print(head(ei_res))
          return(ei_res)
        }, error = function(cond) { return() })
      }
    )
    saveRDS(
      county_eis, 
      file.path(
        county_out_path, 
        paste0("ei_", cc, "_", c, ".rds")
      )
    )
    gc()
  }
)
