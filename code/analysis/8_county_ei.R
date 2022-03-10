suppressWarnings(suppressMessages({
  library(eiCompare)
  library(readr)
  library(tidyverse)
  library(R.utils)
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
seed <- 101010
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
    oth_2018_cvap_int_prop = oth_2018_cvap_int_prop + asi_2018_cvap_int_prop,
    total_votes = whi_true_total + bla_true_total + his_true_total + asi_true_total
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
race_cvap <- c("whi_2018_cvap_ext_prop",
               "bla_2018_cvap_ext_prop",
               "his_2018_cvap_ext_prop",
               "oth_2018_cvap_ext_prop")

# Candidate vector
cand_cols <- c("abrams_prop", "kemp_prop")

# Race types to loop over
race_cols <- list("true" = race_true, "bisg" = race_bisg, "cvap" = race_cvap)

# Counties to loop over
counties <- sort(unique(results$county))

county_out_path <- "../../data/county_ei"

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

purrr::walk(
  1:length(counties),
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

#' Now try again for the ones that didn't work
files <- list.files(county_out_path)
all_ei_county <- map_dfr(
  files,
  function(f) { readRDS(file.path(county_out_path, f)) }
) %>%
  as_tibble() %>% 
  mutate(cand = gsub("_prop", "", cand),
         race = str_sub(race, 1, 3))

full_set <- expand.grid(
  c("abrams", "kemp"),
  c("whi", "bla", "his", "oth"),
  c("iter", "rxc"),
  c("true", "bisg", "cvap")
)
names(full_set) <- c("cand", "race", "type", "race_type")

to_retry <- map_dfr(
  counties,
  function(cc) {
    completed_eis <- all_ei_county %>%
      filter(county == cc) %>%
      select("cand", "race", "type", "race_type")
    x <- rbind(full_set, completed_eis)
    unfinished <- x[! duplicated(x, fromLast=TRUE) & seq(nrow(x)) <= nrow(full_set), ]
    if (nrow(unfinished) > 0) {
      unfinished$county <- cc
      return(unfinished)
    } else {
      return()
    }
  }
)
to_retry <- to_retry %>%
  select(county, type, race_type) %>%
  distinct()

walk(
  1:nrow(to_retry),
  function(ii) {
    race_type = as.character(to_retry[ii, "race_type"])
    type = as.character(to_retry[ii, "type"])
    cc = as.character(to_retry[ii, "county"])
    message(paste(cc, race_type, type, sep = ", "))
    
    data <- results %>% 
      filter(county == cc) %>%
      as_tibble() %>%
      select(-geometry)
    
    if (cc == "Taliaferro") {
      message("Skipping ", cc)
      return()
    } else if (nrow(data) <= 1) {
      message(cc, " has only 1 precinct. Skipping...")
      return()  
    } else {
      
      if (race_type == "cvap") {
        race_colnames <- race_cvap
      } else if (race_type == "bisg") {
        race_colnames <- race_bisg
      } else {
        race_colnames <- race_true
      }

      if(type == "iter") {
        tryCatch(
          {
            res <- withTimeout(eiCompare::ei_iter(
              data = data,
              cand_cols = cand_cols,
              race_cols = race_colnames,
              totals_col = "total_votes",
              verbose = FALSE,
              plots = FALSE,
              par_compute = FALSE,
              name = "",
              seed = seed
            ), timeout = 60)
            res$estimates$type <- "iter"
          }, error = function(cond) {
            if (grepl("reached elapsed time limit|reached CPU time limit", cond$message)) {
              message("Timed out!")
              return()
              # we reached timeout, apply some alternative method or do something else
            } else {
              message("Broke... skipping")
              return()
            }
          }
        )  
      } else {
        tryCatch(
          {
            res <- eiCompare::ei_rxc(
              data = data,
              cand_cols = cand_cols,
              race_cols = race_colnames,
              totals_col = "total_votes",
              verbose = FALSE,
              ntunes = 4,
              samples = 5000,
              thin = 2,
              n_chains = 2,
              par_compute = FALSE,
              seed = seed
            )
            res$estimates$type <- "rxc"
          }, error = function(cond) {
            message("Broke... skipping")
            return()
          }
        )
       }
      tryCatch({
        res$estimates$county <- cc
        res$estimates$race_type <- race_type
        saveRDS(
          res$estimates,
          file.path(county_out_path, paste0("ei_", cc, "_", type, "_", race_type, ".rds"))
        ) 
      }, error = function(cond) {return()})
    }
  }
)
