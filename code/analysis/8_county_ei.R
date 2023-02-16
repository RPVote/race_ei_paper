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
par_compute <- FALSE
seed <- 32142
if (verbose) {
  message("======================================================")
  message("Beginning script. Reading in precinct results...")
}

# Import election results
results <- readRDS(ga_agg_path) %>%
  # Filter out precincts with no votes
  dplyr::filter(total_votes > 0) %>%
  mutate(county = gsub("^(.*?),.*", "\\1", precinct_id_2018)) %>%
  as_tibble()

# Combine Asian and Other to match up with other analyses
results <- results %>%
  mutate(
    oth_true_prop = oth_true_prop + asi_true_prop,
    oth_bisg_prop = oth_bisg_prop + asi_bisg_prop,
    oth_2018_cvap_ext_prop = oth_2018_cvap_ext_prop + asi_2018_cvap_ext_prop,
    oth_2018_cvap_int_prop = oth_2018_cvap_int_prop + asi_2018_cvap_int_prop,
    total_votes = whi_true_total + bla_true_total + his_true_total + asi_true_total + oth_true_total
  )

# Make abrams + kemp == 1 
results <- results %>%
  mutate(
    total = abrams_prop + kemp_prop,
    abrams_prop = abrams_prop / total,
    kemp_prop = kemp_prop / total,
    tt = whi_bisg_prop + bla_bisg_prop + his_bisg_prop + oth_bisg_prop,
    whi_bisg_prop = whi_bisg_prop / tt,
    bla_bisg_prop = bla_bisg_prop / tt,
    his_bisg_prop = his_bisg_prop / tt,
    oth_bisg_prop = oth_bisg_prop / tt
  )

# Race vectors
race_bisg <- c("whi_bisg_prop",
               "bla_bisg_prop",
               "his_bisg_prop",
               "oth_bisg_prop")
race_cvap <- c("whi_2018_cvap_int_prop",
               "bla_2018_cvap_int_prop",
               "his_2018_cvap_int_prop",
               "oth_2018_cvap_int_prop")

results %>%
  select(any_of(race_bisg)) %>%
  rowSums()

results %>%
  select(any_of(race_cvap)) %>%
  rowSums()

# Candidate vector
cand_cols <- c("abrams_prop", "kemp_prop")

# Race types to loop over
race_cols <- list("bisg" = race_bisg, "cvap" = race_cvap)

# Counties to loop over
counties <- sort(unique(results$county))

county_out_path <- "../../data/county_ei"

#counties_to_skip <- c("Coffee", "Early", "Jefferson", "Tattnall", "Upson")
counties_to_skip <- c()
list.files(county_out_path)

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
    ei_iter <- withTimeout(
      eiCompare::ei_iter(
        data = data,
        cand_cols = cand_cols,
        race_cols = race_colnames,
        totals_col = "total_votes",
        verbose = FALSE,
        erho = c(100, 50, 20, 10, 5, 1, .5, .1, .05, .01, .001, seq(49)),
        plots = FALSE,
        par_compute = par_compute,
        name = "",
        seed = seed
      ),
      timeout = 2400
    )
    ei_iter$estimates$type <- "iter"  
  }, error = function(err) {
    message("Iter EI Failed!")
    return("0")
  })
  
  if (all(ei_iter != "0" & ei_rxc != "0")) {
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
    if (!(c %in% counties_to_skip)) {
      if (paste0("ei_", cc, "_", c, ".rds") %in% list.files(county_out_path)) {
        message("Already done!")
        return()
      } else {
        county_results <- results %>% dplyr::filter(county == c)
        if (nrow(county_results) >= 2) {
          county_eis <- purrr::map_dfr(
            1:length(race_cols),
            function(rr) {
              message(names(race_cols)[rr])
              tryCatch({
                ei_res <- run_eis(
                  data = county_results,
                  race_colnames = race_cols[[rr]],
                  par_compute = TRUE,
                  seed = 3247
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
      }
    }
  }
)

files <- list.files(county_out_path)
all_ei_county <- map_dfr(
  files,
  function(f) { readRDS(file.path(county_out_path, f)) }
) %>%
  as_tibble() %>% 
  mutate(cand = gsub("_prop", "", cand),
         race = str_sub(race, 1, 3))
saveRDS(all_ei_county, file.path(base_path, "county_ei.rds"))


