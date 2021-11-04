## #############################################################################
## Author:
## Project: Immunization financing
## Purpose: Pull final estimates of all immunization components from ST-GPR after
##          vetting models. Calculate the envelope of total government spending
##          on immunization and by component
##          
## Inputs: model version names of estimates to pull for each component
##
## Outputs: final_data_draws.feather. This is a file with the final estimates
##          for all country-years of interest with all immunization components
##          including draws for uncertainty calculations
##
## Last update: 11/03/2021
## #############################################################################
pacman::p_load(tidyverse, data.table, dplyr, feather, reshape2, ggplot2)

rm(list = ls())

## Defining j, h, and k 
if (Sys.info()[1] == "Linux"){
  j <- "FILEPATH"
  h <- paste0("FILEPATH", Sys.info()[7])
  k <- "FILEPATH"
} else if (Sys.info()[1] == "Windows"){
  j <- "FILEPATH"
  h <- "FILEPATH"
  k <- "FILEPATH"
}

source(paste0(h, "FILEPATH/helper_functions.R"))

## set input filepath and read in list of immunization countries of interest
input_data_folder <- "FILEPATH"
years <- 2000:2017
imfin_country_list <- fread(paste0(j, 'FILEPATH/imfin_country_list.csv'))
imfin_country_list <- imfin_country_list[, .(ihme_loc_id, gavi_eligible)]

####################################################################################

## input best model version name for each immunization component
components <- c('vaccine', 'delivery', 'routine', 'supplementary', 'immunization')
models <- c('vaccine_model_version',
            'delivery_model_version',
            'routine_model_version',
            'supplementary_model_version',
            'immunization_model_version')

####################################################################################

## read in total government health spending data with draws for all relevant country years
ghes_estimates <- get_he_data('ghes_totes')
ghes_estimates <- ghes_estimates[year %in% years, .(ihme_loc_id, draw, year_id, ghes_totes = data_var)]

## paste on 'draw_' for consistency with estimates
ghes_estimates[, draw := paste0('draw_', (as.numeric(draw) - 1))]

## merge in location ids
ghes_estimates <- get_location_id(ghes_estimates)

## function to read, melt, and select from draws
read_stgpr_draws <- function(x){
  draw <- fread(x)
  draw <- melt.data.table(draw, id.vars = c("location_id", "year_id", "age_group_id", "sex_id"), variable.name = 'draw', value.name = 'proportion')
  draw <- draw[, .(location_id, year_id, draw, proportion)]
  return(draw)
}

## loop through all immunization components with model versions specified above
for (i in 1:5) {
  
  ## get run_id for model version
  run_id_list <- fread(paste0(input_data_folder, "06_", components[i], "_run_ids.csv"))
  run_id <- run_id_list[model_version == models[i]]$run_id
  
  ## list all files in results directory
  results_dir <- paste0("FILEPATH", run_id, "FILEPATH")
  files <- list.files(results_dir)
  
  ## read, melt, select from, and bind all results
  results <- rbindlist(lapply(paste0(results_dir, files), read_stgpr_draws))
  setnames(results, 'proportion', paste0('proportion_', components[i]))
  
  ## create/add to full data table for all components
  if (i == 1) {
    all_results <- results
  } else {
    all_results <- merge(all_results, results, by = c('location_id', 'year_id', 'draw'))
  }
 
}

## subset to immunization countries of interest
all_results <- get_ihme_loc(all_results)
all_results <- all_results[ihme_loc_id %in% imfin_country_list$ihme_loc_id]

## merge in GBD super regions
all_results <- get_region(all_results)
all_results[ihme_loc_id == 'ARG', super_region_name := 'Latin America and Caribbean'] #update ARG super region

## results are in proportion of total government health spending
## add up proportions for vaccine and delivery as well as routine and supplementary
## these are 2 estimates of total government spending on immunization
all_results[, proportion_immunization_vd := proportion_vaccine + proportion_delivery]
all_results[, proportion_immunization_rs := proportion_routine + proportion_supplementary]

## calculate mean of the three estimates of the proportion of total government spending on immunization
all_results[, new_proportion_immunization := (proportion_immunization + proportion_immunization_vd + proportion_immunization_rs) / 3]

## redistribute each immunization component proportion based on the initial ratios to new envelope
all_results[, new_proportion_vaccine := (proportion_vaccine / proportion_immunization_vd) * new_proportion_immunization]
all_results[, new_proportion_delivery := (proportion_delivery / proportion_immunization_vd) * new_proportion_immunization]
all_results[, new_proportion_routine := (proportion_routine / proportion_immunization_rs) * new_proportion_immunization]
all_results[, new_proportion_supplementary := (proportion_supplementary / proportion_immunization_rs) * new_proportion_immunization]

## keep relevant columns
new_results <- all_results[, .(ihme_loc_id, location_id, year_id, draw, new_proportion_immunization, new_proportion_vaccine, new_proportion_delivery, new_proportion_routine, new_proportion_supplementary)]

## merge on total government health spending values
new_results <- merge(new_results, ghes_estimates, by = c('ihme_loc_id', 'location_id', 'year_id', 'draw'))

## calculate expenditures in 2019 USD from proportions
new_results[, c("immunization", "vaccine", "delivery", "routine", "supplementary") := .SD * ghes_totes,
            .SDcols = c("new_proportion_immunization", "new_proportion_vaccine", "new_proportion_delivery", "new_proportion_routine", "new_proportion_supplementary")]

## write out data
write_feather(new_results, 'FILEPATH/final_data_draws.feather')

