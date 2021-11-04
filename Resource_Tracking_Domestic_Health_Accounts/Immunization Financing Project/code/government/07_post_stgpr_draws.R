## #############################################################################
## Author:
## Project: Immunization financing
## Purpose: Pull immunization expenditure estimates for all components from ST-GPR
##          for specified model version
##          
## Inputs: model version name of estimates to pull
##
## Outputs: 07_COMPONENT_draws.feather (file containing 1000 draws of estimates
##          for every country-year by immunization component)
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

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## model version to pull estimates
model_version <- 'model_version_1' 

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


input_data_folder <- "FILEPATH"
output_data_folder <- "FILEPATH"
source("FILEPATH/utility.R")


## function to read, melt, and select from draws
read_stgpr_draws <- function(x){
  draw <- fread(x)
  draw <- melt.data.table(draw,
                          id.vars = c("location_id", "year_id", "age_group_id", "sex_id"),
                          variable.name = 'draw',
                          value.name = 'proportion')
  draw <- draw[, .(location_id, year_id, draw, proportion)]
  return(draw)
}


## immunization components to loop through
components <- c('vaccine', 'routine', 'delivery', 'supplementary', 'immunization')

## loop through immunization components and implement draws function
for (i in components) {
  
  # get run id for model version
  run_id_list <- fread(paste0(input_data_folder, "06_", i, "_run_ids.csv"))
  run_id <- run_id_list[model_version == model_version]$run_id
  
  # list all files in results directory
  results_dir <- paste0("FILEPATH", run_id, "FILEPATH")
  files <- list.files(results_dir)
  
  # read, melt, select from, and bind all results
  results <- rbindlist(lapply(paste0(results_dir, files), read_stgpr_draws))
  
  # add model version
  results[, model_version := model]
  
  # write outputs
  write_feather(results, paste0(output_data_folder, '07_', i, '_draws.feather'))
  
}

