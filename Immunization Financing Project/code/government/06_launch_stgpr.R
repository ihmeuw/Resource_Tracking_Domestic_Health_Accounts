## ######################################################################################################
## Author: 
## Project: Immunization financing
## Purpose: Register and launch an ST-GPR model from R to estimate immunization expenditures
##
## Inputs: model version names as they are in the configuration file. Add additional model version names
##         for new model versions. Do not remove old model version names.
##
## Outputs: Sends model version to ST-GPR. 06_COMPONENT_run_ids.csv contains the run ids for all model
##          versions, which also includes the date the model is run.
## 
## Last update: 11/03/2021
## ######################################################################################################
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

central_root <- 'FILEPATH'
setwd(central_root)
data_folder <- "FILEPATH"

library(data.table)
source('FILEPATH/register.R')
source('FILEPATH/sendoff.R')

## date
date <- format(Sys.Date(), '%m%d%y')

## model versions
model_version <- c('model_version_1', 'model_version_2', 'model_version_3')

## immunization components to loop through
components <- c('vaccine', 'routine', 'delivery', 'supplementary', 'immunization')

## loop through components (send off each component's model(s))
for (j in components) {

## loop over all model versions (if multiple)
for (i in 1:3) { 
  
  ####################################
  # Set arguments
  ####################################
  
  # filepath to configuration file
  config <- "FILEPATH"
  
  # arguments
  me_name <- paste0("imfin_ghes_", j)
  path_to_config <- paste0(config, 'ghes_', j, '_config.csv')
  my_model_id <- i
  project <- 'proj_fgh'
  
  # number of parallelizations
  nparallel <- 50
  
  ####################################
  # Register an ST-GPR model
  ####################################
  
  # create new run id for model 
  run_id <- register_stgpr_model(path_to_config = path_to_config,
                                 model_index_id = my_model_id)
  
  # submit ST-GPR model for the newly created run id
  stgpr_sendoff(run_id,
                project,
                nparallel = nparallel)
  
  # creating a list of run ids by immunization component to extract estimates
  if (i == 1){
    run_id_list <- data.table(config_number = i,
                              model_version = model_version[i],
                              run_id = run_id,
                              component = j,
                              date = date)
  } else {
    run_id_list <- fread(paste0(data_folder, "FILEPATH/06_", j, "_run_ids.csv"))
    run_id_list <- rbind(run_id_list,
                         data.table(config_number = i,
                                    model_version = model_version[i],
                                    run_id = run_id,
                                    component = j,
                                    date = date))
  }
  
  fwrite(run_id_list, paste0(data_folder, "FILEPATH/06_", j, "_run_ids.csv"))
  
}
  
}
