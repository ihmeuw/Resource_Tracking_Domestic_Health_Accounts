#### #----#                    Docstring                    #----# ####
#' Title:    05_stgpr_sendoff.R
#' Project:  IHME Brain Health Initiative
#' Purpose: Sendoff MTUS hours data to ST-GPR
#'     
#' Author: USERNAME
#' Date: 12/17/2024
#' Last Updated: 9/10/2025
#---------------------------------------------------------------------#

# Clean the environment 
rm(list=ls())

if (Sys.info()["sysname"] == "Linux") {
  j_root <- "ROOT" 
  h_root <- "ROOT"
  l_root <- "ROOT"
  functions_dir <- "FILEPATH"
} else { 
  j_root <- "ROOT"
  h_root <- "ROOT"
  l_root <- "ROOT"
  functions_dir <- "FILEPATH"
}

library(data.table)
data_path <- 'FILEPATH'
config_file_path <- paste0(data_path, 'FILEPATH')

## Register and run a model 
source("FILEPATH")

## Specify cluster arguments for launch
cluster_project = "PROJECT_NAME" 
nparallel = 75 
slots = 5 
logs <- 'FILEPATH'

## Select model index from config
model_index <- c(18:20)

## Loop through ST-GPR runs
for (id in model_index) {
  
  ## Register an ST-GPR model using a config for non-decom run. 
  stgpr_run_id <- register_stgpr_model(paste0(config_file_path), model_index_id = id)
  stgpr_run_id
  
  ## Submit ST-GPR model for your newly-created run_id!
  stgpr_sendoff(stgpr_run_id, cluster_project, nparallel = nparallel, log_path = logs)
  
  ## Save to config
  config <- fread(config_file_path)
  config[model_index_id == id, run_id := stgpr_run_id]
  fwrite(config, config_file_path)
  
}


## End of Script ##