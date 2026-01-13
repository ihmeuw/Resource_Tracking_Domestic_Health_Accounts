#### #----#                    Docstring                    #----# ####
#' Title:    04_set_config.R
#' Project:  IHME Brain Health Initiative
#' Purpose : Update configuration file for STGPR for MTUS work and care
#' hours data
#'     
#' Author: USERNAME
#' Date: 02/12/2025
#' Last Updated: 
#---------------------------------------------------------------------#

pacman::p_load(data.table, dplyr, cli, plotrix)
rm(list = ls())

################### #----# ENVIRONMENT SETUP #----# ###################
cat(paste0(" Environment Setup \n"))

#----# Set up directory roots #----#
rm(list=ls())
if (!exists("code_repo"))  {
  code_repo <- unname(ifelse(Sys.info()['sysname'] == "Windows", "ROOT", paste0("ROOT", Sys.info()['user'][1], "ROOT")))
}

## Source functions and paths
bhi_share <- paste0('FILEPATH')
data_path <- paste0(bhi_share, 'FILEPATH')

#---------------------------------------------------------------------#

cat("\n\n")
cat(col_br_green("\t#####################################\n"),
    col_br_green("\t#### BEGIN Analysis file work #######\n"),
    col_br_green("\t#####################################\n\n"))

################### #----# Main #----# ###################

#### ## Read in config ## ####
cat(paste0(" Read in config \n"))
#---------------------------------------------------------------------#

## Assign config file path
config_file_path <- paste0(data_path, 'FILEPATH')

## Read in configuration file
config <- fread(config_file_path)

#### ## Add, update, remove columns ## ####
cat(paste0(" Update config \n"))
#---------------------------------------------------------------------#

for (var in c('adult', 'child', 'all')) {
  
  ## Update parameters
  notes <- paste0('Oct 2: MTUS ', var, ' informal care hours with UN care hours and relative education')
  description <- copy(notes)
  model_index_id <- max(as.numeric(config$model_index_id)) + 1
  
  
  stage_1_model_formula <- 'data ~ cv_un_care + cv_relative_education + (1|level_1/level_2/level_3)'
  ## MEI for this model
  modelable_entity_id <- ID
  
  ## File path to the ST-GPR prepped data
  path_to_data <- paste0(data_path, 'FILEPATH', var, 'FILEPATH')
  
  ## Log transforming data in ST-GPR; provide data in level space
  data_transform <- 'log'
  
  ## Custom offset
  transform_offset <- ''
  
  ## Start and end year of estimates - must match years in custom stage 1 square dataset
  year_start <- 2000
  year_end <- 2026
  
  ## Set currency and currency year or units
  prediction_units <- paste0('hours of ', var, ' informal care per week')
  
  ## GBD ID for number
  metric_id <- 1
  
  ## Age groups, sex IDs, and location IDs to estimate - must match location-sex-age in custom stage 1 square dataset
  prediction_age_group_ids <- '8,9,10,11,12,13,14,15,16,17,18,19,20,30'
  prediction_sex_ids <- '1,2'
  location_set_id <- ID
  
  ## Holdouts
  holdouts <- 0
  
  ## Split up hyperparameters by data density
  density_cutoffs <- ""
  
  ## Hyperparameters for smoothing
  st_lambda <- "0.05" ## temporal
  st_omega <- "1"     ## age
  st_zeta <- "0.001"   ## spatial
  gpr_scale <- "10" 
  gpr_amp_factor <- 2
  
  ## Amplitude calculation method if not default
  amp_method <- ''
  
  rake_logit <- ''
  
  ## If you would like ST-GPR to predict random effects
  predict_re <-  ''
  
  ## GBD related information
  decomp_step <- ''
  release_id <- ID
  gbd_round_id <- ''
  
  ## Number of draws
  gpr_draws <- 250
  
  gbd_covariates <- ''
  
  ## Paths to any square custom covariates or custom stage 1 datasets
  path_to_custom_covariates <- paste0(data_path, 'FILEPATH')
  path_to_custom_stage_1 <- ''
  
  ### Bind row containing above variables to config
  ##--------------------------------------------------
  
  ## Get all current variable names that we want as columns in our config
  vars <- ls()[!ls() %in% c('config', 'config_file_path', 'code_repo', 'bhi_share', 'data_path', 'var')]
  
  ## Get list of variables
  v_list <- mget(vars)
  
  ## Bind all variables together and set names
  new_data <- rbindlist(lapply(v_list, data.table))
  new_data[, names := vars]
  
  ## Get variables wide
  new_data <- data.table(dcast.data.table(new_data, . ~ names, value.var = "V1"))
  new_data[, . := NULL]
  
  ## Bind to config
  config <- rbindlist(list(config, new_data), use.names = T, fill = T)
  rm(new_data, v_list, vars)
  
  ## Write out updated config
  fwrite(config, config_file_path)  
  
}
    
## End of Script ##