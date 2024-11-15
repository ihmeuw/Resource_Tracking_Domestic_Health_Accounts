################################################
#' @author USERNAME
#' 
#' @description Write config file for Malaria government STGPR
################################################
library(data.table)

rm(list = ls())

## *************************************************

version <- "NAME"
notes <- "NOTES"
description <- notes

## *************************************************

## set working directory to write config
setwd("ADDRESS")

data_for_value_codes <- fread('FILEPATH')

for (i in unique(data_for_value_codes$value_code)) {
  
  ##===========================##
  ## Read in or create config  ##
  ##===========================##
  
  ## check for exsiting config and read in if it exists; if not create empty data table
  ## if exists under different name, a new config will be created
  
  if ("FILEPATH" %in% list.files()) {
    config <- fread("FILEPATH")
  } else {
    config <- data.table()
  }
  
  ##====================##
  ## Writing new config ##
  ##====================##
  
  ## many variables for the STGPR config
  
  model_index_id <- ifelse(is.infinite(max(config$model_index_id)), 1, max(config$model_index_id) + 1)
  
  me_name <- paste0("fs_malaria_domestic_public_", tolower(gsub(' ', '_', i)))
  modelable_entity_name <- paste0("Malaria GHE Spending on ", i)
  modelable_entity_id <- 24920
  
  data_transform <- "logit"
  transform_offset <- NA
  
  year_start <- 2000
  year_end <- 2020
  prediction_units <- paste0("Malaria GHES on ", i, " as proportion of GHES")
  
  density_cutoffs <- '6'
  
  st_omega <- "1,1"
  
  if (grepl('Other', i)) {
    st_zeta <- '0.01,0.01'
    st_lambda <- '0.05,0.05'
  } else {
    st_zeta <- '0.005,0.005'
    st_lambda <- '0.05,0.1'
  }
  
  gpr_scale <- "10,10"
  
  gpr_amp_factor <- '2'
  amp_method <- NA
  rake_logit <- NA
  
  predict_re <-  1
  
  decomp_step <- 'iterative'
  path_to_data <- paste0(getwd(), "FILEPATH")
  path_to_custom_stage_1 <- NA
  path_to_custom_covariates <- 'FILEPATH'
  gbd_round_id <- 7
  gpr_draws <- 1000
  
  models <- fread(paste0('FILEPATH'))
  stage_1_model_formula <- models$model[2]
  stage_1_model_formula <- gsub('y  ~  ', 'data  ~  ', stage_1_model_formula)
  
  if (grepl('SSA', i)) {
    stage_1_model_formula <- gsub('\\(1\\|location_name\\)', '(1|level_2/level_3)', stage_1_model_formula)
  } else {
    stage_1_model_formula <- gsub('\\(1\\|location_name\\)', '(1|level_1/level_2/level_3)', stage_1_model_formula)
  }
  
  ##===============================================##
  ## Bind row containing above variables to config ##
  ##===============================================##
  
  ## get all variable names except for config
  vars <- ls()[!(ls() == "config" | ls() == 'data_for_value_codes' | ls() == 'i' | ls() == 'models')]
  
  ## get list of variables
  v_list <- mget(vars)
  
  ## bind all variables together and set names
  new_data <- rbindlist(lapply(v_list, data.table))
  new_data[, names := vars]
  
  ## get variables wide (appropriate format for config)
  new_data <- data.table(dcast.data.table(new_data, . ~ names, value.var = "V1"))
  new_data[, . := NULL]
  
  ## bind to config
  config <- rbindlist(list(config, new_data), use.names = T, fill = T)
  
  ##================================================##
  ## Check for duplicate model_index_id's and write ##
  ##================================================##
  
  if (!any(duplicated(config$model_index_id))) {
    fwrite(config, "FILEPATH")
  } else {
    warning("DUPLICATE MODEL_INDEX_ID'S. Check config.")
  }
  
  rm(new_data, v_list, vars)
  
}
 
## End of Script ##