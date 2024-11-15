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

##===========================##
## Read in or create config  ##
##===========================##

## check for exsiting config and read in if it exists; if not create empty data table

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

me_name <- "fs_malaria_domestic_public"
modelable_entity_name <- "Malaria GHE Spending"
modelable_entity_id <- 25013

data_transform <- "logit"
transform_offset <- NA

year_start <- 2000
year_end <- 2020
prediction_units <-"Malaria GHES as proportion of GHES"

density_cutoffs <- "7,12,16"

st_lambda <- "0.05,0.12,0.17,0.27"
st_omega <- "1,1,1,1"
st_zeta <- "0.003,0.003,0.001,0.001"

gpr_scale <- "10,10,5,5"

gpr_amp_factor <- 2
amp_method <- NA
rake_logit <- NA

predict_re <-  1

decomp_step <- 'iterative'
path_to_data <- paste0(getwd(), "FILEPATH")
path_to_custom_stage_1 <- paste0(getwd(), "FILEPATH")
gbd_round_id <- 7
gpr_draws <- 1000

##===============================================##
## Bind row containing above variables to config ##
##===============================================##

## get all variable names except for config
vars <- ls()[!ls() == "config"]

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
## Check for duplciate model_index_id's and write ##
##================================================##

if (!any(duplicated(config$model_index_id))) {
  fwrite(config, "FILEPATH")
} else {
  warning("DUPLICATE MODEL_INDEX_ID'S. Check config.")
}

## End of Script ##