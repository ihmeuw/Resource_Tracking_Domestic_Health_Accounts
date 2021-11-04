## #############################################################################
## Author: 
## Project: Immunization financing
## Purpose: Merge data with candidate covariates and do AIC BIC calculations on
##          possible models
##
## Inputs: 01_ghes_data.csv, 02a_prep_covariates.csv, parameters(gbd round,
##         years of interest, etc)
##
## Outputs: aic_bic_best_DATE.RData, data_DATE.RData, model_list_DATE.RData
##          
##          
## Most Recent Update: 11/03/2021
## #############################################################################
pacman::p_load(data.table, dplyr, feather, reshape2, ggplot2, readstata13, gridExtra, RColorBrewer, openxlsx, grid, scales, gtools, doBy)

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

## source location function
source("FILEPATH/get_location_metadata.R")

## parameters
date <- format(Sys.Date(),'%m%d%y')
years <- 2000:2017
location_set_id <- 22
gbd_round_id <- 6
data_folder <- "FILEPATH"

## get location set
location <- get_location_metadata(gbd_round_id = gbd_round_id, location_set_id = location_set_id)
loc <- location[level == 3, .(ihme_loc_id, location_id)]

## source logit_trans function and AIC BIC functions
cds_repo <- "FILEPATH"
source(paste0(cds_repo, "data_tools.R"))
source(paste0(cds_repo, "aic_bic_covariate_selection.R"))

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## set workind directory
setwd(data_folder)

## read in prepped covariates and standardized data
covariates <- fread("02_covariates/02a_prep_covariates/02a_prep_covariates.csv")
data <- fread("01_standardized_data/01b_ghes_data.csv")

## offset 0 values for model selection and logit where applicable
offset_value <- median(data$proportion)*0.05  #offset 5% of the median
data[proportion == 0, proportion := offset_value]
data[, y := logit(proportion)]

## merge covariates onto data set
data <- merge(data, covariates, by = c('location_id', 'year_id', 'ihme_loc_id', 'location_name'))

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## set function arguments
# functions to loop through
components <- c('vaccine', 'routine', 'delivery', 'supplementary', 'immunization')

# applicable covariate names
cv_names <- colnames(covariates)[!(colnames(covariates) %in% c('location_id', 'year_id', 'ihme_loc_id', 'location_name', 'super_region_name', 'region_name'))]

# assign random effects variables
random_effects <- c('location_name', 'region_name', 'super_region_name')
package_lib <- 'FILEPATH'

## loop through immunization components to test all possible models for each immunization component
for (i in components) {
  
  # subset to applicable data for each immunization component
  input_data <- data[component == i]
  
  # aic bic calculations for each model
  aic_bic_covariate_selection(input_data,
                              covariates = cv_names,
                              package_lib = package_lib,
                              random_effects_vars = random_effects,
                              output_dir = paste0(data_folder, 'FILEPATH/', i, '/'))
}

## will automatically write out files to specified output_dir
