#################################################################################
#' @author USERNAME
#' 
#' @description run aic/bic tests to select best models for cross-validation for Malaria government
#################################################################################

rm(list = ls())

## source location function
source("FILEPATH")
source('FILEPATH')

## get location set
location <- get_location_metadata(gbd_round_id = 7, location_set_id = 22)
loc <- location[level == 3, .(ihme_loc_id, location_id)]

## source logit_trans function
cds_repo <- "ADDRESS"
source(paste0(cds_repo, "FILEPATH"))
source(paste0(cds_repo, "FILEPATH"))

#=========================================================================================================
#                                       prep data for model input
#=========================================================================================================

# set working directory
setwd('ADDRESS')

## read in covariates
covariates <- fread('FILEPATH')

## select national only
covariates <- covariates[location_id %in% loc$location_id]

## read in out-of-pocket estimates
data <- fread("FILEPATH")

## merge on covariates
data <- merge(data, covariates, by = c('location_id', 'year_id'))


##=====================##
## run function        ##
##=====================##

## set function arguments
out_dir <- "ADDRESS"
cv_names <- colnames(covariates)[!grepl("_id|_name|ssa", colnames(covariates))]
random_effects <- c("location_name")
package_lib <- "ADDRESS"

library(doBy)
  
## run function
code <- unique(data$value_code)
system.time(aic_bic_covariate_selection(data[value_code == code], 
                                          covariates = cv_names, 
                                          package_lib = package_lib, 
                                          random_effects_vars = random_effects, 
                                          output_dir = paste0('ADDRESS')))


## End of Script ##