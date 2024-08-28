################################################################################
# Use LIS-on-ILO_ST-GPR model to make fitted values for all location ids
# required by ST-GPR, and for both sexes (our factor variable). Output file
# saved as LIS_fe_model_estimates_updated_10_31.csv

# Author: Elye Bliss
# Date: Aug 21, 2023
################################################################################

rm(list = ls())

library(data.table)

# Load model
model <- readRDS("FILEPATH/LIS_stage1_model_updated_10_31.rda")

# Load data
data <- fread(file='FILEPATH/LIS_with_ILO_stgpr_updated_10_31.csv')

# Load full ILO data from ST-GPR output as 'covariates'
model_num <- 1
covars <- fread(paste0('FILEPATH/income_mean', model_num, '.csv'))
# Note: because this is ST-GPR output, it should already be square, and have 
# both sex_id values already made. 
covars[,lg_ILO := log(val)]

# Making predictions using lg_ILO as covariate
covars[, lg_LIS := predict(object = model, covars, allow.new.levels = T)]
covars[, pred_LIS := exp(lg_LIS)]

# Adding raw data to predictions
raw_data <- data[, c('location_id', 'year_id', 'LIS_est', 'sex_id','sample_size')]
# Replacing sample_size with dummy of 1000 instead of total population
raw_data[,sample_size := 1000]

# Adding raw data to predictions
estimates <- merge(covars, raw_data, 
                   by= c('location_id', 'year_id', 'sex_id'), all.x = T )

# Saving file with predictions
fwrite(estimates, 
       "FILEPATH/LIS_fe_model_estimates_updated_10_31.csv", 
       row.names = F)
