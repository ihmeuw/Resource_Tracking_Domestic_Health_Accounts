################################################################################
# Use LIS-on-ILO_ST-GPR model to make fitted values for all location ids
# required by ST-GPR, and for both sexes (our factor variable). Output file
# saved as LIS_fe_model_estimates.csv
# Author: Elye Bliss
# Date: Aug 28, 2023
################################################################################


rm(list = ls())

library(data.table)
library(lme4)

# Load model
model <- readRDS("FILEPATH/LIS_age_stage1_model_updated_10_27.rda")

# Load data
data <- fread(file='FILEPATH/LIS_age_with_ILO_stgpr_updated_10_27.csv')

# Load full ILO data from ST-GPR output as 'covariates'
model_num <- 1
covars <- fread(paste0('FILEPATH/income_mean', model_num, '.csv'))
# Note: because this is ST-GPR output, it should already be square, and have 
# both sex_id values already made. 
covars[,lg_ILO := log(val)]

# For this version of the script, we want disaggregated age groups
sort(unique(data$age_group_id))
# 8  9 10 11 12 13 14 15 16 17 18 19 20 21

# Use expand.grid to make large data frame of covars, for each combination of 
# age group
covars$age_group_id <- NULL
covars[,index := 1:nrow(covars)]
age_group_id <- sort(unique(data$age_group_id))
covars_indices <- expand.grid(covars$index,age_group_id)
names(covars_indices) <- c('index','age_group_id')
covars_new <- merge(covars_indices,covars,by='index',all.x=T)
covars_new$index <- NULL
covars <- covars_new
covars_new <- NULL
setDT(covars)

# Making predictions using lg_ILO as covariate
covars[, lg_LIS := predict(object = model, covars, allow.new.levels = T)]
covars[, pred_LIS := exp(lg_LIS)]

# Adding raw data to predictions
setnames(data,old='N_pilabour',new='sample_size')
raw_data <- data[, c('location_id', 'year_id', 'LIS_est', 'sex_id','age_group_id','sample_size')]

# Adding raw data to predictions
estimates <- merge(covars, raw_data, 
                   by= c('location_id', 'year_id', 'sex_id','age_group_id'), all.x = T )

# Saving file with predictions
fwrite(estimates, "FILEPATH/LIS_age_me_model_estimates_updated_10_27.csv", 
       row.names = F)
