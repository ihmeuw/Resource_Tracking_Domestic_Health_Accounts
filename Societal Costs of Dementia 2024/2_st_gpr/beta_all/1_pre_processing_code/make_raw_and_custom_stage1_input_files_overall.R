# Makes raw and custom stage 1 data files for overall ST-GPR input 
# this model does not consider the living conditions / care settings
# Author: Michael Breshock
# Date: 08/04/2023

# Clear environment
rm(list = ls())

# Loading libraries
library(data.table)


# load predictions made in:
# 1_stage1_model/diagnosis_beta/2_regression_predictions.R
pred = fread("FILEPATH/03_beta_predictions_no_living_cond_09.18.23.csv")

# Make custom covariates file: 
# not reported living condition: 
pred_all = pred[,.(year_id, location_id, dx_pred, sex_id, age_group_id)] # select just the columns we need for st-gpr
                                          
setnames(pred_all, old = "dx_pred", new = "cv_custom_stage_1")
# set NA prediction values to 0.01 (not zero to avoid logging issues): 
# these NA values come from the missing location IDs in THE data file
pred_all[is.na(cv_custom_stage_1), cv_custom_stage_1 := 0.00001]

# save out custom covariates file to st-gpr directory: 
fwrite(pred_all, file = "FILEPATH/09.18.23.beta_all_predictions.csv")

# Make raw data file: 
clean_dx = fread("FILEPATH/02_clean_data_no_outliers.csv")
# this file was made in: 
# /1_stage1_model/diagnosis_beta/1_cut_outliers.R

# filter to not reported living condition and select variables of interest
beta_raw = clean_dx[, .(year_id, location_id, clean_value, sample_size)]

# change value name to match with what ST-GPR expects: 
setnames(beta_raw, old = "clean_value", new = "val")

# merge in covariates: 
beta_covs = pred[, .(year_id, location_id, lg_the_pc, lg_frac_over65, 
                     age_group_id, sex_id)]
setnames(beta_covs, old = c("lg_the_pc","lg_frac_over65"), 
         new = c("cv_log_the_pc","cv_log_frac_over65"))

# bring all together: 
beta_all = merge(beta_raw, beta_covs, by = c("year_id","location_id"))
# adding variables for st-gpr input: 
beta_all[, ":=" (measure = "continuous",
                 measure_id = 19, 
                 nid = 999999,
                 is_outlier = 0)]

# fill missing sample sizes with mean:
mean_n = round(mean(beta_all$sample_size, na.rm = T))
beta_all[is.na(sample_size), sample_size := mean_n]

# calculate variance: 
beta_all[, variance := val*(1-val)/sample_size]

# save out raw beta st_gpr input data: 
fwrite(beta_all, file = "FILEPATH/09.18.23.beta_all_data.csv")
