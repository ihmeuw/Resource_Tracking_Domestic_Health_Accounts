# Makes raw and custom stage 1 data files for gender ST-GPR input 
# Author: Michael Breshock
# Date: 09/29/2023

# Clear environment
rm(list = ls())

# Loading libraries
library(data.table)


# load predictions made in:
# /1_stage1_model/caregiving_model/2_regression_predictions.R
pred = fread("FILEPATH/03_gender_predictions_04.18.24.csv")

## make custom stage 1 file: 
# select just the columns we need for st-gpr
pred_cols = pred[,.(year_id, location_id, pct_female_pred, sex_id, age_group_id)]

setnames(pred_cols, old = "pct_female_pred", new = "cv_custom_stage_1")

# save out custom stage1 predictions file to st-gpr directory: 
fwrite(pred_cols, file = "FILEPATH/04.18.24.gender_predictions.csv")

## Make raw data input file: 
clean_gender = fread("FILEPATH/02_clean_data_no_outliers_04.18.24.csv")
# this file was made in: 
# /1_stage1_model/gender_model/1_cut_outliers.R

# select variables needed for st-gpr: 
gender_raw = clean_gender[,.(year_id, location_id, percent_female_cg, 
                             sample_size, lg_frac_over65,
                             lg_pct_fem_pop, age_group_id, sex_id)]
# change value name to match with what ST-GPR expects: 
setnames(gender_raw, old = c("percent_female_cg", "lg_frac_over65", 
                             "lg_pct_fem_pop"), 
         new = c("val", "cv_lg_frac_over65", "cv_lg_pct_fem_pop"))

# adding variables for st-gpr input: 
gender_raw[, ":=" (measure = "continuous",
                   measure_id = 19, 
                   nid = 999999,
                   is_outlier = 0)]

# calculating variance by sample size: 
gender_raw[, variance := val*(1-val)/sample_size]

# save out raw caregiving st_gpr input data: 
fwrite(gender_raw, "FILEPATH/04.18.24.gender_data.csv")

