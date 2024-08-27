################################################################################
# Takes fitted values to make LIS wage 'custom' and 'raw' input data files 
# for ST-GPR

# Author: Elye Bliss
# Date: Aug 28, 2023
################################################################################


rm(list = ls())

library(data.table)


# Load data (output from 03_ST-GPR_stage1_prep)
predictions <- fread("FILEPATH/LIS_age_me_model_estimates_updated_10_27.csv")

################### Making 'custom' covariates input file ######################
pred_custom <- predictions[, c('year_id', 'age_group_id','location_id','sex_id', "pred_LIS")]
setnames(pred_custom, old = "pred_LIS", new = "cv_custom_stage_1")

head(pred_custom)


fwrite(pred_custom,
        'FILEPATH/10_27_23_LIS_age_pred.csv',
        row.names = FALSE)

################### # Making 'raw' covariates input file #######################

raw <- predictions[,c('year_id','age_group_id', 'location_id', 'sex_id','lg_LIS','LIS_est','sample_size')]
# LIS_est is the actual estimate from LIS. This is not to be 
# confused with pred_LIS, which is the predicted value based on the ILO covariate
# from the first round of ST-GPR. 

setnames(raw, old = c('LIS_est','lg_LIS'), new = c('val','cv_lg_LIS'))

# adding missing variables
raw[, measure := "continuous"]
raw[, measure_id := 19]
raw[, nid := 999999]
 
raw[, is_outlier := 0]

raw <- raw[!is.na(val),] # 17793 observations

# # Setting variance
raw[, macro_variance := var(val)]
raw[, variance := macro_variance/sample_size]


fwrite(raw,
       'FILEPATH/10_27_23_LIS_age.csv',
       row.names = FALSE)
