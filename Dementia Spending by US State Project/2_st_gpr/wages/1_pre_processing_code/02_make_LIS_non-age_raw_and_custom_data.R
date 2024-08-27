################################################################################
# Takes fitted values to make LIS wage 'custom' and 'raw' input data files 
# for ST-GPR

# Author: Elye Bliss
# Date: Aug 21, 2023
################################################################################


rm(list = ls())

library(data.table)


# Load data 
predictions <- fread("FILEPATH/LIS_fe_model_estimates_updated_10_31.csv")

################### Making 'custom' covariates input file ######################
pred_custom <- predictions[, c('year_id', 'age_group_id','location_id','sex_id', "pred_LIS")]
setnames(pred_custom, old = "pred_LIS", new = "cv_custom_stage_1")

head(pred_custom)


fwrite(pred_custom,
        'FILEPATH/10_31_23_LIS_pred.csv',
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

raw <- raw[!is.na(val),]

# # Setting variance
raw[, macro_variance := var(val)]
raw[, variance := macro_variance/sample_size]

# LIS estimates contain sample sizes with 0 missing values
fwrite(raw,
       'FILEPATH/10_31_23_LIS.csv',
       row.names = FALSE)
