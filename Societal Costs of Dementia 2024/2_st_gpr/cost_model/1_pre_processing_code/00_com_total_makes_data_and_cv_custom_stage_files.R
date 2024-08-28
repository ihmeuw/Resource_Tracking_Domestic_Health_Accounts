# Takes fitted values and subsets community data to make community 'custom' and 
# 'raw' input data files for ST-GPR
# Author: Elye Bliss with outline from 
# https://stash.ihme.washington.edu/projects/FRH/repos/alzheimers/browse/Pipeline/2_st_gpr/3_total_cost_debugging/community/pre_processing_code/com_total_makes_data_and_cv_custom_stage_files.R
# Date: 08/04/2023

# Clear environment
rm(list = ls())

# Loading libraries
library(data.table)


# Load data (output from 01_ST-GPR_stage1_prep)
predictions <- fread("FILEPATH/cost_model_estimates_11_09.csv")

# Making 'custom' covariates input file
pred_custom <- predictions[care_type == 'community', c('year_id', 'location_id', "pred_cost_GDP")]
setnames(pred_custom, old = "pred_cost_GDP", new = "cv_custom_stage_1")

# set NA prediction values to zero: 
# these NA values come from the missing location IDs in THE data file
pred_custom[is.na(cv_custom_stage_1), cv_custom_stage_1 := 0.01]

# Adding age and sex variables
pred_custom[, age_group_id := 22]
pred_custom[, sex_id := 3]

# Which rows get duplicated?
pred_custom_dups <- pred_custom[duplicated(pred_custom)]
pred_custom_dups <- merge(pred_custom_dups,predictions,by=c('year_id', 'location_id'),all.x=T)
pred_custom_dups <- unique(pred_custom_dups)
pred_custom <- unique(pred_custom)

head(pred_custom)

fwrite(pred_custom,'FILEPATH/11_09_23_community_pred.csv',
       row.names = FALSE)


# Making 'raw' covariates input file
cmc_data <- predictions[care_type == "community", c("year_id", "location_id", 'lg_gdp_pc',"spending_clean", 'sample_size')]

setnames(cmc_data, old = 'spending_clean', new = 'val')
setnames(cmc_data, old = 'lg_gdp_pc', new = 'cv_log_gdp')

# adding missing variables
cmc_data[, age_group_id := 22]
cmc_data[, sex_id := 3]
cmc_data[, measure := "continuous"]
cmc_data[, measure_id := 19]
cmc_data[, nid := 999999]


cmc_data_raw <-cmc_data[!is.na(val), ]



# Set the sample size of NA values equal to the mean sample size of reported
# values
mean_sample <- mean(cmc_data_raw[!is.na(sample_size),]$sample_size)
cmc_data_raw[,sample_size:= as.numeric(sample_size)] # previously stored as int.
cmc_data_raw[is.na(sample_size), sample_size := mean_sample]

# Setting variance
cmc_data_raw[, macro_variance := var(val)]
cmc_data_raw[, variance := macro_variance/sample_size]

cmc_data_raw[, is_outlier := 0]


nrow(cmc_data_raw) # 48 observations for care_type "community" 


fwrite(cmc_data_raw,
       'FILEPATH/11_09_23_community_data.csv',
       row.names = FALSE)
