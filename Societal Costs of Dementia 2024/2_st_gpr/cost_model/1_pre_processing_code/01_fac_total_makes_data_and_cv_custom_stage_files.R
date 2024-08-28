# Takes fitted values and subsets community data to make facility (institutional) 
# 'custom' and 'raw' input data files for ST-GPR
# Author: Elye Bliss with outline from previous pipeline
# Date: 08/04/2023

# Clear environment
rm(list = ls())

# Loading libraries
library(data.table)

# Load data
predictions <- fread("FILEPATH/cost_model_estimates_11_09.csv")

# Making 'custom' covariates input file
pred_custom <- predictions[care_type == 'institutional', c('year_id', 'location_id', "pred_cost_GDP")]
setnames(pred_custom, old = "pred_cost_GDP", new = "cv_custom_stage_1")

# set NA prediction values to zero: 
# these NA values come from the missing location IDs in THE data file
pred_custom[is.na(cv_custom_stage_1), cv_custom_stage_1 := 0.01]

# Adding missing variables
pred_custom[, age_group_id := 22]
pred_custom[, sex_id := 3]

# 4 observations can be de-duplicated after the number of variables is dropped 
# above.
pred_custom <- unique(pred_custom)

fwrite(pred_custom,'FILEPATH/11_09_23_facility_pred.csv',
       row.names = FALSE)


# Making 'raw' covariates input file
cmc_data <- predictions[care_type == "institutional", c("year_id", "location_id", 'lg_gdp_pc',"spending_clean", 'sample_size')]

setnames(cmc_data, old = 'spending_clean', new = 'val')
setnames(cmc_data, old = 'lg_gdp_pc', new = 'cv_log_gdp')

# adding missing variables
cmc_data[, age_group_id := 22]
cmc_data[, sex_id := 3]
cmc_data[, measure := "continuous"]
cmc_data[, measure_id := 19]
cmc_data[, nid := 999999]

cmc_data[, is_outlier := 0]

cmc_data1 <-cmc_data[!is.na(val), ]



# Set the sample size of NA values equal to the mean sample size of reported
# values
mean_sample <- mean(cmc_data1[!is.na(sample_size),]$sample_size)
cmc_data1[,sample_size:= as.numeric(sample_size)] # previously stored as int.
cmc_data1[is.na(sample_size), sample_size := mean_sample]

# Setting variance
cmc_data1[, macro_variance := var(val)]
cmc_data1[, variance := macro_variance/sample_size]


head(cmc_data1) # 19 observations for care_type "institutional" 


fwrite(cmc_data1,
       'FILEPATH/11_09_23_facility_data.csv',
       row.names = FALSE)
