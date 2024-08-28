################################################################################
# Takes fitted values to make gamma model 'custom' and 'raw' input data files 
# for ST-GPR
# Author: Elye Bliss 
# Date: 09/11/2023
################################################################################

rm(list = ls())

library(data.table)


# Load data (output from 03_ST-GPR_stage1_prep)
predictions <- fread("FILEPATH/care_settings_model_estimates.csv")

# Add any necessary variables for ST-GPR
predictions[,sex_id := 3]
predictions[, age_group_id := 22]

# Making 'custom' covariates input file
pred_custom <- predictions[, c('year_id', 'location_id', "gamma_gdp_pc",'sex_id',
                               'age_group_id')]
setnames(pred_custom, old = "gamma_gdp_pc", new = "cv_custom_stage_1")

# Check for NA values
head(pred_custom)
nrow(pred_custom[is.na(cv_custom_stage_1)])
#[1] 26580
# these NA values come from the missing location IDs in THE data file
pred_custom[is.na(cv_custom_stage_1), cv_custom_stage_1 := 0.01]

fwrite(pred_custom,
       'FILEPATH/12_11_23_gamma_pred.csv',
       row.names = FALSE)

# Making 'raw' covariates input file
raw <- fread"FILEPATH/01_outliered_11_08.csv")

# Add any necessary variables for ST-GPR
raw[,sex_id := 3]
raw[, age_group_id := 22]

raw <- raw[,c("year_id", "location_id", 'lg_gdp_pc',"gamma",'sex_id',
                      'age_group_id','sample_size')]

setnames(raw, old = 'gamma', new = 'val')
setnames(raw, old = 'lg_gdp_pc', new = 'cv_lg_gdp_pc')

# adding missing variables
raw[, measure := "continuous"]
raw[, measure_id := 19]
raw[, nid := 999999]

raw[, is_outlier := 0]

# Set the sample size of NA values equal to the mean sample size of reported
# values
mean_sample <- mean(raw[!is.na(sample_size),]$sample_size)
raw[,sample_size:= as.numeric(sample_size)] # previously stored as int.
raw[is.na(sample_size), sample_size := mean_sample]

# Setting variance (formula for rates between 0 and 1)
raw[, variance := val*(1-val)/sample_size]

head(raw)

nrow(raw) # 94 observations


fwrite(raw,'FILEPATH/12_11_23_gamma_data.csv',
       row.names = FALSE)
