# Takes fitted values to make ILO wage 'custom' and 'raw' input data files 
# for ST-GPR
# Author: Elye Bliss 
# Date: 08/11/2023


rm(list = ls())

library(data.table)



# Load data (output from 03_ST-GPR_stage1_prep)
predictions <- fread("FILEPATH/income_fe_model_estimates_10_27.csv")

# re-code sex_id. 1 if male, 2 if female
predictions[,sex_id := ifelse(sex=='male',1, 2)]

# Making 'custom' covariates input file
pred_custom <- predictions[, c('year_id', 'location_id', "pred_income_GDP",'sex_id')]
setnames(pred_custom, old = "pred_income_GDP", new = "cv_custom_stage_1")

# set NA prediction values to zero: 
# these NA values come from the missing location IDs in THE data file
pred_custom[is.na(cv_custom_stage_1), cv_custom_stage_1 := 0.01]

# Adding age variable
pred_custom[, age_group_id := 22]


head(pred_custom)

fwrite(pred_custom,
       paste0('FILEPATH/08_11_23_income_pred.csv'),
       row.names = FALSE)

# Making 'raw' covariates input file
raw <- predictions[,c("year_id", "location_id", 'lg_gdp_pc',"income",'sex_id')]

setnames(raw, old = 'income', new = 'val')
setnames(raw, old = 'lg_gdp_pc', new = 'cv_log_gdp')

# adding missing variables
raw[, age_group_id := 22]
raw[, measure := "continuous"]
raw[, measure_id := 19]
raw[, nid := 999999]

raw[, is_outlier := 0]
raw[, sample_size := 1000]
head(raw)

raw <-raw[!is.na(val), ]
head(raw) # 2361 observations

# Setting variance
raw[, variance := var(val)]

head(raw)

fwrite(raw,
       'FILEPATH/08_11_23_income_data.csv',
       row.names = FALSE)