################################################################################
## Bayesian Model Selection and Regression for attributable fraction models
## 2025 09 25
## Author: USERNAME
################################################################################
# Setting up environment

rm(list = ls())

# Set up directory roots 
if (!exists("code_repo"))  {
  code_repo <- unname(ifelse(Sys.info()['sysname'] == "Windows", "ROOT", paste0("ROOT", Sys.info()['user'][1], "ROOT")))
}

# specify directory roots
if (Sys.info()["sysname"] == "Linux") {
  j_root <- "ROOT" 
  h_root <- "ROOT"
  l_root <- "ROOT"
  functions_dir <- "FILEPATH"
} else { 
  j_root <- "ROOT"
  h_root <- "ROOT"
  l_root <- "ROOT"
  functions_dir <- "FILEPATH"
}

# load in libraries
library(ggplot2)
library(dplyr)
library(stringr)
library(data.table)
library(tidyr)
library(Amelia)
library(RColorBrewer)
library(MASS)
library(BAS, lib.loc = code_repo)

# -----------------------------------------------------------------------------
# READ IN DATA
NHATS_raw <- fread(paste0(j_root, "FILEPATH"))
SP_hours = fread(paste0(j_root, "FILEPATH"))
dw_table <- fread("FILEPATH")
output_dir <- "FILEPATH"

# -----------------------------------------------------------------------------
# recode values raw columns: 
NHATS_raw[, ':=' (
  heart_disease = disescn2 == 1, # has heart disease
  hbp = disescn3 == 1, # has high blood pressure
  arthritis = disescn4 == 1, # has arthritis
  diabetes = disescn6 == 1, # has diabetes
  lung_disease = disescn7 == 1, # has lung disease
  stroke = disescn8 == 1, # had stroke
  cancer = disescn10 == 1, # has cancer
  has_dementia = dementia == 1 # has dementia
)]

# using cut off points from Kroenke et al 2009: An Ultra-Brief Screening Scale for Anxiety and Depression: The PHQ–4
# recoding PHQ-4 questions to appropriate scores 
# 1 not at all = 0
# 2 several days = 1
# 3 more than half the days = 2
# 4 nearly every day = 3
NHATS_raw[, ':=' (dep1 = depresan1 - 1, # little interest or pleasure
                  dep2 = depresan2 - 1, # down depressed hopeless
                  anx1 = depresan3 - 1, # nervous anxious
                  anx2 = depresan4 - 1  # unable to stop worry
)]

# calculating PHQ-2 and GAD-2 scores
NHATS_raw[, ':=' (PHQ = dep1 + dep2, # PHQ-2 score = sum of depression scores
                  GAD = anx1 + anx2)] # GAD-2 score = sum of anxiety scores

# classifying depression and anxiety
NHATS_raw[, ':=' (depression = PHQ >= 3, # PHQ cut-point = 3,
                  anxiety = GAD >= 3)] # GAD cut-point = 3, both from Kroenke 2009

################### Bayesian Model Selection ###################

# merge in care hours with demographic data
model_dt <- merge(NHATS_raw, SP_hours, by = c("year", "spid", "age", "dementia"))

# make new column of log of care hours
model_dt[, log_care_hours_week := log(care_hours_week)]

# create list of dependent variable and covariates being assessed 
dependent_var = "log_care_hours_week"

#rhs_var <- "has_dementia" # change based on which disorder is being assessed

condition_list <- c("cancer","stroke","depression","anxiety","has_dementia")

#create empty data.table for final results
final_af_dt <- data.table()

# loop over all conditions
for(rhs_var in condition_list){
  print(paste0("Running BMS for condition: ", rhs_var))
  
  # create list of control variables
  control_list = c("cancer",
                   "stroke",
                   "depression",
                   "anxiety",
                   "has_dementia",
                   "heart_disease",
                   "hbp",
                   "arthritis",
                   "diabetes",
                   "lung_disease") # add all other causes
  
  control_list <- control_list[!control_list %in% rhs_var]
  
  bms_list <- c(dependent_var, control_list)
  bms_data <- model_dt[get(rhs_var) == TRUE & care_hours_week > 0, ..bms_list]
  
  
  # create RHS formula
  controls <- paste(control_list, collapse = " + ")
  
  # create model formula
  model_string <- as.formula(paste(dependent_var, '~', controls))
  
  # run BMS on raw data
  model_bms <- bas.lm(model_string,
                      data = bms_data,
                      prior = "JZS", # try JZS instead
                      modelprior = uniform())
  
  # save BMS plots
  mip_plot <- plot(model_bms, which = 4)
  
  #bms results
  bms_results <- data.frame(name = model_bms$namesx, prob = model_bms$probne0)
  
  #get a list of names from bms_results where prob is greater than 0.5
  significant_vars <- bms_results$name[bms_results$prob > 0.5]
  
  # remove the string "TRUE" from the end of the variable names
  significant_vars <- gsub("TRUE", "", significant_vars)
  
  # cut "Intercept" from list
  significant_vars <- significant_vars[significant_vars != "Intercept"]
  
  #########################################################################################
  # LOGIT MODEL
  lm_data <- copy(bms_data)
  
  # create model formula
  # Based on MIP plot, which controls are significant in BMS model
  lm_control_list <- significant_vars
  
  # create RHS formula
  lm_controls <- paste(lm_control_list, collapse = " + ")
  
  lm_string <- as.formula(paste(dependent_var, '~', lm_controls))
  
  # run ols
  lm1 <- lm(lm_string,
                   data = lm_data)
  
  summary(lm1)
  
  #########################################################################################
  # Rest of AF calculation should be automated
  # storing coef vals
  temp_lm_coef <- coef(lm1)
  
  # Get the names of the coefficients
  var_names <- names(temp_lm_coef)
  
  # Create the data.table
  coef_dt <- data.table(
    Variable = var_names,
    Coefficient = temp_lm_coef
  )
  
  # adding intercept disorder to intercept row
  coef_dt[Variable == "(Intercept)", Variable := paste0("Intercept_", rhs_var)]
  
  
  # get predicted values from lm1
  pred_vals <- predict(lm1, newdata = lm_data)
  # get error terms from predicted values
  error_terms <- lm_data$log_care_hours_week - pred_vals
  
  # create a new data.table with predicted values and error terms
  pred_dt <- data.table(predicted = pred_vals, error = error_terms)
  intercept <- as.numeric(coef_dt[,2][1]) #constant intercept value
  pred_dt[, numerator := exp(intercept + error)]
  
  # get the column 'log_care_hours_week' from 'lm_data' and put it into pred_dt
  pred_dt[, denominator := exp(lm_data$log_care_hours_week)]
  pred_dt[, ratio := numerator / denominator]
  
  #########################################################################################
  
  af <- mean(pred_dt$ratio, na.rm = TRUE)
  
  # add af to a data.table
  af_dt_temp <- data.table(condition = rhs_var, AF_value = af)
  final_af_dt <- rbind(final_af_dt, af_dt_temp)
}
  
View(final_af_dt)

#########################################################################################
# CORRELATION BETWEEN AF VALUES AND DISABILITY WEIGHTS
# rename columns to match final af dt
setnames(dw_table, old = c("Cause", "Disability Weight"), new = c("condition", "dw_value"))

# renaming condition names to match dw table
final_af_dt[condition == "has_dementia", condition := "Alzheimer's disease and other dementias"]
final_af_dt[condition == "depression", condition := "Depressive disorders"]
final_af_dt[condition == "anxiety", condition := "Anxiety disorders"]
final_af_dt[condition == "cancer", condition := "Brain and central nervous system cancer"]
final_af_dt[condition == "stroke", condition := "Stroke"]

# creating correlation dt
corr_dt <- merge(final_af_dt, dw_table, by = "condition", all.x = TRUE)

# correlation value
correlation <- cor(corr_dt$AF_value, corr_dt$dw_value, use = "complete.obs")

# adding correlation value to corr dt
corr_dt[, corr_val := correlation]

#########################################################################################
# PREDICTING AF VALUES BASED ON DW VALUES
af_pred_int_dt <- copy(dw_table)
af_pred_no_int_dt <- copy(dw_table)


# removing conditions that we already have AF values for
af_pred_int_dt <- af_pred_int_dt[!condition %in% corr_dt$condition]
af_pred_no_int_dt <- af_pred_no_int_dt[!condition %in% corr_dt$condition]

# removing correlation value from dt
af_dt <- corr_dt[, corr_val := NULL]

# logit transforming Af values
af_dt[, logit_AF_value := log(AF_value / (1 - AF_value))]

# run linear regression
# with intercept 
lm_af_int_dw <- lm(logit_AF_value ~ dw_value, 
                    data = af_dt)

summary(lm_af_int_dw)

# without intercept
lm_af_no_int_dw <- lm(logit_AF_value ~ 0 + dw_value, data = af_dt)

summary(lm_af_no_int_dw)

# Predict af values from model
af_pred_int_dt[, logit_AF_value := predict(lm_af_int_dw, newdata = af_pred_int_dt)]
af_pred_no_int_dt[, logit_AF_value := predict(lm_af_no_int_dw, newdata = af_pred_no_int_dt)]

# Reverse logit transforming predicted values
af_pred_int_dt[, AF_value := exp(logit_AF_value) / (1 + exp(logit_AF_value))]
af_pred_no_int_dt[, AF_value := exp(logit_AF_value) / (1 + exp(logit_AF_value))]

# rbind af_pred_dt with af_dt
int_final_dt <- rbind(af_pred_int_dt, af_dt, fill = TRUE)
no_int_final_dt <- rbind(af_pred_no_int_dt, af_dt, fill = TRUE)


# reordering final dt based on condition name
int_final_dt <- int_final_dt[order(condition)]
no_int_final_dt <- no_int_final_dt[order(condition)]

View(int_final_dt)
View(no_int_final_dt)

# Removing logit af value column
int_final_dt[, logit_AF_value := NULL]
no_int_final_dt[, logit_AF_value := NULL]

# Saving out dts
fwrite(int_final_dt, file = paste0(output_dir, "FILEPATH"))
fwrite(no_int_final_dt, file = paste0(output_dir, "FILEPATH"))
