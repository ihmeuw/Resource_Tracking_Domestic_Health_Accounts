# Run regression to make caregiving predictions for custom stage 1 ST-GPR
# Author: Michael Breshock
# Date: 09/12/2023

# Clear environment
rm(list = ls())

# Loading libraries
library(data.table)
library(ggplot2)
library(stargazer)
library(lme4)

# load clean data with outliers cut: 
data = fread("FILEPATH/02_clean_data_no_outliers_04.11.24.csv")

# load covariate data for making predictions for all countries: 
sdi = fread("FILEPATH/sdi_release16_1990_2024.csv")

setnames(sdi, old = "mean_value", new = "sdi")

global_data = sdi[,.(year_id, location_id, location_name, 
                           age_group_id, sex_id, sdi)]


## create model
# this is the best model picked by BMS
# model selection done in this script:
# ~/1_stage1_model/caregiving_model/0_model_selection.R
cg_model = lm(logit_hours ~ sdi + severity + care_type, data = data)
html_outpath = "FILEPATH"
stargazer(cg_model, type = "html",
          covariate.labels = c("Socio-Demographic Index", 
                               "Dementia Severity: Mild", 
                               "Dementia Severity: Mild-to-Moderate",
                               "Dementia Severity: Moderate",
                               "Dementia Severity: Moderate-to-Severe",
                               "Care Type: ADL", 
                               "Care Type: ADL+IADL"), 
          dep.var.labels = "Caregiving Hours per Week", 
          out = paste0(html_outpath,"RTR_cg_hours_stage1_model.html"))

stargazer(cg_model, type = "text")


# add severity and care_type variables to global data
# need these to make predictions since they are covariates in the model
# setting both to blank since we want to make predictions for 
# overall severity and all care types 
global_data[, ":=" (severity = "", 
                    care_type = "")]

# run predictions for all locations globally: 
global_data[, logit_pred := predict(cg_model, global_data)]

# view range of predictions in logit space: 
range(global_data$logit_pred) # -0.8930883 - 2.9786498
# vs range of input data: 
range(data[severity == "" & care_type == ""]$logit_hours) 
# -2.541232 - 5.123964

# view range of predictions after inverse logit transforming: 
inverse_scaled_logit <- function(y,a,b){
  # Function to reverse scaled_logit transformation
  result <- ((b-a)*exp(y))/(1+exp(y))+a
  return(result) 
}
global_data[, hours_pred := inverse_scaled_logit(logit_pred, 0, 169)]
range(global_data$hours_pred) # 49.08992 - 160.82044
# vs range of input data:
range(data[severity == "" & care_type == ""]$weekly_hours) 
# 12.34 - 168.00

# plot one country to see example of predictions
USA <- global_data[location_id == 102]
ggplot() + 
  geom_line(data = USA, aes(x = year_id, y = hours_pred)) + ylim(0,168)

# save out global caregiving predictions: 
fwrite(global_data, file = "FILEPATH/03_cg_predictions_04.17.24.csv")
