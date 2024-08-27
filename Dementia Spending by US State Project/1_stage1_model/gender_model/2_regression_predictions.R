# Run regression to make caregiver gender predictions for custom stage 1 ST-GPR
# Author: Michael Breshock
# Date: 09/29/2023

# Clear environment
rm(list = ls())

# Loading libraries
library(data.table)
library(ggplot2)
library(stargazer)

# load clean data with outliers cut: 
data = fread("FILEPATH/02_clean_data_no_outliers_04.18.24.csv")

# create caregiver gender model: 
gender_model = lm(lg_pct_fm ~ lg_frac_over65 + lg_pct_fem_pop, data = data)
html_outpath = "FILEPATH/"
covar_labels = c("Fraction of Population Over Age 65", 
                 "Percent Female Population")
dep_label = "Percent Female"
stargazer(gender_model, type = "html",
          covariate.labels = covar_labels,
          dep.var.labels = dep_label, 
          out = paste0(html_outpath,"RTR_gender_stage1_model.html"))

# load covariate data for making predictions for all countries: 
pop = fread("FILEPATH/populations_release16_1990_2024.csv")

# create global covariate dataframe: 
global_data = pop[,.(year_id, location_id, age_group_id, sex_id, 
                           frac_over65, pct_fem_pop)]

# log transform the variables of interest: 
global_data[, ":=" (lg_frac_over65 = log(frac_over65),
                    lg_pct_fem_pop = log(pct_fem_pop))]

# make predictions for all countries from 1990-2019
global_data[, log_pred := predict(gender_model, global_data)]
# view range of predictions before capping at 168: 
range(exp(global_data$log_pred), na.rm = T) # 0.07225257 - 1.35397273
# exponentiate and cap at max percent female in raw data: 
max(data$percent_female_cg) # 0.829 -> round this up to 0.85
global_data[, pct_female_pred := pmin(exp(log_pred), 0.85)]
# set minimum percent female rate: 
min(data$percent_female_cg) # 0.523 -> round this down to 0.5
global_data[, pct_female_pred := pmax(pct_female_pred, 0.5)]
# plot one country to see example of predictions
USA <- global_data[location_id == 102]
ggplot() + 
  geom_line(data = USA, aes(x = year_id, y = pct_female_pred)) + ylim(0,1)

# save out global caregiving predictions: 
fwrite(global_data, "FILEPATH/03_gender_predictions_04.18.24.csv")
