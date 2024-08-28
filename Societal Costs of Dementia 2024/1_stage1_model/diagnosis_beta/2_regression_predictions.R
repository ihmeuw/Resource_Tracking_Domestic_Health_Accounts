# Run regression to make beta predictions for custom stage 1 ST-GPR
# Author: Michael Breshock
# Date: 08/04/2023

# Clear environment
rm(list = ls())

# Loading libraries
library(data.table)
library(dplyr)
library(tidyr)
library(stargazer)
library(ggplot2)

# load clean data with outliers cut: 
data = fread("FILEPATH/02_clean_data_no_outliers.csv")
# load covariate data for making predictions for all countries: 
the = fread("FILEPATH/the_pc_1980_2019.csv")
pop = fread("FILEPATH/population_1990_2019.csv")

######### Create predictions for all data ##########
# log transform the variables of interest: 
data[, ":=" (lg_dx_rate = log(clean_value),
             lg_the_pc = log(the_pc), 
             lg_frac_over65 = log(frac_over65))]

# create model without living condition as covariate: 
all_fit = lm(lg_dx_rate ~ lg_the_pc + lg_frac_over65, data = data)
html_outpath = "FILEPATH"
stargazer(all_fit, type = "html",
          covariate.labels = c("Total Health Expenditure Per-Capita", 
                               "Fraction of Population Over Age 65"), 
          dep.var.labels = "Diagnosis Rate", 
          out = paste0(html_outpath,"diagnosis_stage1_model.html"))

### Making predictions for 204 countries from 1990 to 2019
global_data[, log_pred := predict(all_fit, global_data)]
global_data[, dx_pred := exp(log_pred)]

# plot one country to see example of predictions and compare to other predictions
USA_all <- global_data[location_id == 102]
USA_all[, living_conditions := "Overall"]
USA[living_conditions == "", living_conditions := "Not reported"]

# rbind together overall and living condition split predictions:
USA_combined = rbind(USA_all, USA)
# plot: 
USA_p = ggplot() + 
  geom_line(data = USA_combined, 
            aes(x = year_id, y = dx_pred, color = living_conditions)) + 
  labs(title = "US Beta Stage 1 Predictions by Living Condition", 
       x = "year", y = "Diagnosis Rate")

# plot beta predictions for all countries and save to pdf: 
# combine predictions by living condition and overall predictions: 
global_data[, living_conditions := "Overall"] # adding living condition variable to distinguish from other predictions
global[living_conditions == "", living_conditions := "Not-reported"]
table(global$living_conditions)
all_global = rbind(global_data, global)
# read in location metadata to get country location IDs: 
locs = fread("FILEPATH/location_set_22_metadata.csv")
country_ids = locs[level == 3] # level 3 -> countries, location_id 1 -> global

# plot all countries and save to single pdf: 
pdf("FILEPATH/stage1_predictions_by_living_condition.pdf")
for(country in unique(data$location_id)){
  country_df = all_global[location_id == country]
  p = ggplot(country_df, aes(x = year_id, y = dx_pred, color = living_conditions)) +
    geom_line() + geom_point() + ylim(0,1) +
    labs(x = "Year", y = "Diagnosis Rate",
         title = paste(unique(country_df$location_name), 
                       "Diagnosis Rates 1990-2019 by Living Condition"))
  print(p)
}
dev.off()

# save global overall diagnosis rate predictions
fwrite(global_data, file = "FILEPATH/03_beta_predictions_no_living_cond_09.18.23.csv")
