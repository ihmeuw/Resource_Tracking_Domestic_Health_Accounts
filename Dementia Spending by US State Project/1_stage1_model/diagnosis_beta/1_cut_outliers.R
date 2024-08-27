# Cut outlier data from diagnosis extractions
# Author: Michael Breshock
# Date: 08/03/2023

# Clear environment
rm(list = ls())

# Loading libraries
library(data.table)
library(stargazer)
library(influence.ME, lib.loc = "FILEPATH")

# load clean data: 
data = fread("FILEPATH/01_clean_data_covars.csv")

# filter to just diagnosis rate data and remove severity splits: 
# this already gets done in 00_clean_and_rbind_data.R, but filtering here as a fail safe
data = data[clean_units == "dx_over_dem_cases" & is.na(severity)]

####################### FIND OUTLIERS WITH COOKS DISTANCE ######################

# log the variables being modeled:
data[, ":=" (lg_dx_rate = log(clean_value),
             lg_the_pc = log(the_pc),
             lg_frac_over65 = log(frac_over65))]

# convert living_conditions to factor:
data[, living_conditions := as.factor(living_conditions)]

# create model:
fit = lm(lg_dx_rate ~ lg_the_pc + lg_frac_over65 + living_conditions,
         data = data)
stargazer(fit, type = "text")

# plot residuals: 
plot(fitted(fit), resid(fit)) + abline(0,0)

# cut outliers with cook's distance
data[, cooksd := cooks.distance(fit)]
data_incl = data[cooksd < 4/nrow(data)]
data_cut = data[cooksd >= 4/nrow(data)] # 4 obs cut

# save final input data without outliers:
fwrite(data_incl, file = "FILEPATH/02_clean_data_no_outliers.csv")

# save outlier data that was cut:
fwrite(data_cut, file = "FILEPATH/cut_outliers_cooksdistance_fixed.csv")
