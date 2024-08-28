# Outlier Identification for Caregiving Gender Extractions
# Author: Michael Breshock
# Date: 09/29/2023

# Clear environment
rm(list = ls())

# Loading libraries
library(data.table)
library(ggplot2)
library(stargazer)

# load clean data: 
data = fread("FILEPATH/01_clean_data_covars.csv")

# log transform the variables of interest: 
data[, ":=" (lg_pct_fm = log(percent_female_cg),
             lg_frac_over65 = log(frac_over65),
             lg_pct_fem_pop = log(pct_fem_pop))]

# create model: 
gender_model = lm(lg_pct_fm ~ lg_frac_over65 + lg_pct_fem_pop, data = data)
stargazer(gender_model, type = "text")

# calculate cooks distance on gender model: 
data[, cooksd := cooks.distance(gender_model)]
data_incl = data[cooksd < 4/nrow(data)]
data_cut = data[cooksd > 4/nrow(data)] # 3 observations cut with cook's distance

# save final input data without outliers:
fwrite(data_incl, file = "FILEPATH/02_clean_data_no_outliers_04.18.24.csv")
# save outlier data that was cut: 
fwrite(data_cut, file = "FILEPATH/cut_outliers_cooksdistance_04.18.24.csv")
