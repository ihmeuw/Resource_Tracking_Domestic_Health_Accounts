# Outlier Identification for Caregiving Extractions
# Author: Michael Breshock
# Date: 09/11/2023

# Clear environment
rm(list = ls())

# Loading libraries
library(data.table)
library(dplyr)
library(ggplot2)
library(stargazer)
library(influence.ME, lib.loc = "FILEPATH")
library(lme4)

# load clean data: 
data = fread("FILEPATH/01_clean_data_covars_overall.csv")

######################## FIND OUTLIERS WITH COOKS DISTANCE #####################
# scaled logit transform dependent variable: 
scaled_logit <- function(x,a,b){
  # Alternative to regular logit, found from https://otexts.com/fpp2/limits.html
  result <- log((x-a)/(b-x))
  return(result)
}
data[, logit_hours := scaled_logit(weekly_hours, 0, 169)]

# create model: 
cg_model = lm(logit_hours ~ sdi + severity + care_type, data = data)
# this is the best model picked by BMS
# model selection done in this script:
# ~/1_stage1_model/caregiving_model/0_model_selection.R

# cut data based on cooks distance
data[, cooksd := stats::cooks.distance(cg_model)]

data_incl = data[cooksd < 4/nrow(data)]
data_cut = anti_join(data, data_incl)

# save final input data without outliers:
fwrite(data_incl, file = "FILEPATH/02_clean_data_no_outliers_04.11.24.csv")
# save outlier data that was cut: 
fwrite(data_cut, file = "FILEPATH/cut_outliers_04.11.24.csv")
