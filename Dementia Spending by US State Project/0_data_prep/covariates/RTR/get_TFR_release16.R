# Extracting TFR covariate data from GBD release_id 16 (GBD 2023)
# TFR = Total Fertility Rate
# Author: Michael Breshock
# Date: 04/16/2024

## See README

# clear environment
rm(list=ls())

# load libraries and functions 
library(data.table)
library(dplyr)
source("FILEPATH/get_location_metadata.R")
source("FILEPATH/get_covariate_estimates.R")

# initalize release_id variable
release = 16 # GBD 2023

# get location IDs from metadata
locs <- get_location_metadata(location_set_id = 22, # covariate computation
                              release_id = release) 
loc_ids <- locs$location_id

## Pulling Total Fertility Rate (TFR, covariate_id = 149)
tfr <- get_covariate_estimates(covariate_id = 149, 
                               age_group_id = 22, # all ages
                               location_id = loc_ids, 
                               release_id = release, 
                               sex_id = 3, # all sex
                               year_id=c(1990:2024))

# grab just the columns we need and change names for clarity: 
tfr_out = tfr[,.(year_id, location_id, covariate_id, covariate_name_short,
                 age_group_id, sex_id, mean_value, lower_value, upper_value)]
setnames(tfr_out, old = c("mean_value", "lower_value", "upper_value"), 
         new = c("tfr", "tfr_lower", "tfr_upper"))

# save out tfr covariate data: 
out_dir = "FILEPATH"
fwrite(tfr_out, file = paste0(out_dir, "tfr_release16_1990_2024.csv"))
