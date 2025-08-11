##########################################################################
### Author: USERNAME
### Date: 06/07/2024
### Project: Health Spending Effectiveness 
### Purpose: Extracting health worker density (HWD) from GBD
###########################################################################

# clear environment
rm(list=ls())


# load libraries and functions
library(data.table)
library(dplyr)
library(assertthat)
source(paste0(functions_dir,"get_covariate_estimates.R"))
source(paste0(functions_dir,"get_location_metadata.R"))

# set up country location IDs list: 
# get location metadata
locs <- get_location_metadata(location_set_id = 35, 
                              release_id = 16)  # GBD 2023
# save out locs to use in other scripts
fwrite(locs,"FILEPATH/location_set35_release16.csv")
# get country IDs 
country_ids <- locs[level == 3]$location_id # level 3 -> country

# initialize function arguments:
years = c(1995:2023)
all_ages = 22 # All ages (non-standardized)
age_std = 27 # Age standardized
sex = 3 # all sex
release = 16 # GBD 2023
covar = 1111 # Health Worker Density (HWD)

############################# Age Non-Standardized #############################
# pull HWD for all ages non-standardized
HWD = get_covariate_estimates(covariate_id = covar, # 1111
                              age_group_id = all_ages, # 22 
                              location_id = country_ids, 
                              release_id = release, # 9, GBD 2021
                              sex_id = sex, # 3
                              year_id = years) # 1995 - 2021


# verify that mean == lower == upper for all rows
n_intervals = nrow(HWD[mean_value != lower_value | mean_value != upper_value])

if (n_intervals == 0) {
  # no confidence intervals, just mean provided
  # can delete the lower and upper columns then
  HWD[, ":=" (lower_value = NULL, upper_value = NULL)]
  print('No confidence intervals provided, lower & upper value columns deleted')
} else {
  print('Confidence intervals provided')
}
# has confidence intervals! 

# change var names
setnames(HWD, old = c("mean_value", "lower_value", "upper_value"), 
         new = c("HWD", "HWD_lower", "HWD_upper"))

# save out health worker density data
output_dir = "FILEPATH"
fwrite(HWD, paste0(output_dir, 'health_worker_density_1995_2023.csv'))

