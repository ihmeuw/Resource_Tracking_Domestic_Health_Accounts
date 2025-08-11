##########################################################################
### Author: USERNAME
### Date: 07/24/2024
### Project: Health Spending Effectiveness 
### Purpose: Extracting socio-demographic index (SDI) from FGH
###########################################################################

# clear environment
rm(list=ls())

# load libraries and functions
library(data.table)
library(dplyr)
library(assertthat)
source(paste0(functions_dir,"get_covariate_estimates.R"))

# set up country location IDs list: 
loc_dir = "FILEPATH"
locs = fread(paste0(loc_dir, "location_set35_release9.csv"))
# get country IDs 
country_ids <- locs[level == 3]$location_id # level 3 -> country

# initialize function arguments:
years = c(1995:2023)
all_ages = 22 # All ages (non-standardized)
age_std = 27 # Age standardized
sex = 3 # all sex
release = 16 # GBD 2023

############################# Age Non-Standardized #############################
covar = 881 # socio-demographic index (SDI)

# pull SDI for all ages non-standardized
sdi = get_covariate_estimates(covariate_id = covar, # SDI
                              age_group_id = all_ages, # 22 
                              location_id = country_ids, 
                              release_id = release, # GBD 2023
                              sex_id = sex, # all
                              year_id = years) # 1995 - 2023


# change var names
setnames(sdi, old = c("mean_value"), new = c("sdi"))

# save out 
output_dir = "FILEPATH"
fwrite(sdi, file.path(output_dir, 'sdi_1995_2023.csv'))

