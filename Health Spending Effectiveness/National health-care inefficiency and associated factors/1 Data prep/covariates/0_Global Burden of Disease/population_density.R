##########################################################################
### Author: USERNAME
### Date: 11/27/2024
### Project: Health Spending Effectiveness 
### Purpose: Extracting population density from GBD
###########################################################################

# clear environment
rm(list=ls())


# load libraries and functions
library(data.table)
source(paste0("FILEPATH","get_covariate_estimates.R"))
source(paste0("FILEPATH","get_location_metadata.R"))

# get location metadata
locs <- get_location_metadata(location_set_id = 35, 
                              release_id = 16)  # GBD 2023
# filter to just country IDs
loc_ids = locs[level == 3]$location_id

# initialize function arguments:
years = c(1995:2023)
all_ages = 22 # All ages (non-standardized)
sex = 3 # all sex
release = 16 # GBD 2023

covar = 118 

# pull population density (proportion over 1000 ppl/sqkm)
pop_dens = get_covariate_estimates(covariate_id = covar, # 118
                                   age_group_id = all_ages, # 22 
                                   location_id = loc_ids, 
                                   release_id = release, # GBD 2023
                                   sex_id = sex, # 3
                                   year_id = years) # 1995 - 2023


# rename columns
setnames(pop_dens, old = c("mean_value"), new = c("pop_dens"))

# save out Education Absolute Inequality (pop_dens) estimates
output_dir = "FILEPATH"
fwrite(pop_dens, file.path(output_dir, 'population_density.csv'))
