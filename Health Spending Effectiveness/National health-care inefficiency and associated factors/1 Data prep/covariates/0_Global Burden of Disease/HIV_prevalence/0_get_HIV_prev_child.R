##########################################################################
### Author: USERNAME
### Date: 08/15/2024
### Project: Health Spending Effectiveness 
### Purpose: Child script for extracting HIV prevalence draws in parallel
###          Pulls HIV for one location at a time
###########################################################################

# clear environment
rm(list=ls())

# source shared functions
source('FILEPATH/get_draws.R')


# initialize function arguments:
cause = 298 # HIV/AIDS
measure = 5 # prevalence
metric = 3 # rate
years = c(1995:2023)
ages = 22 # all ages
sex = 3 # all sex
release = 16 # GBD 2023
loc = as.numeric(commandArgs()[4]) # get location_id from parent script input

machine = "como"


version = 1591 # COMO Latest Version


# get HIV prevalence draws for all sex, all age non-standardized
prev_draws = get_draws(gbd_id_type = "cause_id", 
                       gbd_id = cause, # HIV/AIDS
                       source = machine, # COMO
                       measure_id = measure, # prevalence
                       metric_id = metric, # rate
                       location_id = loc,
                       year_id = years,
                       age_group_id = ages, 
                       sex_id = sex, # all sex
                       version_id = version, # COMO Annual Latest Version
                       release_id = release, # GBD 2021
                       num_workers = 2) # parallel processing 

# save out HIV prev draws
out_dir = "FILEPATH"
data.table::fwrite(prev_draws, file = paste0(out_dir, "all_age_draws_1995_2023_loc_", loc, ".csv"))

