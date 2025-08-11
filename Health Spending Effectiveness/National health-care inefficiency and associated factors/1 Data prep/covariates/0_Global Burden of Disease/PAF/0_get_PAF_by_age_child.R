##########################################################################
### Author: USERNAME
### Date: 08/14/2024
### Project: Health Spending Effectiveness 
### Purpose: Child script for extracting population attributable fraction (PAF) draws by age group in parallel.
###          Pulls PAFs by age group for one location at a time.
###########################################################################

# clear environment
rm(list=ls())

# source shared functions
source('FILEPATH/get_draws.R')

# initialize function arguments:
rei = 378 # Amenable risk factors
cause = 294 # All causes - STILL NEED THIS FOR REI IDs
measure = 2 # DALYs (Disability-adjusted life years)
metric = 2 # Percent (Pc is percent of deaths or attributable risk fraction if risk id <>0)
years = c(1995:2023)
# Age groups: Under 5, 5-49, 50 plus
ages = c(2, 3, 6:20, 30:32, 34, 235, 238, 388, 389)
sex = c(1,2) # only sex splits available
release = 16 # GBD 2023
version = 348 # Latest Version
loc = as.numeric(commandArgs()[4]) # get location_id from parent script input

# get draws of PAF: REI id 378, DALYs, percent
paf_draws <- get_draws(gbd_id_type = c('rei_id', 'cause_id'),
                       gbd_id = c(rei, cause), 
                       source = 'burdenator', 
                       release_id = release, 
                       measure_id = measure, 
                       metric_id = metric,
                       location_id = loc, 
                       year_id = years, 
                       age_group_id = ages, 
                       sex_id = sex, 
                       version_id = version, 
                       num_workers = 2)

# save out PAF draws
out_dir = "FILEPATH"
data.table::fwrite(paf_draws, file = paste0(out_dir, "age_split_draws_1995_2023_loc_", loc, ".csv"))
