##########################################################################
### Author: USERNAME
### Date: 08/14/2024
### Project: Health Spending Effectiveness 
### Purpose: Child script for extracting population attributable fraction (PAF) and DALY draws in parallel.
###          Pulls PAFs and DALYs by age group for each location individually.
###########################################################################

# clear environment
rm(list=ls())

# source shared functions
source('FILEPATH/get_draws.R')

# initialize function arguments:
rei_child = 1092 # Environmental and occupational risks
rei_adult = 1093 # Joint risk controlled in ADULTS includes environmental, occupational, and behavioral
cause = 294 # All causes - STILL NEED THIS FOR REI IDs
measure = 2 # DALYS
metric_paf = 2 # Percent (Pc is percent of deaths or attributable risk fraction if risk id <>0)
metric_deaths = 1 # Count 
years = c(1995:2023)
# Age groups not needed - joint PAF only reported for age group 22
sex = c(1,2) # only sex splits available
release = 16 # GBD 2023
version_paf = 384 # Burdenator version for joint PAF
version_deaths = 461
loc = as.numeric(commandArgs()[4]) # get location_id from parent script input

# get draws of child PAF
child_paf_draws <- get_draws(gbd_id_type = c('rei_id', 'cause_id'),
                       gbd_id = c(rei_child, cause), 
                       source = 'burdenator', 
                       release_id = release, 
                       measure_id = 2, # DALYS
                       metric_id = metric_paf,
                       location_id = loc, 
                       year_id = years, 
                       sex_id = sex, 
                       version_id = version_paf, 
                       num_workers = 2)

# get draws of adult PAF
adult_paf_draws <- get_draws(gbd_id_type = c('rei_id', 'cause_id'),
                             gbd_id = c(rei_adult, cause), 
                             source = 'burdenator', 
                             release_id = release, 
                             measure_id = 2, # DALYS
                             metric_id = metric_paf,
                             location_id = loc, 
                             year_id = years, 
                             sex_id = sex, 
                             version_id = version_paf, 
                             num_workers = 2)

# save out PAF draws
out_dir_paf = "FILEPATH"
data.table::fwrite(child_paf_draws, file = paste0(out_dir_paf, "joint_child_PAF_DALYS_1995_2023_loc_", loc, ".csv"))

# save out death draws 
out_dir_paf = "FILEPATH"
data.table::fwrite(adult_paf_draws, file = paste0(out_dir_paf, "joint_adult_PAF_DALYS_1995_2023_loc_", loc, ".csv"))
