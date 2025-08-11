##########################################################################
### Author: USERNAME
### Date: 08/14/2024
### Project: Health Spending Effectiveness 
### Purpose: Child script for extacting population attributable fraction (PAF) and death draws by age group in parallel.
###          Pulls PAFs and deaths by age group for each location individually.
###########################################################################

# clear environment
rm(list=ls())

# source shared functions
source('FILEPATH/get_draws.R')

# initialize function arguments:
rei = 202 # Environmental and occupational risks
cause = 294 # All causes
measure = 1 # Deaths
metric_paf = 2 # Percent (Pc is percent of deaths or attributable risk fraction if risk id <>0)
metric_deaths = 1 # Count 
years = c(1995:2023)
# Age groups: Under 5, 5-49, 50 plus
ages = c(2, 3, 6:20, 30:32, 34, 235, 238, 388, 389)
sex = c(1,2) # only sex splits available
release = 16 # GBD 2023
version_paf = 360 # Latest Version
version_deaths = 461
loc = as.numeric(commandArgs()[4]) # get location_id from parent script input

# get draws of PAF: REI id 202, Deaths, percent
paf_draws <- get_draws(gbd_id_type = c('rei_id', 'cause_id'),
                       gbd_id = c(rei, cause), 
                       source = 'burdenator', 
                       release_id = release, 
                       measure_id = measure, 
                       metric_id = metric_paf,
                       location_id = loc, 
                       year_id = years, 
                       age_group_id = ages, 
                       sex_id = sex, 
                       version_id = version_paf, 
                       num_workers = 2)

# get draws of deaths (number)
deaths_draws <- get_draws(gbd_id_type = "cause_id", 
                          gbd_id = cause, 
                          source = 'codcorrect', 
                          release_id = release, 
                          measure_id = measure, 
                          metric_id = metric_deaths,
                          location_id = loc, 
                          year_id = years, 
                          age_group_id = ages, 
                          sex_id = sex, 
                          version_id = version_deaths, 
                          num_workers = 2)


# save out PAF draws
out_dir_paf = "FILEPATH"
data.table::fwrite(paf_draws, file = paste0(out_dir_paf, "age_split_draws_1995_2023_loc_", loc, ".csv"))

# save out death draws 
out_dir_death = "FILEPATH"
data.table::fwrite(deaths_draws, file = paste0(out_dir_death, "deaths_split_draws_1995_2023_loc_", loc, ".csv"))
