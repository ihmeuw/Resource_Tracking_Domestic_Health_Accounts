# Get dementia prevalence numbers globally from GBD 2023
# Child script for running in parallel
# Author: Michael Breshock
# Date: 04/17/2024

## See README

# Clear environment
rm(list = ls())

# Loading libraries
library(data.table)

# source shared functions
source("FILEPATH/get_location_metadata.R")
source("FILEPATH/interpolate.R")

# initalize shared function input variables
release = 16 # GBD 2023

# get location_id from parent script input
loc_id = as.numeric(commandArgs()[6])

# pull dementia prevalence draws from envelope for 1990-2024
prev_draws <- interpolate(gbd_id_type = "modelable_entity_id", # MEID 24351
                          gbd_id = 24351, # Dementia prevalence from DisMod, post-mortality modeling
                          location_id = loc_id,
                          measure_id = 5, # prevalence
                          source = "epi",
                          sex_id = 3, # all sex
                          reporting_year_start = 1990,
                          reporting_year_end = 2024,
                          interp_method = "linear",
                          downsample = TRUE,
                          n_draws = 100, # get 100 draws 
                          age_group_id = 22, # age_group_id = 22: all ages
                          num_workers = 5, # set this to the number of threads your session is using
                          release_id = release) # GBD 2023

# save out prev_draws:
out_dir = "FILEPATH"
fwrite(prev_draws, file = paste0(out_dir, "draws_1990_2024_loc_", loc_id, ".csv"))
