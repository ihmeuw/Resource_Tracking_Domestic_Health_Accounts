################################################################################
# This script is run for an individual location passed by parallel_run_parent.R,
# and uses get_draws to pull the age-normalized prevalence for a given country.
# Author: Elye Bliss
# Date: Nov 15, 2023
################################################################################

# Clean the environment 
rm(list=ls()) 

library(data.table)

# Save outputs in FILEPATH
output_dir = "FILEPATH/"

# Since this is mean to parallelize over location_id, take location from command
# line input. This corresponds to the `lid` variable in the accompanying
# parallel_run_parent.R script
location_input <- commandArgs()[6]


# ages group IDs to run on:
required_ages <- c(13,14,15,16,17,18,19,20,30,31,32,235)
# sex IDs to run on: 
sex_ids <- c(1,2)

source("FILEPATH/get_draws.R")

prev_draws_2019 = get_draws(gbd_id_type = "modelable_entity_id", # MEID 24351
                            gbd_id = 24351, # Dementia prevalence from DisMod, post-mortality modeling
                            location_id = location_input, 
                            year_id = 2019,
                            measure_id = 5, # prevalence
                            source = "epi",
                            sex_id = sex_ids, 
                            age_group_id = required_ages,
                            release_id = 6)

# Save file:
fwrite(prev_draws_2019,paste0(output_dir,'prev_by_age_',location_input,'.csv'))
