# Extracting SDI covariate data from GBD release_id 16 (GBD 2023)
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

# confirming that the locations in set_id 22 contain all locations in set_id 35:
locs_Test = get_location_metadata(location_set_id = 35, # GBD Modeling Outputs
                                  release_id = release) 
sum(!locs_Test$location_id %in% locs$location_id) # 0
# all locations in set_id 35 are in set_id 22

## Pulling SDI data (covariate_id: 881)
sdi <- get_covariate_estimates(covariate_id = 881, # SDI
                               age_group_id = 22, # All ages (non-standardized)
                               location_id = loc_ids, 
                               release_id = release, # GBD 2023 
                               sex_id = 3, # all sex
                               year_id = c(1990:2024))

# save out data: 
out_dir = "FILEPATH"
fwrite(sdi, file = paste0(out_dir, "sdi_release16_1990_2024.csv"))
