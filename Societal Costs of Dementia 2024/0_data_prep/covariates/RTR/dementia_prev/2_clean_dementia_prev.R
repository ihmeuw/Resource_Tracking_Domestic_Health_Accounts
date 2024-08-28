# Clean and bind together all individual location dementia prevalence files 
# These files were created in parallel from scripts 0 & 1 in this directory. 
# Author: Michael Breshock
# Date: 04/17/2024

## See README

# Clear environment
rm(list = ls())

# Loading libraries
library(data.table)

## See how many files were saved out and verify that all 
## locations were pulled successfully:
# file path containing dementia prevalence data for each individual location: 
prev_dir = "FILEPATH/"
prev_files = list.files(prev_dir) # list all file names
# should have 205 files
length(prev_files) # 205 - check
# verify that all the location IDs match the GBD locaation set
file_locs = as.numeric(gsub("draws_1990_2024_loc_", "", 
                            gsub(".csv", "", prev_files)))

# get GBD location ids: 
source("FILEPATH/get_location_metadata.R")
# get location IDs from metadata; location_set_id=22: -> covariate computation
locs <- get_location_metadata(location_set_id=22, 
                              release_id = 16)  # GBD 2023
# extract country location IDs from the location set (country = level 3)
loc_ids <- locs[level == 3]$location_id 
# add Hong Kong (not level 3 in GBD location hierarchy)
HK_id <- 354
GBD_ids <- c(loc_ids, HK_id)

# sum number of location IDs in GBD location set that are not in location data files
sum(!GBD_ids %in% file_locs) # 0 - check

# read in all files and bind together
dem_prev <- rbindlist(lapply(paste0(prev_dir, prev_files), fread))
# double check
length(unique(dem_prev$location_id)) # 205
unique(dem_prev$year_id)

# remove empty model_version_id column
dem_prev[, model_version_id := NULL]

# melt draws from wide to long: 
dem_long = melt(dem_prev, id.vars = c("year_id", "location_id", "measure_id", 
                                      "metric_id", "modelable_entity_id", 
                                      "sex_id", "age_group_id"),
                variable.name = "draw", value.name = "prevalence")

# summarize draws to mean and 95% UI
dem_sum = dem_long[,.(prevalence = mean(prevalence), 
                      prev_lower = quantile(prevalence, 0.025), 
                      prev_upper = quantile(prevalence, 0.975)), 
                   by = .(year_id, location_id, measure_id, metric_id, 
                          modelable_entity_id, sex_id, age_group_id)]

# save out dementia prevalence estimates for all locations 1990-2024
out_dir = "FILEPATH/"
fwrite(dem_sum, file = paste0(out_dir, "dementia_prevalence_release16_1990_2024.csv"))
