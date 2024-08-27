##########################################################################
### Author: Michael Breshock
### Date: 10/05/2023
### Project: Global dementia spending
### Purpose: Prep dementia prevalence forecasts
##########################################################################

rm(list=ls()) 

library(data.table)

locs <- fread(file = "FILEPATH/location_set_22_metadata.csv")
loc_ids = locs[level==3]$location_id

## 2020 - 2030 prevalence draws:
# each country has its own file
prev_path1 ="FILEPATH"
prev_files1 = paste0(prev_path1, 
                     list.files(prev_path1, 
                                pattern = "^collapseddraws_.*.*[0-9]\\.rds$"))
# only want files that end in a location id ^ 
# read each prevalence file and filter all ages (age_group_id = 22)
prev1 = rbindlist(lapply(prev_files1, readRDS))[age_group_id == 22]

## 2031 - 2050 prevalence draws:
# each country has its own file
prev_path2 ="FILEPATH"
prev_files2 = paste0(prev_path2, 
                     list.files(prev_path2, 
                                pattern = "^collapseddraws_.*.*[0-9]\\.rds$"))
# only want files that end in a location id ^ 
# read each prevalence file and filter to all ages (age_group_id = 22)
prev2 = rbindlist(lapply(prev_files2, readRDS))[age_group_id == 22]

# see which locations don't match: 
files1 = list.files(prev_path1, pattern = "^collapseddraws_.*.*[0-9]\\.rds$")
files2 = list.files(prev_path2, pattern = "^collapseddraws_.*.*[0-9]\\.rds$")

missing_files = files2[!(files2 %in% files1)]
missing_paths = paste0(prev_path2, missing_files)
missing_prev = rbindlist(lapply(missing_paths, readRDS))

# bind all years of data together: 
prev = rbind(prev1, prev2)

# see which location IDs from GBD 2019 hierarchy are missing:
missing_locs <- locs[!location_id %in% prev$location_id & level == 3, .(location_id, location_name,region_name)]
fwrite(missing_locs, 'FILEPATH/missing_locs.csv')

# combine sex 1 and 2 - get total count of cases and overall prevalence for both sexes
prev_sum = prev[, .(total_cases = sum(num), prevalence = weighted.mean(prev,num)),
                by = c("year_id", "location_id", "draw")]

# save out prevalence forecast draws
fwrite(prev_sum, file = "FILEPATH/dementia_prevalence_forecast_draws.csv")
