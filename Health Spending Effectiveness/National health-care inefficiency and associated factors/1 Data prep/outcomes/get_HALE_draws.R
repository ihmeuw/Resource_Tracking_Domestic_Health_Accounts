##########################################################################
### Author: USERNAME
### Date: 06/14/24
### Project: Health Spending Effectiveness 
### Purpose: Extracting healthy life expectancy (HALE) draws from individual files for each location-year
##########################################################################

# clear environment
rm(list=ls())


# load libraries
library(data.table)
library(dplyr)
library(assertthat)


# set up country location IDs list: 
loc_dir = "FILEPATH"
locs = fread(paste0(loc_dir, "location_set35_release9.csv"))
# get country IDs 
country_ids <- locs[level == 3]$location_id # level 3 -> country

## Results directory structure: 
# FILEPATH/1990_draws.csv -> 2022_draws.csv
# each location-year data is in an individual .csv file
# with each location_id being the parent directory name
# and the year being in the file name. 
# leverage structure to only read in
# location-years of interest

# create list of location_id file paths for all country IDs
results_dir = 'FILEPATH'
loc_dirs = paste0(results_dir, country_ids, "/")
years = c(1995:2024)

HALE_df <- data.table()
# loop through each country ID directory: 
for (fp in loc_dirs){
  # create list of year file names 
  year_paths = paste0(fp, years, "_draws.csv")
  # read in each year file and bind rows
  loc_df <- rbindlist(lapply(year_paths, fread))
  # filter loc_df to all ages and sexes (files include age/sex splits)
  loc_df = loc_df[sex_id == 3 & age_group_id == 22]
  
  # save to all location df 
  HALE_df = rbind(HALE_df, loc_df)
}

# Verify that all variables have expected values
assert_that(unique(HALE_df$age_group_id) == 22)
assert_that(all(country_ids %in% unique(HALE_df$location_id)))
assert_that(length(unique(HALE_df$location_id)) == length(country_ids))
assert_that(all(years %in% unique(HALE_df$year_id)))
assert_that(length(unique(HALE_df$year_id)) == length(years))
assert_that(unique(HALE_df$sex_id) == 3)

# save out all age and sex HALE draws to single .csv: 
fwrite(HALE_df, 'FILEPATH/draws_1995_2024.csv')
