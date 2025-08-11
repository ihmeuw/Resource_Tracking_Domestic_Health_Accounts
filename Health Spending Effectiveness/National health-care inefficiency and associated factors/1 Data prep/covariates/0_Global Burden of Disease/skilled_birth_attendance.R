### Extracting Skilled Birth Attendance from GBD covariate database
### Project: BMGF Health Spending Effectiveness 
### Author: Michael Breshock
### Date: 11/27/2024

# clear environment
rm(list=ls())


# load libraries and functions
library(data.table)
source(paste0("FILEPATH","get_covariate_estimates.R"))
source(paste0("FILEPATH","get_location_metadata.R"))


# get location metadata
locs <- get_location_metadata(location_set_id = 35, 
                              release_id = 16)  # GBD 2023
# filter to just country IDs
loc_ids = locs[level == 3]$location_id

# initialize function arguments:
years = c(1995:2023)
all_ages = 22 # All ages (non-standardized)
sex = 3 # all sex
release = 9 # GBD 2021
covar = 143 # Skilled Birth Attendance (proportion)

# pull SBA coverage
SBA = get_covariate_estimates(covariate_id = covar, # 143
                              age_group_id = all_ages, # 22 
                              location_id = loc_ids, 
                              release_id = release, # 16, GBD 2023
                              sex_id = sex, # 3
                              year_id = years) # 1995 - 2023

# create draws from confidence interval
SBA[, standard_error := (upper_value - lower_value) / 3.92]

# create 500 draws of vacc coverage for each year-location
set.seed(123)
drawDT <- data.table(t(apply(SBA[,.(mean_value, standard_error)], 1,
                             FUN = function(x) rnorm(500, mean = x[1], sd = x[2]))))
SBA = cbind(SBA, drawDT) # add draws to original data

# cut unnecessary columns: 
SBA[, c("mean_value", "lower_value", "upper_value", "standard_error") := NULL]
SBA[, c("model_version_id", "covariate_name_short", "age_group_id", 
         "age_group_name", "sex_id", "sex") := NULL]

# pivot draws to long format
SBA_draws = melt(SBA,
                 id.vars = c("year_id", "location_id", 
                             "location_name", "covariate_id"), 
                 variable.name = "draw", value.name = "SBA")

# remove "V" from the draw column
SBA_draws[, draw := as.numeric(gsub("V", "", draw))]

# see how many draws are outside the possible value range (0 - 1)
nrow(SBA_draws[SBA < 0 | SBA > 1]) / nrow(SBA_draws) 
# 0.03014089 (3.0% of draws are outside 0-1)

# check mean value before and after setting limits
mean(SBA_draws$SBA) # 0.8504152

# set limits on draws to be between 0 and 1 (percentage)
SBA_draws[SBA < 0, SBA := 0]
SBA_draws[SBA > 1, SBA := 1]

mean(SBA_draws$SBA) # 0.8503266
# not a major change in the mean ^

# save out SBA vaccine coverage draws
output_dir = "FILEPATH"
fwrite(SBA_draws, file.path(output_dir, 'skilled_birth_attendance_draws_2023.csv'))
