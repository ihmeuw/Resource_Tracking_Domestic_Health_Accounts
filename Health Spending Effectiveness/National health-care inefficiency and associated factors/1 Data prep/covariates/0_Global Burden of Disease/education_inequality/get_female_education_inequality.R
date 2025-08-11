##########################################################################
### Author: USERNAME
### Date: 11/26/2024
### Project: Health Spending Effectiveness 
### Purpose: Extracting education inequality index from GBD
###########################################################################

# clear environment
rm(list=ls())

# set up root folder paths
if (Sys.info()["sysname"] == "Linux") {
  j_root <- "FILEPATH" 
  h_root <- "FILEPATH"
  l_root <- "FILEPATH"
  functions_dir <- "FILEPATH"
} else { 
  j_root <- "FILEPATH"
  h_root <- "FILEPATH"
  l_root <- "FILEPATH"
  functions_dir <- "FILEPATH"
}

# load libraries and functions
library(data.table)
source(paste0(functions_dir,"get_covariate_estimates.R"))
source(paste0(functions_dir,"get_location_metadata.R"))
source(paste0(functions_dir,"get_population.R"))

# get location metadata
locs <- get_location_metadata(location_set_id = 35, 
                              release_id = 9)  # GBD 2021
# filter to just country IDs
loc_ids = locs[level == 3]$location_id

# initialize function arguments:
years = c(1995:2021)
# all_ages = 22 # All ages (non-standardized)
# age_std = 27 # Age standardized
sex = 3 # all sex
release = 9 # GBD 2021
covar_GINI = 2025 # Education Relative Inequality (Gini), age-standardized

################# Female Education Relative Inequality (Gini) ##################

# pull education relative inequality, age standardized
GINI_f = get_covariate_estimates(covariate_id = covar_GINI, # 2025
                                 location_id = loc_ids, 
                                 release_id = release, # 9, GBD 2021
                                 sex_id = 2, # female
                                 year_id = years) # 1995 - 2021

# check if mean == lower == upper for all rows
n_intervals = nrow(GINI_f[mean_value != lower_value | mean_value != upper_value])

if (n_intervals == 0) {
  # no confidence intervals, just mean provided
  # can delete the lower and upper columns then
  GINI_f[, ":=" (lower_value = NULL, upper_value = NULL)]
  print('No confidence intervals provided, lower & upper value columns deleted')
} else {
  print('Confidence intervals provided')
}
# no CI

# change variable name
setnames(GINI_f, old = c("mean_value"), new = c("educ_ineq"))

# save out Education Relative Inequality (GINI) estimates
output_dir = "FILEPATH"
fwrite(GINI_f, file.path(output_dir, 'female_education_inequality_GINI_age_std.csv'))
