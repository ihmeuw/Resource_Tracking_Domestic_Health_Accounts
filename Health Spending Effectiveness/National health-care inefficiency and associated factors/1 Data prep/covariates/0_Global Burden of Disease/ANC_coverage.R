##########################################################################
### Author: USERNAME
### Date: 11/27/2024
### Project: Health Spending Effectiveness 
### Purpose: Extracting antenatal care coverage (ANC) from GBD
###########################################################################

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
release = 16 # GBD 2023

# pulling two types of ANC coverage
covar_ANC1 = 7	# Antenatal Care (1 visit) Coverage (proportion)
covar_ANC4 = 8	# Antenatal Care (4 visits) Coverage (proportion)

################################# ANC1 Coverage ################################
# pull ANC1 coverage
ANC1 = get_covariate_estimates(covariate_id = covar_ANC1, # 7
                               age_group_id = all_ages, # 22 
                               location_id = loc_ids, 
                               release_id = release, # 16, GBD 2023
                               sex_id = sex, # 3
                               year_id = years) # 1995 - 2023


# create draws from confidence interval
ANC1[, standard_error := (upper_value - lower_value) / 3.92]

# create 500 draws of coverage for each year-location
set.seed(123)
drawDT <- data.table(t(apply(ANC1[,.(mean_value, standard_error)], 1,
                             FUN = function(x) rnorm(500, mean = x[1], sd = x[2]))))
ANC1 = cbind(ANC1, drawDT) # add draws to original data

# remove unnecessary columns: 
ANC1[, c("mean_value", "lower_value", "upper_value", "standard_error") := NULL]
ANC1[, c("model_version_id", "covariate_name_short", "age_group_id", 
         "age_group_name", "sex_id", "sex") := NULL]

# pivot draws to long format
ANC1_draws = melt(ANC1,
                  id.vars = c("year_id", "location_id", 
                              "location_name", "covariate_id"), 
                  variable.name = "draw", value.name = "ANC1")

# remove "V" from the draw column
ANC1_draws[, draw := as.numeric(gsub("V", "", draw))]

# see how many draws are outside the possible value range (0 - 1)
nrow(ANC1_draws[ANC1 < 0 | ANC1 > 1]) / nrow(ANC1_draws) 
# 0.03687073 (3.7% of draws are outside 0-1)

# check mean value before and after setting limits
mean(ANC1_draws$ANC1) # 0.9183075

# set limits on draws to be between 0 and 1 (percentage)
ANC1_draws[ANC1 < 0, ANC1 := 0]
ANC1_draws[ANC1 > 1, ANC1 := 1]

mean(ANC1_draws$ANC1) # 0.9181811

# save out ANC1 coverage draws
output_dir = "FILEPATH"
fwrite(ANC1_draws, file.path(output_dir, 'ANC1_coverage_draws_2023.csv'))

################################# ANC4 Coverage ################################
# pull ANC4 coverage
ANC4 = get_covariate_estimates(covariate_id = covar_ANC4, # 8
                               age_group_id = all_ages, # 22 
                               location_id = loc_ids, 
                               release_id = release, # 16, GBD 2023
                               sex_id = sex, # 3
                               year_id = years) # 1995 - 2023

# check if mean == lower == upper for all rows
n_intervals = nrow(ANC4[mean_value != lower_value | mean_value != upper_value])

if (n_intervals == 0) {
  # no confidence intervals, just mean provided
  # can delete the lower and upper columns then
  ANC4[, ":=" (lower_value = NULL, upper_value = NULL)]
  print('No confidence intervals provided, lower & upper value columns deleted')
} else {
  print('Confidence intervals provided')
}
# confidence interval provided

# create draws from confidence interval
ANC4[, standard_error := (upper_value - lower_value) / 3.92]

# create 500 draws of coverage for each year-location
set.seed(123)
drawDT <- data.table(t(apply(ANC4[,.(mean_value, standard_error)], 1,
                             FUN = function(x) rnorm(500, mean = x[1], sd = x[2]))))
ANC4 = cbind(ANC4, drawDT) # add draws to original data

# remove unnecessary columns: 
ANC4[, c("mean_value", "lower_value", "upper_value", "standard_error") := NULL]
ANC4[, c("model_version_id", "covariate_name_short", "age_group_id", 
         "age_group_name", "sex_id", "sex") := NULL]

# pivot draws to long format
ANC4_draws = melt(ANC4,
                  id.vars = c("year_id", "location_id", 
                              "location_name", "covariate_id"), 
                  variable.name = "draw", value.name = "ANC4")

# remove "V" from the draw column
ANC4_draws[, draw := as.numeric(gsub("V", "", draw))]

# see how many draws are outside the possible value range (0 - 1)
nrow(ANC4_draws[ANC4 < 0 | ANC4 > 1]) / nrow(ANC4_draws) 
# 0.003892883 (0.4% of draws are outside 0-1)

# check mean value before and after setting limits
mean(ANC4_draws$ANC4) # 0.7650343

# set limits on draws to be between 0 and 1 (percentage)
ANC4_draws[ANC4 < 0, ANC4 := 0]
ANC4_draws[ANC4 > 1, ANC4 := 1]

mean(ANC4_draws$ANC4) # 0.7650165

# save out ANC4 coverage draws
output_dir = "FILEPATH"
fwrite(ANC4_draws, file.path(output_dir, 'ANC4_coverage_draws_2023.csv'))
