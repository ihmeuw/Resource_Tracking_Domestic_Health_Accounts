##########################################################################
### Author: USERNAME
### Date: 11/27/2024
### Project: Health Spending Effectiveness 
### Purpose: Extracting vaccine coverage from GBD
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
covar_Hib = 47 # Hib3 Vaccine Coverage
covar_MCV = 75 # Measles Vaccine Coverage

############################# Hib3 Vaccine Coverage ############################
# pull vaccine coverage
Hib3 = get_covariate_estimates(covariate_id = covar_Hib, # 47
                               age_group_id = all_ages, # 22 
                               location_id = loc_ids, 
                               release_id = release, # 16, GBD 2023
                               sex_id = sex, # 3
                               year_id = years) # 1995 - 2023


# create draws from confidence interval
Hib3[, standard_error := (upper_value - lower_value) / 3.92]

# create 500 draws of vaccine coverage for each year-location
set.seed(123)
drawDT <- data.table(t(apply(Hib3[,.(mean_value, standard_error)], 1,
                             FUN = function(x) rnorm(500, mean = x[1], sd = x[2]))))
Hib3 = cbind(Hib3, drawDT) # add draws to original data

# remove unnecessary columns: 
Hib3[, c("mean_value", "lower_value", "upper_value", "standard_error") := NULL]
Hib3[, c("model_version_id", "covariate_name_short", "age_group_id", 
         "age_group_name", "sex_id", "sex") := NULL]

# pivot draws to long format
Hib3_draws = melt(Hib3,
                  id.vars = c("year_id", "location_id", 
                              "location_name", "covariate_id"), 
                  variable.name = "draw", value.name = "Hib3")

# remove "V" from the draw column
Hib3_draws[, draw := as.numeric(gsub("V", "", draw))]

# see how many draws are outside the possible value range (0 - 1)
nrow(Hib3_draws[Hib3 < 0 | Hib3 > 1]) / nrow(Hib3_draws) 
# 0.0003400947 (0.03% of draws are outside 0-1)

# check mean value before and after setting limits
mean(Hib3_draws$Hib3) # 0.5749932

# set limits on draws to be between 0 and 1 (percentage)
Hib3_draws[Hib3 < 0, Hib3 := 0]
Hib3_draws[Hib3 > 1, Hib3 := 1]

mean(Hib3_draws$Hib3) # 0.5749911

# save out Hib3 vaccine coverage draws
output_dir = "FILEPATH"
fwrite(Hib3_draws, file.path(output_dir, 'Hib3_vaccine_coverage_draws_2023.csv'))

############################# MCV Vaccine Coverage ############################
# pull vaccine coverage
MCV = get_covariate_estimates(covariate_id = covar_MCV, # 75
                              age_group_id = all_ages, # 22 
                              location_id = loc_ids, 
                              release_id = release, # 16, GBD 2023
                              sex_id = sex, # 3
                              year_id = years) # 1995 - 2023


# create draws from confidence interval
MCV[, standard_error := (upper_value - lower_value) / 3.92]

# create 500 draws of vaccine coverage for each year-location
set.seed(123)
drawDT <- data.table(t(apply(MCV[,.(mean_value, standard_error)], 1,
                             FUN = function(x) rnorm(500, mean = x[1], sd = x[2]))))
MCV = cbind(MCV, drawDT) # add draws to original data

# remove unnecessary columns: 
MCV[, c("mean_value", "lower_value", "upper_value", "standard_error") := NULL]
MCV[, c("model_version_id", "covariate_name_short", "age_group_id", 
         "age_group_name", "sex_id", "sex") := NULL]

# pivot draws to long format
MCV_draws = melt(MCV,
                 id.vars = c("year_id", "location_id", 
                             "location_name", "covariate_id"), 
                 variable.name = "draw", value.name = "MCV")

# remove "V" from the draw column
MCV_draws[, draw := as.numeric(gsub("V", "", draw))]

# see how many draws are outside the possible value range (0 - 1)
nrow(MCV_draws[MCV < 0 | MCV > 1]) / nrow(MCV_draws) 
# 0.01176797 (1.2% of draws are outside 0-1)

# check mean value before and after setting limits
mean(MCV_draws$MCV) # 0.8547302

# set limits on draws to be between 0 and 1 (percentage)
MCV_draws[MCV < 0, MCV := 0]
MCV_draws[MCV > 1, MCV := 1]

mean(MCV_draws$MCV) # 0.8546852

# save out MCV vaccine coverage draws
output_dir = "FILEPATH"
fwrite(MCV_draws, file.path(output_dir, 'MCV_vaccine_coverage_draws_2023.csv'))
