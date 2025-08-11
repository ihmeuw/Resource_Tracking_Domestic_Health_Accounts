##########################################################################
### Author: USERNAME
### Date: 08/15/2024
### Project: Health Spending Effectiveness 
### Purpose: Process extracted age/sex split PAF draws
###########################################################################

# Clean the environment 
rm(list=ls()) 

# set up file path roots
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

# load libraries
library(data.table)

# get GBD location ids: 
source(paste0(functions_dir,"get_location_metadata.R"))
# get location IDs from metadata; location_set_id=35: -> Model outputs
locs <- get_location_metadata(location_set_id = 35, 
                              release_id = 9)  # GBD 2021
# extract country location IDs from the location set (country = level 3)
loc_ids <- locs[level == 3]$location_id 

## See how many files were saved out and verify that all locations 
## were pulled successfully for PAF

# file path containing child joint PAF draws for each individual location: 
PAF_dir = "FILEPATH"
child_PAF_files = list.files(PAF_dir) # list all file names
# remove any non-PAF draws files (just in case)
child_PAF_files = child_PAF_files[grepl("joint_child_PAF_DALYS_1995_2023_loc_", child_PAF_files)]
length(child_PAF_files) # expecting 204 - check
# verify that all the location IDs match the GBD locaation set
child_PAF_locs = as.numeric(gsub("joint_child_PAF_DALYS_1995_2023_loc_", "", 
                            gsub(".csv", "", child_PAF_files)))
sum(!loc_ids %in% child_PAF_locs) # expecting 0 - check


# file path containing adult joint PAF draws for each individual location: 
PAF_dir = "FILEPATH"
adult_PAF_files = list.files(PAF_dir) # list all file names
# remove any non-deaths draws files (just in case)
adult_PAF_files = adult_PAF_files[grepl("joint_adult_PAF_DALYS_1995_2023_loc_", adult_PAF_files)]
length(adult_PAF_files) # expecting 204
# verify that all the location IDs match the GBD locaation set
adult_PAF_locs = as.numeric(gsub("joint_adult_PAF_DALYS_1995_2023_loc_", "", 
                              gsub(".csv", "", adult_PAF_files)))
sum(!loc_ids %in% adult_PAF_locs) # expecting 0

# load in draws for each location and rbind togther
child_PAF_all <- rbindlist(lapply(paste0(PAF_dir, child_PAF_files), fread))
adult_PAF_all <- rbindlist(lapply(paste0(PAF_dir, adult_PAF_files), fread))

nrow(child_PAF_all) == nrow(adult_PAF_all) # TRUE
# matching number of rows, which were not in GBD21
# implies that missing PAF estimates have been filled in GBD23

# How many rows should we expect? 
age_n = length(unique(child_PAF_all$age_group_id)) # 25 
loc_n = length(unique(child_PAF_all$location_id)) # 204 locations
sex_n = length(unique(child_PAF_all$sex_id)) # 2
years_n = length(unique(child_PAF_all$year_id)) # 29 years
# Draws are in wide format
# all other metrics are single values
# So the number of rows should be: 
expected_nrow = age_n * loc_n * sex_n * years_n # 295800
nrow(child_PAF_all) == expected_nrow # TRUE
nrow(adult_PAF_all) == expected_nrow # TRUE
# how many rows from deaths are missing
expected_nrow - nrow(adult_PAF_all) # 0

adult_PAF_all[, rei_id := NULL]
child_PAF_all[, rei_id := NULL]

# pivot DALY and PAF draws to long format
child_PAF_long = melt(child_PAF_all, id.vars = c("year_id", "location_id", "measure_id", 
                                              "metric_id", "cause_id", "version_id",
                                              "sex_id", "age_group_id"),
                   variable.name = "draw", value.name = "PAF")

adult_PAF_long = melt(adult_PAF_all, id.vars = c("year_id", "location_id", "measure_id", 
                                        "metric_id", "cause_id", "version_id",
                                        "sex_id", "age_group_id"),
                variable.name = "draw", value.name = "PAF")

# delete metric_id and version_id columns to avoid merge issues
child_PAF_long[, c("metric_id", "version_id") := NULL]
adult_PAF_long[, c("metric_id", "version_id") := NULL]


# save out child and adult PAF draws for use in the risk-deleted DALY processing script
out_dir = "FILEPATH"
fwrite(child_PAF_long, file.path(out_dir, "child_joint_PAF_draws_1995_2023.csv"))
fwrite(adult_PAF_long, file.path(out_dir, "adult_joint_PAF_draws_1995_2023.csv"))

# summarize draws to vet the PAFs
PAF_means = adult_PAF_long[, .(PAF = mean(PAF),
                               PAF_lower = quantile(PAF, 0.025),
                               PAF_upper = quantile(PAF, 0.975)),
                           by = c("year_id", "location_id", "measure_id",
                                  "cause_id", "sex_id")]
summary(PAF_means$PAF) 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.1897  0.3195  0.3772  0.3797  0.4399  0.6358 

PAF_means = child_PAF_long[, .(PAF = mean(PAF),
                               PAF_lower = quantile(PAF, 0.025),
                               PAF_upper = quantile(PAF, 0.975)),
                           by = c("year_id", "location_id", "measure_id",
                                  "cause_id", "sex_id")]
summary(PAF_means$PAF) 
