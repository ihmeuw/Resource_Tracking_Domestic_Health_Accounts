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
## were pulled successfully for PAF & DALYs
# file path containing age/sex split PAF draws for each individual location: 
PAF_dir = "FILEPATH"
PAF_files = list.files(PAF_dir) # list all file names
# remove any non-PAF draws files (just in case)
PAF_files = PAF_files[grepl("age_split_draws_1995_2023_loc_", PAF_files)]
length(PAF_files) # expecting 204
# verify that all the location IDs match the GBD locaation set
PAF_locs = as.numeric(gsub("age_split_draws_1995_2023_loc_", "", 
                            gsub(".csv", "", PAF_files)))
sum(!loc_ids %in% PAF_locs) # expecting 0


# file path containing age/sex split death draws for each individual location: 
deaths_dir = "FILEPATH"
deaths_files = list.files(deaths_dir) # list all file names
# remove any non-deaths draws files (just in case)
deaths_files = deaths_files[grepl("deaths_split_draws_1995_2023_loc_", deaths_files)]
length(deaths_files) # expecting 204
# verify that all the location IDs match the GBD locaation set
deaths_locs = as.numeric(gsub("deaths_split_draws_1995_2023_loc_", "", 
                              gsub(".csv", "", deaths_files)))
sum(!loc_ids %in% deaths_locs) # expecting 0

# load in PAF and DALY draws for each location and append togther
PAF_splits <- rbindlist(lapply(paste0(PAF_dir, PAF_files), fread))
deaths_splits <- rbindlist(lapply(paste0(deaths_dir, deaths_files), fread))

nrow(PAF_splits) == nrow(deaths_splits) # TRUE
# matching number of rows, which were not in GBD21
# implies that missing PAF estimates have been filled in GBD23

# How many rows should we expect? 
age_n = length(unique(deaths_splits$age_group_id)) # 25 age groups
loc_n = length(unique(deaths_splits$location_id)) # 204 locations
sex_n = length(unique(deaths_splits$sex_id)) # 2 sex groups
years_n = length(unique(deaths_splits$year_id)) # 29 years
# Draws are in wide format, so can ignore those
# all other metrics are single values
# So the number of rows should be: 
expected_nrow = age_n * loc_n * sex_n * years_n # 295800
nrow(deaths_splits) == expected_nrow # TRUE
nrow(PAF_splits) == expected_nrow # TRUE
# how many rows from deaths are missing
expected_nrow - nrow(deaths_splits) # 0


# pivot DALY and PAF draws to long format
deaths_long = melt(deaths_splits, id.vars = c("year_id", "location_id", "measure_id", 
                                              "metric_id", "cause_id", "version_id",
                                              "sex_id", "age_group_id"),
                   variable.name = "draw", value.name = "deaths")

PAF_long = melt(PAF_splits, id.vars = c("year_id", "location_id", "measure_id", 
                                        "metric_id", "cause_id", "version_id",
                                        "sex_id", "age_group_id", "rei_id"),
                variable.name = "draw", value.name = "PAF")

# delete metric_id and version_id columns to avoid merge issues
deaths_long[, c("metric_id", "version_id") := NULL]
PAF_long[, c("metric_id", "version_id") := NULL]

# merge DALY and PAF draws together
PAF_long = merge(PAF_long, deaths_long, 
                 by = c("year_id", "location_id", "measure_id", 
                        "cause_id", "sex_id", "age_group_id", "draw"))

# aggregate sex splits: 
PAF_long = PAF_long[, .(PAF = weighted.mean(PAF, w = deaths), 
                        deaths = sum(deaths), 
                        sex_id = 3), # all sex 
                    by = c("year_id", "location_id", "measure_id", 
                           "cause_id", "age_group_id", "rei_id", "draw")]

## aggregate age splits to the following 3 age bins: 
# Under 5 (ids: 2-3, 388, 389, 238, 34)
# 5 - 49 (ids: 6 - 14)
# 50 plus (ids: 15-20, 30-32, 235)

# create new age group bins
PAF_long[, age_bin := dplyr::case_when(
  age_group_id %in% c(2:3, 388, 389, 238, 34) ~ "Under 5",
  age_group_id %in% c(6:14) ~ "5 to 49",
  age_group_id %in% c(15:20, 30:32, 235) ~ "50 plus", 
  TRUE ~ "other?" # shouldn't be any other
)]
unique(PAF_long$age_bin) 
# [1] "Under 5" "5 to 49" "50 plus"

# aggregate to new age bins
PAF_long = PAF_long[, .(PAF = weighted.mean(PAF, w = deaths), 
                        deaths = sum(deaths)), 
                    by = c("year_id", "location_id", "measure_id", 
                           "cause_id", "sex_id", "age_bin", "rei_id", "draw")]

# save out PAF draws by 3 bin ages
out_dir = "FILEPATH"
fwrite(PAF_long, file.path(out_dir, "PAF_env_occ_deaths_draws_age_bins_1995_2023.csv"))

# summarize draws: 
PAF_means = PAF_long[, .(PAF = mean(PAF), 
                         PAF_lower = quantile(PAF, 0.025), 
                         PAF_upper = quantile(PAF, 0.975), 
                         deaths = mean(deaths), 
                         deaths_lower = quantile(deaths, 0.025), 
                         deaths_upper = quantile(deaths, 0.975)),
                     by = c("year_id", "location_id", "measure_id", 
                            "cause_id", "sex_id", "age_bin", "rei_id")]
# save out PAF means
fwrite(PAF_means, file.path(out_dir, "PAF_env_occ_deaths_age_bins_1995_2023.csv"))
