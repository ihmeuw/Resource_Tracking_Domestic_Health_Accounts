##########################################################################
### Author: USERNAME
### Date: 08/15/2024
### Project: Health Spending Effectiveness 
### Purpose: Process extracted age/sex split population attributable fraction draws
###########################################################################

# Clean the environment 
rm(list=ls()) 


# load libraries
library(data.table)

# get GBD location ids: 
source(paste0(functions_dir,"get_location_metadata.R"))
# get location IDs from metadata; location_set_id=35: -> Model outputs
locs <- get_location_metadata(location_set_id = 35, 
                              release_id = 9)  # GBD 2021
# extract country location IDs from the location set (country = level 3)
loc_ids <- locs[level == 3]$location_id 

## Number of files saved out and verify that all locations 
## were pulled successfully for PAF & DALYs
# file path containing age/sex split PAF draws for each individual location: 
PAF_dir = "FILEPATH"
PAF_files = list.files(PAF_dir) # list all file names
# remove any non-PAF draws files (just in case)
PAF_files = PAF_files[grepl("age_split_draws_1995_2023_loc_", PAF_files)]
length(PAF_files) # expecting 204 - check
# verify that all the location IDs match the GBD locaation set
PAF_locs = as.numeric(gsub("age_split_draws_1995_2023_loc_", "", 
                            gsub(".csv", "", PAF_files)))
sum(!loc_ids %in% PAF_locs) # expecting 0 - check


# file path containing age/sex split DALY draws for each individual location: 
DALY_dir = "FILEPATH"
DALY_files = list.files(DALY_dir) # list all file names
# remove any non-DALY draws files (just in case)
DALY_files = DALY_files[grepl("age_split_draws_1995_2023_loc_", DALY_files)]
length(DALY_files) # expecting 204 - check
# verify that all the location IDs match the GBD locaation set
DALY_locs = as.numeric(gsub("age_split_draws_1995_2023_loc_", "", 
                           gsub(".csv", "", DALY_files)))
sum(!loc_ids %in% DALY_locs) # expecting 0 - check

# load in PAF and DALY draws for each location and append togther
PAF_splits <- rbindlist(lapply(paste0(PAF_dir, PAF_files), fread))
DALY_splits <- rbindlist(lapply(paste0(DALY_dir, DALY_files), fread))

nrow(PAF_splits) == nrow(DALY_splits) # TRUE
# matching number of rows, which were not in GBD21
# implies that missing PAF estimates have been filled in GBD23

# expected number of rows
age_n = length(unique(DALY_splits$age_group_id)) # 25 age groups
loc_n = length(unique(DALY_splits$location_id)) # 204 locations
sex_n = length(unique(DALY_splits$sex_id)) # 2 sex groups
years_n = length(unique(DALY_splits$year_id)) # 29 years
# draws are in wide format
# all other metrics are single values
# expected number of rows: 
expected_nrow = age_n * loc_n * sex_n * years_n # 295800
nrow(DALY_splits) == expected_nrow # TRUE
nrow(PAF_splits) == expected_nrow # TRUE
# Number of missing rows from PAF
expected_nrow - nrow(PAF_splits) # 0

# pivot DALY and PAF draws to long format
DALY_long = melt(DALY_splits, id.vars = c("year_id", "location_id", "measure_id", 
                                          "metric_id", "cause_id", "version_id",
                                          "sex_id", "age_group_id"),
                 variable.name = "draw", value.name = "DALY")

PAF_long = melt(PAF_splits, id.vars = c("year_id", "location_id", "measure_id", 
                                        "metric_id", "cause_id", "version_id",
                                        "sex_id", "age_group_id", "rei_id"),
                variable.name = "draw", value.name = "PAF")

# delete metric_id and version_id columns to avoid merge issues
DALY_long[, c("metric_id", "version_id") := NULL]
PAF_long[, c("metric_id", "version_id") := NULL]

# merge DALY and PAF draws together
PAF_long = merge(PAF_long, DALY_long, 
                 by = c("year_id", "location_id", "measure_id", 
                        "cause_id", "sex_id", "age_group_id", "draw"))

# aggregate sex splits: 
PAF_long = PAF_long[, .(PAF = weighted.mean(PAF, w = DALY), 
                        DALY = sum(DALY), 
                        sex_id = 3), # all sex 
                    by = c("year_id", "location_id", "measure_id", 
                           "cause_id", "age_group_id", "rei_id", "draw")]

# create new age group bins
PAF_long[, age_bin := dplyr::case_when(
  age_group_id %in% c(2:3, 388, 389, 238, 34) ~ "Under 5",
  age_group_id %in% c(6:14) ~ "5 to 49",
  age_group_id %in% c(15:20, 30:32, 235) ~ "50 plus", 
  TRUE ~ "other?" # shouldn't be any other
)]
unique(PAF_long$age_bin) 

# aggregate to new age bins
PAF_long = PAF_long[, .(PAF = weighted.mean(PAF, w = DALY), 
                        DALY = sum(DALY)), 
                    by = c("year_id", "location_id", "measure_id", 
                           "cause_id", "sex_id", "age_bin", "rei_id", "draw")]

# save out PAF draws by 3 bin ages
out_dir = "FILEPATH"
fwrite(PAF_long, file.path(out_dir, "PAF_draws_age_bins_1995_2023.csv"))

# summarize draws: 
PAF_means = PAF_long[, .(PAF = mean(PAF), 
                         PAF_lower = quantile(PAF, 0.025), 
                         PAF_upper = quantile(PAF, 0.975), 
                         DALY = mean(DALY), 
                         DALY_lower = quantile(DALY, 0.025), 
                         DALY_upper = quantile(DALY, 0.975)),
                     by = c("year_id", "location_id", "measure_id", 
                            "cause_id", "sex_id", "age_bin", "rei_id")]
# save out PAF means
fwrite(PAF_means, file.path(out_dir, "PAF_age_bins_1995_2023.csv"))

##### Create all age PAF file 
# sum to all ages
PAF_all = PAF_long[, .(PAF = weighted.mean(PAF, w = DALY), 
                       DALY = sum(DALY), 
                       age_group_id = 22), 
                   by = c("year_id", "location_id", "measure_id", 
                          "cause_id", "sex_id", "rei_id", "draw")]

# merge in ihme_loc_id
PAF_all = merge(PAF_all, locs[,.(location_id, ihme_loc_id)], by = "location_id")

# save out PAF draws by all ages
fwrite(PAF_all, file.path(out_dir, "PAF_draws_all_age_1995_2023.csv"))

PAF_all_means = PAF_all[, .(PAF = mean(PAF), 
                            PAF_lower = quantile(PAF, 0.025), 
                            PAF_upper = quantile(PAF, 0.975), 
                            DALY = mean(DALY), 
                            DALY_lower = quantile(DALY, 0.025), 
                            DALY_upper = quantile(DALY, 0.975)),
                        by = c("year_id", "location_id", "ihme_loc_id", "measure_id", 
                               "cause_id", "sex_id", "age_group_id", "rei_id")]

# save out PAF means
fwrite(PAF_all_means, file.path(out_dir, "PAF_all_age_1995_2023.csv"))

########################## CREATE AGE-STANDARDIZED PAF #########################
# create new age group bins in PAF split data
PAF_splits[, age_bin := dplyr::case_when(
  age_group_id %in% c(2:3, 388, 389, 238, 34) ~ "Under 5",
  age_group_id %in% c(6:14) ~ "5 to 49",
  age_group_id %in% c(15:20, 30:32, 235) ~ "50 plus", 
  TRUE ~ "other" # should not be any other
)]

## Age 5 to 49 - WITH injuries in DALYs
# read in DALY data
DALY_dir = "FILEPATH"
five49_DALY = fread(file.path(DALY_dir, "DALY_5to49_split_draws_1995_2021.csv"))

# filter to age bin of interest
PAF_five49_splits = PAF_splits[age_bin == "5 to 49"]

# melt draws from wide to long
PAF_five49_std = melt(PAF_five49_splits, id.vars = c("year_id", "location_id", "measure_id", 
                                                     "metric_id", "cause_id", "version_id",
                                                     "sex_id", "age_group_id", "age_bin", "rei_id"),
                      variable.name = "draw", value.name = "PAF")

# merge age standardized DALY counts with PAF data (to use for weighted means)
PAF_five49_std = merge(PAF_five49_std, 
                       five49_DALY[,.(year_id, location_id, sex_id, age_group_id, 
                                      age_bin, draw, DALY_std)], 
                       by = c("year_id", "location_id", "sex_id", 
                              "age_group_id", "age_bin", "draw"))

# aggregate to new age bins and all sex
PAF_five49_std = PAF_five49_std[, .(PAF_std = weighted.mean(PAF, w = DALY_std), 
                                    DALY_std = sum(DALY_std), 
                                    sex_id = 3), 
                                by = c("year_id", "location_id", "measure_id", 
                                       "cause_id", "age_bin", "rei_id", "draw")]
# save out draws 
out_dir = "FILEPATH"
fwrite(PAF_five49_std, file.path(out_dir, "PAF_draws_5to49_age_std_1995_2021.csv"))

# summarize draws: 
PAF_five49_std_means = PAF_five49_std[, .(PAF_std = mean(PAF_std), 
                                          PAF_std_lower = quantile(PAF_std, 0.025), 
                                          PAF_std_upper = quantile(PAF_std, 0.975), 
                                          DALY_std = mean(DALY_std), 
                                          DALY_std_lower = quantile(DALY_std, 0.025), 
                                          DALY_std_upper = quantile(DALY_std, 0.975)),
                                      by = c("year_id", "location_id", "measure_id", 
                                             "cause_id", "sex_id", "age_bin", "rei_id")]

# save out 5 to 49 age-standardized PAF means
fwrite(PAF_five49_std_means, file.path(out_dir, "PAF_5to49_age_std_1995_2021.csv"))

## Age 5 to 49 - No Injuries in DALYs
# read in DALY data
DALY_dir = "FILEPATH"
five49_DALY_no_inj = fread(file.path(DALY_dir, "DALY_5to49_split_draws_no_inj_1995_2021.csv"))

# melt draws from wide to long
PAF_five49_std_no_inj = melt(PAF_five49_splits, id.vars = c("year_id", "location_id", "measure_id", 
                                                            "metric_id", "cause_id", "version_id",
                                                            "sex_id", "age_group_id", "age_bin", "rei_id"),
                             variable.name = "draw", value.name = "PAF")

# merge age standardized DALY counts with PAF data (to use for weighted means)
PAF_five49_std_no_inj = merge(PAF_five49_std_no_inj, 
                              five49_DALY_no_inj[,.(year_id, location_id, sex_id, 
                                                    age_group_id, age_bin, draw, DALY_std)], 
                              by = c("year_id", "location_id", "sex_id", 
                                     "age_group_id", "age_bin", "draw"))

# aggregate to new age bins and all sex
PAF_five49_std_no_inj = PAF_five49_std_no_inj[, .(PAF_std = weighted.mean(PAF, w = DALY_std), 
                                                  DALY_std = sum(DALY_std), 
                                                  sex_id = 3), 
                                              by = c("year_id", "location_id", "measure_id", 
                                                     "cause_id", "age_bin", "rei_id", "draw")]
# save out draws 
out_dir = "FILEPATH"
fwrite(PAF_five49_std_no_inj, file.path(out_dir, "PAF_draws_5to49_age_std_no_inj_1995_2021.csv"))

# summarize draws: 
PAF_five49_std_no_inj_means = PAF_five49_std_no_inj[, .(PAF_std = mean(PAF_std), 
                                                        PAF_std_lower = quantile(PAF_std, 0.025), 
                                                        PAF_std_upper = quantile(PAF_std, 0.975), 
                                                        DALY_std = mean(DALY_std), 
                                                        DALY_std_lower = quantile(DALY_std, 0.025), 
                                                        DALY_std_upper = quantile(DALY_std, 0.975)),
                                                    by = c("year_id", "location_id", "measure_id", 
                                                           "cause_id", "sex_id", "age_bin", "rei_id")]

# save out 5 to 49 age-standardized PAF means
fwrite(PAF_five49_std_no_inj_means, file.path(out_dir, "PAF_5to49_age_std_no_inj_1995_2021.csv"))

## Age Over 50 
# read in DALY data
DALY_dir = "FILEPATH"
over50_DALY = fread(file.path(DALY_dir, "DALY_over50_split_draws_1995_2021.csv"))

# filter to age bin of interest
PAF_over50_splits = PAF_splits[age_bin == "50 plus"]

# melt draws from wide to long
PAF_over50_std = melt(PAF_over50_splits, id.vars = c("year_id", "location_id", "measure_id", 
                                                     "metric_id", "cause_id", "version_id",
                                                     "sex_id", "age_group_id", "age_bin", "rei_id"),
                      variable.name = "draw", value.name = "PAF")

# merge age standardized DALY counts with PAF data (to use for weighted means)
PAF_over50_std = merge(PAF_over50_std, 
                       over50_DALY[,.(year_id, location_id, sex_id, age_group_id, 
                                      age_bin, draw, DALY_std)], 
                       by = c("year_id", "location_id", "sex_id", 
                              "age_group_id", "age_bin", "draw"))

# aggregate to new age bins and all sex
PAF_over50_std = PAF_over50_std[, .(PAF_std = weighted.mean(PAF, w = DALY_std), 
                                    DALY_std = sum(DALY_std), 
                                    sex_id = 3), 
                                by = c("year_id", "location_id", "measure_id", 
                                       "cause_id", "age_bin", "rei_id", "draw")]
# save out draws 
out_dir = "FILEPATH"
fwrite(PAF_over50_std, file.path(out_dir, "PAF_draws_over50_age_std_1995_2021.csv"))

# summarize draws: 
PAF_over50_std_means = PAF_over50_std[, .(PAF_std = mean(PAF_std), 
                                          PAF_std_lower = quantile(PAF_std, 0.025), 
                                          PAF_std_upper = quantile(PAF_std, 0.975), 
                                          DALY_std = mean(DALY_std), 
                                          DALY_std_lower = quantile(DALY_std, 0.025), 
                                          DALY_std_upper = quantile(DALY_std, 0.975)),
                                      by = c("year_id", "location_id", "measure_id", 
                                             "cause_id", "sex_id", "age_bin", "rei_id")]

# save out 5 to 49 age-standardized PAF means
fwrite(PAF_over50_std_means, file.path(out_dir, "PAF_over50_age_std_1995_2021.csv"))

