# Get age-standardized diabetes prevalence numbers globally from dementia envelope
# Author: Michael Breshock
# Date: 08/25/2023

# Clear environment
rm(list = ls())

# Loading libraries
library(data.table)

# loading shared functions 
source("FILEPATH/get_outputs.R")

# load location metdata: 
locs = fread("FILEPATH/location_set_22_metadata.csv")
loc_ids = locs$location_id

# pull diabetes prevalence
dm <- get_outputs(topic = "cause",
                  measure_id = 5,  # prevalence
                  metric_id = 3,   # rate
                  location_id = loc_ids,   # location set 22 (covariate computation)
                  cause_id = 587, # Diabetes mellitus
                  release_id = 6,   # GBD 2019
                  age_group_id = 27,   # 27 is age-standardized rates
                  sex_id = 3, # both sex
                  year_id = c(1990:2019))

# checking that cause_id 587 diabetes mellitus is the sum of 
# cause_id 975 & 976 diabetes mellitus type 1 & 2
dm_type12 <- get_outputs(topic = "cause",
                  measure_id = 5,  # prevalence
                  metric_id = 3,   # rate
                  location_id = loc_ids,   # location set 22 (covariate computation)
                  cause_id = c(975, 976), # diabetes type 1,2
                  release_id = 6,   # GBD 2019
                  age_group_id = 27,   # 27 is age-standardized rates
                  sex_id = 3, # both sex
                  year_id = c(1990:2019))

# summing type 1 & 2 to get overall prevalence
dm_type_sum = dm_type12[, .(dm12_sum = sum(val)), by = c("year_id","location_id")]
# merging both estimates together
both_dm = merge(dm[,.(year_id, location_id, val)], dm_type_sum, 
                by = c("year_id", "location_id"))
# calculating difference between the two estimates
both_dm[, diff := abs(val - dm12_sum)]
mean(both_dm$diff, na.rm = T) # 4.262204e-18 -> basically zero
range(both_dm$diff, na.rm = T) # (0.000000e+00 - 5.551115e-17) -> basically zero
# NAs from location IDs 44533 44793 44794 44795 44796 44797 44798 44799 44800 
# these are subnationals from varying GBD versions (specifically regions in china & kenya)

# confirmed that cause_id 587 diabetes mellitus is the combination of DM type 1 & 2

# change column names and remove unneccessary columns: 
dm_out = dm[,.(year_id, location_id, age_group_id, metric_id, measure_id, 
               sex_id, cause_name, val, lower, upper)] 
setnames(dm_out, old = c("val", "lower", "upper"), 
         new = c("DM_prevalence", "DM_prevalence_lower", "DM_prevalence_upper"))

# save out diabetes prevalence covariate file
fwrite(dm_out, file = "FILEPATH/age_std_diabetes_prevalence_1990_2019.csv")
