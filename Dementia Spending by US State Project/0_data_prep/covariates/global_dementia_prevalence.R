# Get dementia prevalence numbers globally from dementia envelope
# Author: Michael Breshock
# Date: 07/28/2023

# Clear environment
rm(list = ls())

# Loading libraries
library(data.table)

# source shared functions
source("FILEPATH/get_location_metadata.R")
source("FILEPATH/interpolate.R")

# get location IDs from metadata
locs <- get_location_metadata(location_set_id=22, release_id = 6) 
# location_set_id=22: -> covariate computation
# release_id = 6 -> GBD 2019
loc_ids <- locs[level == 3]$location_id #filter to country level 
# add Hong Kong
HK_id <- 354
loc_ids <- append(loc_ids, HK_id)

# loading in Paola's prevelance file just to view for comparison
prev_pp = fread("FILEPATH/19.06.17.prev.csv")

# try splitting loc_ids into multiple groups 
loc_list <- list()
loc_list <- split(loc_ids, ceiling(seq_along(loc_ids)/10))


# get interpolated dementia prevalence for 1990-2019
# using location_ids from get_location_metadata:
prev_draws <- data.table()
for (i in 1:length(loc_list)){
  #print(paste0("loc list ",i," ", loc_list[[i]]))
  prev_draws_temp <- interpolate(gbd_id_type = "modelable_entity_id", # MEID 24351
                            gbd_id = 24351, # Dementia prevalence from DisMod, post-mortality modeling
                            location_id = loc_list[[i]], # global
                            measure_id = 5, # prevalence
                            source = "epi",
                            sex_id = 3, # all sex
                            reporting_year_start = 1990,
                            reporting_year_end = 2019,
                            interp_method = "linear",
                            downsample = TRUE,
                            n_draws = 1000, # get 1000 draws 
                            age_group_id = 22, # age_group_id = 22: all ages
                            num_workers = 10, # set this to the number of threads your session is using
                            release_id = 6) # GBD 2019
  
  # save out each chunk in case R crashes during this loop
  fwrite(prev_draws_temp, file = paste0("FILEPATH/all_age_sex_dementia_prevalence_draws_loc_list",i,".csv"))
  
}

# get mean, lower, and upper bounds from draws:
draw_cols = colnames(prev_draws)[grepl("draw", colnames(prev_draws))]
prev_draws[, ":=" (prevalence = apply(subset(prev_draws, select = draw_cols), 1, mean),
                   prevalence_lower = apply(subset(prev_draws, select = draw_cols), 1, quantile, probs =.025),
                   prevalence_upper = apply(subset(prev_draws, select = draw_cols), 1, quantile, probs =.975))]
# use this to see which columns to keep:
colnames(prev_draws)[!grepl("draw", colnames(prev_draws))]
# select columns of interest: 
prev = prev_draws[,.(year_id, location_id, age_group_id, sex_id, prevalence, 
                     prevalence_lower, prevalence_upper, measure_id, metric_id, 
                     modelable_entity_id)]
# save prevalence : 
fwrite(prev, file = "FILEPATH/all_age_sex_dementia_prevalence_1990_2019.csv")

################## Add Hong Kong to Dementia Prevalence data ###################
# get dementia prevalence draws for Hong Kong
HK_prev = interpolate(gbd_id_type = "modelable_entity_id", # MEID 24351
                      gbd_id = 24351, # Dementia prevalence from DisMod, post-mortality modeling
                      location_id = 354, # hong kong
                      measure_id = 5, # prevalence
                      source = "epi",
                      sex_id = 3, # all sex
                      reporting_year_start = 1990,
                      reporting_year_end = 2019,
                      interp_method = "linear",
                      downsample = TRUE,
                      age_group_id = 22, # age_group_id = 22: all ages
                      num_workers = 50, # set this to the number of threads your session is using
                      release_id = 6) # GBD 2019

# get mean, lower, and upper bounds from draws:
draw_cols = colnames(HK_prev)[grepl("draw", colnames(HK_prev))]
HK_prev[, ":=" (prevalence = apply(subset(HK_prev, select = draw_cols), 1, mean),
                prevalence_lower = apply(subset(HK_prev, select = draw_cols), 1, quantile, probs =.025),
                prevalence_upper = apply(subset(HK_prev, select = draw_cols), 1, quantile, probs =.975))]
# select columns of interest: 
HK = HK_prev[,.(year_id, location_id, age_group_id, sex_id, prevalence, 
                prevalence_lower, prevalence_upper, measure_id, metric_id, 
                modelable_entity_id)]

# read in previous prevalence data (created above ^)
dem_prev = fread("FILEPATH/all_age_sex_dementia_prevalence_1990_2019.csv")

global_prev = rbind(dem_prev, HK)

# save out prevalence now with hong kong included: 
fwrite(global_prev, file = "FILEPATH/all_age_sex_dementia_prevalence_1990_2019_HK.csv")
