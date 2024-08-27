# Merge clean diagnosis extraction data with covariates
# Author: Michael Breshock
# Date: 07/28/2023

# Clear environment
rm(list = ls())

# Loading libraries
library(data.table)

# load data
data = fread("FILEPATH/00_clean_combined_data.csv")
pop = fread("FILEPATH/population_1990_2019.csv")
prev = fread("FILEPATH/all_age_sex_dementia_prevalence_1990_2019.csv")
cov = fread("FILEPATH/all_covars_1990_2019.csv")

# add location ids to extraction data: 
locs <- fread(file = "FILEPATH/location_set_22_metadata.csv")
data = merge(data, locs[,.(location_id, ihme_loc_id)], by = "ihme_loc_id")

# remove unnecessary column from pop data: 
pop[, age_group_id_over65 := NULL]
# merge population into data: 
data_pop = merge(data, pop, by = c("year_id","location_id"))

# merge in prevalence data: 
data_prev = merge(data_pop, prev[,.(year_id, location_id, age_group_id, sex_id, 
                                    prevalence, prevalence_lower, prevalence_upper)],
                  by = c("year_id", "location_id", "age_group_id", "sex_id"))

# merge in covariates: 
data_all = merge(data_prev, cov, by = c("year_id", "location_id"))

# save out data: 
fwrite(data_all, "FILEPATH/01_clean_data_covars.csv")
