# Get global Labor Force Participation data
# Author: Michael Breshock
# Date: 11/20/2023

# clear environment
rm(list=ls())

# load libraries and functions 
library(data.table)
library(dplyr)
library("metR", lib.loc = "FILEPATH")
source("FILEPATH/get_population.R")

# load lfp model results from output folder: 
results_dir = "FILEPATH"
lfp_files = paste0(results_dir, list.files(results_dir))

# get location IDs from metadata
locs <- fread("FILEPATH/location_set_22_metadata.csv")
# location_set_id=22: -> covariate computation
loc_ids <- locs[level == 3]$location_id

lfp_wide = rbindlist(lapply(lfp_files, fread))

# melt from wide to long
lfp_long = melt(lfp_wide, 
                id.vars = c("year_id", "location_id", "age_group_id", "sex_id"),
                variable.name = "draw", value.name = "lfp")

# filter to years of interest and country level estimates
lfp = lfp_long[year_id %in% c(1990:2019) & location_id %in% loc_ids]

# add missing age groups from LIS data: 19-21 (70 - 80+)
# for ages 70-74 (id 19) set LFP to 50% of age group 18 estimates (65-69)
# for ages 75+ (id 20,21) set LFP to zero

# create age group id 19 estimates:
age_group18 = lfp[age_group_id == 18]
age_group19 = copy(age_group18)
age_group19[, ":=" (lfp = lfp/2, 
                    age_group_id = 19)]
# sanity check:
sum(2*age_group19$lfp != age_group18$lfp) # 0

# create rest of missing age groups 
age_group20 = copy(age_group18)
age_group20[, ":=" (lfp = 0, 
                    age_group_id = 20)]

age_group21 = copy(age_group18)
age_group21[, ":=" (lfp = 0, 
                    age_group_id = 21)]

# bind new age group data with modeled data: 
lfp_all_ages = rbind(lfp, age_group19, age_group20, age_group21)

# run this so Rstudio doesnt crash (goes over 100GB memory otherwise)
rm(lfp_wide, lfp_long, age_group18, 
   age_group19, age_group20, age_group21)
gc()


# get age_group_ids from lfp results:
ages = unique(lfp_all_ages$age_group_id)
# get population numbers to do population weighted age group aggregation
# using release_id 16 here because that is what was used for the LFP data
pop = get_population(release_id = 16, location_id = loc_ids,
                     year_id = c(1990:2019), sex_id = c(1,2),
                     age_group_id = ages)
pop[, run_id := NULL]

# merge population into lfp data: 
lfp_pop = merge(lfp_all_ages, pop, 
                by = c("year_id", "location_id", "sex_id", "age_group_id"))

# aggregate estimates to be for all ages: 
lfp_sum = lfp_pop[, .(lfp = weighted.mean(lfp, w = population), 
                      population = sum(population)), 
                  by = c("year_id", "location_id", "sex_id", "draw")]

# save out LFP data: 
fwrite(lfp_sum, file = "FILEPATH/lfp_draws_by_sex_1990_2019.csv")

### Sensitivity analysis: 
# cut LFP in half ->
# assuming that people who are caregiving are less likely to have been working 
lfp_pop_sens = copy(lfp_pop)
lfp_pop_sens[, lfp_half := lfp/2]

# aggregate estimates to be for all ages: 
lfp_sens = lfp_pop_sens[, .(lfp_half = weighted.mean(lfp_half, w = population),
                            population = sum(population)), 
                        by = c("year_id","location_id","sex_id","draw")]

fwrite(lfp_sens, "FILEPATH/sensitivity_lfp_draws_by_sex_1990_2019.csv")

################################### FORECAST ###################################
# just need the 2019 data for this: 
lfp2019 = lfp_all_ages[year_id == 2019]

# clear memory: 
rm(lfp, lfp_all_ages, pop, lfp_pop, lfp_sum, lfp_pop_sens, lfp_sens)
gc()
## create LFP forecasts by: 
# taking static LFP rates from 2019 by age group / sex and
# aggregate LFP to just sex splits by weighted average using 
# forecasted population by age group as weights 

# get US population forecasts for 2020 - 2050: 
forecast_file = "FILEPATH/population.nc"
pop_forecast = ReadNetCDF(forecast_file, 
                          subset = list(year_id = c(2020, 2050), 
                                        location_id = loc_ids, 
                                        age_group_id = c(ages, 30, 31, 32, 235), 
                                        scenario = 0)) # reference scenario

# age group id 21 (80+) not available in pop forecast file 
# need to roll up age groups (30, 31, 32, 235) [80-84, 85-89, 90-94, 95+]
pop80plus = pop_forecast[age_group_id %in% c(30, 31, 32, 235)]
under80pop = pop_forecast[age_group_id %in% ages]

pop_age_group21 = pop80plus[,.(population = sum(population), 
                               age_group_id = 21), 
                            by = c("year_id", "location_id", "sex_id", 
                                   "draw", "scenario")]
# combine rolled up 80+ population with under 80 population 
pop_fix = rbind(under80pop, pop_age_group21)
# change draw variable to allow for merging with LFP: 
pop_fix[, draw := paste0("draw_", draw)]

# merge 2019 lfp rates with population forecasts: 
# remove year_id from lfp data to allow for merge
lfp2019[,year_id := NULL]
lfp_forecast = merge(pop_fix, lfp2019, 
                     by = c("location_id", "age_group_id", "sex_id", "draw"))

# aggregate age groups: 
lfp_forecast_sum = lfp_forecast[,.(lfp = weighted.mean(lfp, w = population), 
                                   pop = sum(population)), 
                                by = c("year_id", "location_id", "sex_id", "draw")]

# save out forecasts: 
fwrite(lfp_forecast_sum, "FILEPATH/lfp_forecast_draws_by_sex_2020_2050.csv")

### Sensitivity analysis: 
# cut LFP in half ->
# assuming that people who are caregiving are less likely to have been working 
lfp_forecast_sens = copy(lfp_forecast)
lfp_forecast_sens[, lfp_half := lfp/2]

# aggregate estimates to be for all ages: 
lfp_forecast_sens_sum = lfp_forecast_sens[, .(lfp_half = weighted.mean(lfp_half, w = population),
                                              population = sum(population)), 
                                          by = c("year_id","location_id","sex_id","draw")]

fwrite(lfp_forecast_sens_sum, "FILEPATH/sensitivity_lfp_forecast_draws_by_sex_2020_2050.csv")
