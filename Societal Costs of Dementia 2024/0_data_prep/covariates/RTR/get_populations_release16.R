# Get global population numbers for all ages and 65+
# Also get percent of population that is female
# Author: Michael Breshock
# Date: 04/16/2024

## See README

# Clear environment
rm(list = ls())

# Loading libraries
library(data.table)

# source shared functions
source("FILEPATH/get_population.R")
source("FILEPATH/get_location_metadata.R")

# initalize shared function input variables
release = 16 # GBD 2023
years =  c(1990:2024)

# get location IDs from metadata; location_set_id=22: -> covariate computation
locs <- get_location_metadata(location_set_id=22, 
                              release_id = release) 
loc_ids <- locs$location_id # extract location IDs from the location set

# get population for all ages for all countries
all_pop <- get_population(release_id = release, 
                          location_id = loc_ids,
                          year_id = years, 
                          sex_id = 3, # all sex
                          age_group_id = 22) # 22 -> all ages
# pulled run_id 355

# get population for age over 65 
over65_pop <- get_population(release_id = release, 
                             location_id = loc_ids,
                             year_id = years, 
                             sex_id = 3, # all sex
                             age_group_id = 154) # 154 -> age group 65 plus
# pulled run_id 355

# change column names for merging: 
setnames(over65_pop, old = c("age_group_id", "population"), 
         new = c("age_group_id_over65","pop_over65"))

# merge populations together: 
pop = merge(all_pop, over65_pop, 
            by = c("year_id","location_id","sex_id","run_id"))
# add fraction over 65 variable: 
pop[,frac_over65 := pop_over65 / population]

## Get percent of population that is female:
fem_pop <- get_population(release_id = release, 
                          location_id = loc_ids,
                          year_id = years, 
                          sex_id = 2, # sex 2 = female
                          age_group_id = 22) # 22 -> all ages

# change names for merging
setnames(fem_pop, old = c("population","sex_id"), 
         new = c("female_pop", "female_sex_id")) 
# merge female population with overall population:
pop = merge(fem_pop, pop, 
            by = c("year_id", "location_id", "age_group_id", "run_id"))

# calculate percent of population that is female: 
pop[, pct_fem_pop := female_pop/population]
mean(pop$pct_fem_pop) # 0.5018666 -> makes sense for the most part
range(pop$pct_fem_pop) # 0.2377142 - 0.5749967
# the low end of the range seems off, investigate further: 
# see locations where female population is less than 40%
locs_test = unique(pop[pct_fem_pop < 0.4]$location_id)
locs[location_id %in% locs_test]$location_name
# "Bahrain", "Oman", "Qatar", "United Arab Emirates", "Maldives"

# save out overall population data: 
out_dir = "FILEPATH"
fwrite(pop, file = paste0(out_dir, "populations_release16_1990_2024.csv"))
