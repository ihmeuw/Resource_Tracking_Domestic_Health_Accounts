# Get global population numbers for all ages and 65+
# Author: Michael Breshock
# Date: 07/31/2023

# Clear environment
rm(list = ls())

# Loading libraries
library(data.table)

# source shared functions
source("FILEPATH/get_population.R")
source("FILEPATH/get_location_metadata.R")

# get location IDs from metadata
locs <- get_location_metadata(location_set_id=22, release_id = 6) 
# location_set_id=22: -> covariate computation
# release_id = 6 -> GBD 2019
# confirming that all of the location IDs in set 35 are contained in set 22: 
locs35 <- get_location_metadata(location_set_id=35, release_id = 6) 
# location_set_id=35: -> GBD modeling
sum(!(locs35$location_id %in% locs$location_id)) # -> 0
# this confirms that all location IDs in set 35 are contained in set 22.

# save out location metadata for later use: 
fwrite(locs, file = "FILEPATH/location_set_22_metadata.csv")

loc_ids <- locs$location_id # extract location IDs from the location set

# get population for all ages for all countries
all_pop <- get_population(release_id = 6, location_id = loc_ids,
                          year_id = c(1990:2019), sex_id = 3,
                          age_group_id = 22) # 22 -> all ages
all_pop[, run_id := NULL] # remove unnecessary column

# get population for age over 65 
over65_pop <- get_population(release_id = 6, location_id = loc_ids,
                             year_id = c(1990:2019), sex_id = 3,
                             age_group_id = 154) # 154 -> age group 65 plus
over65_pop[, run_id := NULL] # remove unnecessary column
# change column names for merging: 
setnames(over65_pop, old = c("age_group_id", "population"), 
         new = c("age_group_id_over65","pop_over65"))

# merge populations together: 
pop = merge(all_pop, over65_pop, by = c("year_id","location_id","sex_id"))
# add fraction over 65 variable: 
pop[,frac_over65 := pop_over65 / population]

## Get percent of population that is female:
fem_pop <- get_population(release_id = 6, location_id = loc_ids,
                          year_id = c(1990:2019), sex_id = 2, # sex 2 = female
                          age_group_id = 22) # 22 -> all ages
fem_pop[, run_id := NULL] # remove unnecessary column
# change names for merging
setnames(fem_pop, old = c("population","sex_id"), 
         new = c("female_pop", "female_sex_id")) 
# merge female population with overall population:
pop = merge(fem_pop, pop, by = c("year_id", "location_id", "age_group_id"))
# calculate percent of population that is female: 
pop[, pct_fem_pop := female_pop/population]
mean(pop$pct_fem_pop) # 0.5028557 -> makes sense for the most part
range(pop$pct_fem_pop) # 0.2362785 - 0.5688493
# the low end of the range seems a little weird, investigate further: 
# see locations where female population is less than 40%
locs_test = unique(pop[pct_fem_pop < 0.4]$location_id)
locs[location_id %in% locs_test]$location_name
# "Bahrain", "Oman", "Qatar", "United Arab Emirates", "Maldives"

# save out overall population data: 
fwrite(pop, file = "FILEPATH/population_1990_2019.csv")
