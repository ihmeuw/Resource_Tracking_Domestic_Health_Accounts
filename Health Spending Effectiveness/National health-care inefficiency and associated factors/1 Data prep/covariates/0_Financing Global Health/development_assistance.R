##########################################################################
### Author: USERNAME
### Date: 04/08/2024
### Project: Health Spending Effectiveness 
### Purpose: Extracting development assistance for health data from FGH
###########################################################################

# clear environment
rm(list=ls())


# load libraries
library(data.table)
library(dplyr)
library(ggplot2)
library(feather)
# source functions 
source("FILEPATH/helper_functions.R")
source("FILEPATH/currency_conversion.R")
source(file.path("FILEPATH", "get_location_metadata.R"))
source(paste0("FILEPATH","get_population.R"))

################################### FGH 2024 ###################################
# initialize FGH2024 directory
fgh_dir = 'FILEPATH'

# get development assistance for health data from feather file
dah_totes = read_feather(file.path(fgh_dir, "dah_totes_feather.feather"))

# convert to data.table
setDT(dah_totes)

# filter to years of interest
dah_totes = dah_totes[year %in% c(1995:2022)]

# change names of FGH data to match GBD
setnames(dah_totes, old = c("year", "iso3"), new = c("year_id", "ihme_loc_id"))

# pivot draws to long format
dah_totes = melt(dah_totes, 
                 id.vars = c("ihme_loc_id", "year_id"), 
                 variable.name = "draw", 
                 value.name = "dah")

# get GBD location metadata
locs <- get_location_metadata(location_set_id=35, release_id=16) # GBD 2023

# filter to just national level
locs = locs[level == 3] # level 3 -> country

# merge with location metadata
dah_totes = merge(dah_totes, locs[, .(location_id, ihme_loc_id, 
                                      location_name, super_region_name)], 
                  by = "ihme_loc_id")

# convert currency from 2023 USD to 2023 PPP
dah_ppp = currency_conversion(dah_totes, 
                              col.loc = "ihme_loc_id", # ISO3 codes
                              col.value = "dah", # name of column with values to convert
                              currency = "usd", # current currency unit
                              currency.year = 2023, # current currency year
                              base.year = 2023, # year of currency to convert to
                              base.unit = "ppp") # unit of currency to convert to

# convert to per capita with population estimates: 
pop <- get_population(release_id = 16, # GBD 2023
                      location_id = unique(dah_ppp$location_id),
                      year_id = unique(dah_ppp$year_id), 
                      sex_id = 3, # 3 -> all sexes
                      age_group_id = 22) # 22 -> all ages

# remove unnecessary column
pop[, run_id := NULL]

# merge population with dah data
dah_ppp = merge(dah_ppp, pop, 
                by = c("year_id", "location_id"))

# calculate dah per capita: 
dah_ppp[, dah_pc := dah / population]

# create per capita values in USD, not PPP, and save out
dah_usd = merge(dah_totes, pop, 
                by = c("year_id", "location_id"))
dah_usd[, dah_pc := dah / population]

# save out dah in USD data:
output_dir = "FILEPATH"
fwrite(dah_usd, paste0(output_dir, "dah_usd23_1995_2022.csv"))

# save out dah in PPP data: 
fwrite(dah_ppp, paste0(output_dir, "dah_ppp23_1995_2022.csv"))


################################### FGH 2023 ###################################
# get dah data
dah_totes = get_he_data("dah_totes", full = TRUE, fgh_round_id = 16) # FGH 2023
# explore what data this pulls: 
length(unique(dah_totes$location_id)) # 204
range(dah_totes$year_id) # 1995 - 2050
range(dah_totes$draw) # 1 - 500

# filter to 1995-2021
dah_totes = dah_totes[year_id %in% c(1995:2021)]

# convert currency from 2022 USD to 2021 PPP
dah_ppp = currency_conversion(dah_totes, 
                              col.loc = "ihme_loc_id", # ISO3 codes
                              col.value = "data_var", # name of column with values to convert
                              currency = "usd", # current currency unit
                              currency.year = 2022, # current currency year
                              base.year = 2021, # year of currency to convert to
                              base.unit = "ppp") # unit of currency to convert to

# convert to per capita: 
# source shared functions
source(paste0(functions_dir,"get_population.R"))
source(paste0(functions_dir,"get_location_metadata.R"))

# get location IDs from metadata
locs <- get_location_metadata(location_set_id=35, release_id = 16) 
# location_set_id 35 -> Model Results 
# release_id 9 -> GBD 2021
# get country IDs 
country_ids <- locs[level == 3]$location_id # level 3 -> country

# get population for all ages and sexes for all countries
pop <- get_population(release_id = 16, # GBD 2021
                      location_id = country_ids,
                      year_id = c(1995:2021), 
                      sex_id = 3, # 3 -> all sexes
                      age_group_id = 22) # 22 -> all ages

# remove unnecessary column
pop[, run_id := NULL]

# merge population with dah data
dah_ppp = merge(dah_ppp, pop, 
                by = c("year_id", "location_id"))

# calculate DAH per capita: 
dah_ppp[, dah_pc := data_var / population]

# change data_var name
setnames(dah_ppp, old = "data_var", new = "dah_totes")

# save out DAH data: 
output_dir = "FILEPATH"
fwrite(dah_ppp, paste0(output_dir, "DAH_pc_ppp_1995_2021_fgh2023.csv"))
