##########################################################################
### Author: USERNAME
### Date: 04/08/2024
### Project: Health Spending Effectiveness 
### Purpose: Extracting out of pocket health spending data from FGH
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
fgh_dir = "FILEPATH"

# get out of pocket health spending data from feather file
oop_totes = read_feather(file.path(fgh_dir, "oop_totes_feather.feather"))

# convert to data.table
setDT(oop_totes)

# filter to years of interest
oop_totes = oop_totes[year %in% c(1995:2022)]

# change names of FGH data to match GBD
setnames(oop_totes, old = c("year", "iso3"), new = c("year_id", "ihme_loc_id"))

# pivot draws to long format
oop_totes = melt(oop_totes, 
                 id.vars = c("ihme_loc_id", "year_id"), 
                 variable.name = "draw", 
                 value.name = "oop")

# get GBD location metadata
locs <- get_location_metadata(location_set_id=35, release_id=16) # GBD 2023

# filter to just national level
locs = locs[level == 3] # level 3 -> country

# merge with location metadata
oop_totes = merge(oop_totes, locs[, .(location_id, ihme_loc_id, 
                                      location_name, super_region_name)], 
                  by = "ihme_loc_id")

# convert currency from 2023 USD to 2023 PPP
oop_ppp = currency_conversion(oop_totes, 
                              col.loc = "ihme_loc_id", # ISO3 codes
                              col.value = "oop", # name of column with values to convert
                              currency = "usd", # current currency unit
                              currency.year = 2023, # current currency year
                              base.year = 2023, # year of currency to convert to
                              base.unit = "ppp") # unit of currency to convert to

# convert to per capita with population estimates: 
pop <- get_population(release_id = 16, # GBD 2023
                      location_id = unique(oop_ppp$location_id),
                      year_id = unique(oop_ppp$year_id), 
                      sex_id = 3, # 3 -> all sexes
                      age_group_id = 22) # 22 -> all ages

# remove unnecessary column
pop[, run_id := NULL]

# merge population with out of pocket spending data
oop_ppp = merge(oop_ppp, pop, 
                by = c("year_id", "location_id"))

# calculate oop per capita: 
oop_ppp[, oop_pc := oop / population]

# create per capita values in USD, not PPP, and save out
oop_usd = merge(oop_totes, pop, 
                by = c("year_id", "location_id"))
oop_usd[, oop_pc := oop / population]


# save out oop in USD data:
output_dir = "FILEPATH"
fwrite(oop_usd, paste0(output_dir, "oop_usd23_1995_2022.csv"))

# save out oop in PPP data: 
fwrite(oop_ppp, paste0(output_dir, "oop_ppp23_1995_2022.csv"))


################################### FGH 2023 ###################################
# get out of pocket spending data
oop_totes = get_he_data("oop_totes", full = TRUE, fgh_round_id = 16) # FGH 2023
# explore what data this pulls: 
length(unique(oop_totes$location_id)) # 204
range(oop_totes$year_id) # 1995 - 2050
range(oop_totes$draw) # 1 - 500

# filter to 1995-2021
oop_totes = oop_totes[year_id %in% c(1995:2021)]

# convert currency from 2022 USD to 2021 PPP
oop_ppp = currency_conversion(oop_totes, 
                              col.loc = "ihme_loc_id", # ISO3 codes
                              col.value = "data_var", # name of column with values to convert
                              currency = "usd", # current currency unit
                              currency.year = 2022, # current currency year
                              base.year = 2021, # year of currency to convert to
                              base.unit = "ppp") # unit of currency to convert to

# check range before vs after conversion:
range(oop_totes$data_var) # 4.27691e-05 5.53771e+11
range(oop_ppp$data_var) # 4.343281e-05 5.618402e+11

# convert to per capita: 
# source shared functions
source(paste0(functions_dir,"get_population.R"))
source(paste0(functions_dir,"get_location_metadata.R"))

# get location IDs from metadata
locs <- get_location_metadata(location_set_id=35, release_id = 16) 
# location_set_id 35 -> Model Results 
# release_id 16 -> GBD 2023
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

# merge population with out of pocket spending data
oop_ppp = merge(oop_ppp, pop, 
                 by = c("year_id", "location_id"))

# calculate OOP per capita: 
oop_ppp[, oop_pc := data_var / population]

# change data_var name
setnames(oop_ppp, old = "data_var", new = "oop_totes")

# save out OOP data: 
output_dir = "FILEPATH"
fwrite(oop_ppp, paste0(output_dir, "OOP_pc_ppp_1995_2021_fgh2023.csv"))

