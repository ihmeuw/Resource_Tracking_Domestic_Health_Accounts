##########################################################################
### Author: USERNAME
### Date: 04/08/2024
### Project: Health Spending Effectiveness 
### Purpose: Extracting prepaid private health spending data from FGH
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

# get prepaid private spending data from feather file
ppp_totes = read_feather(file.path(fgh_dir, "ppp_totes_feather.feather"))

# convert to data.table
setDT(ppp_totes)

# filter to years of interest
ppp_totes = ppp_totes[year %in% c(1995:2022)]

# change names of FGH data to match GBD
setnames(ppp_totes, old = c("year", "iso3"), new = c("year_id", "ihme_loc_id"))

# pivot draws to long format
ppp_totes = melt(ppp_totes, 
                 id.vars = c("ihme_loc_id", "year_id"), 
                 variable.name = "draw", 
                 value.name = "ppp_totes")

# get GBD location metadata
locs <- get_location_metadata(location_set_id=35, release_id=16) # GBD 2023

# filter to just national level
locs = locs[level == 3] # level 3 -> country

# merge with location metadata
ppp_totes = merge(ppp_totes, locs[, .(location_id, ihme_loc_id, 
                                      location_name, super_region_name)], 
                  by = "ihme_loc_id")

# convert currency from 2023 USD to 2023 PPP
ppp_ppp = currency_conversion(ppp_totes, 
                              col.loc = "ihme_loc_id", # ISO3 codes
                              col.value = "ppp_totes", # name of column with values to convert
                              currency = "usd", # current currency unit
                              currency.year = 2023, # current currency year
                              base.year = 2023, # year of currency to convert to
                              base.unit = "ppp") # unit of currency to convert to

# convert to per capita with population estimates: 
pop <- get_population(release_id = 16, # GBD 2023
                      location_id = unique(ppp_ppp$location_id),
                      year_id = unique(ppp_ppp$year_id), 
                      sex_id = 3, # 3 -> all sexes
                      age_group_id = 22) # 22 -> all ages

# remove unnecessary column
pop[, run_id := NULL]

# merge population with prepaid private spending data
ppp_ppp = merge(ppp_ppp, pop, 
                by = c("year_id", "location_id"))

# calculate ppp per capita: 
ppp_ppp[, ppp_pc := ppp_totes / population]

# create per capita values in USD, not PPP, and save out
ppp_usd = merge(ppp_totes, pop, 
                by = c("year_id", "location_id"))
ppp_usd[, ppp_pc := ppp_totes / population]


# save out ppp in USD data:
output_dir = "FILEPATH"
fwrite(ppp_usd, paste0(output_dir, "PPP_usd23_1995_2022.csv"))

# save out ppp in PPP data: 
fwrite(ppp_ppp, paste0(output_dir, "PPP_ppp23_1995_2022.csv"))


################################### FGH 2023 ###################################
# get prepaid private spending data
priv_totes = get_he_data("ppp_totes", full = TRUE, fgh_round_id = 16) # FGH 2023
# explore what data this pulls: 
length(unique(priv_totes$location_id)) # 204
range(priv_totes$year_id) # 1995 - 2050
range(priv_totes$draw) # 1 - 500

# filter to 1995-2021
priv_totes = priv_totes[year_id %in% c(1995:2021)]

# convert currency from 2022 USD to 2021 PPP
priv_ppp = currency_conversion(priv_totes, 
                               col.loc = "ihme_loc_id", # ISO3 codes
                               col.value = "data_var", # name of column with values to convert
                               currency = "usd", # current currency unit
                               currency.year = 2022, # current currency year
                               base.year = 2021, # year of currency to convert to
                               base.unit = "ppp") # unit of currency to convert to

# check range before vs after conversion:
range(priv_totes$data_var) # 1.464632e-05 1.572101e+12
range(priv_ppp$data_var) # 1.350784e-05 1.469180e+12

# convert to per capita: 
# source shared functions
source(paste0("FILEPATH","get_population.R"))
source(paste0("FILEPATH","get_location_metadata.R"))

# get location IDs from metadata
locs <- get_location_metadata(location_set_id=35, release_id = 16) 

# get country IDs 
country_ids <- locs[level == 3]$location_id # level 3 -> country

# get population for all ages and sexes for all countries
pop <- get_population(release_id = 16, # GBD 2023
                      location_id = country_ids,
                      year_id = c(1995:2021), 
                      sex_id = 3, # 3 -> all sexes
                      age_group_id = 22) # 22 -> all ages

# remove unnecessary column
pop[, run_id := NULL]

# merge population with prepaid private spending data
priv_ppp = merge(priv_ppp, pop, 
                 by = c("year_id", "location_id"))

# calculate PPP per capita: 
priv_ppp[, priv_pc := data_var / population]

# change data_var name
setnames(priv_ppp, old = "data_var", new = "priv_totes")

# save out PPP data: 
output_dir = "FILEPATH"
fwrite(priv_ppp, paste0(output_dir, "PRIV_pc_ppp_1995_2021_fgh2023.csv"))

