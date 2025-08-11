##########################################################################
### Author: USERNAME
### Date: 04/03/2024
### Project: Health Spending Effectiveness 
### Purpose: Extracting total health expenditure data from FGH
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

# get total health expenditure data from feather file
the_totes = read_feather(file.path(fgh_dir, "the_totes_feather.feather"))

# convert to data.table
setDT(the_totes)

# filter to years of interest
the_totes = the_totes[year %in% c(1995:2022)]

# change names of FGH data to match GBD
setnames(the_totes, old = c("year", "iso3"), new = c("year_id", "ihme_loc_id"))

# pivot draws to long format
the_totes = melt(the_totes, 
                 id.vars = c("ihme_loc_id", "year_id"), 
                 variable.name = "draw", 
                 value.name = "the")

# get GBD location metadata
locs <- get_location_metadata(location_set_id=35, release_id=16) # GBD 2023

# filter to just national level
locs = locs[level == 3] # level 3 -> country

# merge with location metadata
the_totes = merge(the_totes, locs[, .(location_id, ihme_loc_id, 
                                      location_name, super_region_name)], 
                  by = "ihme_loc_id")

# convert currency from 2023 USD to 2023 PPP
the_ppp = currency_conversion(the_totes, 
                              col.loc = "ihme_loc_id", # ISO3 codes
                              col.value = "the", # name of column with values to convert
                              currency = "usd", # current currency unit
                              currency.year = 2023, # current currency year
                              base.year = 2023, # year of currency to convert to
                              base.unit = "ppp") # unit of currency to convert to

# convert to per capita with population estimates: 
pop <- get_population(release_id = 16, # GBD 2023
                      location_id = unique(the_ppp$location_id),
                      year_id = unique(the_ppp$year_id), 
                      sex_id = 3, # 3 -> all sexes
                      age_group_id = 22) # 22 -> all ages

# remove unnecessary column
pop[, run_id := NULL]

# merge population with total health expenditure data
the_ppp = merge(the_ppp, pop, 
                by = c("year_id", "location_id"))

# calculate THE per capita: 
the_ppp[, the_pc := the / population]

# create per capita values in USD, not PPP, and save out
the_usd = merge(the_totes, pop, 
                by = c("year_id", "location_id"))
the_usd[, the_pc := the / population]


# save out THE in USD data:
output_dir = "FILEPATH"
fwrite(the_usd, paste0(output_dir, "THE_usd23_1995_2022.csv"))

# save out THE in PPP data: 
fwrite(the_ppp, paste0(output_dir, "THE_ppp23_1995_2022.csv"))


################################### FGH 2023 ###################################
# get total health expenditure data
the_totes = get_he_data("the_totes", full = TRUE, fgh_round_id = 16) # FGH 2023
# explore what data this pulls: 
length(unique(the_totes$location_id)) # 204
range(the_totes$year_id) # 1995 - 2050
range(the_totes$draw) # 1 - 500

# filter to 1995-2021
the_totes = the_totes[year_id %in% c(1995:2021)]
# summarize means from draws, then sum to global total: 
the_means = the_totes[,.(the = mean(data_var)), 
                      by = .(year_id, location_id, location_name)]
the_global = the_means[,.(global_the = sum(the)), 
                       by = "year_id"]

# convert currency from 2022 USD to 2021 PPP
the_ppp = currency_conversion(the_totes, 
                              col.loc = "ihme_loc_id", # ISO3 codes
                              col.value = "data_var", # name of column with values to convert
                              currency = "usd", # current currency unit
                              currency.year = 2022, # current currency year
                              base.year = 2021, # year of currency to convert to
                              base.unit = "ppp") # unit of currency to convert to


# convert to per capita: 
# source shared functions
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

# merge population with total health expenditure data
the_ppp = merge(the_ppp, pop, 
                by = c("year_id", "location_id"))

# calculate THE per capita: 
the_ppp[, the_pc := data_var / population]

# change data_var name
setnames(the_ppp, old = "data_var", new = "the_totes")

# create per capita values in USD, not PPP, and save out
the_usd = merge(the_totes, pop, 
                by = c("year_id", "location_id"))
the_usd[, the_pc := data_var / population]
# change data_var name
setnames(the_usd, old = "data_var", new = "the_totes")

# plot overlapping histograms of THE in PPP vs USD:
the_usd[, currency := 'usd']
the_ppp[, currency := 'ppp']
the_both = rbind(the_usd, the_ppp)

ggplot(the_both, aes(the_pc, fill = currency)) + 
  geom_histogram(alpha = 0.5, position = 'identity') + 
  labs(x = "THE")


# save out THE in USD data:
output_dir = "FILEPATH"
fwrite(the_usd, paste0(output_dir, "THE_pc_usd22_1995_2021_fgh2023.csv"))

# save out THE in PPP data: 
fwrite(the_ppp, paste0(output_dir, "THE_pc_ppp_1995_2021_fgh2023.csv"))

