# Summarize SDI to Regional Average
# Author: Michael Breshock
# Date: 09/25/2023

# clear environment
rm(list=ls())

# load libraries and functions 
library(data.table)
library(dplyr)

# load SDI data
sdi = fread(file = "FILEPATH/sdi_levels_1_to_3_1990_2019.csv")
# load location metadata
locs <- fread(file = "FILEPATH/location_set_22_metadata.csv")

# remove location name from sdi: 
sdi[, location_name := NULL]

# merge region IDs into SDI data: 
sdi_reg = merge(sdi, locs[,.(location_id, location_name, region_id, region_name)], 
                by = "location_id")

# calculate mean SDI by region: 
sdi_mean = sdi_reg[,.(region_SDI = mean(mean_value)), 
                   by = c("year_id", "region_id", "region_name")]

# merge regional sdi back into overall sdi -> have value for each location id
sdi_reg_mean = merge(sdi_reg, sdi_mean, 
                     by = c("year_id", "region_id", "region_name"))

sdi_out = sdi_reg_mean[,.(year_id, region_id, region_name, location_id, 
                          location_name, region_SDI)]

# save out regional SDI: 
fwrite(sdi_out, file = "FILEPATH/regional_SDI_1990_2019.csv")
