########################################################################################
## Fill price data with regional values from linear regression
## Author: Emilie Maddison
## Date: May 4, 2020
## Description: 
########################################################################################

## Set up environment 
rm(list = ls())  

## Set filepaths to directories
if (Sys.info()[1] == "Linux") {  
  j <- FILEPATH
  h <- FILEPATH
  k <- FILEPATH
} else if (Sys.info()[1] == "Windows") {  
  j <- FILEPATH  
  h <- FILEPATH  
  k <- FILEPATH  
}  

## load required packages
require(data.table)
require(classInt, lib.loc = FILEPATH)
library(readxl)
library(ggplot2)
code.dir <- FILEPATH
## source mapping function, which will be called with "gbd_map"
source(paste0(FILEPATH, "GBD_WITH_INSETS_MAPPING_FUNCTION.R"))
source(paste0(FILEPATH, "get_location_metadata.R")) 
source(paste0(code.dir, "helper_functions.R"))

locations <- get_location_metadata(location_set_id = 1, gbd_round_id = 5)
locations <- locations[level == 3, .(ihme_loc_id, location_name, super_region_id, super_region_name)]

in.dir <- FILEPATH
out.dir <- FILEPATH
out.filename <- "Immunization price map.pdf"

## -------------------------
## Filling gaps with regional data
price_iso <- fread(paste0(out.dir, "price/price_9_vaccines.csv"))
setnames(price_iso, "Year", "year_id")
price_reg <- fread(paste0(FILEPATH, "/region_vaccine_specific_prices.csv"))
names(price_reg)  

## Create square data.table
id_square <- CJ(ihme_loc_id = unique(locations$ihme_loc_id), 
                Vaccine = unique(price_iso$Vaccine),
                year_id = c(2000:2018))
id_square <- get_region(id_square)
price_all <- merge(id_square, price_iso[, super_region_name := NULL], 
                   by = c('ihme_loc_id', 'Vaccine', 'year_id'), all = T)

price_all <- merge(price_all, price_reg, by = c("Vaccine", 'super_region_name'), all = T)
price_all[!is.na(PricePerDoseInUSD), price := PricePerDoseInUSD]
price_all[is.na(price), price := mean]

price_2016 <- price_all[year_id==2016, ]

fwrite(price_all, paste0(out.dir, "price/filled_price_9_vaccines.csv"))
fwrite(price_2016, paste0(out.dir, "price/filled_price_9_vaccines_2016.csv"))
