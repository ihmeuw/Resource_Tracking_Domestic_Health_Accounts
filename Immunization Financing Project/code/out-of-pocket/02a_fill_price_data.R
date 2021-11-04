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
require(classInt, lib.loc = paste0(FILEPATH))
library(readxl)
library(ggplot2)
code.dir <- paste0(FILEPATH)
## source mapping function, which will be called with "gbd_map"
source(paste0(FILEPATH, "GBD_WITH_INSETS_MAPPING_FUNCTION.R"))
source(paste0(FILEPATH, "get_location_metadata.R")) 
source(paste0(FILEPATH, "helper_functions.R"))

locations <- get_location_metadata(location_set_id = 1, gbd_round_id = 5)
locations <- locations[level == 3, .(ihme_loc_id, location_name, super_region_id, super_region_name)]

in.dir <- paste0(FILEPATH)
out.dir <- paste0(FILEPATH)
# out.filename <- "Immunization price map.pdf"

## Today's date
date1 <- format(Sys.time(), "%Y%m%d")

## -------------------------
## Filling gaps with country-average and regional data
price_iso <- fread(paste0(FILEPATH, "price_9_vaccines_MRseparate.csv"))
setnames(price_iso, "Year", "year_id", skip_absent = TRUE)
setnames(price_iso, "Vaccine", "vaccine", skip_absent = TRUE)
price_iso <- price_iso[, .(ihme_loc_id, Country, year_id, vaccine, PricePerDoseInUSD)]
price_iso <- price_iso[, .(PricePerDoseInUSD = mean(PricePerDoseInUSD)), 
                       .(ihme_loc_id, Country, year_id, vaccine)]

# ## Fake 1000 draws of unmodeled data
price_iso_1000 <- price_iso[rep(seq_len(nrow(price_iso)), each = 1000), ]
price_iso_1000[ , draw := 1:1000,
                by = c("ihme_loc_id", 'Country', 'year_id', 'vaccine') ]
max(price_iso_1000$draw) == 1000

price_reg <- fread(paste0(FILEPATH, "regional_price_10_vaccines_draws_20200722.csv"))
names(price_reg)  

## Calculate Country-vaccine averages
price_iso_avg <- price_iso[, .(price_avg = mean(PricePerDoseInUSD)), .(ihme_loc_id, vaccine)]
## Fake 1000 draws of unmodeled data
price_iso_avg <- price_iso_avg[rep(seq_len(nrow(price_iso_avg)), each = 1000), ]
price_iso_avg[ , draw := 1:1000, by = c("ihme_loc_id", 'vaccine', 'price_avg') ]
max(price_iso_avg$draw) == 1000

## Create square data.table
id_square <- CJ(ihme_loc_id = unique(locations$ihme_loc_id), 
                vaccine = unique(price_iso$vaccine),
                year_id = c(2000:2017),
                draw = c(1:1000))
id_square <- get_region(id_square)
price_all <- merge(id_square, 
                   price_iso_1000, by = c('ihme_loc_id', 'vaccine', 'year_id', 'draw'), 
                   all = T)

## Merge on country averages
price_all <- merge(price_all, price_iso_avg, by = c('ihme_loc_id', 'vaccine', 'draw'), 
                   all = T)
price_all <- merge(price_all, 
                   price_reg[, .(super_region_name, year_id, vaccine, draw, price_reg = price)], 
                   by = c("vaccine", 'super_region_name', 'year_id', 'draw'), all = T)

## Reported prices first
price_all[!is.na(PricePerDoseInUSD), price := PricePerDoseInUSD]
## Then country-vaccine averages
price_all[is.na(price), price := price_avg]
## Finally regional averages
price_all[is.na(price), price := price_reg]

## Make sure all rows have a filled value
stop(nrow(price_all[is.na(price), ]) == 0)

price_data <- price_all[, .(price = mean(price)), .(ihme_loc_id, super_region_name, year_id, vaccine, draw)]

length(unique(price_data$vaccine))
length(unique(price_data$year_id))
length(unique(price_data$ihme_loc_id))

stop(nrow(price_data) == 10 * 18 * 195 * 1000)
## -----------------------------------------------------------------------------------##
## 5. NIP adjustment
## -----------------------------------------------------------------------------------##

epi1 <- fread("FILEPATH/gavi_vaccine_EPIschedule_bycountryyear.csv")

head(epi1)
names(epi1)

## rubella is not the same as the MR vaccine
epi <- epi1[, .(ihme_loc_id, year_id, 
                `schedule HPV`, `schedule IPV`, `schedule JE`, `schedule Mening`, 
                `schedule MCV2`, `schedule Pneumo_conj`,`schedule Rotavirus`, 
                # `schedule Rubella`, 
                `schedule YF`)]

setnames(epi, 
         old = c('schedule HPV', 'schedule IPV', 'schedule JE', 'schedule Mening', 
                 'schedule MCV2', 'schedule Pneumo_conj','schedule Rotavirus', 
                 #'schedule Rubella',
                 'schedule YF'),
         new = c('HPV', 'IPV', 'JE', 'MenA', 'Measles', 'PCV', 'RVV', 'YF'))
         #new = c('HPV', 'IPV', 'JE', 'MenA', 'MR1', 'PCV', 'RVV', 'MR2', 'YF'))

epi <- melt(epi, id.vars = c('ihme_loc_id', 'year_id'), variable.name = 'vaccine')

dt_adj <- merge(price_data, epi, by = c('ihme_loc_id', 'year_id', 'vaccine'),
                       all.x = T)

## Adjust price to zero where NIP is provided in that country-year
dt_adj[, value := 1 - value]
dt_adj[!is.na(value), price := price * value]

## Total reduced to % of total
sum(dt_adj$price) / sum(price_data$price)

## -------------------------------------------------------------------------------------
## Write final dataset
dt_adj[, draw := draw - 1]
fwrite(dt_adj, paste0(FILEPATH, "filled_price_10_vaccines_", date1, ".csv"))

unique(dt_adj$vaccine)
