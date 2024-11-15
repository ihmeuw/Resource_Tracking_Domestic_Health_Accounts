#####################
# USERNAME
# 
# Code to prepare malaria data for modeling
# Updated to use DEX numbers for USA data
#####################

#########
# Prepare workspace
#########

# load packages
pacman::p_load(readstata13, data.table, ggplot2, feather)

rm(list = ls())

if (Sys.info()["sysname"] == "Linux") {
  j <- "ADDRESS"
  h <- "ADDRESS"
} else {
  j <- "ADDRESS"
  h <- "ADDRESS"
}

source("FILEPATH")
source('FILEPATH')
source('FILEPATH')

## -------------------
## get smoothed total spend
## -------------------

tot_mean <- get_he_data('ghes_totes')
tot_mean <- tot_mean[, .(ghes_sm = mean(data_var)),
                     by = .(ihme_loc_id, year_id)]
tot_mean <- get_location_id(tot_mean)

## select data of interest
denom <- tot_mean[,.(ihme_loc_id, location_id, year_id, hs_public = ghes_sm)]

############### get malaria spending data ######################################
# read malaria data and subset to public data only
malaria_spend <- fread('FILEPATH')
malaria_spend <- get_location_id(malaria_spend)

#=====================================================================================
malaria_spend <- merge(malaria_spend, denom, by = c('location_id', 'year_id'), all.x = T)

# cleaned up columns
malaria_spend <- malaria_spend[, .(year_id, location_id, value_code, hs_public, value, source_type)]
setnames(malaria_spend, c('value'), c('data'))

## remove NAs and erroneous values
malaria_spend <- malaria_spend[data <= hs_public]

##########
## Finish preparing data
##########

## save fraction for later 
malaria_spend[, y_frac := data / hs_public]

# logit fraction transition
malaria_spend[, y := logit_trans(data / hs_public, lemon = T)]

# output data to .csv
fwrite(malaria_spend, paste0('FILEPATH'))

## End of Script ##