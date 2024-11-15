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

########## get total spend to use as denominator for logit transformation ######################################
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

# for sensitivity analysis
malaria_spend_25 <- fread('FILEPATH')
malaria_spend_75 <- fread('FILEPATH')

#=====================================================================================
data_list <- list(malaria_spend, malaria_spend_25, malaria_spend_75) 
data_25_75 <- lapply(data_list, function(x) merge(x, denom, by = c('location_id','year_id'), all.x = T))

# cleaned up columns
data_25_75 <- lapply(data_25_75, function(x) x[ , .(year_id, location_id, hs_public, value, source_id, source_type)])
lapply(data_25_75, function(x) setnames(x, c('value'), c('data')))

## remove NAs and huge values
data_25_75 <- lapply(data_25_75, function(x) x[(!is.na(data)) & data <= hs_public])

##########
## Finish preparing data
##########

## save fraction for later 
lapply(data_25_75, function(x) x[ ,y_frac := data/hs_public])

# logit fraction transition
lapply(data_25_75, function(x) x[ ,y := logit_trans(data/hs_public, lemon = T)])

# output data
fwrite(data_25_75[[1]], paste0("FILEPATH"))
fwrite(data_25_75[[2]], paste0("FILEPATH"))
fwrite(data_25_75[[3]], paste0("FILEPATH"))

## End of Script ##