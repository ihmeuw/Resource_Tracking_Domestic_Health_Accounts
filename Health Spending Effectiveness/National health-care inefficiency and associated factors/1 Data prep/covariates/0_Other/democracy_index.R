##########################################################################
### Author: USERNAME
### Date: 08/08/24
### Project: Health Spending Effectiveness 
### Purpose: load/setup democracy score data from Polity
###################################################


# clear environment
rm(list=ls())


# load libraries and functions
library(data.table)

# read in location data
loc_dir = "FILEPATH"
locs = fread(paste0(loc_dir, "location_set35_release9.csv"))

locs <- locs[, .(location_id, ihme_loc_id, location_name)]

# read in democracy score data
data_path = 'FILEPATH'
polity_data <- fread(paste0(data_path, "democracy-index-polity.csv"))
polity_data[, Entity:=NULL]

setnames(polity_data, c("Year","Code"), c("year_id","ihme_loc_id"))

# which locs are causing issues? 
temp <- polity_data[!(ihme_loc_id %in% locs$ihme_loc_id),]
table(temp$ihme_loc_id) 

polity_data <- merge(locs, polity_data, by = c("ihme_loc_id"))

# remove years prior to 1995
polity_data <- polity_data[year_id >= 1995]

# save out data
fwrite(polity_data, "FILEPATH/democracy_index.csv")
# 165 countries are in this dataset

