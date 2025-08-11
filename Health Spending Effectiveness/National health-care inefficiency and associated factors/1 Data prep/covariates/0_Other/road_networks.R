##########################################################################
### Author: USERNAME
### Date: 07/22/24
### Project: Health Spending Effectiveness 
### Purpose: Process road network data
##########################################################################

# clear environment
rm(list=ls())


# load libraries and functions
library(data.table)
library(readxl)

# read in road data
filename <- "WORLD_ROAD_STATISTICS_2018_DATA_TABLES_2011_2016_Y2019M06D13.XLSM"

road_data <- read_excel(paste0(j_root,"FILEPATH/IRF_WORLD_ROAD_STATISTICS/",filename), sheet = '4.Road_Networks', skip = 1)

road_data_2011 <- road_data[, c(1:2,4:10)]
road_data_2012 <- road_data[, c(1:2,12:18)]
road_data_2013 <- road_data[, c(1:2,20:26)]
road_data_2014 <- road_data[, c(1:2,28:34)]
road_data_2015 <- road_data[, c(1:2,36:42)]
road_data_2016 <- road_data[, c(1:2,44:50)]
latest_road_data <- road_data[, c(1:2,52:60)]


name_list <- c("Country","ihme_loc_id","Motorways Km","Highways Main or National Km",
               "Secondary or Regional Km","Other Roads Km","Total Network Km","Percent paved","Density")
setnames(road_data_2011, name_list)
setnames(road_data_2012, name_list)
setnames(road_data_2013, name_list)
setnames(road_data_2014, name_list)
setnames(road_data_2015, name_list)
setnames(road_data_2016, name_list)

# add year column to each DF
road_data_2011$year_id <- 2011
road_data_2012$year_id <- 2012
road_data_2013$year_id <- 2013
road_data_2014$year_id <- 2014
road_data_2015$year_id <- 2015
road_data_2016$year_id <- 2016

# combine all years into one DF
road_data_all <- rbind(road_data_2011, road_data_2012, road_data_2013, road_data_2014, road_data_2015, road_data_2016)


# check alignment with 2011-2016 data
setDT(latest_road_data)
name_list <- c("Country","ihme_loc_id","year_id","Motorways Km","Highways Main or National Km",
               "Secondary or Regional Km","Other Roads Km","Total Network Km","Percent paved","Density_latest","Percent Motorways")
setnames(latest_road_data, name_list)

### check 2011
latest_2011 <- latest_road_data[year_id==2011,.(ihme_loc_id, Density_latest)]
# merge latest_2011 with road_data_2011
test_2011 <- merge(road_data_2011, latest_2011, by="ihme_loc_id")
test_2011$diff <- test_2011$Density_latest - test_2011$Density

### check 2012
latest_2012 <- latest_road_data[year_id==2012,.(ihme_loc_id, Density_latest)]
# merge latest_2012 with road_data_2012
test_2012 <- merge(road_data_2012, latest_2012, by="ihme_loc_id")
test_2012$diff <- test_2012$Density_latest - test_2012$Density

### check 2013
latest_2013 <- latest_road_data[year_id==2013,.(ihme_loc_id, Density_latest)]
# merge latest_2013 with road_data_2013
test_2013 <- merge(road_data_2013, latest_2013, by="ihme_loc_id")
test_2013$diff <- test_2013$Density_latest - test_2013$Density

### check 2014
latest_2014 <- latest_road_data[year_id==2014,.(ihme_loc_id, Density_latest)]
# merge latest_2014 with road_data_2014
test_2014 <- merge(road_data_2014, latest_2014, by="ihme_loc_id")
test_2014$diff <- test_2014$Density_latest - test_2014$Density

### check 2015
latest_2015 <- latest_road_data[year_id==2015,.(ihme_loc_id, Density_latest)]
# merge latest_2015 with road_data_2015
test_2015 <- merge(road_data_2015, latest_2015, by="ihme_loc_id")
test_2015$diff <- test_2015$Density_latest - test_2015$Density

### check 2016
latest_2016 <- latest_road_data[year_id==2016,.(ihme_loc_id, Density_latest)]
# merge latest_2016 with road_data_2016
test_2016 <- merge(road_data_2016, latest_2016, by="ihme_loc_id")
test_2016$diff <- test_2016$Density_latest - test_2016$Density

# save the years that we don't already have in the road_data_all
latest_road_data <- latest_road_data[year_id < 2011,]

# keep column names that are in road_data_all
latest_road_data[,`Percent Motorways` := NULL]
setnames(latest_road_data, "Density_latest", "Density")

# combine the latest data with the rest of the data
road_data_all <- rbind(road_data_all, latest_road_data)
setDT(road_data_all)
setnames(road_data_all, "Country","location_name")

# merge with IHME locations
loc_dir = "FILEPATH"
locs = fread(paste0(loc_dir, "location_set35_release9.csv"))
locs <- locs[level==3]

name_test = merge(road_data_all, locs[,.(location_id, location_name)],
                  by = "location_name", all.x = TRUE)
unique(name_test[is.na(location_id)]$location_name)
# Countries not aligning: 
# Anguilla, Cayman Islands, Hong Kong, Macao, New Caledonia - nonsovereign IHME

# fix recipient_country names to match GBD names
road_data_all[location_name=="Bahamas, The", location_name := "Bahamas"]
road_data_all[location_name=="Bolivia", location_name := "Bolivia (Plurinational State of)"]
road_data_all[location_name=="Cape Verde", location_name := "Cabo Verde"]
road_data_all[location_name=="Congo, Dem. Rep.", location_name := "Democratic Republic of the Congo"]
road_data_all[location_name=="Congo, Rep.", location_name := "Congo"]


road_data_all[location_name=="Cote d'Ivoire", location_name := "Côte d'Ivoire"]
road_data_all[location_name=="Czech Republic", location_name := "Czechia"]
road_data_all[location_name=="Egypt, Arab Rep.", location_name := "Egypt"]

road_data_all[location_name=="Gambia, The", location_name := "Gambia"]
road_data_all[location_name=="Iran, Islamic Rep.", location_name := "Iran (Islamic Republic of)"]

road_data_all[location_name=="Kyrgyz Republic", location_name := "Kyrgyzstan"]
road_data_all[location_name=="Lao PDR", location_name := "Lao People's Democratic Republic"]
road_data_all[location_name=="Macedonia, FYR", location_name := "North Macedonia"]
road_data_all[location_name=="Micronesia, Fed. Sts.", location_name := "Micronesia (Federated States of)"]

road_data_all[location_name=="Moldova", location_name := "Republic of Moldova"]
road_data_all[location_name=="Korea, Dem. Rep.", location_name := "Democratic People's Republic of Korea"]
road_data_all[location_name=="Korea, Rep.", location_name := "Republic of Korea"]
road_data_all[location_name=="Occupied Palestinian Territory", location_name := "Palestine"]

road_data_all[location_name=="Slovak Republic", location_name := "Slovakia"]

road_data_all[location_name=="Swaziland", location_name := "Eswatini"]
road_data_all[location_name=="Tanzania", location_name := "United Republic of Tanzania"]
road_data_all[location_name=="United States", location_name := "United States of America"]

road_data_all[location_name=="Turkey", location_name := "Türkiye"]
road_data_all[location_name=="Venezuela, RB", location_name := "Venezuela (Bolivarian Republic of)"]
road_data_all[location_name=="Vietnam", location_name := "Viet Nam"]
road_data_all[location_name=="Yemen, Rep.", location_name := "Yemen"]

road_data_all = merge(road_data_all, locs[,.(location_id, location_name)],
                  by = "location_name")

# remove Monaco (unrealistic/erroneous)
road_data_all <- road_data_all[location_name != "Monaco"]

# also remove rows where Density is NA
road_data_all <- road_data_all[!is.na(Density)]

# plot a histogram of road_data_all$Density
# we will want to use logged Density b/c it's a very skewed distribution
road_data_all[, log_density := log(Density)]
ggplot(road_data_all, aes(x=log_density)) + geom_histogram(bins=50) + ggtitle("Density of Roads in 2010")

# save out
fwrite(road_data_all,"FILEPATH/road_network_data.csv")

