##########################################################################
### Author: USERNAME
### Date: 07/09/2024
### Project: Health Spending Effectiveness 
### Purpose: Load in the DAH data & 
###          calculate HHI by country-year based on channel and program 
###########################################################################

# clear environment
rm(list=ls())


cov_dir = 'FILEPATH'
library(data.table)

# read in IHME location data
loc_dir = "FILEPATH/"
locs = fread(paste0(loc_dir, "location_set35_release9.csv"))

# subset to national level
locs <- locs[level==3]

# read in development assistance for health estimates
dah_data <- "FILEPATH/BMGF_long_dt.csv"


# cut rows where recipient_country, health_focus_area, or value are NA
dah_data <- dah_data[recipient_country != ""]
dah_data <- dah_data[recipient_country != "INKIND"]

# create "program area" variable 
dah_data[, program_area := NA]

# assign program areas based on health focus areas
dah_data[, program_area := ifelse(startsWith(health_focus_area,"hiv"), "HIV", program_area )]
dah_data[, program_area := ifelse(startsWith(health_focus_area,"tb"), "TB", program_area )]
dah_data[, program_area := ifelse(startsWith(health_focus_area,"mal"), "MAL", program_area )]
dah_data[, program_area := ifelse(startsWith(health_focus_area,"ncd"), "NCD", program_area )]
dah_data[, program_area := ifelse(startsWith(health_focus_area,"nch"), "NCH", program_area )]
dah_data[, program_area := ifelse(startsWith(health_focus_area,"oid"), "OID", program_area )]
dah_data[, program_area := ifelse(startsWith(health_focus_area,"rmh"), "RMH", program_area )]
dah_data[, program_area := ifelse(startsWith(health_focus_area,"swap"), "SWAP", program_area )]
dah_data[, program_area := ifelse(startsWith(health_focus_area,"other"), "Other", program_area )]
dah_data[, program_area := ifelse(startsWith(health_focus_area,"unalloc"), "Other", program_area )]

#########################
## 1) HHI - based on channel and program area
#########################

# condense to 'program_area' 
dah_sum <- dah_data[, .(spend = sum(value)), by = c("year","channel","program_area","recipient_country")]

# create a column in dah_sum that is the sum of all spend by year and recipient_country
dah_sum[, spend_total := sum(spend), by = c("year","recipient_country")]

# create a column that is the percent of total spend
dah_sum[, spend_percent := (spend/spend_total)*100]

dah_hhi <- dah_sum[, .(hhi = sum(spend_percent^2)), by = c("year","recipient_country")]

#------------------

# merge locs with dah_hhi
locs <- locs[,.(location_id,location_name,super_region_name)]

temp <- dah_hhi[!(recipient_country %in% locs$location_name),]

# update location names for alignment
dah_hhi[recipient_country=="Bolivia", recipient_country := "Bolivia (Plurinational State of)"]
dah_hhi[recipient_country=="Christmas Island", recipient_country := "Republic of Kiribati"]
dah_hhi[recipient_country=="Cote d'Ivoire", recipient_country := "Côte d'Ivoire"]
dah_hhi[recipient_country=="Czech Republic", recipient_country := "Czechia"]
dah_hhi[recipient_country=="Federated States of Micronesia", recipient_country := "Micronesia (Federated States of)"]
dah_hhi[recipient_country=="Iran", recipient_country := "Iran (Islamic Republic of)"]
dah_hhi[recipient_country=="Laos", recipient_country := "Lao People's Democratic Republic"]
dah_hhi[recipient_country=="Moldova", recipient_country := "Republic of Moldova"]
dah_hhi[recipient_country=="North Korea", recipient_country := "Democratic People's Republic of Korea"]
dah_hhi[recipient_country=="Russia", recipient_country := "Russian Federation"]
dah_hhi[recipient_country=="South Korea", recipient_country := "Republic of Korea"]
dah_hhi[recipient_country=="Swaziland", recipient_country := "Eswatini"]
dah_hhi[recipient_country=="Syria", recipient_country := "Syrian Arab Republic"]
dah_hhi[recipient_country=="Tanzania", recipient_country := "United Republic of Tanzania"]
dah_hhi[recipient_country=="The Gambia", recipient_country := "Gambia"]
dah_hhi[recipient_country=="Turkey", recipient_country := "Türkiye"]
dah_hhi[recipient_country=="Venezuela", recipient_country := "Venezuela (Bolivarian Republic of)"]
dah_hhi[recipient_country=="Vietnam", recipient_country := "Viet Nam"]



# merge locs and dah_hhi, keep all locs so the map will have something for all countries
setnames(dah_hhi,"recipient_country","location_name")
dah_hhi <- merge(locs,dah_hhi, by = "location_name", all.x = TRUE)

# after merging with loc, locations that did not have any DAH have year=NA
# set year to 2019 since that is the year we want to create a map for
dah_hhi[is.na(year), year := 2019]

summary(dah_hhi$hhi)

# save out csv with the dah_hhi data
#fwrite(dah_hhi, paste0(cov_dir,"dah_HHI_8.26.24.csv"))



#############################
## 2) HHI_channel - based on channel only
#############################

# condense to 'channel'
dah_sum_channel <- dah_data[, .(spend = sum(value)), by = c("year","channel","recipient_country")]

#create a column in dah_sum_channel that is the sum of all spend by year and recipient_country
dah_sum_channel[, spend_total := sum(spend), by = c("year","recipient_country")]

#create a column that is the percent of total spend
dah_sum_channel[, spend_percent := (spend/spend_total)*100]

dah_hhi_channel <- dah_sum_channel[, .(hhi_channel = sum(spend_percent^2)), by = c("year","recipient_country")]

# update location names for alignment
dah_hhi_channel[recipient_country=="Bolivia", recipient_country := "Bolivia (Plurinational State of)"]
dah_hhi_channel[recipient_country=="Christmas Island", recipient_country := "Republic of Kiribati"]
dah_hhi_channel[recipient_country=="Cote d'Ivoire", recipient_country := "Côte d'Ivoire"]
dah_hhi_channel[recipient_country=="Czech Republic", recipient_country := "Czechia"]
dah_hhi_channel[recipient_country=="Federated States of Micronesia", recipient_country := "Micronesia (Federated States of)"]
dah_hhi_channel[recipient_country=="Iran", recipient_country := "Iran (Islamic Republic of)"]
dah_hhi_channel[recipient_country=="Laos", recipient_country := "Lao People's Democratic Republic"]
dah_hhi_channel[recipient_country=="Moldova", recipient_country := "Republic of Moldova"]
dah_hhi_channel[recipient_country=="North Korea", recipient_country := "Democratic People's Republic of Korea"]
dah_hhi_channel[recipient_country=="Russia", recipient_country := "Russian Federation"]
dah_hhi_channel[recipient_country=="South Korea", recipient_country := "Republic of Korea"]
dah_hhi_channel[recipient_country=="Swaziland", recipient_country := "Eswatini"]
dah_hhi_channel[recipient_country=="Syria", recipient_country := "Syrian Arab Republic"]
dah_hhi_channel[recipient_country=="Tanzania", recipient_country := "United Republic of Tanzania"]
dah_hhi_channel[recipient_country=="The Gambia", recipient_country := "Gambia"]
dah_hhi_channel[recipient_country=="Turkey", recipient_country := "Türkiye"]
dah_hhi_channel[recipient_country=="Venezuela", recipient_country := "Venezuela (Bolivarian Republic of)"]
dah_hhi_channel[recipient_country=="Vietnam", recipient_country := "Viet Nam"]

# merge locs and dah_hhi_channel, keep all locs so the map will have something for all countries
setnames(dah_hhi_channel,"recipient_country","location_name")
dah_hhi_channel <- merge(locs,dah_hhi_channel, by = "location_name", all.x = TRUE)

# after merging with loc, locations that did not have any DAH have year=NA
# set year to 2019 since that is the year we want to create a map for
dah_hhi_channel[is.na(year), year := 2019]

summary(dah_hhi_channel$hhi_channel)
summary(dah_hhi$hhi)

# save out csv with the dah_hhi data
#fwrite(dah_hhi_channel, paste0(cov_dir,"dah_HHI_channel_8.26.24.csv"))


###################################
##### 3) HHI_ALL - based on all spending
#####################################

# read in GHE per capita & collapse to means
ghe <- fread(paste0(cov_dir,"GHES_totes_pc_1995_2021.csv"))
setnames(ghe, "ghes_pc", "ghe_pc")
ghe <- ghe[, .(location_name,location_id, year_id, region_name, super_region_name, ghes_totes)]
ghe <- ghe[, .(spend = mean(ghes_totes)), by = c("location_name","location_id", "year_id","region_name", "super_region_name")]

# read in OOP per capita & collapse to means
oop <- fread(paste0(cov_dir, "OOP_totes_pc_1995_2021.csv"))
oop <- oop[, .(location_name,location_id, year_id, region_name, super_region_name, oop_totes)]
oop <- oop[, .(spend = mean(oop_totes)), by = c("location_name","location_id", "year_id","region_name", "super_region_name")]

# read in PRIV per capita & collapse to means
ppp <- fread(paste0(cov_dir, "PRIV_totes_pc_1995_2021.csv"))
ppp <- ppp[, .(location_name,location_id, year_id, region_name, super_region_name, priv_totes)]
ppp <- ppp[, .(spend = mean(priv_totes)), by = c("location_name","location_id", "year_id","region_name", "super_region_name")]

# get DAH data ready to append with other spending data
# 1) condense to 'channel' 
dah_sum_all <- dah_data[, .(spend = sum(value)), by = c("year","channel","recipient_country")]

# 2) merge with locs and fix locations 
dah_sum_all[recipient_country=="Bolivia", recipient_country := "Bolivia (Plurinational State of)"]
dah_sum_all[recipient_country=="Christmas Island", recipient_country := "Republic of Kiribati"]
dah_sum_all[recipient_country=="Cote d'Ivoire", recipient_country := "Côte d'Ivoire"]
dah_sum_all[recipient_country=="Czech Republic", recipient_country := "Czechia"]
dah_sum_all[recipient_country=="Federated States of Micronesia", recipient_country := "Micronesia (Federated States of)"]
dah_sum_all[recipient_country=="Iran", recipient_country := "Iran (Islamic Republic of)"]
dah_sum_all[recipient_country=="Laos", recipient_country := "Lao People's Democratic Republic"]
dah_sum_all[recipient_country=="Moldova", recipient_country := "Republic of Moldova"]
dah_sum_all[recipient_country=="North Korea", recipient_country := "Democratic People's Republic of Korea"]
dah_sum_all[recipient_country=="Russia", recipient_country := "Russian Federation"]
dah_sum_all[recipient_country=="South Korea", recipient_country := "Republic of Korea"]
dah_sum_all[recipient_country=="Swaziland", recipient_country := "Eswatini"]
dah_sum_all[recipient_country=="Syria", recipient_country := "Syrian Arab Republic"]
dah_sum_all[recipient_country=="Tanzania", recipient_country := "United Republic of Tanzania"]
dah_sum_all[recipient_country=="The Gambia", recipient_country := "Gambia"]
dah_sum_all[recipient_country=="Turkey", recipient_country := "Türkiye"]
dah_sum_all[recipient_country=="Venezuela", recipient_country := "Venezuela (Bolivarian Republic of)"]
dah_sum_all[recipient_country=="Vietnam", recipient_country := "Viet Nam"]

# merge locs and dah_sum_all, keep all locs so the map will have something for all countries
setnames(dah_sum_all,"recipient_country","location_name")
dah_sum_all <- merge(locs,dah_sum_all, by = "location_name", all.x = TRUE)

# after merging with loc, locations that did not have any DAH have year=NA
# set year to 2019 since that is the year we want to create a map for
dah_sum_all[is.na(year), year := 2019]


# append ghe, oop, and ppp with dah_sum_all
setnames(dah_sum_all, "year","year_id")

ghe[, channel := "GHE"]
ghe[, region_name := NULL]

oop[, channel := "OOP"]
oop[, region_name := NULL]

ppp[, channel := "PPP"]
ppp[, region_name := NULL]

# create HHI_all metric
all_spend <- rbind(dah_sum_all, ghe, oop, ppp)

# the countries without any DAH have NA for spending for DAH, remove these rows
all_spend <- all_spend[!is.na(spend)]

all_spend <- all_spend[, .(spend = sum(spend)), by = c("year_id","location_name","location_id","channel")]

# create a column in all_spend that is the sum of all spend by year and recipient_country
all_spend[, spend_total := sum(spend), by = c("year_id","location_name","location_id")]

# create a column that is the percent of total spend
all_spend[, spend_percent := (spend/spend_total)*100]

all_spend <- all_spend[, .(hhi_all = sum(spend_percent^2)), c("year_id","location_name","location_id")]

summary(all_spend$hhi_all)
summary(dah_hhi_channel$hhi_channel)
summary(dah_hhi$hhi)

# save out csv with the dah_sum_all data
fwrite(all_spend, paste0(cov_dir,"dah_HHI_all_8.28.24.csv"))
