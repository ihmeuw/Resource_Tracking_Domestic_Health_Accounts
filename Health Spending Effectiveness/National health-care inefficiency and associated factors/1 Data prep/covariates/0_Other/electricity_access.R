##########################################################################
### Author: USERNAME
### Date: 11/27/24
### Project: Health Spending Effectiveness 
### Purpose: Processing World Bank access to electricity data
##########################################################################

## Downloaded data from: 
## https://databank.worldbank.org/source/world-development-indicators/Series/EG.ELC.ACCS.ZS#
## Source Details: 
## World Bank DataBank -> World Development Indicators -> Access to electricity (% of population) -> Download as CSV
## Selected all countries available, excluding regions and income groups

# clear environment
rm(list=ls())


# load libraries and functions
library(data.table)

# load data: 
data_path = 'FILEPATH/WB_access_to_electricity.csv'
raw_df <- fread(data_path)

# cut empty rows at the end of dataframe
raw_df <- raw_df[`Series Code` != ""]

# remove unnecessary columns
raw_df[, c("Series Name", "Series Code") := NULL]

# change column names
setnames(raw_df, old = c("Country Name", "Country Code"), 
         new = c("country_name", "ihme_loc_id"))

# melt data to long format
df = melt(raw_df, id.vars = c("country_name", "ihme_loc_id"), 
          variable.name = "year_id", value.name = "electricity_access")

# extract year number from column and convert to numeric
df[, year_id := as.numeric(substr(year_id, 1, 4))] # year in the first four digits of string

# convert electricity_access to numeric
df[, electricity_access := as.numeric(electricity_access)]

# get location metadata for location_id 
source(paste0(functions_dir,"get_location_metadata.R"))
# get location metadata
locs <- get_location_metadata(location_set_id = 35,
                              release_id = 9)[level == 3] # filter to country level

# test merge location metadata to df to identify mismatching location names
test_df <- merge(df, locs[,.(ihme_loc_id,location_id,location_name)], 
            by = "ihme_loc_id", all.x = TRUE)

# see which locations have mismatching names
unique(test_df[is.na(location_id)]$country_name)
# [1] "Aruba"                     "Channel Islands"           "Curacao"                   "Cayman Islands"           
# [5] "Faroe Islands"             "Gibraltar"                 "Hong Kong SAR, China"      "Isle of Man"              
# [9] "Liechtenstein"             "Macao SAR, China"          "St. Martin (French part)"  "New Caledonia"            
# [13] "French Polynesia"          "Sint Maarten (Dutch part)" "Turks and Caicos Islands"  "British Virgin Islands"   
# [17] "Kosovo"


df = test_df[!is.na(location_id)]

# remove missing data
df = df[!is.na(electricity_access)]

# save out processed data
covar_dir = "FILEPATH"
fwrite(df, file.path(covar_dir, "electricity_access.csv"))

# save out number of years of data available for each country
df_N = df[, .(N_years = .N), by = .(location_name, location_id)]
fwrite(df_N, file.path(covar_dir, "electricity_access_N.csv"))
