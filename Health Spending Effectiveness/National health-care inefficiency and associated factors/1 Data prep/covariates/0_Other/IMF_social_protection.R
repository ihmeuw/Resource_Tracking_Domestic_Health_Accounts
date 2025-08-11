##########################################################################
### Author: USERNAME
### Date: 11/25/24
### Project: Health Spending Effectiveness 
### Purpose: Processing social protection data from International Monetary Fund (IMF)
##########################################################################

## Data Source: https://www.imf.org/en/Home
## Navigate to the IMF Data Portal
## Government Finance Statistics (GFS) Database: Expenditure by Functions of Government
## Select the 'Bulk Download' button on the top right
## Select Panel Data format, All Countries, All Sectors, All Units
## COFOG Function: Expenditure on social protection
## From: 01/01/1990 To: 12/31/2021

# clear environment
rm(list=ls())

# load libraries and functions
library(data.table)

# load data: 
data_path = 'FILEPATH/IMF_GFSCOFOG_govt_expend_07_11_2024.csv'
raw_df <- fread(data_path)

# pull just the columns of interest: 
pull_cols = c("Country Name", "Country Code", "Sector Name", "Sector Code", 
              "Unit Name", "Unit Code", "Time Period", 
              "Expenditure on social protection (GF10)")
df <- raw_df[, ..pull_cols] 

# shorten column names for ease: 
setnames(df, old = pull_cols, 
         new = c("country_name", "country_code", 
                 "sector_name", "sector_code", 
                 "unit_name", "unit_code", 
                 "year_id", "social_protection_expend"))

# filter to non-missing estimates: 
df = df[!is.na(social_protection_expend)]

# filter to 1995-2021
df = df[year_id >= 1995 & year_id <= 2021]

## see what sectors are provided: 
unique(df$sector_name)
# "Budgetary central government", 
# "Extrabudgetary central government", 
# "Central government (incl. social security funds)",
# "Social security funds"                           
# "Local governments"                               
# "Central government (excl. social security funds)"
# "General government"                              
# "State governments"

# Based on documentation here: https://datahelp.imf.org/knowledgebase/articles/577248-in-the-government-finance-statistics-gfs-what-c
# use the 'all government' aggregate "General government"
desired_sectors = c("General government")
# desired_sectors = c("Central government (incl. social security funds)",
#                     "Local governments", 
#                     "General government", 
#                     "State governments")

## units available:
unique(df$unit_name)
# "Percent of GDP", "Domestic currency", "Percent of total expenditure"

# unit of interest: 
desired_unit = "Percent of total expenditure"

## filter to the desired unit and sectors: 
df = df[unit_name == desired_unit & sector_name %in% desired_sectors]
head(df)

### Edit location names to match GBD 
# load GBD location meta data
locs = fread('FILEPATH/location_set35_release9.csv')[level == 3]
# filter to country level

# identify mismatching location names
mismatch_names = unique(df[!country_name %in% locs$location_name]$country_name)
print(mismatch_names)
# [1] "China, P.R.: Hong Kong"       "Azerbaijan, Rep. of"          "Armenia, Rep. of"            
# [4] "Belarus, Rep. of"             "Afghanistan, Islamic Rep. of" "China, P.R.: Mainland"       
# [7] "China, P.R.: Macao"           "Bolivia"                      "Estonia, Rep. of"            
# [10] "Egypt, Arab Rep. of"          "Czech Rep."                   "Croatia, Rep. of"            
# [13] "Iran, Islamic Rep. of"        "Moldova, Rep. of"             "Kazakhstan, Rep. of"         
# [16] "Korea, Rep. of"               "Kyrgyz Rep."                  "Kosovo, Rep. of"             
# [19] "Slovenia, Rep. of"            "Serbia, Rep. of"              "San Marino, Rep. of"         
# [22] "Netherlands, The"             "Poland, Rep. of"              "Slovak Rep."                 
# [25] "Nauru, Rep. of"               "United States"                "Yemen, Rep. of"              
# [28] "Türkiye, Rep of"              "Timor-Leste, Dem. Rep. of"    "Uzbekistan, Rep. of"         
# [31] "Tajikistan, Rep. of"       

## align in for loop
df[, location_name := country_name] # initialize column to replace names in
unfixed = c() # initalize empty list to store country names that failed to align
for(name in mismatch_names){
  # if name contains a "," pull the string before the ","
  if(grepl(",", name)){
    test_name = strsplit(name, ",")[[1]][1]
  } else { # otherwise, use as is
    test_name = name
  }
  
  # look for a name match in the GBD location meta data
  GBD_name = locs[location_name %like% test_name]$location_name
  
  if(identical(GBD_name, character(0))){
    # if GBD_name returns nothing, print the country_name that could not be matched
    print(paste("Could not find match for:", name))
    unfixed = c(unfixed, name) # store name in list
    
  } else if (length(GBD_name) > 1){ 
    # if more than one possible match is found, print the name and the possible matches
    print(paste("Multiple matches found for:", name))
    print(paste("Matches:", paste(GBD_name, collapse = ", ")))
    unfixed = c(unfixed, name) # store name in list
  }
  else {
    # if a match is found, update the location_name
    df[country_name == name, location_name := GBD_name]
  }
}

# see what names still need to be fixed
print(unfixed)
# "China, P.R.: Hong Kong" "China, P.R.: Macao"     "China, P.R.: Mainland"  
# "Czech Rep."             "Korea, Rep. of"         "Kyrgyz Rep."           
# "Kosovo, Rep. of"        "Slovak Rep."            "United States" 

# Hong Kong & Macao are considered subnationals of China in GBD location hierarchy
# Remove these from data
df = df[country_name != "China, P.R.: Hong Kong"]
df = df[country_name != "China, P.R.: Macao"]

# Fix Mainland China
df[country_name == "China, P.R.: Mainland", location_name := "China"]

# Fix "Czech Rep." 
locs[location_name %like% "Czech"]
df[country_name == "Czech Rep.", location_name := "Czechia"]

# Fix "Korea, Rep. of"
df[country_name == "Korea, Rep. of", location_name := "Republic of Korea"]

# Fix "Kyrgyz Rep." 
locs[location_name %like% "Kyrgyz"]
df[country_name == "Kyrgyz Rep.", location_name := "Kyrgyzstan"]

# Fix "Kosovo, Rep. of" 
locs[location_name %like% "Kosovo"]
# Not in GBD location hierarchy, remove from data
df = df[country_name != "Kosovo, Rep. of"]

# Fix "Slovak Rep."
locs[location_name %like% "Slovak"]
df[country_name == "Slovak Rep.", location_name := "Slovakia"]

# Fix "United States"
df[country_name == "United States", location_name := "United States of America"]

# check for any remaining misalignments
df[!location_name %in% locs$location_name]$country_name

# select just the relevant columns
df = df[, c("year_id", "location_name", "unit_name", "social_protection_expend")]

# merge in location meta data
df = merge(df, locs[,.(location_id, location_name)], by = "location_name")
# double check all countries have a location_id now
nrow(df[is.na(location_id)])

# save out cleaned data 
covar_dir = "FILEPATH"
fwrite(df, file.path(covar_dir, "FILEPATH/social_protections_expenditure.csv"))

# save out number of years of data available for each country
df_N = df[, .(N_years = .N), by = .(location_name, location_id)]
fwrite(df_N, file.path(covar_dir, "FILEPATH/social_protection_N.csv"))

