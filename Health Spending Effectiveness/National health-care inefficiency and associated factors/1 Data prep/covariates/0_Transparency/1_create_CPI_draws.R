##########################################################################
### Author: USERNAME
### Date: 07/17/24
### Project: Health Spending Effectiveness 
### Purpose: Generating draws from Global Corruption Perceptions Index Score uncertainty intervals
##########################################################################

# clear environment
rm(list=ls())


# load libraries and functions
library(data.table)

# load data
output_dir = "FILEPATH"
cpi11 = fread(paste0(output_dir, 'CPI_scores_2000_2011.csv'))
cpi23 = fread(paste0(output_dir, 'CPI_scores_2012_2023.csv'))

# noticed some unrealistic/erroneous values in the upper column 
cpi11[iso == "EST" & year == 2007]$upper <- 70
cpi11[iso == "VEN" & year == 2005]$upper <- 24

# calculate standard errors from uncertainty interval for 2000-2011 data
cpi11[, standard_error := (upper - lower)/3.29] # divide by 3.29 here since the 90% confidence interval is reported

# 2011 data provided the standard deviation in the 'interval' column
# assign as standard error
cpi11[year == 2011, standard_error := as.numeric(interval)]

# change column names to append
setnames(cpi11, old = c("iso", "score"), 
         new = c("ihme_loc_id", "CPI_score"))

# append data together: 
df = rbind(cpi11[,.(year, country, ihme_loc_id, CPI_score, standard_error)], 
           cpi23[,.(year, country = location_name, ihme_loc_id, CPI_score, standard_error)])

# create 500 draws of scores for each year-location
tempDT <- data.table(t(apply(df[,.(CPI_score ,standard_error)], 1,
                             FUN = function(x) rnorm(500, mean = x[1], sd = x[2]))))
df = cbind(df, tempDT) # add draws to original data

# remove unnecessary columns: 
df[, c("CPI_score", "standard_error") := NULL]

# melt draws to long format: 
df_long = melt(df, id.vars = c("year", "country", "ihme_loc_id"), 
               variable.name = "draw")

# remove "V" from the draw column
df_long[, draw := as.numeric(gsub("V", "", draw))]
# rename "value" to "CPI_score"
setnames(df_long, "value", "CPI_score")

# remove NA values from draws: 
df_long = df_long[!is.na(CPI_score)]

# higher CPI score = less government corruption
# adjust scale to higher CPI score = more government corruption for ease of understanding
df_long[, CPI_score := 100 - CPI_score]
# score = 100 before, now = 0 (no corruption)
# score = 0 before, now = 100 (most corruption)

# merge in location IDs/names
source(paste0("FILEPATH","get_location_metadata.R"))
# get location IDs from metadata; location_set_id=35: -> GBD Model results
locs <- get_location_metadata(location_set_id=35, 
                              release_id = 9)  # GBD 2021
loc_ids = locs[level == 3, .(ihme_loc_id, location_id, location_name)] # level 3 = countries

df_long = merge(df_long, loc_ids, by = "ihme_loc_id", all.x = TRUE) 

# locations misaligning: 
df_long[is.na(location_id), unique(ihme_loc_id)]
# "HKG" "KSV" "MAC" "YUG"

cpi11[ihme_loc_id %like% "HKG"] # Hong Kong
cpi11[ihme_loc_id %like% "KSV"] # Kosovo
cpi11[ihme_loc_id %like% "MAC"] # Macau
cpi11[ihme_loc_id %like% "YUG"] # Yugoslavia

# Hong Kong and Macau are sub-nationals in GBD
df_long = df_long[!is.na(location_id)]

# change year to year_id for consistency 
setnames(df_long, old = "year", new = "year_id")
# remove unnecessary country column
df_long[, country := NULL]

# Save out CPI score draws: 
fwrite(df_long, paste0(output_dir, 'CPI_scores_draws_2000_2023.csv'))
