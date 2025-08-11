##########################################################################
### Author: USERNAME
### Date: 07/17/24
### Project: Health Spending Effectiveness 
### Purpose: Extracting Global Corruption Perceptions Index Scores
##########################################################################

# clear environment
rm(list=ls())


# load libraries and functions
library(data.table)
library(dplyr)
library(assertthat)
library(readxl)

############################ Process 2012-2023 Data ############################
# read in corruption data: 
data_dir = "FILEPATH"
raw_df = read_xlsx(paste0(data_dir, "CPI2023_Global_Results__Trends.xlsx"), 
                   sheet = "CPI Timeseries 2012 - 2023", skip = 3) # first three rows are title etc. 
setDT(raw_df) # set data.table

## melt CPI scores from each year to long format
# extract only the columns of interest 
score_cols = colnames(raw_df)[colnames(raw_df) %like% "CPI score " | # get all columns with CPI score
                                colnames(raw_df) %like% "CPI Score "]

# keep country name, iso3, and score columns
keep_cols = c("Country / Territory", "ISO3", score_cols)
score_df = raw_df[, ..keep_cols]

# change column names
# extract year from column names
new_scores = sub("CPI Score ", "", sub("CPI score ", "", score_cols))
setnames(score_df, old = keep_cols, 
         new = c("location_name", "iso3", new_scores))

# melt data to long format
scores2023 = melt(score_df, id.vars = c("location_name", "iso3"), 
                  variable.name = "year", value.name = "CPI_score")

# change year column to numeric
scores2023$year = as.numeric(as.character(scores2023$year))

## now melt Standard errors from each year to long format: 
# get all columns with CPI score
SE_cols = colnames(raw_df)[colnames(raw_df) %like% "Standard error "] 
# keep country name, iso3, and score columns
keep_cols = c("Country / Territory", "ISO3", SE_cols)
SE_df = raw_df[, ..keep_cols]

# change column names
# extract year from column names
new_SE = sub("Standard error ", "", SE_cols)
setnames(SE_df, old = keep_cols, 
         new = c("location_name", "iso3", new_SE))

# melt data to long format
SE2023 = melt(SE_df, id.vars = c("location_name", "iso3"), 
              variable.name = "year", value.name = "standard_error")

# change year column to numeric
SE2023$year = as.numeric(as.character(SE2023$year))

# merge long standard errors with long scores: 
CPI23 = merge(scores2023, SE2023, by = c("location_name", "iso3", "year"))

# merge in location IDs:
loc_dir = "FILEPATH"
locs = fread(paste0(loc_dir, "location_set35_release9.csv"))

setnames(CPI23, old = c('iso3', "location_name"), 
         new = c('ihme_loc_id', "short_name"))

CPI23 = merge(CPI23, locs[,.(location_id, location_name,ihme_loc_id)], 
            by = c("ihme_loc_id"), all.x = TRUE)

# location misalignment: 
unique(CPI23[is.na(location_id)]$short_name)
#  "Hong Kong" "Kosovo"

# missing scores for: 
table(CPI23[is.na(CPI_score), .(short_name)])

# save out data as we have it now: 
output_dir = "FILEPATH"
fwrite(CPI23, paste0(output_dir, 'CPI_scores_2012_2023.csv'))

############################ Process 2000-2011 Data ############################

# Read in all data; each year of data is in it's own file
files <- list.files(paste0(data_dir, "1995_2011/"))

# remove files from before 2000: 
files = files[!grepl("199[5-9]", files)]

# initialize empty data frame to store data in 
CPI11 = data.table()
# loop over each file in files
for (f in files) {
  # set up filepath 
  fp = paste0(data_dir, "FILEPATH", f)
  # read in the file
  file_df<- fread(fp)
  
  # get the year from the file name
  if (grepl("Archive", f)) { # if file name contains 'Archive" in it
    # extract the 4 numbers following 'CPI-Archive-' from file name
    f_year = sub("CPI-Archive-([0-9]{4}).*", "\\1", f)
  } else {
    # extract the 4 numbers following 'CPI-' from file name
    f_year = sub("CPI-([0-9]{4}).*", "\\1", f)
  }
  # add the year to the data
  file_df$year = as.numeric(f_year)
  
  # update 2005 data interval column name
  if (f_year == "2005") {
    setnames(file_df, old = "range", new = "interval")
  }
  
  # in 2008 data one of the intervals has a hyphen where a period should be:
  # Montenegro 2008 interval Currently: '2-5 - 4.0'
  # Change to: '2.5 - 4.0'
  if (f_year == "2008"){
    file_df[iso == "MNE"]$interval <- sub("-", ".", file_df[iso == "MNE"]$interval)
  }
  
  # extract lower and upper bounds from interval column: 
  file_df[, c("lower", "upper") := tstrsplit(interval, ".-.")]
  # convert lower and upper to numeric
  file_df$lower = as.numeric(file_df$lower)
  file_df$upper = as.numeric(file_df$upper)
  
  # save year of data to dataframe
  CPI11 = rbind(CPI11, file_df)
}

# Check to see if an NAs were introduced: 
sum(is.na(CPI11))

# update scores with commas to decimals: 
CPI11$score <- sub(",", ".", CPI11$score)

# Scale scores from 0-10 to 0-100 to align with 2012-2023 scores
CPI11$score <- as.numeric(CPI11$score)
CPI11[, ":=" (score = score*10,
              lower = lower*10, 
              upper = upper*10)]

# The values in the 'interval' column in the 2011 data is
# the standard deviation. Assign the lower and upper bounds to NA
CPI11[year == 2011, ":=" (lower = NA_real_, 
                          upper = NA_real_)]


# save out 2000-2011 data
output_dir = "FILEPATH"
fwrite(CPI11, paste0(output_dir, 'CPI_scores_2000_2011.csv'))
