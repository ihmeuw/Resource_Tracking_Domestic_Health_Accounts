##########################################################################
### Author: USERNAME
### Date: 12/05/2024
### Project: Health Spending Effectiveness 
### Purpose: Processing sex-aggregated education inequality index (Gini, age-standardized)
###          custom files from IHME education team
###########################################################################

# clear environment
rm(list=ls())


# load libraries
library(data.table)

# initialize directory paths
custom_path = "FILEPATH"

# read all the file paths in the directory
file_paths <- list.files(custom_path)
# contains files for each country and all sub-nationals
# national level data has only the country ISO code 
# subnationals have the country ISO and the subnational ID attached to the file name
# Ex: national -> "USA.rds"; subnational -> "USA_573.rds"
# Only need the national level estimates, so will filter out all the 
# file names with numbers in them to remove the subnationals

# filter to file_paths to only names that do not contain numbers
file_paths <- file_paths[!grepl("[0-9]", file_paths)]
length(file_paths) == 204 
# TRUE, now only have 204 file names for the 204 GBD countries

# read all the .RDS files in file_paths and append into a data.table
df <- rbindlist(lapply(file_paths, function(x) {
  readRDS(file.path(custom_path, x))
}))

# check the year range
range(df$year_id) # 1960 - 2024

# filter to our project years: 1995-2021
df <- df[year_id >= 1995 & year_id <= 2021]

# should have 27 years * 204 locations of data now: 
nrow(df) == 27 * 204 # TRUE

# change variable name
setnames(df, old = c("mean_value"), new = c("educ_ineq"))

# save out Education Relative Inequality (GINI) estimates
output_dir = "FILEPATH"
fwrite(df, file.path(output_dir, 'aggregate_education_inequality_GINI_age_std.csv'))
