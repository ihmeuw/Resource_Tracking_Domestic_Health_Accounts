# Select and rename columns from new data and recode values as needed to match
# those found in previously cleaned file. Append data as new rows to old data.
# Author: Elye Bliss
# Date: 07/14/2023

# Clear environment
rm(list = ls())

# Loading libraries
library(data.table)
library(ggplot2)

options(scipen = 1000)

new_data <- fread("FILEPATH/02_filtered_by_measure.csv")

prev_data <- fread("FILEPATH/10.adds_covariates.2021.csv")
prev_data$the_pc <- NULL # we will replace previously-merged covariates with newly-pulled

################## Note missing column names and recode values #################

prev_cols <- colnames(prev_data)
cols_to_change <- prev_cols[!(prev_cols %in% colnames(new_data))]

# Add location ids to new_data
source("FILEPATH/get_location_metadata.R")

locs <- as.data.table(get_location_metadata(location_set_id=1,release_id=6))
new_data <- merge(new_data,locs[,.(ihme_loc_id,location_id)],by = 'ihme_loc_id', all.x = T)
                  


# cost components were lower cased in later script in previous pipeline:
prev_data[, cost_components := tolower(cost_components)]

# standardize case in values for other variables as well
new_data[, care_type := tolower(care_type)]
prev_data[, care_type := tolower(care_type)]

################################### rbind data #################################

data <- rbind(prev_data, new_data)

dim(data) # rows 1323 by 11 cols (reweighted by severity)

############################ Recode/standardize data ###########################

setnames(data, old = 'care_type', new = 'care_settings')
data[, care_settings := as.factor(care_settings)]
data[, cost_components := as.factor(cost_components)]
data[, cost_type := as.factor(cost_type)]


# recode payer type 
data[payer_type == "" | payer_type == 'all', payer_type := 'all' ]

########################### Save output ########################################

fwrite(data, file = "FILEPATH/03_combined_with_prev.csv")
