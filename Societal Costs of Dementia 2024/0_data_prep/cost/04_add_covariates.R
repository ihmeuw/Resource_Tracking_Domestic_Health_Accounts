# Add covariates extracted from get_covariate_data.R
# Author: Elye Bliss
# Date: 07/18/2023

# Clear environment
rm(list = ls())

# Loading libraries
library(data.table)
library(ggplot2)

options(scipen = 1000)

data <- fread("FILEPATH/03_combined_with_prev.csv")
covars <- fread("FILEPATH/all_covars_1990_2019.csv")

# Pull location id file from previous pipeline
prev_location <- fread("FILEPATH/locs_loc_set_id_22.csv")


########################### Merge covars #######################################

# Merge the `ihme_loc_id` codes from the previous reference table
covars <- merge(covars, prev_location[, .(location_id, ihme_loc_id)],
  by = c("location_id"), all.x = T)


# merge covars to data on ihme_loc_id==map_id
data <- merge(data, covars,
  by.x = c("ihme_loc_id", "year_estimate"),
  by.y = c("ihme_loc_id", "year_id"), all.x = T)

test <- data[is.na(gdp_pc), ]
# 0 observations lost

dim(data) # 1323 by 24

########################### Save output ########################################

# Write to Intermediate directory
fwrite(data, file = "FILEPATH/04_merged_with_covariates.csv")
