# Use wage model to make fitted values for all location ids
# required by ST-GPR, and for both sexes (our factor variable). Output file
# saved as wage_fe_model_estimates.csv
# Author: Elye Bliss
# Date: 08/11/2023

rm(list = ls())

library(data.table)

# Get location IDs from metadata
source("FILEPATH/get_location_metadata.R")

# location_set_id=22: -> Using set 22, which is for covariate computation,
# in order to be consistent with location set id that was used for pulling covariates.
# release_id = 6 -> GBD 2019
loc_ids <- get_location_metadata(location_set_id=22, release_id = 6) 

# Only keep needed columns
loc_ids <- loc_ids[, c('ihme_loc_id', 'location_id', 'super_region_id', 'super_region_name','region_id', 'region_name')]

# Load final filtered data used for model 
data <- fread("FILEPATH/ILO_removed_outliers.csv")

# Load model
model <- readRDS("FILEPATH/stage1_ILO_wage_model.rda")

# Replace covariate file with full gdp_pc data for 'square':
covars <- fread(file = "FILEPATH/gdp_pc_1950_2027.csv")

# The population data has all location_ids needed by ST-GPR. Merge those location_ids
# to get a complete square (note, population covariate not needed)
pop = fread("FILEPATH/population_1990_2019.csv")
covars = merge(covars, pop[,.(location_id, year_id)], all.y = T, by = c("year_id","location_id"))

# log main covariate, and merge location ids
covars[, lg_gdp_pc := log(gdp_pc)]
covars <- covars[, .(year_id,location_id,location_name,lg_gdp_pc,gdp_pc)]
covars <- merge(covars, loc_ids, by = c('location_id'))

# Creating copy of data for each sex (the factor variable from model)

males <- copy(covars)
males[, sex := 'male']
females <- copy(covars)
females[, sex := 'female']

total <- rbind(males,females)

# Making predictions using gdp_pc covariate
total[, lg_income_GDP := predict(object = model, total, allow.new.levels = T)]
total[, pred_income_GDP := exp(lg_income_GDP)]

# Adding raw data to predictions
raw_data <- data[, c('ihme_loc_id', 'year_id', 'income', 'sex')]

# Adding raw data to predictions
estimates <- merge(total, raw_data, 
               by= c('ihme_loc_id', 'year_id', 'sex'), all.x = T )

# Saving file with predictions
fwrite(estimates, 
       "FILEPATH/income_fe_model_estimates.csv",
       row.names = F)

