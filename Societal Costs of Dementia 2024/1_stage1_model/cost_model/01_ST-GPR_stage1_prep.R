# Use final cost model specification to make fitted values for all location ids
# required by ST-GPR, and for each level in our factor variables. Output file
# saved as cost_fe_model_estimates.csv
# Author: Elye Bliss
# Date: 08/03/2023

# Clear environment
rm(list = ls())

# Loading libraries
library(data.table)

# Get location IDs from metadata
source("FILEPATH/get_location_metadata.R")

# location_set_id=22: -> Using set 22, which is for covariate computation
# release_id = 6 -> GBD 2019
loc_ids <- get_location_metadata(location_set_id=22, release_id = 6) 

# Only keep needed columns
loc_ids <- loc_ids[, c('ihme_loc_id', 'location_id', 'super_region_id', 'super_region_name','region_id', 'region_name')]

# Load final filtered data used for model 
data <- fread("FILEPATH/final_data_for_model_training.csv")

# Load model
model <- readRDS("FILEPATH/stage1_cost_model.rda")

# Replace covariate file with full the_pc data, as ST-GPR must include all 
# location levels (country as well as subnational) for 'square':
covars <- fread(file = "FILEPATH/all_covars_1990_2019.csv")

# The population data has all location_ids needed by ST-GPR. Merge those location_ids
# to get a complete square (note, population covariate not needed)
pop = fread("FILEPATH/population_1990_2019.csv")
covars = merge(covars, pop[,.(location_id, year_id)], all.y = T, by = c("year_id","location_id"))

# log main covariate, and merge location ids
covars[, lg_gdp_pc := log(gdp_pc)]
covars <- covars[, .(year_id,location_id,location_name,lg_gdp_pc,gdp_pc)]
covars <- merge(covars, loc_ids, by = c('location_id'))

# Creating copy of data for each level in our factor variables from model
covars[, cost_components := 'total']
covars[, cost_type := 'direct']
covars[, payer_type := 'all']

# creating copies of the df for care settings
com <- copy(covars)
com[, care_type := 'community']
fac <- copy(covars)
fac[, care_type := 'institutional']
tt <- copy(covars)
# The 'total' level in care_type is new this round, compared to the previous pipeline
tt[, care_type := 'total'] 

cs <- rbind(com, fac, tt)

# Making predictions using the as covariate
cs[, lg_cost_GDP := predict(object = model, cs, allow.new.levels = T)]
cs[, pred_cost_GDP := exp(lg_cost_GDP)]

tp <- data[, c('ihme_loc_id', 'location_id', 'year_estimate', 'spending_clean', 'cost_type', 'care_type', 'cost_components', 'sample_size')]

# Adding raw data to predictions
total <- merge(cs, tp, 
             by.x = c('location_id', 'ihme_loc_id', 'year_id', 'cost_components', 'cost_type', 'care_type'), 
             by.y = c('location_id', 'ihme_loc_id','year_estimate', 'cost_components', 'cost_type', 'care_type'), all.x = T )


# Saving file with predictions
fwrite(total, 
       "FILEPATH/cost_model_estimates_11_09.csv", 
       row.names = F)
