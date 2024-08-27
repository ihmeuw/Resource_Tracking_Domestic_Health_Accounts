################################################################################
# Use final care settings model specification to make fitted values for all 
# location ids required by ST-GPR, and for each level in our factor variables. 
# Output file saved as care_settings_model_estimates.csv
# Author: Elye Bliss
# Date: 09/05/2023
################################################################################

# Clear environment
rm(list = ls())

# Loading libraries
library(data.table)

# Load model
model <- readRDS("FILEPATH/stage1_care_settings_model.rda")

# Replace covariate file with full the_pc data, as ST-GPR must include all 
# location levels (country as well as subnational) for 'square':
covars <- fread(file = "FILEPATH/gdp_pc_1950_2027.csv")

# Limit to GDP_pc, the main covariate used
covars <- covars[, .(year_id,location_id,location_name,gdp_pc)]
covars[,lg_gdp_pc := log(gdp_pc)]

# Use population covars to get all location ids required by ST-GPR
pop = fread("FILEPATH/population_1990_2019.csv")
covars = merge(covars, pop[,.(location_id, year_id)], all.y = T, by = c("year_id","location_id"))

# Making predictions using lg_gdp_pc as covariate
covars[, lg_gamma_gdp_pc := predict(object = model, covars)]
covars[, gamma_gdp_pc := exp(lg_gamma_gdp_pc)]

# Saving file with predictions
fwrite(covars, 
       "FILEPATH/care_settings_model_estimates.csv",
       row.names = F)
