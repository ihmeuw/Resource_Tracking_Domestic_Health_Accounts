
##########################################################################
### Author: Amy Lastuka
### Date: 11/09/2023
### Project: Global dementia spending
### Purpose: estimate forecast theta values for multiple scenarios:
###          baseline scenario
###          accelerated beta (beta and beta_inst are different)
###          accelerated gamma (gamma is different)
###          decelerated gamma (gamma is different)
##########################################################################

rm(list=ls()) 

library(data.table)
library(ggplot2)


source(paste0(functions_dir,"get_location_metadata.R"))
locs <- get_location_metadata(location_set_id=22, release_id = 6) 
# location_set_id=22: -> covariate computation
# release_id = 6 -> GBD 2019
loc_ids <- locs[level == 3]$location_id #filter to country level 

## initialize which forecast scenario is being run: 
# scenario = 0 -> Reference Scenario (baseline)
# scenario = 1 -> Accelerated Beta
# scenario = 2 -> Accelerated Gamma
# scenario = 3 -> Decelerated Gamma
scenario = 3

# load GAMMA, BETA, and BETA_INST


# Loading "overall" beta

beta_filename = ifelse(scenario == 1, "FILEPATH/accelerated_beta_forecast.csv",
                       "FILEPATH/reference_beta_forecast.csv")
beta <- fread(beta_filename)

setnames(beta,"data_var","beta")


# loading beta institution
beta_inst_filename = ifelse(scenario == 1, "FILEPATH/accelerated_beta_inst_forecast.csv",
                            "FILEPATH/reference_beta_inst_forecast.csv")
beta_inst <- fread(beta_inst_filename)


# loading gamma
if(scenario == 2){
  gamma_filename <- "FILEPATH/accelerated_gamma_forecast.csv"
}else if(scenario == 3){
  gamma_filename <- "FILEPATH/decelerated_gamma_forecast.csv"
}else{
  gamma_filename <- "FILEPATH/reference_gamma_forecast.csv"
}

gamma <- fread(gamma_filename)
setnames(gamma,"data_var","gamma")


data <- merge(beta, beta_inst, by = c('year_id', 'location_id', 'draw'))
data <- merge(data, gamma, by = c('year_id', 'location_id', 'draw'))



################################################################################
#
# Calculate theta
#
################################################################################


data[, nursing_home_ratio := ((beta * (1 - beta_inst))/((1-beta)*beta_inst))]

# cap theta at 1
# theta = gamma*((beta * (1 - beta_inst))/((1-beta)*beta_inst))
data[, theta := pmin(gamma * nursing_home_ratio,1)]


data_th <- data[,c('year_id', 'location_id', 'draw', 'theta')]

setDT(data_th)


if(scenario == 1){
  theta_filename <- "FILEPATH/theta_for_accelerated_beta_forecast.csv"
}else if(scenario == 2){
  theta_filename <- "FILEPATH/theta_for_accelerated_gamma_forecast.csv"
}else if(scenario == 3){
  theta_filename <- "FILEPATH/theta_for_decelerated_gamma_forecast.csv"
}else{
  theta_filename <- "FILEPATH/reference_theta_forecast.csv"
}

# write out the theta projections 
fwrite(data_th,theta_filename)

