
##########################################################################
### Author: Amy Lastuka
### Date: 09/05/2023
### Project: Global dementia spending
### Purpose: estimate theta (proportion of undiagosed PLWD who are
#                  living in nursing homes) 
##########################################################################

rm(list=ls()) 


library(data.table)
library(ggplot2)

source(paste0(functions_dir,"get_location_metadata.R"))
locs <- get_location_metadata(location_set_id=22, release_id = 6) 
# location_set_id=22: -> covariate computation
# release_id = 6 -> GBD 2019
loc_ids <- locs[level == 3]$location_id #filter to country level 

# load GAMMA, BETA, and BETA_INST

stgpr_path <- "FILEPATH"

beta_path <- "FILEPATH"
beta_run <- '06'

beta_inst_path <- "FILEPATH"
beta_inst_run <- '06'

gamma_path <- "FILEPATH"
gamma_run <- 6

# Loading probability of living in an institution/comunity given that you have a diagnois
gamma <- fread(paste0(gamma_path, 'gamma_model_',gamma_run,'_draws.csv')) # These are from ST-GPR
setnames(gamma, old = "value", new = 'gamma')


# Loading "overall" beta
beta <- fread(paste0(beta_path,'dx_rate_all_draws_run_',beta_run,'.csv'))
setnames(beta, old = "value", new = "beta")


# loading beta institution
beta_inst <- fread(paste0(beta_inst_path,'institution_dx_rate_draws_run_',beta_inst_run,'.csv'))
setnames(beta_inst, old = c("draw","dx_rate"), new = c("variable","beta_institution"))


data <- merge(beta, beta_inst, by = c('year_id', 'location_id', 'variable'))
data <- merge(data, gamma, by = c('year_id', 'location_id', 'variable'))


# cut global location & also subset to nationals only
data <- data[(location_id != 1),]
data <- data[location_id %in% loc_ids,]
head(data)

################################################################################
#
# Calculate theta
#
################################################################################




data[, nursing_home_ratio := ((beta * (1 - beta_institution))/((1-beta)*beta_institution))]

data[, theta := gamma * nursing_home_ratio]

summary(data$theta)

data[, delta_beta := beta_institution - beta]
summary(data$delta_beta)
data[delta_beta <0, .N, by = c('year_id', 'location_id')]

data[, delta_gamma_theta := gamma - theta]
summary(data$delta_gamma_theta)
data[delta_gamma_theta <0, .N, by = c('year_id', 'location_id')]

data_th <- data[,c('year_id', 'location_id', 'variable', 'theta')]

setDT(data_th)

fwrite(data_th,"FILEPATH/theta_draws.csv", row.names = F)

