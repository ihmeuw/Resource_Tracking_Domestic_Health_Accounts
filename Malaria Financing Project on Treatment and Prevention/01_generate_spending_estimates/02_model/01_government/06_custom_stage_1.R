####################################################################################
## Creator: USERNAME
## 
## Description: Prep the data to be used for ST-GPR and run stage 1 of ST-GPR for Malaria OOP
####################################################################################

rm(list = ls())

### runtime configuration
if (Sys.info()["sysname"] == "Linux") {
  j_root <- "ADDRESS"
  h_root <- "ADDRESS"
} else {
  j_root <- "ADDRESS"
  h_root <- "ADDRESS"
}

# load in packages
pacman::p_load(data.table, tidyverse, lme4)
source('FILEPATH')
source('FILEPATH')


#################################################################
# set parameters
location_set_version_id = 397
gbd_round_id = 7
years <- 2000:2020


#################################################################

## %%%%%%%%%%%%
## Read files
## %%%%%%%%%%%%

setwd("ADDRESS")

## main data
final.dt <- fread("FILEPATH")

## custom covariates
cov_sq <- fread('FILEPATH')

## %%%%%%%%%%%%
## define function to model stage 1, untransform data, and return prepped data and custom prior 
## %%%%%%%%%%%%

stage_1 <- function(data, ssa_formula, non_ssa_formula, me_name){
  
  LMER_ssa <- lmer(ssa_formula, data[grepl("Sahara", region_name) & flag_cd == 0])  
  LMER_non_ssa <- lmer(non_ssa_formula, data[!grepl("Sahara", region_name) & flag_cd == 0])  
  
  cv_ssa <- cov_sq[grepl("Sahara", region_name),
                   .(location_id, 
                     year_id, 
                     me_name = me_name, 
                     sex_id = 3,
                     age_group_id = 22, 
                     cv_custom_stage_1 = inv_logit_trans(predict(LMER_ssa,
                                                                 cov_sq[grepl("Sahara", region_name)], 
                                                                 allow.new.levels = T), lemon = F))] ## predict based on covariates and prior model, convert to normal space
  
  cv_non_ssa <- cov_sq[!grepl("Sahara", region_name),
                       .(location_id, 
                         year_id, 
                         me_name = me_name, 
                         sex_id = 3,
                         age_group_id = 22, 
                         cv_custom_stage_1 = inv_logit_trans(predict(LMER_non_ssa, 
                                                                     cov_sq[!grepl("Sahara", region_name)],
                                                                     allow.new.levels = T), lemon = F))] ## predict based on covariates and prior model, convert to normal space
  
  cv <- rbind(cv_non_ssa, cv_ssa)
  
  d <- data[,
            .(location_id,
              year_id,
              val = y_frac,
              nid = 99999,
              sample_size = 1000,
              age_group_id = 22,
              sex_id = 3,
              me_name = me_name,
              measure_id = 18,
              is_outlier = ifelse(flag_cd == 0, 0, 1),
              variance = var(y_frac))]
  
  return(list(data = d, custom_prior = cv))
}

## %%%%%%%%%%%%
## run and write data from stage 1 models 
## %%%%%%%%%%%% 

mod_ssa <- fread('FILEPATH')
mod_ssa <- mod_ssa$model[2]

mod_non_ssa <- fread("FILEPATH")
mod_non_ssa <- mod_non_ssa$model[2]

### ----------------------------------------------------------
### checking for multicollinearity 
### ----------------------------------------------------------

mod_non_ssa_inc <- str_remove(mod_non_ssa, "\\+ logit_mal_prev")
mod_non_ssa_prev <- str_remove(mod_non_ssa, "\\+ logit_mal_inc")

data <- final.dt[!grepl("Sahara", region_name)]

LMER_non_ssa <- lmer(mod_non_ssa, data)  
LMER_non_ssa_inc <- lmer(mod_non_ssa_inc, data)  
LMER_non_ssa_prev <- lmer(mod_non_ssa_prev, data)  

library(carData, lib.loc = "ADDRESS")
library(car, lib.loc = "ADDRESS")

r2_and_vif <- function(model){
  VIF <- vif(model)
  Rsqared <- (VIF-1)/(VIF)
  return(list("VIF" = VIF,
              "R2" = Rsqared))
}

r2_and_vif(LMER_non_ssa)
r2_and_vif(LMER_non_ssa_inc)
r2_and_vif(LMER_non_ssa_prev)


### ----------------------------------------------------------
### checking for best model
### ----------------------------------------------------------

## ********
## AIC BIC
## ********
AIC(LMER_non_ssa) # both = best AIC
AIC(LMER_non_ssa_inc)
AIC(LMER_non_ssa_prev)

BIC(LMER_non_ssa) #both = best BIC
BIC(LMER_non_ssa_inc)
BIC(LMER_non_ssa_prev)

## ********
## OOS RMSE
## ********
library(modelr)
temp <- copy(data)
fold <- 10

## assign folds
temp$folds <- rep_len(sample(fold), length.out = nrow(temp))

oos_rmse_list <- numeric()
is_rmse_list <- numeric()


formula <- mod_non_ssa

for(i in 1:fold){ # "i" is the fold to be left out
  
  model <- lmer(formula, data = temp[folds!=i])
  temp[, paste0("resid_", i)] <- predict(model, newdata=temp,allow.new.levels = T) - temp$y
  
  # get oos rmse for i th fold, and add to the list
  oos_rmse <- sqrt(mean(temp[folds ==i,get(paste0("resid_", i))]^2, na.rm = T))
  oos_rmse_list[i] <- oos_rmse
  
  # get in sample rmse for i th fold, and add to the list
  is_rmse <- sqrt(mean(temp[folds !=i,get(paste0("resid_", i))]^2, na.rm = T))
  is_rmse_list[i] <- is_rmse
  
}


## then calculate mean rmse from the 10 folds
oos.rmse <- mean(oos_rmse_list)
is.rmse <- mean(is_rmse_list)


### ----------------------------------------------------------
### outputting final results
### ----------------------------------------------------------

stage_1_out <- stage_1(data = final.dt,
                       ssa_formula = mod_ssa,
                       non_ssa_formula = mod_non_ssa,
                       me_name = "fs_malaria_domestic_public")

## check for bad values
stage_1_out[["custom_prior"]][cv_custom_stage_1 <= 0]
stage_1_out[["custom_prior"]][cv_custom_stage_1 >= 1]
stage_1_out[["data"]][val <= 0]
stage_1_out[["data"]][val >= 1]

fwrite(stage_1_out[["data"]], "FILEPATH")
fwrite(stage_1_out[["custom_prior"]], "FILEPATH")

## End of Script ##