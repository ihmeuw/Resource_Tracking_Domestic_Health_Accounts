# ######################################################
# USERNAME
# 
# Description: Identify outliers/high leverage points for each health sector model using cook's distance 
# ######################################################

rm(list = ls())

# system config
if (Sys.info()["sysname"] == "Linux") {
  j <- "ADDRESS" 
  h <- "ADDRESS"
} else { 
  j <- "ADDRESS"
  h <- "ADDRESS"
}

library(lme4)
pacman::p_load(data.table, dplyr, tidyr, reshape2, ggplot2, foreign, readstata13, feather, gtools)

# install dependency for cook's distance in my home drive
library(influence.ME, lib.loc = paste0(h, 'ADDRESS'))

# source in locations
source("FILEPATH")
location <- get_location_metadata(location_set_id = 22, gbd_round_id = 7)
loc <- location[level == 3,.(location_name, ihme_loc_id, location_id, region_name, super_region_name)]

setwd('ADDRESS')

data <- fread(paste0('FILEPATH'))

#===================================================
# read in data
data_new_list <- list()

  data <- fread(paste0('FILEPATH'))
  data <- data[!is.na(y),.(location_id,year_id,data,y,y_frac,source_id,source_type,hs_public)]
  
  # merge in the covariates
  cov <- fread('FILEPATH')
  data <- merge(cov, data, by = c('location_id','year_id'))
  
  # calculate cutoffs for cook's distance: 4/(# of obs)
  cutoff_ssa <- data[super_region_name == 'Sub-Saharan Africa',.(cutoff = 4/length(y)), by = 'super_region_name']
  cutoff_non_ssa <- unique(data[super_region_name != 'Sub-Saharan Africa',.(super_region_name, cutoff = 4/length(y))])
  cutoff <- rbind(cutoff_ssa, cutoff_non_ssa)
  
  #===================================
  # get cook's distance for each model
  #===================================
  
  
  #------------------------------------------------------------
  # after model selection - LHS in fraction space
  ssa <- data[super_region_name == 'Sub-Saharan Africa']
  mod <- fread('FILEPATH')
  mod <- mod[oos_rmse == min(oos_rmse),model]
  ssa.model <- lmer(as.formula(mod), data = ssa)
  infl.ssa <- influence(ssa.model, obs = T)
  cd.ssa <- cooks.distance(infl.ssa)
  #------------------------------------------------------------
  non_ssa <- data[super_region_name != 'Sub-Saharan Africa']
  mod <- fread('FILEPATH')
  mod <- mod[oos_rmse == min(oos_rmse),model]
  non_ssa.model <- lmer(as.formula(mod), non_ssa)
  infl.non_ssa <- influence(non_ssa.model, obs = T)
  cd.non_ssa <- cooks.distance(infl.non_ssa)
  
  #######################################################################
  # assign points as high leverage points (outliers) based on cook's distance
  #######################################################################
  
  myfunc <- function(X, Y){
    X[, cookd := Y]
    return(X)
  }
  
  # store data and cook's distance in a list separately 
  list.dt <- list(non_ssa, ssa)
  list.cd <- list(cd.non_ssa, cd.ssa)
  
  # for each dt, map cook's distance values to each data point
  data.new <- mapply(function(x,y) myfunc(x,y), x = list.dt, y = list.cd, SIMPLIFY = F)
  data.new <- rbindlist(data.new)
  
  # merge in cutoff and identify outliers
  data.new <- merge(data.new, cutoff, by = c('super_region_name'))
  data.new[, flag_cd := ifelse(cookd < cutoff, 0, 1)]
  
  # store in the list
  data_new_list[[i]] <- data.new

## Write out data
fwrite(data_new_list[[1]], paste0('FILEPATH'))

## End of Script ##