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
pacman::p_load(data.table, dplyr, tidyr, reshape2, ggplot2,foreign,readstata13, feather, gtools)

# source in locations
source("FILEPATH")
location <- get_location_metadata(location_set_id = 22, gbd_round_id = 7)
loc <- location[level == 3,.(location_name, ihme_loc_id, location_id, region_name, super_region_name)]

setwd('ADDRESS')

dt <- fread(paste0('FILEPATH'))

#===================================================

for (j in unique(dt$value_code)) {
    
    data <- copy(dt)
    data <- data[value_code == j]
    data <- data[!is.na(y), .(location_id, year_id, value_code, data, y, y_frac, source_type, hs_public)]
    
    # merge in the covariates
    cov <- fread('FILEPATH')
    data <- merge(cov, data, by = c('location_id','year_id'))
    
    # calculate cutoffs for cook's distance: 4/(# of obs)
    cutoff <- unique(data[, .(super_region_name, cutoff = 4/length(y))])
    
    #===================================
    # get cook's distance for each model
    #===================================
    
    
    #------------------------------------------------------------
    # after model selection - LHS in fraction space
    mod <- fread(paste0('FILEPATH'))
    mod <- mod[oos_rmse == min(oos_rmse), model]
    model <- lmer(as.formula(mod), data = data)
    infl <- influence(model, obs = T)
    cd <- cooks.distance(infl)
    
    #######################################################################
    # assign points as high leverage points (outliers) based on cook's distance
    #######################################################################
    
    myfunc <- function(X, Y){
      X[, cookd := Y]
      return(X)
    }
    
    data.new <- myfunc(data, cd)
    
    # merge in cutoff and identify outliers
    data.new <- merge(data.new, cutoff, by = c('super_region_name'))
    data.new[,flag_cd := ifelse(cookd < cutoff, 0, 1)]
    
    
    if (j == 'Diagnostics') {
      final <- copy(data.new)
    } else {
      final <- rbind(final, data.new)
    }
    
  
}

final[, sex_id := 3]
final[, age_group_id := 22]
final <- final[, .(location_id, year_id, sex_id, age_group_id, value_code, y_frac, flag_cd)]

## Write out data
fwrite(final, paste0('ghes_disaggregated_cookd_dah_version_', data_date, '.csv'))


## End of Script ##