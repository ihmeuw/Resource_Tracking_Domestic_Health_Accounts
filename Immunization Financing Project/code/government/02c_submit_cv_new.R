## #############################################################################
## Author:
## Project: Immunization financing
## Purpose: Cross validate the best AIC/BIC models for government spending on immunizations
##          
## Inputs: filepath to outputs from 02b
##
## Outputs: 10 candidate models with lowest OOS RMSE values,
##          02c_submit_cv folder files for each immunization component
##
## Last update: 11/03/2021
## #############################################################################
pacman::p_load(tidyverse, data.table, dplyr, feather, reshape2, ggplot2, foreach, lme4, parallel)

rm(list = ls())

## Defining j, h, and k 
if (Sys.info()[1] == "Linux"){
  j <- "FILEPATH"
  h <- paste0("FILEPATH", Sys.info()[7])
  k <- "FILEPATH"
} else if (Sys.info()[1] == "Windows"){
  j <- "FILEPATH"
  h <- "FILEPATH"
  k <- "FILEPATH"
}

##===================##
## Prepare workspace ##
##===================##

# set date
date <- format(Sys.Date(), '%m%d%y')


#############################################
## define cross validation function 
#############################################
custom_cv <- function(data,
                      mod
){
  
  formula <- as.formula(mod)
  
  temp <- copy(data)
  
  fold <- 10
  
  ## assign folds
  temp$folds <- rep_len(sample(fold), length.out = nrow(temp))
  
  oos_rmse_list <- numeric()
  is_rmse_list <- numeric()
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
  
  print('all folds are done, mean RMSE is calculated.')
  
  return(data.frame(model = mod,
                    is_rmse = is.rmse,
                    oos_rmse  = oos.rmse)
  )
}


#==============================##
# Get cross validation results ##
#==============================##

## immunzation components to loop through
components <- c('vaccine', 'routine', 'delivery', 'supplementary', 'immunization')

## run function to subset best models for each immunization component
for (i in components) {

  ## inputs path
  path <- paste0("FILEPATH/", i, "/")
  
  ## outputs path
  data_folder <- 'FILEPATH'
  
  ## load best AIC/BIC models
  load(paste0(path,"data_", date, ".RData"))
  load(paste0(path,"aic_bic_best_", date, ".RData"))
  
  
  ## write function to get model results for each model using custom_cv function - provide a numeric vector the length of the number of models
  get_model_results <- function(x) {
    mod_i <- aic_bic_dt$subset.models[x]
    model_results <- custom_cv(dataset, mod_i)
    model_results <- data.table(model_results)
    return(model_results)
  }
  
  
  ## apply function
  system.time(model_result_list <- mclapply(c(1:nrow(aic_bic_dt)), function(x) get_model_results(x), mc.cores = 10)) ## ~1 minute elapsed time
  
  ## bind model_result_list
  model_results <- rbindlist(model_result_list)
  
  ## order models by oos_rmse, select top 10
  top_model_results <- model_results[rank(oos_rmse, ties.method = 'min') <= 10]
  
  ## reorder by OOS RMSE
  top_model_results <- top_model_results[order(oos_rmse)]
  
  ## write outputs
  fwrite(top_model_results, paste0(data_folder, "02c_rmse_ghes_", i, "_top10.csv"))
  fwrite(top_model_results, paste0(data_folder, "FILEPATH/02c_rmse_ghes_", i, "_top10_", date, ".csv"))

}

