#######################################################
# USERNAME
#
# Cross validate the best AIC/BIC models for Malaria government
#######################################################

##===================##
## Prepare workspace ##
##===================##

rm(list = ls())

pacman::p_load(foreach, data.table, dplyr, lme4, parallel, doBy)


#############################################
# define cross validation function 
#############################################
custom_cv <- function(data,
                      mod
){
  
  formula    <- as.formula(mod)
  
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

data_for_value_codes <- fread('FILEPATH')

## output path
path <- 'ADDRESS'

for (i in 1:length(unique(data_for_value_codes$value_code))) {
  
  code <- unique(data_for_value_codes$value_code)[i]
  
  ## load best AIC/BIC models
  load(paste0(path, code, "FILEPATH"))
  load(paste0(path, code, "FILEPATH"))
  
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
  model_results <- model_results[rank(oos_rmse, ties.method = 'min') <= 10]
  
  ## write outputs
  fwrite(model_results, paste0(path, code, "FILEPATH"))
  
  max_loop <- length(unique(data_for_value_codes$value_code))
  
  
}

## End of Script ##