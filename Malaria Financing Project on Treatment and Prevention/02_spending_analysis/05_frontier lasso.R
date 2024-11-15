################################################
#' @author USERNAME
#' 
#' @description Running lasso on covariates to predict 
#' malaria outcomes and select models for SFMA
################################################
pacman::p_load(data.table, dplyr, reticulate, gtools, glmnet, ggplot2, parallel, lme4, MASS, AICcmodavg, readxl, BMS)
rm(list = ls())

cds_repo <- "ADDRESS"
source('FILEPATH')
source('FILEPATH')
source("FILEPATH")
source("FILEPATH")
source("FILEPATH")
source("FILEPATH")
source('FILEPATH')
source(paste0(cds_repo, "FILEPATH"))
source(paste0(cds_repo, "FILEPATH"))

malaria_country_list <- fread('FILEPATH')

##-------------------------------------------------------------------------

## Read in spending, covariates, and outcome estimates
spending <- fread('FILEPATH')
incidence <- fread('FILEPATH')
case_fatality <- fread('FILEPATH')

## get column names for merge
cols <- colnames(incidence)[colnames(incidence) %in% colnames(case_fatality)]

## Merge data together
data <- merge(incidence, case_fatality, by = cols, all = T)
data <- merge(data, spending, by = c('location_id', 'year_id'))

## Get column names that are not outcome or outcome variance
cols <- colnames(data)[!(colnames(data) %like% 'incidence' | colnames(data) %like% 'case_fatality')]

## Melt data by outcome variable
data <- setDT(data)
data <- melt.data.table(data,
                      id.vars = cols,
                      measure.vars = c('incidence', 'case_fatality'),
                      variable.factor = F)

#specify the family you are modelling
family <- 'gaussian'

covs_to_include <-  c('haqi',
                      'urbanicity',
                      'sdi',
                      'under_5_prop',
                      'mat_edu',
                      'life_exp',
                      'temp',
                      'log_gdp_pc',
                      'gei',
                      'latitude',
                      'rainfall',
                      'region_inc_mean')

## set weights if applicable
data[, w := 1]

for (outcome_var in c('incidence', 'case_fatality')) {
  
  data.1 <- copy(data)
  
  ## remove other outcome var and remove NA values depending on outcome loop
  data.1 <- data.1[variable == outcome_var]
  data.1 <- data.1[!is.na(value)]
  
  ## loop through spending variables
  for (spending_var in c('mal_the', 'mal_ghes', 'mal_dah')) {
    
    dt <- copy(data.1)
    
    ## remove NA values if DAH
    if (spending_var == 'mal_dah') {
      dt <- dt[!is.infinite(mal_dah)]
      dt <- dt[!is.na(mal_dah)]
    }
    
    ## standardize covariates
    dt <- dt[, c(covs_to_include, 'location_id', 'year_id', spending_var, 'value', 'w'), with = F]
    dt <- melt.data.table(dt,
                              id.vars = c('location_id', 'year_id', 'value', 'w'),
                              variable.factor = F)
    cov.dt <- dt[, .(mean = mean(value.1),
                         sd = sd(value.1)),
                     by = .(variable)]
    dt <- merge(dt, cov.dt, by = c('variable'))
    dt[, value.1 := (value.1 - mean) / sd]
    dt <- dcast.data.table(dt, location_id + year_id + value + w ~ variable, value.var = 'value.1')
    
    ## shuffle the data into five random folds
    dt <- dt[sample(nrow(dt))]
    dt[, fold_id := cut(seq(1, nrow(dt)), breaks = 5, labels = FALSE)]
    
    ## add a row id column
    dt[, a_rowid := seq(1:nrow(dt))]
    
    #define variables to include (as a matrix)
    covariates <- c(spending_var, covs_to_include)
    
    if (outcome_var == 'case_fatality') {
      covariates <- covariates[1:12]
    }
    
    vars <- as.matrix(dt[, covariates, with = F])
    colnames(vars) <- covariates
    
    #fit cross validated lasso to select lambda
    cv_lasso = cv.glmnet(x = vars , y = dt$value, family = family, alpha = 1, weights = dt$w, nfolds = 5, foldid = dt$fold_id)
    
    #print out the model coefficients.
    cv_lasso$lambda.1se
    print(coef(cv_lasso, s = "lambda.1se"))
    

    
  }
}


##END-------------------------------------------------------------------