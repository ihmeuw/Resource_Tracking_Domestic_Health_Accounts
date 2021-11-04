## #############################################################################
## Author:
## Project: Immunization financing
## Purpose: To scale up Gavi co-financing data points by leveraging data from
##          other vaccine spending sources. Gavi co-financing data is not inclusive
##          of all vaccine spending
##
## Inputs: 01b_ghes_data.csv, 02a_prep_covariates.csv, 02c_rmse_ghes_vaccine_top10.csv
##         
##
## Outputs: 03_ghes_data.csv. Data set ready for outliering processing
##          
## Most Recent Update: 11/03/2021
## #############################################################################
pacman::p_load(tidyverse, data.table, dplyr, feather, reshape2, ggplot2)

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

## date
date <- format(Sys.Date(),'%m%d%y')

## data folder to read from/write to
data_folder <- "FILEPATH"

## read in data
data <- fread(paste0(data_folder, 'FILEPATH/01b_ghes_data.csv'))

## read in prepped covariates file
covariates <- fread(paste0(data_folder, 'FILEPATH/02a_prep_covariates.csv'))

## subset to ghes vaccine data for Gavi co-financing
vaccine_data <- data[component == 'vaccine' & source %in% c('JRF', 'Gavi co- and self-financing')]

## merge on covariates
vaccine_data <- merge(vaccine_data, covariates, by = c('location_id', 'ihme_loc_id', 'year_id', 'location_name'))

## flag gavi co-financing values
## create the indicator variable
vaccine_data[, source_dummy := ifelse(grepl("Gavi", source), 1, 0)]

## transform to logit (and offset where applicable)
offset_value <- median(data$proportion)*0.05
vaccine_data[proportion == 0, proportion := offset_value]
vaccine_data[, y := logit(proportion)]

## read in model from model selectionand add indicator variable
model <- fread(paste0(data_folder, 'FILEPATH/02c_rmse_ghes_vaccine_top10.csv'))$model[1]
formula <- gsub('y  ~  ', 'y ~ source_dummy + ', model)

## run the regression
mod <- lmer(formula, vaccine_data)

## get the coefficient
dummy_coef <- summary(mod)$coefficients[2]

## determine which values to increase (JRF > Gavi)
gavi_data <- vaccine_data[source_dummy == 1, .(ihme_loc_id, year_id, gavi_proportion = proportion)]
data_compare <- merge(vaccine_data[source == 'JRF'], gavi_data, by = c('ihme_loc_id', 'year_id'))
data_compare[, adjust_indicator := ifelse(gavi_proportion < proportion, 1, 0)]
vaccine_data <- merge(vaccine_data, data_compare[, .(ihme_loc_id, year_id, source, adjust_indicator)], by = c('ihme_loc_id', 'year_id', 'source'), all.x = T)

## remove/add coefficient from gavi data source & calculate new proportions
vaccine_data[adjust_indicator == 1, y := y - dummy_coef]
vaccine_data[adjust_indicator == 1,
             `:=` (proportion = inv.logit(y),
                   expenditure = ghes_totes * (inv.logit(y)),
                   `expenditure per capita` = (ghes_totes * (inv.logit(y))) / pop_totes)]

## rebind data to save out
new_data <- rbind(data[!(source == 'Gavi co- and self-financing' | (component == 'vaccine' & source == 'JRF'))],
                  vaccine_data[, .(location_id, ihme_loc_id, location_name, year_id, pop_totes, ghes_totes, currency,
                                   currency_year, proportion, expenditure, `expenditure per capita`, component, source)])

## write out data
fwrite(new_data, paste0(data_folder, 'FILEPATH/03_ghes_data.csv'))
fwrite(new_data, paste0(data_folder, 'FILEPATH/03_ghes_data_', date, '.csv'))
