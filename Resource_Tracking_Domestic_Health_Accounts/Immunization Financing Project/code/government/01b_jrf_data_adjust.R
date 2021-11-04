## #############################################################################
## Author:
## Project: Immunization financing
## Purpose: Use cMYP/FSP data to produce glm to scale up JRF vaccine data points
##          to account for not including supplementary vaccine spending
##          
## Inputs: cMYP/FSP data and JRF vaccine data from 01a_ghes_data.csv
##
## Outputs: 01b_ghes_data.csv. This data is ready for covariate analysis and
##          subsequent model selection processing
##
## Last update: 11/03/2021
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

## parameters
source(paste0(h, "FILEPATH/helper_functions.R"))
imfin_country_list <- fread("FILEPATH/imfin_country_list.csv")
years <- 2000:2017

## read in cmyp data
cmyp <- fread('FILEPATH/extractions_06252020.csv')[flag != 1] # remove erroneous data

## keep country-years with supplementary spending data
cmyp <- cmyp[!(value_code == 'ghes_supplementary' & is.na(`sup vaccine spending`))]

## get iso3 codes
cmyp[, ihme_loc_id := str_sub(location_name_id, -3)]

## subset to country-years with both vaccine and supplementary vaccine spending
vac_cmyp <- cmyp[value_code == 'ghes_vaccine',
                 .(ihme_loc_id, year_id = spending_year, vaccine = value)]
sup_cmyp <- cmyp[value_code == 'ghes_supplementary',
                 .(ihme_loc_id, year_id = spending_year, supplementary = value)]
sup_vac_cmyp <- cmyp[!(is.na(`sup vaccine spending`)),
                     .(ihme_loc_id, year_id = spending_year, supplementary_vaccine = `sup vaccine spending`)]

## merge data together to have wide
data <- merge(vac_cmyp, sup_cmyp, by = c('ihme_loc_id', 'year_id'), all.x = T)
data <- merge(data, sup_vac_cmyp, by = c('ihme_loc_id', 'year_id'), all.x = T)

## calculate proportions of vaccine spending that are attributable to supplementary campaigns
data[vaccine != 0, sup_vac_vac_prop := supplementary_vaccine / vaccine]

## merge in GBD super regions
data <- get_region(data)

## remove NA values if a component was missing
data <- data[!(is.na(sup_vac_vac_prop))]

###################################################

## keep relevant columns and rename for outcome variable
data <- data[, .(ihme_loc_id, year_id, sup_vac_vac_prop, super_region_name)]
setnames(data, 'sup_vac_vac_prop', 'y')

## read in total health spending and GDP estimates by relevant country-years for glm
he <- get_he_data('the_totes')
he <- get_gdp(he, draw_var = 'draw')
he <- he[year_id %in% years,
         .(gdp_totes = mean(gdp_totes)),
         by = .(ihme_loc_id, year_id)]

## merge onto data
data <- merge(data, he, by = c('ihme_loc_id', 'year_id'), all.x = T)

## merge in populations to calculate GDP per capita in log space
data <- get_pops(data)
data[, log_gdp_pc := log(gdp_totes / pop_totes)]

## vetted formula for glm
formula <- 'y ~ log_gdp_pc + super_region_name'

## run the regression
mod <- lm(formula, data)

## pull intercept, GDP coefficient, and coefficients by GBD super region
intercept <- summary(mod)$coefficient[1]
gdp_coefficient <- summary(mod)$coefficient[2]
name_coefficient <- summary(mod)$coefficient[3]
sa_coefficient <- summary(mod)$coefficient[4]
saeao_coefficient <- summary(mod)$coefficient[5]
ssa_coefficient <- summary(mod)$coefficient[6]

## read in jrf vaccine data from 01a
all_data <- fread('FILEPATH/01a_ghes_data.csv')
jrf_data <- all_data[component == 'vaccine' & source == 'JRF']

## merge on to data set for appropriate calculations and scaling up
jrf_data <- merge(jrf_data, he, by = c('ihme_loc_id', 'year_id'), all.x = T)

## calculate scaled up datapoints using intercept and GDP per capita
jrf_data[, log_gdp_totes_pc := log(gdp_totes / pop_totes)]
jrf_data[, sup_vac_prop := intercept + log_gdp_totes_pc*gdp_coefficient]

## merge in GBD super regions
jrf_data <- get_region(jrf_data)

## finish calculation of scaled up data points using super region specific coefficients
jrf_data[super_region_name == 'North Africa and Middle East',
     sup_vac_prop := sup_vac_prop + name_coefficient]
jrf_data[super_region_name == 'South Asia',
     sup_vac_prop := sup_vac_prop + sa_coefficient]
jrf_data[super_region_name == 'Southeast Asia, East Asia, and Oceania',
     sup_vac_prop := sup_vac_prop + saeao_coefficient]
jrf_data[super_region_name == 'Sub-Saharan Africa',
     sup_vac_prop := sup_vac_prop + ssa_coefficient]

## recalculate expenditure values using supplementary vaccine proportions
jrf_data[, expenditure := expenditure / (1 - sup_vac_prop)]

## remove unnecessary columns
jrf_data[, c('gdp_totes', 'log_gdp_totes_pc', 'super_region_name', 'sup_vac_prop') := NULL]

## bind JRF vaccine data back to 01a
all_data <- rbind(all_data[!(source == "JRF" & component == 'vaccine')], jrf_data)

## write out data file
fwrite(all_data, 'FILEPATH/01b_ghes_data.csv')

