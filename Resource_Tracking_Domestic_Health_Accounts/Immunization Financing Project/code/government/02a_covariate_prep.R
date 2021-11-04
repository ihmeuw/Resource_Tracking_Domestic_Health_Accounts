## #############################################################################
## Author: 
## Project: Immunization financing
## Purpose: Pull GBD covariates, create custom covariates and a custom covariate
##          dataset, and merge them into one csv
##
## Inputs: GBD covariate ids of interest and parameters (gbd round, years of interest)
##
## Outputs: 02a_ghes_custom_covariates.csv (square custom covariate file for ST-GPR)
##          02a_prep_covariates.csv (file with all covariates for selection script)
##          
## Most Recent Update: 11/03/2021
## #############################################################################
pacman::p_load(data.table, dplyr, feather, reshape2, ggplot2, readstata13, gridExtra, RColorBrewer, openxlsx, grid, scales, gtools)

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

## source gbd shared functions (location, covariate, population, outputs)
source(paste0(k, 'FILEPATH/get_location_metadata.R'))
source(paste0(k, 'FILEPATH/get_covariate_estimates.R'))
source(paste0(k, 'FILEPATH/get_population.R'))
source(paste0(k, 'FILEPATH/get_outputs.R'))
source(paste0(h, 'FILEPATH/data_tools.R'))
source(paste0(h, 'FILEPATH/helper_functions.R'))
data_folder <- "FILEPATH"

## parameters
date <- format(Sys.Date(), '%m%d%y')
years <- 2000:2017
location_set_id <- 22
gbd_round_id <- 6

## pull location data and subset to level 3s
location <- get_location_metadata(location_set_id = location_set_id, gbd_round_id = gbd_round_id)
loc <- location[level >= 3, .(location_id, ihme_loc_id, location_name, parent_id, level)]
level_3 <- location[level == 3, .(ihme_loc_id, location_id, location_name, super_region_name, region_name)]

## gbd covariates of interest
cov_ids <- c(32, 75, 463, 1099)
## 32 DTP3_coverage_prop - 	Fraction of children born in a given country-year who have received 3 doses of DTP3
## 75 measles_vacc_cov_prop - Percentage of population with measles vaccination
## 463 maternal_educ_yrs_pc - mean level of maternal education attainment
## 1099 haqi - Healthcare access and quality index

## pulling covariates function
covar <- lapply(
  cov_ids,
  function(x)
    get_covariate_estimates(
      gbd_round_id = gbd_round_id,
      covariate_id = x,
      age_group_id = 22,
      sex_id = 3,
      year_id = years,
      location_id = loc$location_id,
      decomp_step = "iterative"
    )
)
covar <- lapply(
  covar,
  function(x)
    subset(x,
           select = c("covariate_name_short", "location_id", "year_id", "mean_value")
    )
)

## bind gbd covariates
covar <- rbindlist(covar)

## transform covariates long to wide
covar <- dcast.data.table(covar, formula = location_id + year_id ~ covariate_name_short, value.var = "mean_value")

##%%%%%% Custom Covariates %%%%%%%%

## pull infant mortality rate per infant
infant_mortality <- get_outputs("cause",
                                gbd_round_id = gbd_round_id,
                                decomp_step = 'step4',
                                version = 'latest',
                                location_id = loc$location_id,
                                age_group_id = 28,
                                measure_id = 1,
                                year_id = years,
                                metric_id = 3,
                                sex_id = 3)

## pull live births in thousands
live_births_thousands <- get_covariate_estimates(gbd_round_id = gbd_round_id,
                                                 covariate_id = 60,
                                                 age_group_id = 22,
                                                 sex_id = 3,
                                                 year_id = years,
                                                 location_id = loc$location_id,
                                                 decomp_step = "iterative")
live_births <- live_births_thousands[, live_births := mean_value * 1000]

## merge and calculate surviving infant population = (surviving infant = births * (1 - infant mortality rate))
custom_covariates <- merge(live_births, infant_mortality[, .(location_id, year_id, cv_infant_mortality = val)], by = c("location_id", "year_id"))
custom_covariates[, cv_surviving_infant_pop_thousands := (live_births * (1 - cv_infant_mortality)) / 1000]                                                    

## merge level onto custom covariates and fill NA subnats with national values for covariate squaring required for ST-GPR
custom_covariates <- merge(custom_covariates, loc, by = c('location_id', 'location_name'))
custom_covariates <- check_na_fill_subnat(custom_covariates)

## read in ghes pc for country-years of interest
ghes_pc <- get_he_data('ghes_pc')
ghes_pc <- ghes_pc[year_id %in% years,
                   .(cv_ghes_pc = mean(data_var)),
                   by = .(ihme_loc_id, year_id)] 

## merge ghes pc onto other custom covariates
custom_covariates <- merge(custom_covariates, ghes_pc, by = c('ihme_loc_id', 'year_id'), all.x = T)
  
## fill in ghes pc NA subnats with national values
custom_covariates[is.na(cv_ghes_pc) & level == 3,
         cv_ghes_pc := 1] ## missing level 3's do not affect estimates in ST-GPR
custom_covariates <- check_na_fill_subnat(custom_covariates)

## gavi recipient status 
## pull in Gavi recipient csv and attach location_ids for merging
gavi_recipient_data <- fread(paste0(j, "FILEPATH/gavi_recipient_by_country_year.csv"))
gavi_recipient_data <- gavi_recipient_data[, .(ihme_loc_id, year_id, cv_gavi_recipient = gavi_recipient, cv_gavi_country = gavi_country)]

## merge gavi recipient onto custom covariates to create binary covariate
custom_covariates <- merge(custom_covariates, gavi_recipient_data, by = c('ihme_loc_id', 'year_id'), all.x = T)
custom_covariates[is.na(cv_gavi_recipient) & level == 3,
                  cv_gavi_recipient := 0]
custom_covariates[is.na(cv_gavi_country) & level == 3,
                  cv_gavi_country := 0]

## fill in gavi recipient NA subnats with national values
custom_covariates <- check_na_fill_subnat(custom_covariates)

## subset to desired columns and write out square file for ST-GPR modelling
custom_covariates <- custom_covariates[, .(location_id, year_id, age_group_id, sex_id, cv_infant_mortality, cv_surviving_infant_pop_thousands, cv_gavi_recipient, cv_gavi_country, cv_ghes_pc)]
fwrite(custom_covariates, paste0(data_folder, '02a_custom_covariates.csv'))
fwrite(custom_covariates, paste0(data_folder, 'FILEPATH/02a_custom_covariates_', date, '.csv'))

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## merge custom covariates onto other covariates
covar <- merge(covar, custom_covariates[, .(location_id, year_id, cv_infant_mortality, cv_surviving_infant_pop_thousands, cv_gavi_recipient, cv_gavi_country, cv_ghes_pc)], by = c('location_id', 'year_id'))
covariate <- merge(covar, level_3, by = c("location_id"))

## calculate haqi and transform relevant covariates to logit space
covariate[, haqi := haqi / 100]
vars <- c('DTP3_coverage_prop', 'haqi', 'cv_infant_mortality', 'measles_vacc_cov_prop')
trans_vars <- paste("logit", vars, sep = "_")

## logit function
covariate[, eval(trans_vars) := lapply(
  covariate[, vars, with = F],
  function(x) logit(x)
)]

## transform appropriate covariates to log space
vars <- c('cv_surviving_infant_pop_thousands', 'cv_ghes_pc')
trans_vars <- paste("log", vars, sep = "_")

## log function
covariate[, eval(trans_vars) := lapply(
  covariate[, vars, with = F],
  function(x) log(x)
)]

## remove pre-transformed covariates
covariate[, c('cv_surviving_infant_pop_thousands', 'haqi', 'DTP3_coverage_prop', 'cv_infant_mortality', 'measles_vacc_cov_prop', 'cv_ghes_pc') := NULL]

## write out data files
fwrite(covariate, paste0(data_folder, '02a_prep_covariates.csv'))
fwrite(covariate, paste0(data_folder, 'FILEPATH/02a_prep_covariates_', date, '.csv'))

