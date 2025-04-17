#### #----#                    Docstring                    #----# ####
#' Title:    01_currency_convert_raw_datasets
#' Project:  Resource Tracking - Brain Health Initiative (BHI)
#' Purpose : Updates appropriate datasets using currency conversion function
#'           - Adjusts DEX and PHC estimates to level space
#'           - Currency converts DEX and PHC estimates
#'           
#' Author: USERNAME
#' Date: 2023-02-16
#---------------------------------------------------------------------#

#----# Set up directory roots #----#
rm(list=ls())
if (!exists("code_repo"))  {
  code_repo <- unname(ifelse(Sys.info()['sysname'] == "Windows", "ADDRESS", paste0("ADDRESS", Sys.info()['user'][1], "ADDRESS")))
}

#----# Source functions #----#
source(paste0(code_repo, 'FILEPATH'))
source(paste0(code_repo, 'FILEPATH'))
source(paste0(dah.roots$k, "FILEPATH"))
source(paste0(dah.roots$k, "FILEPATH"))
source(paste0(h, "FILEPATH"))
library(arrow)

# INPUTS
date <- format(Sys.Date(), '%Y%m%d')  # date for archived files
currency_convert_yr <- 2021           # year to convert all estimates
dex_yr <- 2019                        # year of DEX data
nha_yr <- 2017                        # year of NHA data
gbd_release_id <- 16                  # release id for GBD estimates
converter_version <- 7                # version of currency converter

# paths
bhi_share <- paste0('FILEPATH')
raw_data_path <- paste0(bhi_share, 'FILEPATH')
int_data_path <- paste0(bhi_share, 'FILEPATH')
fin_data_path <- paste0(bhi_share, 'FILEPATH')

## #############################################################################
## Gathering and processing estimates - DEX health expenditure 2010-2019
## #############################################################################

## Assign dex file path to estimates directory
scaled_version <- "119"
national_data_path <- paste0("FILEPATH", scaled_version, "FILEPATH")

## Loop through dex data
for (value_year in 2000:dex_yr) {
  
  print(paste0('Pulling data for ', value_year))
  
  ## Pull in year specific data
  temp <- open_dataset(national_data_path) %>%
    filter(is.finite(mean_spend), year_id == value_year, payer != 'oth') %>%
    group_by(year_id, toc, acause, age_group_years_start, sex_id) %>%
    summarize(spend = sum(mean_spend, na.rm = TRUE)) %>%
    collect()
  
  ## Add columns for currency conversion
  temp <- setDT(temp)
  temp[, `:=` (ihme_loc_id = 'USA', location_id = 102)]
  
  ## Update 0-1 to part of 0-5 age group
  temp[age_group_years_start == 1, age_group_years_start := 0]
  temp <- temp[, .(spend = sum(spend, na.rm = T)),
               by = .(ihme_loc_id, location_id, year_id, toc, acause, age_group_years_start, sex_id)]
  
  ## Currency conversion
  temp <- currency_conversion(temp, 
                              inflate.usd = F,
                              col.loc = 'ihme_loc_id',  
                              col.value = 'spend', 
                              currency = 'usd',
                              currency.year = dex_yr,
                              base.year = currency_convert_yr, 
                              base.unit = 'usd',
                              converter.version = converter_version,
                              simplify = T)
  
  ## Adjust columns and column names
  temp <- temp[, .(year = year_id, location_id, acause, toc, age_group_years_start, sex_id, spend)]
  
  ## Add in currency columns
  temp[, `:=` (currency = 'usd', currency_year = currency_convert_yr, currency_converter_version = converter_version)]
  
  ## Write out currency converted estimates
  fwrite(temp, paste0(int_data_path, 'FILEPATH', value_year, '.csv'))
  rm(temp)
  
}

## #############################################################################
## Gathering and processing estimates - Primary health care expenditure (NHA/SHA format)
## #############################################################################

## Read in data
nha_raw <- fread('FILEPATH')

## Rename columns
setnames(nha_raw,
         old = c('iso3', 'year'),
         new = c('ihme_loc_id', 'year_id'))

## Subset to years of interest
nha_raw <- nha_raw[year_id %in% 2000:2019]

## Convert from per capita to level space 
# Get population
locs <- get_location_metadata(location_set_id = 35, release_id = gbd_release_id)[level == 3]
population <- get_population(location_id = locs$location_id,
                             year_id = 2000:2019,
                             sex_id = 3,
                             age_group_id = 22,
                             release_id = gbd_release_id)
population <- merge(population, locs[, .(location_id, ihme_loc_id)], by = 'location_id', all.x = T)
nha_raw <- nha_raw[, .(ihme_loc_id, year_id, hc, hp, sha_hc_name, sha_hp_name, raked_pred_value_pc)]
nha_raw <- merge(nha_raw, population, by = c('ihme_loc_id', 'year_id'), all.x = T)

# Convert to level space
nha_raw[, value := raked_pred_value_pc * population]
nha_raw[, `:=` (raked_pred_value_pc = NULL, population = NULL)]

## Currency conversion
nha_raw <- currency_conversion(nha_raw, 
                               inflate.usd = F,
                               col.loc = 'ihme_loc_id',  
                               col.value = 'value', 
                               currency = 'usd',
                               currency.year = nha_yr,
                               base.year = currency_convert_yr, 
                               base.unit = 'usd',
                               converter.version = converter_version,
                               simplify = T)

## Add in currency columns
nha_raw[, `:=` (currency = 'usd', currency_year = currency_convert_yr, currency_converter_version = converter_version)]

## Write out currency converted estimates
fwrite(nha_raw, paste0(int_data_path, 'FILEPATH'))


## End of Script##