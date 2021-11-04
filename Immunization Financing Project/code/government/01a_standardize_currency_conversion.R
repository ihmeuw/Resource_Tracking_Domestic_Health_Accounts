## #############################################################################
## Author:
## Project: Immunization financing
## Purpose: Clean, currency convert (to 2019 USD), standardize, and aggregate data from sources
##          for government health spending on immunizations by component
##
## Inputs: Extracted datasets for government health spending on immunizations and
##         parameters (gbd round, years of interest, currency convertor version)
##
## Outputs: 01a_ghes_data.csv (file of standardized data ready for covariate selection/analysis)
##          
## Most Recent Update: 11/03/2021
## #############################################################################
pacman::p_load(data.table, dplyr, feather, reshape2, ggplot2, readstata13, gridExtra, RColorBrewer, openxlsx, grid, scales, readxl)

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

## source location function and helper functions
source(paste0(k, "FILEPATH/get_location_metadata.R"))
source(paste0(h, "FILEPATH/currency_conversion.R"))
source(paste0(h, "FILEPATH/helper_functions.R"))
data_folder <- "FILEPATH"

## parameters
date <- format(Sys.Date(),'%m%d%y')
years <- 2000:2017
imfin_country_list <- fread("FILEPATH/imfin_country_list.csv")
gbd_round_id <- 6
currency_convert_version <- 4.7

## get location set
location <- get_location_metadata(gbd_round_id = gbd_round_id, 
                                  location_set_id = 22) ## 22 is covariate/modeling locations for level 3's

## read in raw/processed data
jrf_vaccine <- data.table(read.xlsx(paste0(j, "FILEPATH/Govt spending on vaccines_indicator 6510.xlsx"), startRow = 3))
jrf_routine <- data.table(read.xlsx(paste0(j, 'FILEPATH/Govt spending on RI_indicator 6540.xlsx'), startRow = 3))

gavi_cofinancing <- fread(paste0(j, "FILEPATH/Gavicofinancing_current.csv"))
gavi_selffinancing <- fread(paste0(j, 'FILEPATH/Gaviselffinancing_current.csv'))

cmyp <- fread(paste0(j, 'FILEPATH/extractions_06082020.csv'))
cmyp <- cmyp[value_code != 'ghes_gavi_cofin' & value_code != 'ghes_injection_supplies' & flag != 1] # remove prelim values and flagged errooneous values

idcc <- fread('FILEPATH/gov_delivery_data_08112020.csv')[, .(ihme_loc_id, year_id, value_new = delivery)]

ghed <- fread('FILEPATH/ghed_ghes_immunization_processed.csv')

nha <- fread('FILEPATH/extractions_07092020.csv')[flag != 1] # remove erroneous values

## NHA standardization processing

# get iso3 codes
nha[, ihme_loc_id := str_sub(location_name_id, -3)]

# rename and keep relevant columns and data
nha <- nha[, .(ihme_loc_id, year_id = year_end, fp_vaccine_govt, dis_immunization_govt, mult_factor, currency = units)]
nha <- nha[!(is.na(dis_immunization_govt) & is.na(fp_vaccine_govt))]

# add source identifier
nha[, source := '2011 SHA']

# multiplication factor (if applicable)
nha[, `:=` (dis_immunization_govt = dis_immunization_govt * mult_factor,
            fp_vaccine_govt = fp_vaccine_govt * mult_factor)]

# separate into immunization data and vaccine data
imm_nha <- nha[, .(ihme_loc_id, year_id, value = dis_immunization_govt, currency, source)]
vaccine_nha <- nha[!(is.na(fp_vaccine_govt)),
                   .(ihme_loc_id, year_id, value = fp_vaccine_govt, currency, source)]

## aggregate gavi co-financing and self-financing data
gavi_data <- merge(gavi_cofinancing[, .(year_id, ihme_loc_id, value)],
                   gavi_selffinancing[, .(year_id, ihme_loc_id, value)],
                   by = c('year_id', 'ihme_loc_id'), all = T)
gavi_data[is.na(gavi_data)] <- 0
gavi_data[, value := value.x + value.y]

## get iso3 codes
cmyp[, ihme_loc_id := str_extract(location_name_id, "[:upper:]{3}")]

## transform JRF data wide to long
jrf_vaccine <- melt.data.table(jrf_vaccine, id.vars = c('ISO.Code', 'Country', 'Region'), na.rm = T, variable.name = 'year_id')
jrf_routine <- melt.data.table(jrf_routine, id.vars = c('ISO.Code', 'Country', 'Region'), na.rm = T, variable.name = 'year_id')

## transform GHED data wide to long
ghed <- ghed[, .(ihme_loc_id, `2015` = `2015 Value`, `2016` = `2016 Value`, `2017` = `2017 Value`, source = 'GHED')]
ghed <- melt.data.table(ghed, id.vars = c('ihme_loc_id', 'source'), variable.name = 'year_id', variable.factor = F, na.rm = T)


## subset raw data to relevant columns, years of interest, and countries of interest
jrf_vaccine <- jrf_vaccine[year_id %in% years & ISO.Code %in% imfin_country_list$ihme_loc_id,
                           .(year_id, ihme_loc_id = ISO.Code, value)]
jrf_routine <- jrf_routine[year_id %in% years & ISO.Code %in% imfin_country_list$ihme_loc_id,
                           .(year_id, ihme_loc_id = ISO.Code, value)]
gavi_data <- gavi_data[year_id %in% years & ihme_loc_id %in% imfin_country_list$ihme_loc_id,
                                     .(year_id, ihme_loc_id, value)]
cmyp <- cmyp[spending_year %in% years & ihme_loc_id %in% imfin_country_list$ihme_loc_id,
             .(year_id = spending_year, ihme_loc_id, value, source = `data source`, component = value_code)]
ghed <- ghed[year_id %in% years & ihme_loc_id %in% imfin_country_list$ihme_loc_id]
imm_nha <- imm_nha[year_id %in% years & ihme_loc_id %in% imfin_country_list$ihme_loc_id]
vaccine_nha <- vaccine_nha[year_id %in% years & ihme_loc_id %in% imfin_country_list$ihme_loc_id]

## add data source and immunization component
jrf_vaccine[, `:=` (component = 'vaccine',
                    source = "JRF",
                    currency = 'USD')]
jrf_routine[, `:=` (component = 'routine',
                    source = 'JRF',
                    currency = 'USD')]
gavi_data[, `:=` (component = 'vaccine',
                  source = "Gavi co- and self-financing",
                  currency = 'USD')]
cmyp[, `:=` (component = gsub('ghes_', '', component),
             currency = 'USD')]
idcc[, `:=` (component = 'delivery',
             source = 'IDCC',
             currency = 'USD')]
ghed[, `:=` (value = value * 1000000,
             currency = 'LCU',
             component = 'immunization')]
imm_nha[, `:=` (component = 'immunization')]
vaccine_nha[, `:=` (component = 'vaccine')]

## combine raw data into one data table
govt_data_current <- rbind(jrf_vaccine, jrf_routine, gavi_data, cmyp, ghed, imm_nha, vaccine_nha)

## transform integer columns to numeric columns for currency conversion
govt_data_current[, `:=` (value = as.numeric(value),
                          year_id = as.numeric(as.character(year_id)))]

## currency conversion to 2019 USD
govt_data_constant <- currency_conversion(govt_data_current,
                                          converter.version = currency_convert_version,
                                          col.loc = "ihme_loc_id",
                                          col.value = "value",
                                          col.currency.year = "year_id",
                                          col.currency = 'currency',
                                          base.unit = "USD",
                                          base.year = 2019,
                                          simplify = F)

## bind with IDCC data (already in 2019 USD)
govt_data_constant <- rbind(govt_data_constant, idcc, fill = T)

## get/merge populations and location metadata
govt_data_constant <- get_pops(govt_data_constant)
govt_data_constant <- merge(govt_data_constant, location[, .(location_id, location_name, ihme_loc_id)], by = c('ihme_loc_id'), all.x = T)

## read in ghes totes and take mean by draws for all country-years of interest
ghes_totes <- get_he_data('ghes_totes')
ghes_totes <- ghes_totes[year %in% years, .(ihme_loc_id, draw, year_id, ghes_totes = data_var)]
ghes_totes <- ghes_totes[, .(ghes_totes = mean(ghes_totes)),
                         by = .(ihme_loc_id, year_id)]  

## merge total government health spending onto data to calculate proportions
govt_data_constant <- merge(govt_data_constant, ghes_totes, by = c("ihme_loc_id", "year_id"), all.x = T)

## calculate proportion of ghes and expenditure per capita
govt_data_constant <- govt_data_constant[, `:=` (proportion = value_new / ghes_totes,
                                                 `expenditure per capita` = value_new / pop_totes)] 

## rename columns and subset to relevant columns
govt_data_constant <- govt_data_constant[, .(location_id, ihme_loc_id, location_name, year_id, pop_totes, ghes_totes, currency = currency_new, currency_year = year_id_new, 
                                             proportion, expenditure = value_new, `expenditure per capita`, component, source)]           

## write out data file
fwrite(govt_data_constant, paste0(data_folder, "FILEPATH/01a_ghes_data_", date, ".csv"))
fwrite(govt_data_constant, paste0(data_folder, "01a_ghes_data.csv"))

