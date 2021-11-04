########################################################################################
## Create OOP estimates
## Formula (vaccine price + delivery cost) * (volume of doses) * (OOP scalar)
## Author: Emilie Maddison
### Date: 20 May 2020
########################################################################################
## -------------------------------------------------------------------------------------
## 1. Set up environment
## -------------------------------------------------------------------------------------
## Clear environment
rm(list = ls())

## Set filepaths to directories
if (Sys.info()[1] == "Linux") {  
        j <- FILEPATH
        h <- FILEPATH
        k <- FILEPATH
} else if (Sys.info()[1] == "Windows") {  
        j <- FILEPATH
        h <- FILEPATH
        k <- FILEPATH
}  

## Set up directories
code.dir <- paste0(FILEPATH)
out.dir <- paste0(FILEPATH)

## Load libraries
library(data.table)
library(ggplot2)
library(readxl)
library(classInt, lib.loc = FILEPATH)

source(paste0(FILEPATH, "helper_functions.R"))
source(paste0(FILEPATH, "get_population.R"))
source(paste0(FILEPATH, 'get_covariate_estimates.R'))
source(paste0(FILEPATH, 'get_outputs.R'))
## source mapping function
source(paste0(FILEPATH, "GBD_WITH_INSETS_MAPPING_FUNCTION.R"))

## Today's date
#date1 <- '20200629'
date1 <- format(Sys.time(), "%Y%m%d")
tag = 'best'

## Get list of 135 locations of interest (LMICs)
locs <- get_ig(locs)
locs_lm <- locs[income_group != 'H',]

## -------------------------------------------------------------------------------------
## 2. Read in datasets
## -------------------------------------------------------------------------------------
## Vaccine Volume (modeled in ST-GPR from DOVE dataset)
volume <- fread(paste0(FILEPATH, "draws_135_total_volume_vaccines_20200723.csv"))
head(volume)
setnames(volume, "volume1", "volume")
setnames(volume, "Vaccine", "vaccine")
volume$draw <- as.integer(substring(volume$draw, 6))
## HPV ,IPV, JE, Measles, MR, MenA, PCV, Penta, RVV, YF
stop(nrow(volume) == (195 * 4 * 1000) + (195 * 3 * 1000) + (195 * 13 * 1000) + 
       (195 * 18 * 1000) + (195 * 18 * 1000) + (195 * 8 * 1000) +  (195 * 9 * 1000) + 
       (195 * 18 * 1000) + (195 * 12 * 1000) +  (195 * 18 * 1000))
stop(max(volume$draw) == 999)
## -------------------------------------------------------------------------------------
## Vaccine Price (linear model from the MI4A dataset)
price <- fread(paste0(FILEPATH, "filled_price_10_vaccines_20200722.csv"))
stop(max(price$draw) == 999)
price[vaccine == "Rota", vaccine := "RVV"]
stop(nrow(price) == 195 * 18 * 10 * 1000)

## -------------------------------------------------------------------------------------
## Delivery Cost (linear model from IDCC dataset)
delivery <- fread(paste0(FILEPATH, "modeled_delivery_costs_draws_20200722.csv"))
head(delivery)
delivery <- delivery[, .(super_region_name, year_id, vaccine, draw, delivery_cost)]
delivery_meas <- delivery[vaccine == "MR", .(super_region_name, year_id, vaccine, draw, delivery_cost)]
delivery_meas[, vaccine := "Measles"]
delivery <- rbind(delivery, delivery_meas)
stop(max(delivery$draw) == 999)
stop(nrow(delivery) == 6 * 18 * 10 * 1000)
rm(delivery_meas)

## -------------------------------------------------------------------------------------
## OOP Scalar (modeled in ST-GPR from literature dataset)
scalar <- fread(paste0(FILEPATH, "oop_scalar_stgpr_draws_20200628.csv"))
scalar <- merge(scalar, locs[, .(location_id, ihme_loc_id)],
                by = 'location_id', all.x = T)

stop(nrow(scalar) == 195 * 18 * 1000)
scalar$draw <- as.integer(substring(scalar$draw, 6))
scalar[value < 0, value := 0]

## -------------------------------------------------------------------------------------
## 3. Adjustments
## -------------------------------------------------------------------------------------
## Vaccine price - adjustments for Middle East (From LNCT Report)
naame_dt <- data.table(read_excel(paste0(FILEPATH, 
      "middle_east_gov_private_immunization_adjustment_20200520.xlsx")))
naame_dt <- naame_dt[, .(ihme_loc_id, private_vac_allowed, gov_provides_private, 
                         private_NIP_only, urbanicity_type)]

price <- merge(price, naame_dt, by = 'ihme_loc_id', all.x = T)
mean(price$price)
price[private_vac_allowed == 0, price := 0]
price[gov_provides_private == 1, price := 0]
mean(price$price)
price <- price[, .(ihme_loc_id, year_id, super_region_name, vaccine, draw, price)]

## Vaccine volume - adjustments for China (from literature)
chn_dt <- data.table(read_excel(paste0(FILEPATH, 
                                 "china_gov_private_imm_adj_20200521.xlsx")))
setnames(chn_dt, "Vaccine", "vaccine")
volume <- merge(volume, chn_dt, by = c('vaccine', 'ihme_loc_id'), all.x = T)
mean(volume$volume)
volume[private_vac_allowed == 0, volume := 0]
mean(volume$volume)
volume <- volume[, .(ihme_loc_id, location_id, year_id, vaccine, draw, volume)]

## OOP scalar for Georgia
scalar[ihme_loc_id == 'GEO', value := 0.26]

## -------------------------------------------------------------------------------------
## 3. Calculate OOP Spending
## -------------------------------------------------------------------------------------
## Merge Price and volume
volume[, draw := as.integer(draw)]
price_volume <- merge(price, volume, by = c('ihme_loc_id', 'year_id', 'vaccine', 'draw'),
                      all.x = T)
## Fix super-region name for Argentina (classified by GBD as "High-income")
price_volume <- price_volume[ihme_loc_id == 'ARG', 
                             super_region_name := 'Latin America and Caribbean']
## Merge on delivery cost
price_volume_delivery <- merge(price_volume, delivery, 
                               by = c('super_region_name', 'vaccine', 'year_id', 'draw'))
price_volume_delivery <- price_volume_delivery[ihme_loc_id %in% locs_lm$ihme_loc_id, ]
stop(nrow(price_volume_delivery) == 135 * 10 * 18 * 1000)
## Too many countries in input sheet

## Calculate total spending on vaccines
price_volume_delivery[, spending := (price + delivery_cost) * volume]
price_volume_delivery[, del_spending := (delivery_cost) * volume]
price_volume_delivery[, vac_spending := (price) * volume]

## Merge on OOP scalar
price_volume_scalar <- merge(price_volume_delivery, scalar, 
                             by = c('ihme_loc_id', 'location_id', 'year_id', 'draw'), 
                             all.x = T)

## Scale down total price to Out-of-pocket spending, 
## for total, delivery, and vaccine spending
price_volume_scalar[, oop_spending := spending * value]
price_volume_scalar[, oop_del_spending := del_spending * value]
price_volume_scalar[, oop_vac_spending := vac_spending * value]
price_volume_scalar2 <- price_volume_scalar[!is.na(oop_spending), 
                                            .(oop_spending = sum(oop_spending),
                                              oop_del_spending = sum(oop_del_spending),
                                              oop_vac_spending = sum(oop_vac_spending)),
                                            .(ihme_loc_id, location_id, year_id, draw)]

## Read in list of Gavi countries
gavi <- fread("FILEPATH/imfin_country_list_updated.csv")
gavi <- unique(gavi[, .(ihme_loc_id, gavi_eligible)])

## Merge list of Gavi countries onto results dataframe
price_volume_scalar2 <- merge(price_volume_scalar2, gavi, by = 'ihme_loc_id', all.x = T)
# price_volume_scalar2[is.na(gavi_country), gavi_country := 0]

## -------------------------------------------------------------------------------------
## Calculate population of surviving infants

## Pull infant mortality rate per infant
infant_mortality <- get_outputs("cause",
                                gbd_round_id = 6,
                                decomp_step = 'step4',
                                version = 'latest',
                                location_id = locs_lm$location_id,
                                age_group_id = 28,
                                measure_id = 1,
                                year_id = 2000:2017,
                                metric_id = 3,
                                sex_id = 3)

## Pull surviving infant population (surviving infant = births * (1 - infant mortality rate))
live_births_thousands <- get_covariate_estimates(gbd_round_id = 6,
                                                 covariate_id = 60,
                                                 age_group_id = 22,
                                                 sex_id = 3,
                                                 year_id = 2000:2017,
                                                 location_id = locs_lm$location_id,
                                                 decomp_step = "iterative")

live_births <- live_births_thousands[, live_births := mean_value * 1000]
custom_covariates <- merge(live_births, 
                           infant_mortality[, .(location_id, year_id, cv_infant_mortality = val)], 
                           by = c("location_id", "year_id"))
custom_covariates[, cv_surviving_infant_pop := live_births * (1 - cv_infant_mortality)]                         
custom_covariates <- custom_covariates[, .(location_id, year_id, cv_surviving_infant_pop)]

## -------------------------------------------------------------------------------------
## Calculate results per surviving infant
price_volume_scalar3 <- price_volume_scalar2[ihme_loc_id %in% locs_lm$ihme_loc_id, ]

price_volume_scalar3 <- merge(price_volume_scalar3, 
                              custom_covariates, 
                              by = c('location_id', 'year_id'), all.x = T)
price_volume_scalar3[, `:=`(oop_spending_per_infant = oop_spending / cv_surviving_infant_pop,
                            oop_del_spending_per_infant = oop_del_spending / cv_surviving_infant_pop,
                            oop_vac_spending_per_infant = oop_vac_spending / cv_surviving_infant_pop)]
price_volume_scalar4 <- price_volume_scalar3[, .(oop = mean(oop_spending),
                                                 oop_lower = quantile(oop_spending, 0.025),
                                                 oop_upper = quantile(oop_spending, 0.975),
                                                 
                                                 oop_del = mean(oop_del_spending),
                                                 oop_del_lower = quantile(oop_del_spending, 0.025),
                                                 oop_del_upper = quantile(oop_del_spending, 0.975),
                                                 
                                                 oop_vac = mean(oop_vac_spending),
                                                 oop_vac_lower = quantile(oop_vac_spending, 0.025),
                                                 oop_vac_upper = quantile(oop_vac_spending, 0.975),
  
                                                 oop_spi = mean(oop_spending_per_infant),
                                                 oop_spi_lower = quantile(oop_spending_per_infant, 0.025),
                                                 oop_spi_upper = quantile(oop_spending_per_infant, 0.975),
                                                 
                                                 oop_del_spi = mean(oop_del_spending_per_infant),
                                                 oop_del_spi_lower = quantile(oop_del_spending_per_infant, 0.025),
                                                 oop_del_spi_upper = quantile(oop_del_spending_per_infant, 0.975),
                                                 
                                                 oop_vac_spi = mean(oop_vac_spending_per_infant),
                                                 oop_vac_spi_lower = quantile(oop_vac_spending_per_infant, 0.025),
                                                 oop_vac_spi_upper = quantile(oop_vac_spending_per_infant, 0.975)),
                                             .(ihme_loc_id, year_id)]

## Output results
length(unique(price_volume_scalar3$location_id))
length(unique(price_volume_scalar3$ihme_loc_id))
length(unique(price_volume_scalar3$year_id))
length(unique(price_volume_scalar3$draw))
nrow(price_volume_scalar3) == 135*18*1000

length(unique(price_volume_scalar4$ihme_loc_id))
length(unique(price_volume_scalar4$year_id))
nrow(price_volume_scalar4) == 135*18

fwrite(price_volume_scalar4[ihme_loc_id %in% locs_lm$ihme_loc_id,],
       paste0("FILEPATH/data_OOP_by_country_",
              date1, "_", tag, ".csv"))

fwrite(price_volume_scalar3[ihme_loc_id %in% locs_lm$ihme_loc_id, .(location_id, ihme_loc_id, year_id, draw, oop_spending, oop_del_spending, oop_vac_spending)], 
       paste0("FILEPATH/draws_data_component_OOP_by_country_", 
              date1, "_", tag, ".csv"))

## -------------------------------------------------------------------------------------
## Calculate Global and Gavi results
price_volume_scalar_gbl <- price_volume_scalar3[, .(oop_spending = sum(oop_spending),
                                                   oop_del_spending = sum(oop_del_spending),
                                                   oop_vac_spending = sum(oop_vac_spending),
                                                   cv_surviving_infant_pop = sum(cv_surviving_infant_pop)),
                                                  .(year_id, draw)]

price_volume_scalar_gbl[, `:=`(oop_spending_per_infant = oop_spending / cv_surviving_infant_pop,
                               oop_del_spending_per_infant = oop_del_spending / cv_surviving_infant_pop,
                               oop_vac_spending_per_infant = oop_vac_spending / cv_surviving_infant_pop)]

price_volume_scalar_gbl <- price_volume_scalar_gbl[, .(oop = mean(oop_spending),
                                                       oop_lower = quantile(oop_spending, 0.025),
                                                       oop_upper = quantile(oop_spending, 0.975),
                                                       
                                                       oop_del = mean(oop_del_spending),
                                                       oop_del_lower = quantile(oop_del_spending, 0.025),
                                                       oop_del_upper = quantile(oop_del_spending, 0.975),
                                                       
                                                       oop_vac = mean(oop_vac_spending),
                                                       oop_vac_lower = quantile(oop_vac_spending, 0.025),
                                                       oop_vac_upper = quantile(oop_vac_spending, 0.975),
                                                       
                                                       oop_spi = mean(oop_spending_per_infant),
                                                       oop_spi_lower = quantile(oop_spending_per_infant, 0.025),
                                                       oop_spi_upper = quantile(oop_spending_per_infant, 0.975),
                                                        
                                                       oop_del_spi = mean(oop_del_spending_per_infant),
                                                       oop_del_spi_lower = quantile(oop_del_spending_per_infant, 0.025),
                                                       oop_del_spi_upper = quantile(oop_del_spending_per_infant, 0.975),
                                                        
                                                       oop_vac_spi = mean(oop_vac_spending_per_infant),
                                                       oop_vac_spi_lower = quantile(oop_vac_spending_per_infant, 0.025),
                                                       oop_vac_spi_upper = quantile(oop_vac_spending_per_infant, 0.975)),
                        .(year_id)]

## Check results
length(unique(price_volume_scalar_gbl$location_id))
length(unique(price_volume_scalar_gbl$ihme_loc_id))
length(unique(price_volume_scalar_gbl$year_id))
nrow(price_volume_scalar_gbl) == 1*18

## Output results
fwrite(price_volume_scalar_gbl, 
       paste0("FILEPATH/gbl_OOP_", 
              date1, "_", tag, ".csv"))

price_volume_scalar_gavi <- price_volume_scalar3[, .(oop_spending = sum(oop_spending),
                                                    oop_del_spending = sum(oop_del_spending),
                                                    oop_vac_spending = sum(oop_vac_spending),
                                                    cv_surviving_infant_pop = sum(cv_surviving_infant_pop)),
                                                .(year_id, gavi_eligible, draw)]

price_volume_scalar_gavi[, `:=`(oop_spending_per_infant = oop_spending / cv_surviving_infant_pop,
                               oop_del_spending_per_infant = oop_del_spending / cv_surviving_infant_pop,
                               oop_vac_spending_per_infant = oop_vac_spending / cv_surviving_infant_pop)]

price_volume_scalar_gavi <- price_volume_scalar_gavi[, .(oop = mean(oop_spending),
                                                         oop_lower = quantile(oop_spending, 0.025),
                                                         oop_upper = quantile(oop_spending, 0.975),
                                                         
                                                         oop_del = mean(oop_del_spending),
                                                         oop_del_lower = quantile(oop_del_spending, 0.025),
                                                         oop_del_upper = quantile(oop_del_spending, 0.975),
                                                         
                                                         oop_vac = mean(oop_vac_spending),
                                                         oop_vac_lower = quantile(oop_vac_spending, 0.025),
                                                         oop_vac_upper = quantile(oop_vac_spending, 0.975),
                                                         
                                                         oop_spi = mean(oop_spending_per_infant),
                                                       oop_spi_lower = quantile(oop_spending_per_infant, 0.025),
                                                       oop_spi_upper = quantile(oop_spending_per_infant, 0.975),
                                                       
                                                       oop_del_spi = mean(oop_del_spending_per_infant),
                                                       oop_del_spi_lower = quantile(oop_del_spending_per_infant, 0.025),
                                                       oop_del_spi_upper = quantile(oop_del_spending_per_infant, 0.975),
                                                       
                                                       oop_vac_spi = mean(oop_vac_spending_per_infant),
                                                       oop_vac_spi_lower = quantile(oop_vac_spending_per_infant, 0.025),
                                                       oop_vac_spi_upper = quantile(oop_vac_spending_per_infant, 0.975)),
                                                   .(gavi_eligible, year_id)]

price_volume_scalar_gbl[year_id == 2017, .(round(oop_spi, 2),
                                           round(oop_del_spi, 2),
                                           round(oop_vac_spi, 2))]
price_volume_scalar_gavi[year_id == 2017, .(gavi_eligible,
                                            round(oop_spi, 2),
                                            round(oop_del_spi, 2),
                                            round(oop_vac_spi, 2))]

## Check results
length(unique(price_volume_scalar_gavi$gavi_eligible))
length(unique(price_volume_scalar_gavi$year_id))
nrow(price_volume_scalar_gavi) ==  2*18

## Output results
fwrite(price_volume_scalar_gavi, 
       paste0("FILEPATH/gavi_non-gavi_OOP_", 
              date1, "_", tag, ".csv"))

source("FILEPATH/immunization_financing/OOP/04b_mapping_spending_results.R")
source("FILEPATH/immunization_financing/OOP/05_plot_spending_results.R")
