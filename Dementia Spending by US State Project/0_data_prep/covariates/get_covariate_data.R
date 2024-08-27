# Extracting covariate data for Global Dementia Models
# Author: Michael Breshock
# Date: 06/29/2023

# clear environment
rm(list=ls())

# load libraries and functions 
library(data.table)
library(dplyr)
library(feather)
source("FILEPATH/get_location_metadata.R")
source("FILEPATH/get_covariate_estimates.R")
source("FILEPATH/get_population.R")

# initialize release_id variable for shared functions
release = 6 # GBD 2019
# get location IDs from metadata
locs <- get_location_metadata(location_set_id=22, release_id = release) 
# location_set_id=22: -> covariate computation
loc_ids <- locs$location_id

######################### Socio-demographic Index (SDI) ########################
# Getting SDI (covariate_id: 881)
sdi <- get_covariate_estimates(covariate_id = 881, age_group_id = 22, location_id = loc_ids, 
                               release_id = release, sex_id = 3, year_id=c(1990:2019))
# age_group_id=22 -> All Ages

# save to .csv file:
fwrite(sdi, file = "FILEPATH/sdi_levels_1_to_3_1990_2019.csv")

################## Lag Distributed Income Per Capita (LDI_pc) ##################
# Getting LDI (covariate_id: 57)
ldi_pc <- get_covariate_estimates(covariate_id = 57, age_group_id = 22, location_id = loc_ids, 
                                  release_id = release, sex_id = 3, year_id=c(1990:2019))
# save to .csv file:
fwrite(ldi_pc, file = "FILEPATH/ldi_pc_levels_1_to_3_1990_2019.csv")

######################## Total Health Expenditure (THE) ########################
## THE Total Estimates: 
# read in draws: 
THE = data.table(read_feather("FILEPATH/the_totes_feather.feather"))
# melt to long format: 
THE_long = melt(THE, id.vars = c("year", "iso3"))
# load currency conversion factors
gdp_deflator <- fread("FILEPATH/gdp_deflator.csv")
# get factor for converting from 2021 USD to 2019 USD
convert_factor = gdp_deflator[year == 2021]$factor2019
# get mean, lower, and upper values from draws and convert to 2019 USD: 
THE = THE_long[,.(the = mean(value)*convert_factor, 
                  the_lower = quantile(value, 0.025)*convert_factor,
                  the_upper = quantile(value, 0.975)*convert_factor), 
               by = c("year", "iso3")]
# change names: 
setnames(THE, old = c("year", "iso3"), new = c("year_id", "ihme_loc_id"))
# merge in location id and location names: 
THE_named = merge(THE, locs[,.(location_id, location_name, ihme_loc_id)],
                    by = "ihme_loc_id")
#save to .csv file:
fwrite(THE_named, file = "FILEPATH/the_totes_1980_2019.csv")

## THE per-capita estimates: 
THEpc = data.table(read_feather("FILEPATH/thepc_feather.feather"))
# melt to long format: 
THEpc_long = melt(THEpc, id.vars = c("year", "iso3"))
# get mean, lower, and upper values from draws and convert to 2019 USD: 
THEpc = THEpc_long[,.(the_pc = mean(value)*convert_factor, 
                      the_pc_lower = quantile(value, 0.025)*convert_factor,
                      the_pc_upper = quantile(value, 0.975)*convert_factor), 
                   by = c("year", "iso3")]
# change names: 
setnames(THEpc, old = c("year", "iso3"), new = c("year_id", "ihme_loc_id"))
# merge in location id and location names: 
THEpc_named = merge(THEpc, locs[,.(location_id, location_name, ihme_loc_id)],
                  by = "ihme_loc_id")
#save to .csv file:
fwrite(THEpc_named, file = "FILEPATH/the_pc_1980_2019.csv")

################## Gross Domestic Product per-capita (GDPpc) ###################
# read in draws: 
GDPpc = data.table(read_feather("FILEPATH/part6_incl_LDI_20220712_new7_log_2021USD_draws.feather"))
# filter to just summary stats and convert to 2019 USD: 
GDPpc = GDPpc[, .(year, iso3, gdp_pc = mean*convert_factor, 
                  gdp_pc_lower = lower*convert_factor, 
                  gdp_pc_upper = upper*convert_factor)]
# change names: 
setnames(GDPpc, old = c("year", "iso3"), new = c("year_id", "ihme_loc_id"))
# change naming scheme for Hong Kong to prevent data loss in merging:
# note: this does not reflect the personal beliefs of Michael Breshock
GDPpc[ihme_loc_id == "HKG", ihme_loc_id := "CHN_354"]
# merge in location id and location names: 
GDPpc_named = merge(GDPpc, locs[,.(location_id, location_name, ihme_loc_id)],
                    by = "ihme_loc_id")
#save to .csv file:
fwrite(GDPpc_named, file = "FILEPATH/gdp_pc_1950_2027.csv")

############################ Merge All Covariates ##############################
# change names for merging: 
setnames(sdi, old = c("mean_value", "lower_value", "upper_value"), 
         new = c("sdi", "sdi_lower", "sdi_upper"))
setnames(ldi_pc, old = c("mean_value", "lower_value", "upper_value"), 
         new = c("ldi_pc", "ldi_pc_lower", "ldi_pc_upper"))

# merge SDI and LDI:
covars = merge(sdi[,.(year_id, location_id, sdi)], 
               ldi_pc[,.(year_id, location_id, ldi_pc)], 
               by = c("year_id", "location_id"))
# merge in THE:
covars = merge(covars, THE_named[,.(year_id, location_id, location_name, 
                                    the, the_lower, the_upper)], 
               by = c("year_id", "location_id"))
# merge in THE per-capita:
covars = merge(covars, THEpc_named[,.(year_id, location_id, location_name, 
                                      the_pc, the_pc_lower, the_pc_upper)], 
               by = c("year_id", "location_id", "location_name"))
# merge in GDP per-capita: 
covars = merge(covars, GDPpc_named[,.(year_id, location_id, location_name, 
                                      gdp_pc, gdp_pc_lower, gdp_pc_upper)], 
               by = c("year_id", "location_id", "location_name"))

# save merged covariates out: 
fwrite(covars, file = "FILEPATH/all_covars_1990_2019.csv")

######################### Get Mean Years of Education ##########################
# covariate_id = 1975 > Education (years per capita) aggregated by age (15+) and sex
edu <- get_covariate_estimates(covariate_id = 1975, age_group_id = 22, location_id = loc_ids, 
                               release_id = release, sex_id = 3, year_id=c(1990:2019))
# grab just the columns we need and change names for clarity: 
edu_out = edu[,.(year_id, location_id, covariate_id, covariate_name_short,
                 age_group_id, sex_id, mean_value, lower_value, upper_value)]
setnames(edu_out, old = c("mean_value", "lower_value", "upper_value"), 
         new = c("edu_years_pc", "edu_lower", "edu_upper"))

fwrite(edu_out, file = "FILEPATH/education_years_pc_1990_2019.csv")

## get just female education years: (sex_id = 2)
# covariate_id = 33 -> Mean level of educational attainment
edu_F <- get_covariate_estimates(covariate_id = 33, location_id = loc_ids, 
                                 release_id = release, sex_id = 2, year_id=c(1990:2019))
# age_group_id = 22 (all) did not work on this call, estimates only available with age group splits
ages = unique(edu_F$age_group_id)
# get female population by age group to aggregate age grouped education estimates with weighted.means: 
pop_F <- get_population(release_id = release, location_id = loc_ids,
                        year_id = c(1990:2019), sex_id = 2,
                        age_group_id = ages)
# remove run_id variable from pop data: 
pop_F[, run_id := NULL]
# merge population into education data: 
edu_pop_F = merge(edu_F, pop_F, by = c("year_id", "location_id", 
                                       "sex_id", "age_group_id"))

# aggregate age_group splits: 
sum_edu_F = edu_pop_F[,.(female_edu_years_pc = weighted.mean(mean_value, w = population), 
                         female_pop = sum(population)), 
                      by = c("year_id", "location_id", "sex_id", "location_name")]

fwrite(sum_edu_F, file = "FILEPATH/female_education_years_pc_1990_2019.csv")

### female/male education rate ratio: 
## get just male education: 
# covariate_id = 33 -> Mean level of educational attainment
edu_M <- get_covariate_estimates(covariate_id = 33, location_id = loc_ids, 
                                 release_id = release, sex_id = 1, year_id=c(1990:2019))
# age_group_id = 22 (all) did not work on this call, estimates only available with age group splits
ages = unique(edu_M$age_group_id)
#  age group IDs: 2,3,4 -> early, late, and post neonatal
# 5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,30,31,32 -> 5 year age groups 1-94
# 235 -> 95 plus
# get male population by age group to aggregate age grouped education estimates with weighted.means: 
pop_M <- get_population(release_id = release, location_id = loc_ids,
                        year_id = c(1990:2019), sex_id = 1,
                        age_group_id = ages)
# remove run_id variable from pop data: 
pop_M[, run_id := NULL]
# merge population into education data: 
edu_pop_M = merge(edu_M, pop_M, by = c("year_id", "location_id", 
                                       "sex_id", "age_group_id"))

# aggregate age_group splits: 
sum_edu_M = edu_pop_M[,.(male_edu_years_pc = weighted.mean(mean_value, w = population), 
                         male_pop = sum(population)), 
                      by = c("year_id", "location_id", "sex_id", "location_name")]

# merge male and female education rates: 
setnames(sum_edu_M, old = "sex_id", new = "male_sex_id")
setnames(sum_edu_F, old = "sex_id", new = "female_sex_id")
sum_edu = merge(sum_edu_M, sum_edu_F, 
                by = c("year_id", "location_id", "location_name"))
# calculate female - male education ratio: 
sum_edu[, fem_edu_ratio := female_edu_years_pc / male_edu_years_pc]
range(sum_edu$fem_edu_ratio) # 0.2828446 - 1.1139630

# save out education ratio
fwrite(sum_edu, file = "FILEPATH/sex_split_education_years_pc_1990_2019.csv")

########################## Get Total Fertility Rate ############################
# covariate_id = 149 -> Total Fertility Rate (TFR)
tfr <- get_covariate_estimates(covariate_id = 149, age_group_id = 22, location_id = loc_ids, 
                               release_id = release, sex_id = 3, year_id=c(1990:2019))
# grab just the columns we need and change names for clarity: 
tfr_out = tfr[,.(year_id, location_id, covariate_id, covariate_name_short,
                 age_group_id, sex_id, mean_value, lower_value, upper_value)]
setnames(tfr_out, old = c("mean_value", "lower_value", "upper_value"), 
         new = c("tfr", "tfr_lower", "tfr_upper"))
# save out tfr covariate data: 
fwrite(tfr_out, file = "FILEPATH/total_fertility_rate_1990_2019.csv")

