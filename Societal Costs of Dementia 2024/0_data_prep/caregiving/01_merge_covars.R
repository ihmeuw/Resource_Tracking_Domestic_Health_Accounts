##########################################################################
### Author: Michael Breshock
### Date: 08/28/2023
### Project: Global dementia spending
### Purpose: Merging in covariate data with clean caregiving extractions
##########################################################################

# clear environment
rm(list=ls())

# load library
library(dplyr)
library(data.table)


############################## MERGE OVERALL DATA ##############################
# merge covars with caregiving data that has been summarized for overall estimates
# for all severities and care types 

# load data: 
overall = fread("FILEPATH/00_clean_data_per_patient.csv")
pop = fread("FILEPATH/population_1990_2019.csv")
dementia = fread("FILEPATH/all_age_sex_dementia_prevalence_1990_2019_HK.csv")
cov = fread("FILEPATH/all_covars_1990_2019.csv")
diabetes = fread("FILEPATH/age_std_diabetes_prevalence_1990_2019.csv")
edu = fread("FILEPATH/education_years_pc_1990_2019.csv")

# remove unnecessary column from pop data: 
pop[, age_group_id_over65 := NULL]
# merge population into data: 
all_pop = merge(overall, pop, by = c("year_id","location_id")) 

# merge in prevalence data: 
# first change names for clarity: 
setnames(dementia, old = "prevalence", new = "dementia_prevalence")
all_dem = merge(all_pop, dementia[,.(year_id, location_id, age_group_id, sex_id, 
                                       dementia_prevalence)],
                 by = c("year_id", "location_id", "age_group_id", "sex_id"))

# merge in SDI, LDI, THE, & GDP:
all_covs = merge(all_dem, cov, by = c("year_id", "location_id", "location_name"))

# merge in diabetes prevalence: 
all_diab = merge(all_covs, diabetes[,.(year_id, location_id, DM_prevalence)],
                  by = c("year_id", "location_id"))

# merge in education data: 
overall_covs = merge(all_diab, edu[,.(year_id, location_id, edu_years_pc)], 
                 by = c("year_id", "location_id"))

# merge in regional SDI data: 
reg_SDI = fread(file = "FILEPATH/regional_SDI_1990_2019.csv")
overall_covs = merge(overall_covs, reg_SDI[,.(year_id, location_id, region_id, 
                                              region_name, region_SDI)], 
                     by = c("year_id", "location_id"))

# save out clean data with all covariates: 
fwrite(overall_covs, "FILEPATH/01_clean_data_covars_overall.csv")

