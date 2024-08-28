##########################################################################
### Author: Michael Breshock
### Date: 09/19/2023
### Project: Global dementia spending
### Purpose: Merging in covariate data with clean caregiver gender extractions
##########################################################################

# clear environment
rm(list=ls())

# load library
library(dplyr)
library(data.table)

data = fread("FILEPATH/00_clean_data.csv")
pop = fread("FILEPATH/populations_release16_1990_2024.csv")
dementia = fread("FILEPATH/dementia_prevalence_release16_1990_2024.csv")
cov = fread("FILEPATH/all_covars_1990_2019.csv")
diabetes = fread("FILEPATH/age_std_diabetes_prevalence_1990_2019.csv")
edu = fread("FILEPATH/education_years_pc_1990_2019.csv")
edu_F = fread("FILEPATH/sex_split_education_years_pc_1990_2019.csv")
tfr = fread("FILEPATH/tfr_release16_1990_2024.csv")

# remove unnecessary column from pop data: 
pop[, age_group_id_over65 := NULL]
pop[, female_sex_id := NULL]
# merge population into data: 
data_pop = merge(data, pop, by = c("year_id","location_id")) 

# merge in prevalence data: 
# first change names for clarity: 
setnames(dementia, old = "prevalence", new = "dementia_prevalence")
data_dem = merge(data_pop, dementia[,.(year_id, location_id, age_group_id, sex_id, 
                                     dementia_prevalence)],
                by = c("year_id", "location_id", "age_group_id", "sex_id"))

# merge in SDI, LDI, THE, & GDP:
data_covs = merge(data_dem, cov, 
                  by = c("year_id", "location_id", "location_name"))

# merge in diabetes prevalence: 
data_diab = merge(data_covs, diabetes[,.(year_id, location_id, DM_prevalence)],
                 by = c("year_id", "location_id"))

# merge in overall education data: 
data_edu = merge(data_diab, edu[,.(year_id, location_id, edu_years_pc)], 
                     by = c("year_id", "location_id"))

# merge in female education data: 
data_edu_F = merge(data_edu, edu_F[,.(year_id, location_id, 
                                      female_edu_years_pc, fem_edu_ratio)], 
                   by = c("year_id", "location_id"))

# merge in total fertility rate: 
data_all = merge(data_edu_F, tfr[,.(year_id, location_id, tfr)], 
                 by = c("year_id", "location_id"))

# save out clean data with all covariates: 
fwrite(data_all, "FILEPATH/01_clean_data_covars.csv")
