##########################################################################
# Title:    05_create_pred_dataset_all_ages.R
# Author: USERNAME
# Date: 2025 03 13
# Project:  BHI - Brain Health Initiative
# Purpose : To create dataset with all potential covariates that might 
#           be used for Model #2 for all locations, years, and causes. 
##########################################################################
# Setting up environment
# clear environment
rm(list = ls())

# packages
library(data.table)

# functions
source("FILEPATH/get_location_metadata.R")
source('FILEPATH/currency_conversion.R')
source('FILEPATH/get_covariate_estimates.R')
source('FILEPATH/get_population.R')

# file paths
age_group_id_path <- "FILEPATH/age_group_id_table.csv"
un_caregiver_data_path <- "FILEPATH/06_stgpr_estimates.csv"
disability_weights_path <- "FILEPATH/average_disability_weights_by_loc_year_no_age_sex.csv"
direct_spending_path <- "FILEPATH/BHI_Spending_DATA_post_calibration_summary.csv"

# GBD release id
gbd_release_id <- 16

# add location id to
hierarchy <- get_location_metadata(
  location_set_id = 35,
  release_id = gbd_release_id)

# Loading in data
age_group_table <- fread(age_group_id_path)
un_caregiver_dt <- fread(un_caregiver_data_path)
disability_weights_dt <- fread(disability_weights_path)
hh_direct_spending_dt <- fread(direct_spending_path)[type_of_care == "HH" & cause_name != "Other neurological disorders"]

# Standardizing UN and disability weights data
# Renaming columns
setnames(un_caregiver_dt, old = "mean", new = "un_mean_wkly_care_hrs")

setnames(disability_weights_dt, old = "yld_per_case", new = "disability_weight")

# removing CI columns from UN caregiver dt
un_caregiver_dt[, `:=` (lower = NULL,
                        upper = NULL)]

# removing all age and all sex columns for disability weights
disability_weights_dt[, `:=` (age_group_name = NULL, 
                              age_group_id = NULL,
                              sex_id = NULL)]

# Data info
# storing country level location ids
lvl3_locs <- sort(hierarchy[level == 3]$location_id)

# data info
years <- 2000:2021
ages <- 22
sexes <- 3
causes <- sort(unique(disability_weights_dt$cause_name))

# Creating pred dt shell for merging model variables
pred_dt <- CJ(location_id = lvl3_locs, 
              year_id = years,
              cause_name = causes,
              age_group_id = ages,
              age_group_name = "All Ages",
              sex_id = sexes)

# Reading in population data for weighted means
#  get population values stratified by age
pop_wt_dt <- get_population(location_id = lvl3_locs,
                            year_id = years,
                            age_group_id = c(8:20),
                            sex_id = c(1, 2),
                            release_id = gbd_release_id 
)

# adding ihme loc id 
pop_wt_dt <- merge(pop_wt_dt, hierarchy[, .(location_id, ihme_loc_id)], by = "location_id")


# condensing columns
pop_wt_dt <- pop_wt_dt[, .(ihme_loc_id, 
                           location_id,
                           year_id, 
                           age_group_id,
                           sex_id,
                           population)]

# Adding population weights to un caregiver dt
un_caregiver_dt <- merge(un_caregiver_dt, pop_wt_dt, by = c("location_id", "year_id", "age_group_id", "sex_id"))

# Aggregate UN caregiver means to all ages
all_ages_un_caregiver_dt <- un_caregiver_dt[, .(un_mean_wkly_care_hrs = weighted.mean(un_mean_wkly_care_hrs, population, na.rm = T)), 
                                            by = .(location_id, year_id)]

# adding age group id and sex id
all_ages_un_caregiver_dt[, `:=` (age_group_id = ages,
                                 sex_id = sexes,
                                 age_group_name = "All Ages")]

# Merging datasets and adding columns from hierarchy
# merging UN caregiver data
pred_dt <- merge(pred_dt, all_ages_un_caregiver_dt, by = c("location_id", "year_id", "age_group_id", "age_group_name", "sex_id"), all.x = T)

# adding disability weights
pred_dt <- merge(pred_dt, disability_weights_dt, by = c("location_id", "year_id", "cause_name"), all.x = TRUE)

# adding location info
pred_dt <- merge(pred_dt, hierarchy[, .(ihme_loc_id, location_name, location_id, super_region_name, region_name)], 
                 by = c("location_id", "location_name"), all.x = TRUE)

# Adding care type group
pred_dt[, care_type_group := "All care"]
pred_dt[, care_type := "All care"]

# Adding condition classification
mental <- c("Attention-deficit/hyperactivity disorder",
            "Alcohol use disorders",
            "Anxiety disorders",
            "Autism spectrum disorders",
            "Bipolar disorder",
            "Conduct disorder",
            "Depressive disorders",
            "Eating disorders",
            "Idiopathic developmental intellectual disability",
            "Non-opioid drug use disorders",
            "Other mental disorders",
            "Opioid use disorders",
            "Schizophrenia",
            "Self-harm")

neuro <- c("Stroke",
           "Meningitis",
           "Brain and central nervous system cancer",
           "Alzheimer's disease and other dementias",
           "Idiopathic epilepsy", 
           "Multiple sclerosis",
           "Motor neuron disease",
           "Parkinson's disease",
           "Encephalitis",
           "Headache disorders",
           "Other neurological disorders")

# adding disorder classification
pred_dt[, disorder_classification := ifelse(cause_name %in% mental, "mental",
                                            ifelse(cause_name %in% neuro, "neurological", NA))]

# Adding HH direct spending values to pred dt
# creating 2020 and 2021 values by multiplying by AROC
# 2020 dt
hh_dt_2020 <- hh_direct_spending_dt[year_id == 2019]

hh_dt_2020[, year_id := 2020]

aroc_val <- 1.0547

hh_dt_2020[, spend := spend * aroc_val]

# copying 2020 dt to create 2021 dt and then multiplying by AROC
hh_dt_2021 <- copy(hh_dt_2020)

hh_dt_2021[, year_id := 2021]

hh_dt_2021[, spend := spend * aroc_val]

# adding values back to original dataset
hh_direct_spending_dt <- rbindlist(list(hh_direct_spending_dt, hh_dt_2020, hh_dt_2021))

# adding age group id
hh_direct_spending_dt <- merge(hh_direct_spending_dt, age_group_table, by = "age_group_name")

# adding location name
hh_direct_spending_dt <- merge(hh_direct_spending_dt, hierarchy[, .(location_id, location_name)], by = "location_id", all.x = TRUE)

# reorder columns 
hh_direct_spending_dt <- hh_direct_spending_dt[, .(location_id, 
                                                   location_name,
                                                   year_id, 
                                                   cause_name,
                                                   type_of_care,
                                                   age_group_id,
                                                   age_group_name,
                                                   sex_id,
                                                   hh_direct_spending = spend)]
# reorder dataset
setorder(hh_direct_spending_dt, location_id, year_id, cause_name, age_group_id, sex_id)

# creating all cause dt
all_cause_dt <- hh_direct_spending_dt[cause_name == "All cause"]

# only retaining individual causes (24 causes)
hh_direct_spending_dt <- hh_direct_spending_dt[!cause_name %in% c("All cause", "Mental", "Neurological")]

# removing cause name and type of care
all_cause_dt[, cause_name := NULL]
all_cause_dt[, type_of_care := NULL]

# renaming spending value of all cause
setnames(all_cause_dt, old = "hh_direct_spending", new = "hh_direct_spending_all_cause")

# adding all cause spending to hh direct spending dt
hh_direct_spending_dt <- merge(hh_direct_spending_dt, all_cause_dt, by = c("location_id", "location_name", "year_id", "age_group_id", "age_group_name", "sex_id"), all.x = T)

# aggregating over age and sex for hh direct spending
hh_direct_spending_dt <- hh_direct_spending_dt[, lapply(.SD, sum, na.rm = T), .SDcols = c("hh_direct_spending", "hh_direct_spending_all_cause"), by = .(location_id, location_name, year_id, cause_name)]

# calculating proportional spending
hh_direct_spending_dt[, prop_spend := hh_direct_spending / hh_direct_spending_all_cause]

# adding on to pred dt 
pred_dt <- merge(pred_dt, hh_direct_spending_dt, by = c("location_id", 
                                                        "location_name", 
                                                        "year_id", 
                                                        "cause_name"), all.x = T)

# give NA values for HH direct spending a 0
pred_dt[is.na(hh_direct_spending), hh_direct_spending := 0]
pred_dt[is.na(hh_direct_spending_all_cause), hh_direct_spending_all_cause := 0]
pred_dt[hh_direct_spending == 0, prop_spend := 0]
pred_dt[hh_direct_spending_all_cause == 0, prop_spend := 0]

# GDP pc
# get gdp
gdp <- data.table(read_feather("FILEPATH"))
gdp <- gdp[scenario == 0 & year <= 2021 & year >= 2000]
gdp <- melt.data.table(gdp, id.vars = c("year", "iso3", "scenario"), value.name = "data_var", variable.name = "draw")
gdp <- gdp[, draw := str_replace(draw, "draw_", "")]
gdp <- gdp[, draw := as.numeric(as.character(draw))]
gdp[, scenario := NULL]

# condensing and renaming columns
gdp <- gdp[year %in% years,
           .(gdp_pc = mean(data_var)),
           by = .(year_id = year, ihme_loc_id = iso3)]

# creating gbd dt
gdp <- merge(gdp[, .(ihme_loc_id, 
                     year_id, 
                     gdp_pc)], hierarchy[, .(location_id, 
                                             location_name, 
                                             ihme_loc_id)], by = 'ihme_loc_id')

# convert from 2022 USD to 2021 USD
gdp_dt_conv <- currency_conversion(gdp,
                                   col.loc = 'ihme_loc_id',
                                   col.value = 'gdp_pc',
                                   currency = 'usd',
                                   currency.year = 2022 ,
                                   base.year = 2021,
                                   base.unit = 'usd',
                                   converter.version = 8,
                                   simplify = F)

# storing keep columns 
keep_cols <- c("ihme_loc_id", "year_id", "location_id", "currency_year_new", "gdp_pc_new")

# retaining keep columns
gdp_dt <- gdp_dt_conv[, ..keep_cols]

# renaming gdp column
setnames(gdp_dt, old = "gdp_pc_new", new = "gdp_pc")

# removing currency year new col
gdp_dt[, currency_year_new := NULL]

# adding gdp pc to pred dt
pred_dt <- merge(pred_dt, gdp_dt, by = c("location_id",
                                         "ihme_loc_id", 
                                         "year_id"), all.x = T)

# Adding SDI value
sdi_dt <- get_covariate_estimates(covariate_id = 881,
                                  location_id = lvl3_locs,
                                  year_id = years,
                                  release_id = gbd_release_id) 

# condensing dt
sdi_dt <- sdi_dt[, .(location_id, 
                     year_id,
                     sdi_val = mean_value)]

# adding sdi to pred dt
pred_dt <- merge(pred_dt, sdi_dt, by = c("location_id", "year_id"), all.x = T)

# Adding HAQI values
# retreiving haqi values
haqi <- get_covariate_estimates(covariate_id = 1099,
                                location_id = lvl3_locs,
                                year_id = years,
                                release_id = gbd_release_id)

# condense dt
haqi_dt <- haqi[, .(location_id, 
                    year_id, 
                    haqi_val = mean_value)]

# adding haqi to pred dt
pred_dt <- merge(pred_dt, haqi_dt, by = c("location_id", "year_id"), all.x = T)


# Adding sex label to pred dt
pred_dt[, sex_label := "All Sex"]

# Reordering columns
pred_dt <- pred_dt[, .(ihme_loc_id,
                       location_id,
                       location_name,
                       super_region_name,
                       region_name,
                       year_id,
                       cause_name,
                       disorder_classification,
                       age_group_id,
                       age_group_name,
                       sex_id,
                       sex_label,
                       care_type_group,
                       care_type,
                       hh_direct_spending,
                       hh_direct_spending_all_cause,
                       prop_spend,
                       gdp_pc,
                       sdi_val,
                       haqi_val,
                       yld,
                       prevalence,
                       disability_weight,
                       un_mean_wkly_care_hrs)]

# Saving out pred idt
fwrite(pred_dt, "FILEPATH/caregiver_global_all_ages_pred_dt.csv")

# Save out population weighted UN caregiver dt
fwrite(pop_wt_dt, "FILEPATH/population_weighted_dt.csv")