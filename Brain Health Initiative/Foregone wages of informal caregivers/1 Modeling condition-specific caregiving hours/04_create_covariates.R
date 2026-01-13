##########################################################################
# Title:    04_create_covariates.R
# Author: USERNAME
# Date: 2025 03 13
# Project:  BHI - Brain Health Initiative
# Purpose : To create predicted values from mixed-effects model for 
#           caregiver manuscript
##########################################################################

# Setting up environment
# clear environment
rm(list = ls())

# packages
library(data.table)
library(ggplot2)
library(lme4)
library(splines)
library(lmerTest)
library(arrow)
library(stringr)
library(dplyr)

# functions
source("FILEPATH/get_location_metadata.R")
source('FILEPATH/currency_conversion.R')
source('FILEPATH/get_covariate_estimates.R')
source("FILEPATH/get_outputs.R")

# file paths
model_data_path <- "FILEPATH/caregiving_all_ages_model_dt.csv"
disability_weights_path <-"FILEPATH/average_disability_weights_by_loc_year_no_age_sex.csv"
pred_data_path <- "FILEPATH/caregiver_global_all_ages_pred_dt.csv"
utilization_path <- "FILEPATH/Cause_Utilization_formatted_2025_09_12.csv"
age_group_id_path <-"FILEPATH/age_group_id_table.csv"
care_needs_path <-"FILEPATH/m_all_age.csv"

# GBD release ID
gbd_release_id <- 16

# add location id to
hierarchy <- get_location_metadata(
  location_set_id = 35,
  release_id = gbd_release_id
)

# Loading in Data
model_dt <- fread(model_data_path)
disability_weights_dt <- fread(disability_weights_path)
pred_dt <- fread(pred_data_path)[cause_name != "Other neurological disorders"]
util_dt <- fread(utilization_path)
age_group_id_table <- fread(age_group_id_path)
care_score_dt <- fread(care_needs_path)

# Data Info
# storing country level location ids
lvl3_locs <- sort(hierarchy[level == 3]$location_id)

# data info
years <- 2000:2021
ages <- 8:20
all_causes <- sort(unique(pred_dt$cause_name))

# Data cleaning
# consolidating and cleaning care type 
model_dt[care_type == "", care_type := "Not specified"]
model_dt[care_type == "not specified", care_type := "Not specified"]
model_dt[care_type == "not_specified", care_type := "Not specified"]
model_dt[care_type == "attendane and companionship", care_type := "All care"]
model_dt[care_type == "all", care_type := "All care"]
model_dt[care_type == "all_care", care_type := "All care"]
model_dt[care_type == "all informal care", care_type := "All care"]
model_dt[care_type == "total", care_type := "All care"]
model_dt[care_type == "Total", care_type := "All care"]
model_dt[care_type == "Basic ADL", care_type := "ADL"]
model_dt[care_type == "Instrumental ADL", care_type := "ADL"]
model_dt[care_type == "PADL", care_type := "ADL"]
model_dt[care_type == "personal care", care_type := "ADL"]
model_dt[care_type == "practical assistance", care_type := "ADL"]
model_dt[care_type == "ST", care_type := "Supervision"]
model_dt[care_type == "supervision", care_type := "Supervision"]

# removing North America location from dataset
model_dt <- model_dt[location_name != "North America"]

# adding year id
model_dt[, year_id := data_collection_year]

# adding loc id, ihme loc id, and super region name
model_dt <- merge(model_dt, hierarchy[, .(ihme_loc_id, location_name, location_id, super_region_name, region_name)], by = c("location_id", "location_name"), all.x = TRUE)

# Cleaning Estimation Method
# per dyad (per caregiver)
model_dt[estimation_method == "dyad", estimation_method := "per_dyad"]
model_dt[estimation_method == "per dyad", estimation_method := "per_dyad"]
model_dt[estimation_method == "per caregiver", estimation_method := "per_dyad"]

# per patient
model_dt[estimation_method == "patient", estimation_method := "per_patient"]
model_dt[estimation_method == "per patient", estimation_method := "per_patient"]

# Creating modeling dt
model_dt <- model_dt[, .(pmid, 
                         location_id,
                         location_name,
                         ihme_loc_id,
                         super_region_name,
                         region_name,
                         year_id,
                         n_caregiver_female,
                         n_caregiver_male,
                         estimation_method,
                         cause_name,
                         care_type,
                         un_mean_hrs,
                         percent_receiving_informal_care,
                         hours_informal_care_wk)]

# Standardizing disability weights data
# Renaming columns
setnames(disability_weights_dt, old = "yld_per_case", new = "disability_weight")

# No sex model dt
# Removing all age and sex cols
disability_weights_dt[, `:=` (age_group_name = NULL,
                              age_group_id = NULL,
                              sex_id = NULL)]

model_dt <- merge(model_dt, disability_weights_dt, by = c("location_id",
                                                          "location_name",
                                                          "year_id",
                                                          "cause_name"), all.x = TRUE)

# Adding new care type grouping column
model_dt[care_type %in% c("ADL", "IADL", "Not specified"), care_type_group := "Non_supervisory care"]
model_dt[care_type == "All care", care_type_group := "All care"]
model_dt[care_type == "Supervision", care_type_group := "Supervision"]

# Adding condition classification
# adding cause name and condition category
mental <- c("Self−harm",
            "Attention−deficit/hyperactivity disorder",
            "Alcohol use disorders",
            "Anxiety disorders",
            "Bipolar disorder",
            "Conduct disorder",
            "Opioid use disorders",
            "Non-opioid drug use disorders",
            "Eating disorders",
            "Idiopathic developmental intellectual disability",
            "Other mental disorders",
            "Autism spectrum disorders",
            "Schizophrenia",
            "Depressive disorders")

neuro <- c("Stroke",
           "Encephalitis",
           "Meningitis",
           "Brain and central nervous system cancer",
           "Alzheimer's disease and other dementias",
           "Idiopathic epilepsy",
           "Headache disorders",
           "Multiple sclerosis",
           "Motor neuron disease",
           "Other neurological disorders",
           "Parkinson's disease")

# adding disorder classification
model_dt[, disorder_classification := ifelse(cause_name %in% mental, "mental",
                                             ifelse(cause_name %in% neuro, "neurological", NA))]

# Adding GDP pc
# get gdp
gdp <- data.table(read_feather("FILEPATH/scenario_draws.feather"))
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
model_dt <- merge(model_dt, gdp_dt, by = c("location_id",
                                           "ihme_loc_id",
                                           "year_id"), all.x = T)

# Adding SDI values
sdi_dt <- get_covariate_estimates(covariate_id = 881,
                                  location_id = lvl3_locs,
                                  year_id = years,
                                  release_id = gbd_release_id) 

# condensing dt
sdi_dt <- sdi_dt[, .(location_id, 
                     year_id,
                     sdi_val = mean_value)]

# adding sdi to pred dt
model_dt <- merge(model_dt, sdi_dt, by = c("location_id", "year_id"), all.x = T)

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
model_dt <- merge(model_dt, haqi_dt, by = c("location_id", "year_id"), all.x = T)

# Keeping years 2000-2021 and working age groups
model_dt <- model_dt[year_id %in% years]

# Converting percentage values to proportions
# percentage receiving informal care
model_dt[, percent_receiving_informal_care := as.numeric(gsub("%", "", percent_receiving_informal_care)) / 100]

# # Correcting hours of informal care for per dyad values
model_dt <- merge(model_dt, util_dt, by = "cause_name", all.x = T)

# for missing causes, giving 5% util val
model_dt[is.na(utilization_med), utilization_med := 0.05]

# correcting hours of informal care for per per patient values
model_dt[estimation_method == "per_patient", adj_hours_informal_care_wk := hours_informal_care_wk / utilization_med]
model_dt[estimation_method == "per_dyad", adj_hours_informal_care_wk := hours_informal_care_wk]

# Adding care score to model dt
# removing unused columns
care_score_dt[, `:=` (location_name = NULL,
                      location_type = NULL,
                      ihme_loc_id = NULL,
                      super_region_name = NULL,
                      super_region_id = NULL,
                      region_id = NULL,
                      region_name = NULL,
                      disorder_classification = NULL,
                      cause_id = NULL,
                      acause = NULL,
                      expected = NULL,
                      metric_id = NULL,
                      metric_name = NULL,
                      daly = NULL,
                      yld = NULL,
                      yll = NULL)]

# only retaining relevant years
care_score_dt <- unique(care_score_dt[year_id %in% years])


# adding m to model dt and pred dt
# model dt
model_dt <- merge(model_dt, care_score_dt, by = c("location_id", "year_id", "cause_name"), all.x = TRUE)
nrow(model_dt)
# 138
nrow(model_dt[is.na(m)])
# 0

# Relabeling M as prop_pre_mortality
# model dt
model_dt[, prop_pre_mortality := m]

# pred dt
pred_dt[, prop_pre_mortality := m]

# MTUS Weekly Hours of Work
# model dt
model_dt <- merge(model_dt, mtus_hrs_dt, by = c("location_id", "year_id"), all.x = TRUE)

# Creating log values for covariate
# offset value (small as possible)
offset_val <- 1e-10

# hours of informal care
model_dt[, lg_hours_informal_care_wk := log(adj_hours_informal_care_wk)]

# log(GDPpc)
# model dt
model_dt[, lg_gdp_pc := log(gdp_pc)]

# pred dt
pred_dt[, lg_gdp_pc := log(gdp_pc)]

# UN hours of care
# model dt
model_dt[, lg_mtus_mean_wklyhrs := log(mean_hours)]

# pred dt
pred_dt[, lg_mtus_mean_wklyhrs := log(mean_hours)]

# YLDs
# model dt
model_dt[, lg_yld := log(yld)]

# pred dt
pred_dt[, yld_offset := yld + offset_val]
pred_dt[, lg_yld := log(yld_offset)]

# Adding indicator variable for mental disorders
# model dt
model_dt[, mental_indicator_var := ifelse(cause_name %in% mental, 1, 0)]

# pred dt
pred_dt[, mental_indicator_var := ifelse(cause_name %in% mental, 1, 0)]

# Saving out Model dt
fwrite(model_dt, "FILEPATH/caregiving_all_ages_covariates_model_dt.csv")
fwrite(pred_dt, "FILEPATH/caregiver_global_all_ages_pred_dt.csv")
