#################################################################
# Title:    02_create_disability_weights_all_ages.R
# Author: USERNAME
# Date: 2024 12 31
# Project:  BHI - Brain Health Initiative
# Purpose : Pull YLDs and prevalent cases from the GBD and 
#           calculate disability weights for brain health causes
#################################################################

# Setting up environment
# clear environment
rm(list = ls())

# packages
library(data.table)

# file paths
age_group_id_path <- "FILEPATH/age_group_id_table.csv"

# functions
source("FILEPATH/get_location_metadata.R")
source("FILEPATH/get_age_metadata.R")
source("FILEPATH/get_outputs.R")

# Importing Age group info
age_group_id_table <- fread(age_group_id_path)

# Data info
data_yr <- 2021
gbd_release_id <- 16
years <- 2000:(data_yr)
locs <- get_location_metadata(location_set_id = 35, release_id = gbd_release_id)[level == 3]
ages <- get_age_metadata(age_group_set_id = 24, release_id = gbd_release_id)

####----# Pull and process burden data #----####
cat(' Pull and process burden data\n')

## Pull YLD values
burden <- get_outputs('cause',
                      cause_id = c(332,    #  meningitis
                                   337,    #  encephalitis
                                   477,    #  neo_brain
                                   494,    #  cvd_stroke
                                   543,    #  neuro_dementia
                                   544,    #  neuro_parkinsons
                                   545,    #  neuro_epilepsy
                                   546,    #  neuro_ms
                                   554,    #  neuro_neurone
                                   557,    #  neuro_other
                                   559,    #  mental_schizo
                                   567,    #  mental_unipolar
                                   570,    #  mental_bipolar
                                   571,    #  mental_anxiety
                                   572,    #  mental_eating
                                   575,    #  mental_pdd
                                   578,    #  mental_adhd
                                   579,    #  mental_conduct
                                   582,    #  mental_id
                                   585,    #  mental_other
                                   560,    #  mental_alcohol
                                   562,    #  mental_drug_opioids
                                   563, 564, 565, 566,    #  mental_drug_other (other substances to aggregate together)
                                   718,    #  inj_suicide
                                   972),   #  neuro_headach
                      location_id = c(1, locs[level == 3]$location_id), # global and country locations
                      year_id = years, # years 2000 to 2021
                      sex_id = 3, # both sexes
                      age_group_id = 22, # all ages
                      measure_id = c(3, 5), # prevalence and YLDs
                      metric_id = 1, # numeric
                      release_id = gbd_release_id)

# Formatting cause ids
## Reassign other drug use disorders
# Non-opioid use disorders is a BHI-specific cause that is an aggregate of the following disorders:
#   - cocaine use disorders
#   - amphetamine use disorders
#   - cannabis use disorders
#   - other drug use disorders

burden[cause_id %in% 563:566, cause_name := 'Non-opioid drug use disorders']

## Aggregate measures for reassigned cause
burden_agg <- burden[, .(val = sum(val, na.rm = T)),
                     by = .(location_id, location_name, year_id, sex_id, age_group_name, cause_name, measure)]

## Reshape long to wide by measure
burden_wide <- dcast.data.table(burden_agg, location_id + location_name + year_id + sex_id + age_group_name + cause_name ~ measure, value.var = 'val')

## Calculate YLDs per case (disability weight)
burden_wide[, yld_per_case := yld / prevalence]

# adding age group id
burden_wide <- merge(burden_wide, age_group_id_table, by = 'age_group_name', all.x = T)

## Reshape long to wide by year to make more readable
burden_wide_years <- dcast.data.table(burden_wide, location_id + location_name + sex_id + age_group_name + age_group_id + cause_name ~ year_id, value.var = 'yld_per_case')

# Saving out dt
fwrite(burden_wide, "FILEPATH/average_disability_weights_by_loc_year_no_age_sex.csv")
fwrite(burden_wide_years, "FILEPATH/average_disability_weights_by_loc_year_no_age_sex_wide_by_year.csv")
