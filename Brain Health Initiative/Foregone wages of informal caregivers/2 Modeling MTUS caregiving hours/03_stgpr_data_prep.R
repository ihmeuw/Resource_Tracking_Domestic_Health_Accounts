#### #----#                    Docstring                    #----# ####
#' Title:    03_stgpr_data_prep.R
#' Project:  IHME Brain Health Initiative
#' Purpose: Prep and standardize format of MTUS data
#' for STGPR. Two datasets for two separate streams of ST-GPR will be produced.
#'     
#' Author: USERNAME
#' Date: 02/10/2024
#' Last Updated: 
#---------------------------------------------------------------------#

pacman::p_load(data.table, dplyr, cli, plotrix, xlsx, writexl, lme4)
rm(list = ls())

################### #----# ENVIRONMENT SETUP #----# ###################
cat(paste0(" Environment Setup \n"))

#----# Set up directory roots #----#
rm(list=ls())
if (!exists("code_repo"))  {
  code_repo <- unname(ifelse(Sys.info()['sysname'] == "Windows", "ROOT", paste0("ROOT", Sys.info()['user'][1], "ROOT")))
}

## Source functions
source("FILEPATH")
source("FILEPATH")
source("FILEPATH")
source("FILEPATH")

## Date for archived data
date <- gsub('-', '_', Sys.Date())
bhi_share <- paste0('FILEPATH')
data_path <- paste0(bhi_share, 'FILEPATH')
years <- 2000:2026
gbd_release_id <- ID
locs <- get_location_metadata(location_set_id = ID, release_id = gbd_release_id)

#---------------------------------------------------------------------#

cat("\n\n")
cat(col_br_green("\t#####################################\n"),
    col_br_green("\t#### BEGIN Analysis file work #######\n"),
    col_br_green("\t#####################################\n\n"))

################### #----# Main #----# ###################

#### ## ST-GPR data prep and processing ## ####
cat(paste0(" ST-GPR data prep and processing \n"))
#---------------------------------------------------------------------#

## Read in cleaned mtus data
dt <- fread(paste0(data_path, 'FILEPATH'))

## Subset to years for modeling
dt <- dt[year_id >= 2000]

#### ## Add, update, remove columns ## ####
cat(paste0(" Add, update, remove columns \n"))
#---------------------------------------------------------------------#

## location_id
dt <- merge(dt, locs[, .(location_id, ihme_loc_id)], by = 'ihme_loc_id', all.x = T)
dt[, ihme_loc_id := NULL]

## year_id, year_start, year_end
dt[, `:=` (year_start = year_id, year_end = year_id)]

## sex
dt[, sex := ifelse(sex_id == 1, 'Male', 'Female')]

## age_start, age_end
age_ids <- get_age_metadata(release_id = gbd_release_id)
age_ids <- age_ids[, .(age_group_id, age_start = age_group_years_start, age_end = age_group_years_end)]
dt <- merge(dt, age_ids, by = 'age_group_id', all.x = T)

## measure
dt[, measure := 'continuous']
dt[, measure_id := 19]

## underlying_nid
dt[, underlying_nid := '']

## is_outlier
dt[, is_outlier := 0]

## seq
dt <- dt[order(location_id, year_id, year_start, year_end, sex, age_group_id)]

## Value
setnames(dt, 'mean', 'val')

## Subset, sequence, and save data sets
for (var in c('adult', 'child', 'all')) {
  temp <- copy(dt[variable == paste0(var, '_hours')])
  temp[, seq := 1:nrow(temp)]
  fwrite(temp, paste0(data_path, 'FILEPATH', var, 'FILEPATH'))
}


#### ## ST-GPR custom stage one models ## ####
cat(paste0(" ST-GPR custom stage one models \n"))
#---------------------------------------------------------------------#

## Model formulas
model_formula1 <- 'log_offset_val ~ cv_un_care + age_group_id + sex_id + cv_relative_education + (1|super_region_name/region_name/ihme_loc_id)'

## Offset data
model_data <- copy(dt[, .(location_id, age_group_id, sex_id, year_id, variable, val)])

offset_adult <- quantile(model_data[variable == 'adult_hours' & val != 0]$val, 0.01)
offset_child <- quantile(model_data[variable == 'child_hours' & val != 0]$val, 0.01)
offset_all <- quantile(model_data[variable == 'all_hours' & val != 0]$val, 0.01)

model_data[variable == 'adult_hours', offset_val := val + offset_adult]
model_data[variable == 'child_hours', offset_val := val + offset_child]
model_data[variable == 'all_hours', offset_val := val + offset_all]

model_data[, log_offset_val := log(offset_val)]

## Read in custom covariates
custom_covariates <- fread(paste0(data_path, 'FILEPATH'))

## Merge on UN care estimates and relative education
model_data <- merge(model_data, custom_covariates[, .(location_id, year_id, age_group_id, sex_id, cv_un_care, cv_relative_education)], by = c('location_id', 'age_group_id', 'sex_id', 'year_id'), all.x = T)
model_data <- merge(model_data, locs[, .(location_id, ihme_loc_id, region_name, super_region_name)], by = 'location_id', all.x = T)

## Factor necessary variables
model_data[, age_group_id := factor(age_group_id, ordered = F)]
model_data[, sex_id := factor(sex_id, ordered = F)]

## Model
model_adult <- lmer(model_formula1, REML = F, data = model_data[variable == 'adult_hours'])
model_child <- lmer(model_formula1, REML = F, data = model_data[variable == 'child_hours'])
model_all <- lmer(model_formula1, REML = F, data = model_data[variable == 'all_hours'])

## Generate square data set
final_data <- CJ(location_id = locs[level >= 3]$location_id,
                 year_id = 2000:2026,
                 age_group_id = c(8:20, 30),
                 sex_id = 1:2)

## Merge on location variables and make subnationals equal to national location id
final_data <- merge(final_data, locs[, .(location_id, ihme_loc_id, region_name, super_region_name)], by = c('location_id'), all.x = T)
final_data[, ihme_loc_id := substr(ihme_loc_id, 1, 3)]


custom_covariates <- custom_covariates[location_id %in% locs[level == 3]$location_id]
custom_covariates <- merge(custom_covariates, locs[, .(location_id, ihme_loc_id)], by = 'location_id', all.x = T)

final_data <- merge(final_data, custom_covariates[, .(ihme_loc_id, year_id, age_group_id, sex_id, cv_un_care, cv_relative_education)], by = c('ihme_loc_id', 'year_id', 'age_group_id', 'sex_id'), all.x = T)

final_data[, age_group_id := factor(age_group_id, ordered = F)]
final_data[, sex_id := factor(sex_id, ordered = F)]


final_data[, predict_adult := predict(object = model_adult, final_data, allow.new.levels = T)]
final_data[, predict_child := predict(object = model_child, final_data, allow.new.levels = T)]
final_data[, predict_all := predict(object = model_all, final_data, allow.new.levels = T)]

final_data[, predict_adult := exp(predict_adult) - offset_adult]
final_data[, `:=` (predict_child = exp(predict_child), offset = offset_child)]
final_data[, predict_all := exp(predict_all) - offset_all]

final_data[predict_adult < 0, predict_adult := 0]
final_data[predict_child < 0, predict_child := 0]
final_data[predict_all < 0, predict_all := 0]

for (var in c('adult', 'child', 'all')) {
  
  temp <- copy(final_data)
  setnames(temp, paste0('predict_', var), 'cv_custom_stage_1')
  
  if (var == 'child') {
    temp <- temp[, .(location_id, year_id, age_group_id, sex_id, cv_custom_stage_1, offset)]
  } else {
    temp <- temp[, .(location_id, year_id, age_group_id, sex_id, cv_custom_stage_1)]
  }
  
  temp[, age_group_id := as.numeric(as.character(age_group_id))]
  temp[, sex_id := as.numeric(as.character(sex_id))]
  fwrite(temp, paste0(data_path, 'FILEPATH', var, 'FILEPATH'))
  
}

## End of Script ##