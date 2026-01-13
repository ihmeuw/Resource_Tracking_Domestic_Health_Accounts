#### #----#                    Docstring                    #----# ####
#' Title:    02_predict_work_variables.R
#' Project:  Brain Health Initiative
#' Purpose : Use MTUS two part regression and disease-specific caregiving
#'           hour estimates to generate draws of employment probability,
#'           work hours, baseline employment probability, and baseline
#'           work hours by cause.
#'     
#' Author: USERNAME
#' Date: 03/11/2025
#' Last Updated: 10/07/2025
#---------------------------------------------------------------------#

pacman::p_load(data.table, dplyr, cli, plotrix, xlsx, writexl, janitor, arrow, MASS)
rm(list = ls())

################### #----# ENVIRONMENT SETUP #----# ###################
cat(paste0(" Environment Setup \n"))

#----# Set up directory roots #----#
if (!exists("code_repo"))  {
  code_repo <- unname(ifelse(Sys.info()['sysname'] == "Windows", "ROOT", paste0("ROOT", Sys.info()['user'][1], "ROOT")))
}

## Parameters
date <- gsub('-', '_', Sys.Date())
gbd_release_id <- ID
years <- 2000:2021

## Source functions
source('FILEPATH')
source('FILEPATH')
source("FILEPATH")
source("FILEPATH")
source("FILEPATH")

## Filepaths
data_path <- 'FILEPATH'

## Lists
locs <- get_location_metadata(location_set_id = ID, release_id = gbd_release_id)
ages <- get_age_metadata(release_id = gbd_release_id)
causes <- get_cause_metadata(cause_set_id = ID, release_id = gbd_release_id)
bhi_causes <- causes[parent_id %in% c(542, 558, 561) | acause %in% c('cvd_stroke', 'encephalitis', 'inj_suicide', 'meningitis', 'mental_alcohol', 'neo_brain')]

#---------------------------------------------------------------------#

###############################################################################
###                                                                     #######
###                       Pull and process data
###                                                                     #######
###############################################################################

## Read in condition-specific informal care hours
all_disease_care <- fread('FILEPATH')
draw_cols <- paste0('care_hours', 1:500)
all_disease_care[, c(draw_cols) := .SD / 7, .SDcols = paste0('Draw_', 1:500)] ## Convert from per week to per day space
all_disease_care <- all_disease_care[, c('location_id', 'location_name', 'year_id', 'cause_name', draw_cols), with = F]

## Read in IHME labor force participation estimates
lfp <- fread('FILEPATH')
lfp <- lfp[location_id %in% locs[level == 3]$location_id & year_id %in% years]
lfp <- lfp[, .(location_id, year_id, sex_id, age_group_name, lfp = val)]
lfp <- dcast.data.table(lfp, location_id + year_id + sex_id ~ age_group_name, value.var = 'lfp')
lfp[, `70 to 74` := 0.50 * `65 to 69`] ## Not modeled by IHME, assign to 50% of 65 to 69
lfp[, `75 to 79` := 0.25 * `65 to 69`] ## Not modeled by IHME, assign to 25% of 65 to 69
lfp <- melt.data.table(lfp,
                       id.vars = c('location_id', 'year_id', 'sex_id'),
                       variable.name = 'age_group_name',
                       variable.factor = F,
                       value.name = 'lfp')
covariates <- copy(lfp)

## Pull and merge socio-demographic index from GBD
sdi <- get_covariate_estimates(covariate_id = ID,
                               year_id = years,
                               location_id = locs[level == 3]$location_id,
                               sex_id = 3,
                               age_group_id = 22,
                               release_id = gbd_release_id)
sdi <- sdi[, .(location_id, year_id, sdi = mean_value)]
covariates <- merge(covariates, sdi, by = c('location_id', 'year_id'), all.x = T)

## Adjust sex_id to a binary variable to align with two-part regression
covariates[, sex_id := sex_id - 1]

## Factor age group name to align with two-part regression
covariates <- merge(covariates, ages[, .(age_group_id, age_group_name)], by = 'age_group_name', all.x = T)
covariates[, age_group_name := factor(age_group_name, levels = ages[age_group_id %in% 8:20]$age_group_name, ordered = F)]


###############################################################################
###                                                                     #######
###                     Generate work variable draws
###                                                                     #######
###############################################################################

## Source two part regression generated in "01_two_part_regression.R"
employment_model <- readRDS(paste0(data_path, 'FILEPATH'))
work_model <- readRDS(paste0(data_path, 'FILEPATH'))

## Pull vcov matrix and pull draws for each model
set.seed(9114)

employment_vcov_mat <- vcov(employment_model)
employment_beta_means <- coefficients(summary(employment_model))[, 1]
employment_beta_temp <- data.table(mvrnorm(500, employment_beta_means, employment_vcov_mat))

work_beta_means <- coefficients(summary(work_model))[, 1]
work_vcov_mat <- vcov(work_model)
work_beta_temp <- data.table(mvrnorm(500, work_beta_means, work_vcov_mat))

## Merge on GBD cause IDs for loop
all_disease_care <- merge(all_disease_care, bhi_causes[, .(cause_name, cause_id)], by = 'cause_name', all.x = T)
all_disease_care[cause_name == 'Non-opioid drug use disorders', cause_id := 9999] ## Aggregate cause for BHI work only

count <- 1
## Loop through causes and save out by cause ID
for (cause in sort(unique(all_disease_care$cause_id))) {
  
  # Print statement to follow progress
  if (cause == 9999) {
    print(paste0('Generating draws for Non-opioid drug use disorders (cause ', count, ' of 24)'))
  } else {
    print(paste0('Generating draws for ', bhi_causes[cause_id == cause]$cause_name, ' (cause ', count, ' of 24)'))
  }
  
  ## Generate sex-/age-specific estimates by duplicating all age and all sex estimates
  temp <- copy(covariates)
  temp <- merge(temp, all_disease_care[cause_id == cause], by = c('location_id', 'year_id'), all.x = T)
  
  ## Loop through draws/betas to generate draws of employment probability and work hours w. baseline
  for (draw in 1:500) {
    
    # Print statement to follow progress
    if (draw %in% c(seq(100, 500, 100))) {
      print(paste0(draw, ' of 500 draws completed'))
    }
    
    # Set coefficients for each draw
    employment_model$coefficients[1:30] <- c(as.vector(unlist(employment_beta_temp[draw,])))
    work_model$coefficients[1:29] <- c(as.vector(unlist(work_beta_temp[draw,])))
    
    # Predict employment probability and work hours
    setnames(temp, paste0('care_hours', draw), 'care_hours')
    temp[, paste0('emp_prob', draw) := predict(employment_model, type = 'response', newdata = temp)]
    temp[, paste0('work_hours', draw) := predict(work_model, type = 'response', newdata = temp)]
    
    # Predict baseline employment probability and work hours
    temp[, care_hours := 0]
    temp[, paste0('emp_prob_baseline', draw) := predict(employment_model, type = 'response', newdata = temp)]
    temp[, paste0('work_hours_baseline', draw) := predict(work_model, type = 'response', newdata = temp)]
    
    # Drop care hours draw
    temp[, `:=` (care_hours = NULL)]
    
  }
  
  ## Drop columns and re-adjust sex_id to match GBD sex IDs
  temp[, `:=` (sex_id = sex_id + 1, location_name = NULL, age_group_name = NULL, lfp = NULL, sdi = NULL, cause_id = NULL, cause_name = NULL)]
  
  ## Add age group id 80+
  to_append <- copy(temp[age_group_id == 20])
  to_append[, age_group_id := 21]
  to_append[, c(paste0('emp_prob', 1:500), paste0('work_hours', 1:500), paste0('emp_prob_baseline', 1:500), paste0('work_hours_baseline', 1:500)) := 0]
  temp <- rbind(temp, to_append)
  rm(to_append)
  
  ## Order and save out draws
  temp <- temp[order(location_id, year_id, sex_id, age_group_id)]
  arrow::write_feather(temp, paste0(data_path, 'FILEPATH', cause, '.feather'))
  rm(temp)
  
  ## Add to count to follow progress
  count <- count + 1
  
}

## End of Script ##