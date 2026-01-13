#### #----#                    Docstring                    #----# ####
#' Title:    01_prep_variables.R
#' Project:  IHME Brain Health Initiative
#' Purpose : Pull, process, and save annual wage, number of informal 
#'           caregivers, and caregiving by sex proportions.
#'     
#' Author: USERNAME
#' Date: 09/17/2025
#' Last Updated: 10/07/2025
#---------------------------------------------------------------------#

pacman::p_load(data.table, dplyr, cli, plotrix, xlsx, writexl, janitor, arrow, stringr)
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
source("FILEPATH")
source("FILEPATH")
source("FILEPATH")
source("FILEPATH")

## Filepaths
bhi_share <- 'FILEPATH'
data_path <- paste0(bhi_share, 'FILEPATH')

## Lists
locs <- get_location_metadata(location_set_id = ID, release_id = gbd_release_id)
ages <- get_age_metadata(release_id = gbd_release_id)
causes <- get_cause_metadata(cause_set_id = 3, release_id = gbd_release_id)
bhi_causes <- causes[parent_id %in% c(542, 558, 561) | acause %in% c('cvd_stroke', 'encephalitis', 'inj_suicide', 'meningitis', 'mental_alcohol', 'neo_brain')]

#---------------------------------------------------------------------#

###############################################################################
###                                                                     #######
###                   Annual wage estimates
###                                                                     #######
###############################################################################

## Print statement to track progress
print('Processing wage estimates')

## Read in annual wage estimates
global_wages <- as.data.table(arrow::read_feather("FILEPATH"))
global_wages <- global_wages[year_id %in% years]

## Take first 500 draws
setnames(global_wages, paste0('draw_', 0:499), paste0('annual_wage', 1:500))
global_wages <- global_wages[, c('location_id', 'year_id', 'age_group_id', 'sex_id', paste0('annual_wage', 1:500)), with = F]

## Extend estimates out for missing years using 5-year annualized rate of change
global_wages <- melt.data.table(global_wages,
                                id.vars = c('location_id', 'year_id', 'age_group_id', 'sex_id'),
                                variable.factor = F)
global_wages <- dcast.data.table(global_wages,
                                 location_id + age_group_id + sex_id + variable ~ year_id,
                                 value.var = 'value')
global_wages[, aroc := (((`2019` / `2015`)^(1 / (2019 - 2015)) - 1))]
global_wages[, `2020` := `2019` * (1 + aroc)]
global_wages[, `2021` := `2020` * (1 + aroc)]

## Reshape wide on draws
global_wages <- melt.data.table(global_wages,
                                id.vars = c('location_id', 'age_group_id', 'sex_id', 'variable'),
                                measure.vars = as.character(years),
                                variable.name = 'year_id',
                                variable.factor = F)
global_wages <- dcast.data.table(global_wages, location_id + year_id + age_group_id + sex_id ~ variable, value.var = 'value')
global_wages[, year_id := as.numeric(year_id)]

## Reassign 80+ age group to $0
global_wages[age_group_id == 21, paste0('annual_wage', 0:499) := 0]

## Formatting and save
setcolorder(global_wages, c('location_id', 'year_id', 'age_group_id', 'sex_id', paste0('annual_wage', 1:500)))
global_wages <- global_wages[order(location_id, year_id, age_group_id, sex_id)]
arrow::write_feather(global_wages, paste0(data_path, 'FILEPATH'))

#########################################################################################################
###                                                                                               #######
###  Number of caregivers: Utilization, prevalent cases, and age matrix (volume and utilization)
###                                                                                               #######
#########################################################################################################

## Utilization
## ---------------------------------------------
## Read in informal care utilization
utilization <- fread('FILEPATH')

## Update utilization for each analysis (regular, sensitivity, etc.)
utilization[, utilization_med_reg := ifelse(is.na(utilization_med), 0.05, utilization_med)]
utilization[, utilization_med_sens1 := ifelse(is.na(utilization_med), 0.10, utilization_med)]
utilization[, utilization_med_sens2 := ifelse(is.na(utilization_med), 0.15, utilization_med)]
utilization[, utilization_med := NULL]

## Merge on cause ids
utilization <- merge(utilization, bhi_causes[, .(cause_id, cause_name)], by = 'cause_name', all.x = T)
utilization[cause_name == 'Non-opioid drug use disorders', cause_id := 9999]

## Cases by patient age (# of patients)
## ---------------------------------------------

## Print statement to track progress
print('Processing GBD prevalence estimates')

## Prevalent cases by patient age
prevalence <- get_outputs('cause',
                          cause_id = c(utilization[cause_id != 9999]$cause_id, 563, 564, 565, 566),
                          location_id = locs[level == 3]$location_id,
                          year_id = years,
                          age_group_id = c(1, 6:20, 30, 160),
                          sex_id = 3,
                          metric_id = 1,
                          measure_id = 5,
                          release_id = gbd_release_id)

## Aggregate non-opioid (other) drug use disorders
prevalence[cause_id %in% 563:566, `:=` (cause_id = 9999, cause_name = 'Non-opioid drug use disorders')]
prevalence <- prevalence[, .(val = sum(val, na.rm = T)),
                         by = .(location_id, year_id, age_group_id, cause_id, cause_name)]

## Convert from patient age to caregiver age (# of caregivers)
## ---------------------------------------------------------------
## Caregiver/patient age matrix
matrix <- fread(paste0(data_path, 'FILEPATH'))
matrix[, cause_group := NULL]
matrix[, `21` := `30` + `160`]
matrix[, `:=` (`30` = NULL, `160` = NULL)]
setnames(matrix, 'age_group_id_patient', 'age_group_id')

## Merge prevalent cases and age matrix by patient age and merge utilization by cause
prevalence <- merge(prevalence, matrix, by = 'age_group_id', all.x = T)
prevalence <- merge(prevalence, utilization, by = c('cause_name', 'cause_id'), all.x = T)

## Calculate number of prevalent cases by patient and caregiver age
prevalence[, paste0('reg_', 8:21) := (.SD / 100) * val * utilization_med_reg, .SDcols = as.character(8:21)]
prevalence[, paste0('sens1_', 8:21) := (.SD / 100) * val * utilization_med_sens1, .SDcols = as.character(8:21)]
prevalence[, paste0('sens2_', 8:21) := (.SD / 100) * val * utilization_med_sens2, .SDcols = as.character(8:21)]
prevalence[, c(as.character(8:21), 'val', 'utilization_med_reg', 'utilization_med_sens1', 'utilization_med_sens2') := NULL]

## Reshape long
prevalence <- melt.data.table(prevalence,
                              id.vars = c('age_group_id', 'location_id', 'year_id', 'cause_id', 'cause_name'),
                              variable.factor = F)

## Assign cases under 1 to zero
prevalence[value < 1, value := 0]

## Assign analysis type
prevalence[, version := ifelse(grepl('reg', variable), 'reg',
                               ifelse(grepl('sens1', variable), 'sens1', 'sens2'))]
prevalence[, variable := gsub('reg_|sens1_|sens2_', '', variable)]

## Reshape wide by age
prevalence <- dcast.data.table(prevalence,
                               age_group_id + location_id + year_id + cause_id + cause_name + version ~ variable,
                               value.var = 'value')

## Drop rows of zero
prevalence[, total := rowSums(.SD), .SDcols = as.character(8:21)]
prevalence <- prevalence[total != 0]
prevalence[, total := NULL]

## Order columns
setcolorder(prevalence, c('age_group_id', 'location_id', 'year_id', 'cause_id', 'cause_name', 'version', as.character(8:21)))

## Save out prevalence for two-way raking
prevalence_unraked <- copy(prevalence)
arrow::write_feather(prevalence_unraked, paste0(data_path, 'FILEPATH'))
rm(prevalence)

##------------------------------------------------------
##
## NOTE: Run two-way raking and return to this script
##
##------------------------------------------------------

## Read in new prevalence (informal caregivers) from two-way raking
prevalence_reg <- fread(paste0(data_path, 'FILEPATH'))
prevalence_sens1 <- fread(paste0(data_path, 'FILEPATH'))
prevalence_sens2 <- fread(paste0(data_path, 'FILEPATH'))

prevalence_reg[, version := 'reg']
prevalence_sens1[, version := 'sens1']
prevalence_sens2[, version := 'sens2']

prevalence_raked <- rbind(prevalence_reg, prevalence_sens1, prevalence_sens2)
setnames(prevalence_raked, 'patient', 'age_group_id')

## Assign any NAs or Inf to 0
prevalence_raked[, as.character(8:21) := lapply(.SD, function(x) fifelse(is.na(x), 0, x)), .SDcols = as.character(8:21)]
prevalence_raked[, as.character(8:21) := lapply(.SD, function(x) fifelse(is.infinite(x), 0, x)), .SDcols = as.character(8:21)]

## Assign any values under 1 to zero
prevalence_raked[, as.character(8:21) := lapply(.SD, function(x) fifelse(x < 1, 0, x)), .SDcols = as.character(8:21)]

## Generate dataset to fill
prevalence <- CJ(age_group_id = c(1, 6:20, 30, 160),
                 location_id = locs[level == 3]$location_id,
                 year_id = 2000:2021,
                 cause_id = unique(prevalence_unraked$cause_id),
                 version = c('reg', 'sens1', 'sens2'))
setnames(prevalence_unraked, as.character(8:21), paste0(8:21, '_unraked'))
setnames(prevalence_raked, as.character(8:21), paste0(8:21, '_raked'))

## Fill dataset
prevalence <- merge(prevalence, prevalence_unraked, by = c('age_group_id', 'cause_id', 'location_id', 'year_id', 'version'), all.x = T)
prevalence <- merge(prevalence, prevalence_raked, by = c('age_group_id', 'cause_id', 'location_id', 'year_id', 'version'), all.x = T)
prevalence[is.na(`8_unraked`), paste0(8:21, '_unraked') := 0]
prevalence[is.na(`8_raked`), paste0(8:21, '_raked') := 0]
rm(prevalence_unraked, prevalence_raked)

## Calculate totals
prevalence[, total_unraked := rowSums(.SD), .SDcols = paste0(8:21, '_unraked')]
prevalence[, total_raked := rowSums(.SD), .SDcols = paste0(8:21, '_raked')]

## Unraked total nonzero & raked total zero: use unraked values
prevalence[total_unraked > 0 & total_raked == 0, paste0(8:21, '_raked') := .SD, .SDcols = paste0(8:21, '_unraked')]
prevalence[, total_raked := rowSums(.SD), .SDcols = paste0(8:21, '_raked')]

## Unraked and raked totals differ: Use raked proportions and apply to unraked total
prevalence[abs(total_unraked - total_raked) > 1, paste0(8:21, '_raked') := .SD / total_raked * total_unraked, .SDcols = paste0(8:21, '_raked')]

## Clean up
prevalence <- prevalence[, c('age_group_id', 'cause_id', 'location_id', 'year_id', 'version', paste0(8:21, '_raked')), with = F]
setnames(prevalence, paste0(8:21, '_raked'), as.character(8:21))

##------------------------------------------------------------------------


## Assign as child or adult caregiving
prevalence[, variable := ifelse(age_group_id %in% c(1, 6, 7, 8), 'child_hours', 'adult_hours')]

## Aggregate
prevalence <- prevalence[, lapply(.SD, sum),
                         .SDcols = as.character(8:21),
                         by = .(version, variable, location_id, cause_id, year_id)]

arrow::write_feather(prevalence, paste0(data_path, 'FILEPATH'))

## Reshape wide to long by caregiver age
prevalence <- melt.data.table(prevalence,
                              id.vars = c('version', 'variable', 'location_id', 'year_id', 'cause_id'),
                              variable.name = 'age_group_id',
                              variable.factor = F)

prevalence[, age_group_id := as.numeric(age_group_id)]

## Reshape wide by version
prevalence <- dcast.data.table(prevalence,
                               cause_id + variable + location_id + year_id + age_group_id ~ version,
                               value.var = 'value')

## Formatting
setcolorder(prevalence, c('cause_id', 'variable', 'location_id', 'year_id', 'age_group_id'))
prevalence <- prevalence[order(cause_id, variable, location_id, year_id, age_group_id)]

## Save out
arrow::write_feather(prevalence[, .(cause_id, variable, location_id, year_id, age_group_id, n_caregivers = reg)],
                     paste0(data_path, 'FILEPATH'))
arrow::write_feather(prevalence[, .(cause_id, variable, location_id, year_id, age_group_id, n_caregivers = sens1)],
                     paste0(data_path, 'FILEPATH'))
arrow::write_feather(prevalence[, .(cause_id, variable, location_id, year_id, age_group_id, n_caregivers = sens2)],
                     paste0(data_path, 'FILEPATH'))


###############################################################################
###                                                                     #######
###          Split caregivers by sex using care hour ratios
###                                                                     #######
###############################################################################

## Print statement to track progress
print('Processing caregiving sex ratio estimates')

## Pull in care estimates from ST-GPR and calculate care hour male to female ratios
mtus_care <- data.table(arrow::read_feather(paste0(data_path, 'FILEPATH')))
setnames(mtus_care, paste0('draw_', 0:999), paste0('draw_', 1:1000))
mtus_care <- mtus_care[, c('location_id', 'year_id', 'age_group_id', 'sex_id', 'variable', paste0('draw_', 1:500)), with = F]
mtus_care[age_group_id == 30, age_group_id := 21]

## Populations
population <- get_population(location_id = locs[level == 3]$location_id,
                             year_id = years,
                             age_group_id = 8:21,
                             sex_id = 1:2,
                             release_id = gbd_release_id)

## Merge and calculate total hours
mtus_care <- merge(mtus_care, population, by = c('location_id', 'year_id', 'age_group_id', 'sex_id'), all.x = T)
mtus_care[, paste0('draw_', 1:500) := .SD * population, .SDcols = paste0('draw_', 1:500)]

## Reshape wide by sex
mtus_care <- melt.data.table(mtus_care,
                             id.vars = c('location_id', 'year_id', 'age_group_id', 'sex_id', 'variable'),
                             measure.vars = paste0('draw_', 1:500),
                             variable.factor = F)
mtus_care <- dcast.data.table(mtus_care,
                              location_id + year_id + age_group_id + variable + variable.1 ~ sex_id,
                              value.var = 'value')

## Calculate male and female caregiving proportions
mtus_care[, male_care_prop := `1` / (`1` + `2`)]
mtus_care[, female_care_prop := `2` / (`1` + `2`)]
mtus_care <- melt.data.table(mtus_care,
                             id.vars = c('location_id', 'year_id', 'age_group_id', 'variable', 'variable.1'),
                             measure.vars = c('male_care_prop', 'female_care_prop'),
                             variable.factor = F)
mtus_care[, sex_id := ifelse(variable.2 == 'female_care_prop', 2, 1)]

## Reshape wide on draws
mtus_care <- dcast.data.table(mtus_care, location_id + year_id + age_group_id + sex_id + variable ~ variable.1, value.var = 'value')

## Formatting
setnames(mtus_care, paste0('draw_', 1:500), paste0('care_prop', 1:500))
setcolorder(mtus_care, c('variable', 'location_id', 'year_id', 'age_group_id', 'sex_id', paste0('care_prop', 1:500)))
mtus_care <- mtus_care[order(variable, location_id, year_id, age_group_id, sex_id)]

## Save
arrow::write_feather(mtus_care, paste0(data_path, 'FILEPATH'))


## End of Script ##