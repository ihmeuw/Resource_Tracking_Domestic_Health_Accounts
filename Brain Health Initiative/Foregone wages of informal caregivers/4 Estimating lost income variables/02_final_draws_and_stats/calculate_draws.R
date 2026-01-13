#### #----#                    Docstring                    #----# ####
#' Title:    calculate_draws.R
#' Project:  Resource Tracking - Brain Health Initiative (BHI)
#' Purpose : Function is called from within a loop to calculate lost
#'           income and lost work hours draws for each BHI cause
#'           at the country-level.
#'           Data gets saved to cause specific file in BHI share drive
#'     
#' Author: USERNAME
#' Date: 2025-09-29
#' Last updated: 2025-10-07
#---------------------------------------------------------------------#

################### #----# ENVIRONMENT SETUP #----# ###################
cat(paste0(" Environment Setup \n"))
pacman::p_load(data.table, dplyr, arrow, stringr)

#----# Set up directory roots #----#
rm(list=ls())
if (!exists("code_repo"))  {
  code_repo <- unname(ifelse(Sys.info()['sysname'] == "Windows", "ROOT", paste0("ROOT", Sys.info()['user'][1], "ROOT")))
}

#----# Local CONSTANTS #----#
cat('starting program \n')
cause <- commandArgs(TRUE)
cat('this is the cause ID ', cause)

## Source functions
source("FILEPATH")

## Variable prep
currentDate <- format(Sys.time(), "%Y%m%d")
gbd_release_id <- ID
years <- 2000:2021

## Paths
bhi_share <- 'FILEPATH'
data_path <- paste0(bhi_share, 'FILEPATH')

## Causes
causes <- get_cause_metadata(cause_set_id = ID, release_id = gbd_release_id)
bhi_causes <- causes[parent_id %in% c(542, 558, 561) | acause %in% c('cvd_stroke', 'encephalitis', 'inj_suicide', 'meningitis', 'mental_alcohol', 'neo_brain')]
bhi_causes <- bhi_causes[!cause_id %in% c(557, 563:566), .(cause_id, cause_name)]
bhi_causes <- rbind(bhi_causes, data.table(cause_id = 9999, cause_name = 'Non-opioid drug use disorders'))

#---------------------------------------------------------------------#


###############################################################################
###                                                                     #######
###                     Read in prepped variables
###                                                                     #######
###############################################################################

## Annual wage
global_wages <- data.table(arrow::read_feather(paste0(data_path, 'FILEPATH')))

## Population-weighted caregiving proportions by sex and type of hours (child/adult patient)
care_sex_prop <- data.table(arrow::read_feather(paste0(data_path, 'FILEPATH')))
care_sex_prop <- care_sex_prop[variable != 'all_hours']

## Attributable fractions: scale down for comorbidities
att_frac <- fread('FILEPATH')
att_frac <- att_frac[, .(cause_name = condition, AF_value)]
att_frac <- merge(att_frac, bhi_causes, by = 'cause_name', all.x = T)

## Weekly hours of care per caregiver (convert to annual)
disease_care <- fread(paste0(data_path, 'FILEPATH'))
disease_care <- disease_care[cause_name == bhi_causes[cause_id == cause]$cause_name]
disease_care <- disease_care[, c('location_id', 'year_id', paste0('Draw_', 1:500)), with = F]
disease_care[, paste0('Draw_', 1:500) := .SD / 7 * 365.25, .SDcols = paste0('Draw_', 1:500)]
setnames(disease_care, paste0('Draw_', 1:500), paste0('care_hours_per_caregiver', 1:500))

###############################################################################
###                                                                     #######
###                 Calculate lost income estimates
###                                                                     #######
###############################################################################

## Loop through analysis and sensitivity analyses
for (version in c('reg', 'sens1', 'sens2')) {
  
  if (version == 'reg') {
    data_dir <- copy(data_path)
  } else if (version == 'sens1') {
    data_dir <- paste0(data_path, 'FILEPATH')
  } else if (version == 'sens2') {
    data_dir <- paste0(data_path, 'FILEPATH')
  }
  
  # Read and merge on total number of informal caregivers, care by sex proportions, and attributable fractions
  caregivers <- data.table(arrow::read_feather(paste0(data_dir, 'FILEPATH')))
  caregivers <- caregivers[cause_id == cause]
  caregivers <- merge(caregivers, care_sex_prop, by = c('location_id', 'year_id', 'age_group_id', 'variable'), all.x = T)
  caregivers <- merge(caregivers, att_frac, by = 'cause_id', all.x = T)
  
  # Calculate draws - number of informal caregivers by sex and patient age group
  for (draw in 1:500) {
    caregivers[, paste0('total_caregivers', draw) := n_caregivers * get(paste0('care_prop', draw)) * AF_value]
    caregivers[, paste0('care_prop', draw) := NULL]
  }
  
  # If number of caregivers is under 0.5 update to 0
  caregivers[, paste0('total_caregivers', 1:500) := lapply(.SD, function(x) fifelse(x < 0.5, 0, x)), .SDcols = paste0('total_caregivers', 1:500)]
  
  # Aggregate to all patient ages (previously adult and child caregiving separate)
  caregivers <- caregivers[, lapply(.SD, sum),
                           .SDcols = paste0('total_caregivers', 1:500),
                           by = .(location_id, year_id, age_group_id, sex_id, cause_name)]
  
  # Read in draws of work variables
  dt <- data.table(arrow::read_feather(paste0(bhi_share, 'FILEPATH', cause, '.feather')))
  
  # Merge on caregivers and care hours
  dt <- merge(dt, caregivers, by = c('location_id', 'year_id', 'age_group_id', 'sex_id'), all.x = T)
  dt <- merge(dt, disease_care, by = c('location_id', 'year_id'), all.x = T)
  
  # Calculate draws - number of informal caregivers by margin and care hours
  for (draw in 1:500) {
    dt[, paste0('intensive_caregivers', draw) := get(paste0('emp_prob', draw)) * get(paste0('total_caregivers', draw))]
    dt[, paste0('extensive_caregivers', draw) := (get(paste0('emp_prob_baseline', draw)) - get(paste0('emp_prob', draw))) * get(paste0('total_caregivers', draw))]
    dt[, paste0('care_hours', draw) := get(paste0('care_hours_per_caregiver', draw)) * get(paste0('total_caregivers', draw))]
  }
  
  # Save out draws - care hours and number of informal caregivers
  temp <- copy(dt)
  temp <- temp[, c('cause_name', 'location_id', 'year_id', 'age_group_id', 'sex_id', paste0('care_hours', 1:500), paste0('total_caregivers', 1:500)), with = F]
  temp <- temp[order(cause_name, location_id, year_id, age_group_id, sex_id)]
  arrow::write_feather(temp, paste0(data_dir, 'FILEPATH', cause, '.feather'))
  rm(temp)
  
  # Remove total caregivers and care hours (not needed for further calculations)
  dt[, c(paste0('total_caregivers', 1:500), paste0('care_hours', 1:500), paste0('care_hours_per_caregiver', 1:500)) := NULL]
  
  # Merge on annual wages
  dt <- merge(dt, global_wages, by = c('location_id', 'year_id', 'age_group_id', 'sex_id'), all.x = T)
  
  # Calculate draws - lost income and lost work hours per intensive/extensive caregiver
  for (draw in 1:500) {
    
    # Lost income (extensive is annual wage)
    dt[, paste0('intensive_lost_income_per_caregiver', draw) := (1 - (get(paste0('work_hours', draw)) / (get(paste0('work_hours_baseline', draw))))) * get(paste0('annual_wage', draw))]
    setnames(dt, paste0('annual_wage', draw), paste0('extensive_lost_income_per_caregiver', draw))
    
    # Annual lost work hours (extensive is baseline work hours)
    dt[, paste0('intensive_lost_hours_per_caregiver', draw) := (get(paste0('work_hours_baseline', draw)) - get(paste0('work_hours', draw))) * 365.25]
    dt[, paste0('work_hours_baseline', draw) := get(paste0('work_hours_baseline', draw)) * 365.25]
    setnames(dt, paste0('work_hours_baseline', draw), paste0('extensive_lost_hours_per_caregiver', draw))
    
    # Drop draws of work variables
    dt[, paste0('work_hours', draw) := NULL]
    dt[, paste0('emp_prob', draw) := NULL]
    dt[, paste0('emp_prob_baseline', draw) := NULL]
    
  }
  
  # Calculate draws - lost income and lost work hours total
  for (draw in 1:500) {
    dt[, paste0('intensive_lost_income', draw) := get(paste0('intensive_lost_income_per_caregiver', draw)) * get(paste0('intensive_caregivers', draw))]
    dt[, paste0('extensive_lost_income', draw) := get(paste0('extensive_lost_income_per_caregiver', draw)) * get(paste0('extensive_caregivers', draw))]
    dt[, paste0('intensive_lost_hours', draw) := get(paste0('intensive_lost_hours_per_caregiver', draw)) * get(paste0('intensive_caregivers', draw))]
    dt[, paste0('extensive_lost_hours', draw) := get(paste0('extensive_lost_hours_per_caregiver', draw)) * get(paste0('extensive_caregivers', draw))]
    
    dt[, paste0('intensive_caregivers', draw) := NULL]
    dt[, paste0('extensive_caregivers', draw) := NULL]
  }
  
  ## Assign lost income and work hours to 0 for age group 21
  dt[age_group_id == 21, c(paste0('intensive_lost_income', 1:500), paste0('extensive_lost_income', 1:500), paste0('intensive_lost_hours', 1:500), paste0('extensive_lost_hours', 1:500)) := 0]
  
  # Save out draws - lost income and lost work hours
  temp <- copy(dt)
  temp <- temp[, c('cause_name', 'location_id', 'year_id', 'age_group_id', 'sex_id', paste0('intensive_lost_income', 1:500), paste0('extensive_lost_income', 1:500)), with = F]
  temp <- temp[order(cause_name, location_id, year_id, age_group_id, sex_id)]
  arrow::write_feather(temp, paste0(data_dir, 'FILEPATH', cause, '.feather'))
  rm(temp)
  
  dt <- dt[, c('cause_name', 'location_id', 'year_id', 'age_group_id', 'sex_id', paste0('intensive_lost_hours', 1:500), paste0('extensive_lost_hours', 1:500)), with = F]
  dt <- dt[order(cause_name, location_id, year_id, age_group_id, sex_id)]
  arrow::write_feather(dt, paste0(data_dir, 'FILEPATH', cause, '.feather'))
  rm(dt)
  
}

## End of Script ##