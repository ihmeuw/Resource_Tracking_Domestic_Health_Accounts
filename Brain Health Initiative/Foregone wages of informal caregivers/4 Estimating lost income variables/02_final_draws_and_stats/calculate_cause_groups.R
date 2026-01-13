#### #----#                    Docstring                    #----# ####
#' Title:    calculate_cause_groups.R
#' Project:  IHME Brain Health Initiative
#' Purpose : Function is called from within a loop to calculate draws
#'           and estimates of lost income and lost work hours for the 
#'           cause group aggregates from cause-specific estimates.
#'           Data gets saved to cause specific file in BHI share drive
#'     
#' Author: USERNAME
#' Date: 03/11/2025
#' Last Updated: 09/30/2025
#---------------------------------------------------------------------#

################### #----# ENVIRONMENT SETUP #----# ###################
cat(paste0(" Environment Setup \n"))
pacman::p_load(data.table, dplyr, arrow, stringr)

#----# Set up directory roots #----#
rm(list=ls())
if (!exists("code_repo"))  {
  code_repo <- unname(ifelse(Sys.info()['sysname'] == "Windows", "ROOT", paste0("ROOT", Sys.info()['user'][1], "ROOT")))
}

## Source functions
source('FILEPATH')
source('FILEPATH')

#----# Local CONSTANTS #----#
cat('starting program \n')
version <- commandArgs(TRUE)
cat('this is the cause ID ', version)

## Variable prep
currentDate <- format(Sys.time(), "%Y%m%d")
gbd_release_id <- ID
years <- 2000:2021
locs <- get_location_metadata(location_set_id = ID, release_id = gbd_release_id)

## Paths
bhi_share <- 'FILEPATH'
data_path <- paste0(bhi_share, 'FILEPATH')

## Causes
causes <- get_cause_metadata(cause_set_id = ID, release_id = gbd_release_id)
bhi_causes <- causes[parent_id %in% c(542, 558, 561) | acause %in% c('cvd_stroke', 'encephalitis', 'inj_suicide', 'meningitis', 'mental_alcohol', 'neo_brain')]
bhi_causes <- bhi_causes[!cause_id %in% c(557, 563:566)]
mental_causes <- bhi_causes[parent_id %in% c(558, 561) | acause %in% c('inj_suicide', 'mental_alcohol')]
neuro_causes <- bhi_causes[parent_id == 542 | acause %in% c('cvd_stroke', 'encephalitis', 'meningitis', 'neo_brain')]

#---------------------------------------------------------------------#

## Update data path for sensitivity analyses
if (version == 'sens1') {
  data_path <- paste0(data_path, 'FILEPATH')
} else if (version == 'sens2') {
  data_path <- paste0(data_path, 'FILEPATH')
}

## Draw cols
draw_cols <- c(paste0('intensive_lost_income', 1:500),
               paste0('extensive_lost_income', 1:500),
               paste0('intensive_lost_hours', 1:500),
               paste0('extensive_lost_hours', 1:500),
               paste0('care_hours', 1:500),
               paste0('total_caregivers', 1:500))


###############################################################################
###                                                                     #######
###                   Aggregate mental causes
###                                                                     #######
###############################################################################

## Loop through and aggregate by cause
mental_dt <- data.table()
for (cause in c(mental_causes$cause_id, 9999)) {
  
  ## Read in and merge cause-specific draws
  dt1 <- data.table(arrow::read_feather(paste0(data_path, 'FILEPATH', cause, '.feather')))
  dt2 <- data.table(arrow::read_feather(paste0(data_path, 'FILEPATH', cause, '.feather')))
  dt <- merge(dt1, dt2, by = c('cause_name', 'location_id', 'year_id', 'age_group_id', 'sex_id'), all.x = T)
  rm(dt1, dt2)
  dt3 <- data.table(arrow::read_feather(paste0(data_path, 'FILEPATH', cause, '.feather')))
  dt <- merge(dt, dt3, by = c('cause_name', 'location_id', 'year_id', 'age_group_id', 'sex_id'), all.x = T)
  rm(dt3)
  
  ## Aggregate within loop
  dt[, cause_name := 'Mental causes']
  mental_dt <- rbind(mental_dt, dt)
  rm(dt)
  mental_dt <- mental_dt[, lapply(.SD, sum),
                         .SDcols = draw_cols,
                         by = .(cause_name, location_id, year_id, age_group_id, sex_id)]
  
}

## Save draws - mental health causes
temp <- copy(mental_dt)
temp <- temp[, c('cause_name', 'location_id', 'year_id', 'age_group_id', 'sex_id', draw_cols[1:1000]), with = F]
arrow::write_feather(temp, paste0(data_path, 'FILEPATH'))

temp <- copy(mental_dt)
temp <- temp[, c('cause_name', 'location_id', 'year_id', 'age_group_id', 'sex_id', draw_cols[1001:2000]), with = F]
arrow::write_feather(temp, paste0(data_path, 'FILEPATH'))

mental_dt <- mental_dt[, c('cause_name', 'location_id', 'year_id', 'age_group_id', 'sex_id', draw_cols[2001:3000]), with = F]
arrow::write_feather(mental_dt, paste0(data_path, 'FILEPATH'))
rm(temp, mental_dt)

###############################################################################
###                                                                     #######
###                   Aggregate neurological causes
###                                                                     #######
###############################################################################

## Loop through and aggregate by cause
neuro_dt <- data.table()
for (cause in neuro_causes$cause_id) {
  
  ## Read in and merge cause-specific draws
  dt1 <- data.table(arrow::read_feather(paste0(data_path, 'FILEPATH', cause, '.feather')))
  dt2 <- data.table(arrow::read_feather(paste0(data_path, 'FILEPATH', cause, '.feather')))
  dt <- merge(dt1, dt2, by = c('cause_name', 'location_id', 'year_id', 'age_group_id', 'sex_id'), all.x = T)
  rm(dt1, dt2)
  dt3 <- data.table(arrow::read_feather(paste0(data_path, 'FILEPATH', cause, '.feather')))
  dt <- merge(dt, dt3, by = c('cause_name', 'location_id', 'year_id', 'age_group_id', 'sex_id'), all.x = T)
  rm(dt3)
  
  ## Aggregate within loop
  dt[, cause_name := 'Neurological causes']
  neuro_dt <- rbind(neuro_dt, dt)
  rm(dt)
  neuro_dt <- neuro_dt[, lapply(.SD, sum),
                       .SDcols = draw_cols,
                       by = .(cause_name, location_id, year_id, age_group_id, sex_id)]
  
}

## Save draws - neurological causes
temp <- copy(neuro_dt)
temp <- temp[, c('cause_name', 'location_id', 'year_id', 'age_group_id', 'sex_id', draw_cols[1:1000]), with = F]
arrow::write_feather(temp, paste0(data_path, 'FILEPATH'))

temp <- copy(neuro_dt)
temp <- temp[, c('cause_name', 'location_id', 'year_id', 'age_group_id', 'sex_id', draw_cols[1001:2000]), with = F]
arrow::write_feather(temp, paste0(data_path, 'FILEPATH'))

temp <- copy(neuro_dt)
temp <- temp[, c('cause_name', 'location_id', 'year_id', 'age_group_id', 'sex_id', draw_cols[2001:3000]), with = F]
arrow::write_feather(temp, paste0(data_path, 'FILEPATH'))
rm(temp)

###############################################################################
###                                                                     #######
###                   Aggregate all cause
###                                                                     #######
###############################################################################

## Read in and merge mental draws
dt1 <- data.table(arrow::read_feather(paste0(data_path, 'FILEPATH')))
dt2 <- data.table(arrow::read_feather(paste0(data_path, 'FILEPATH')))
mental_dt <- merge(dt1, dt2, by = c('cause_name', 'location_id', 'year_id', 'age_group_id', 'sex_id'), all.x = T)
rm(dt1, dt2)
dt3 <- data.table(arrow::read_feather(paste0(data_path, 'FILEPATH')))
mental_dt <- merge(mental_dt, dt3, by = c('cause_name', 'location_id', 'year_id', 'age_group_id', 'sex_id'), all.x = T)
rm(dt3)

## Append mental and neuro draws
all_cause_dt <- rbind(neuro_dt, mental_dt)
rm(neuro_dt, mental_dt)

## Aggregate
all_cause_dt[, cause_name := 'All causes']
all_cause_dt <- all_cause_dt[, lapply(.SD, sum),
                             .SDcols = draw_cols,
                             by = .(cause_name, location_id, year_id, age_group_id, sex_id)]

## Save draws - all BHI causes
temp <- copy(all_cause_dt)
temp <- temp[, c('cause_name', 'location_id', 'year_id', 'age_group_id', 'sex_id', draw_cols[1:1000]), with = F]
arrow::write_feather(temp, paste0(data_path, 'FILEPATH'))

temp <- copy(all_cause_dt)
temp <- temp[, c('cause_name', 'location_id', 'year_id', 'age_group_id', 'sex_id', draw_cols[1001:2000]), with = F]
arrow::write_feather(temp, paste0(data_path, 'FILEPATH'))

all_cause_dt <- all_cause_dt[, c('cause_name', 'location_id', 'year_id', 'age_group_id', 'sex_id', draw_cols[2001:3000]), with = F]
arrow::write_feather(all_cause_dt, paste0(data_path, 'FILEPATH'))

## End of Script ##