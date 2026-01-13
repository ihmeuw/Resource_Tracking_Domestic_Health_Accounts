#### #----#                    Docstring                    #----# ####
#' Title:    calculate_stats.R
#' Project:  Brain Health Initiative
#' Purpose : Function is called from within a loop to calculate aggregates
#'           of lost income and lost work hours for each BHI cause at the
#'           draw level and final stats.
#'           Aggregates: Global, super-region, region, income group,
#'                       all age, all sex.
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
source(paste0(code_repo, 'FILEPATH'))

#----# Local CONSTANTS #----#
cat('starting program \n')
cause <- commandArgs(TRUE)
cat('this is the cause ID ', cause)

## Variable prep
currentDate <- format(Sys.time(), "%Y%m%d")
gbd_release_id <- ID
years <- 2000:2021

## Locations
locs <- get_location_metadata(location_set_id = ID, release_id = gbd_release_id)
locs <- locs[level <= 3, .(location_id, region_id, super_region_id, level)]
locs <- get_ig(locs)

## Paths
bhi_share <- 'FILEPATH'
data_path <- paste0(bhi_share, 'FILEPATH')

#---------------------------------------------------------------------#


## Draw cols
draw_cols <- c('intensive_lost_income', 'extensive_lost_income', 'intensive_lost_hours', 'extensive_lost_hours', 'care_hours', 'total_caregivers')
draw_pc_cols <- as.vector(outer(draw_cols[1:5], 1:500, function(x, y) paste0(x, '_per_caregiver', y)))
draw_cols <- as.vector(outer(draw_cols, 1:500, paste0))

## Stats calculation function
make_stats <- function(draws, variable, remove_draws = F) {
  
  cols <- paste0(variable, 1:500)
  
  draws[, paste0(variable, '_mean') := rowMeans(.SD, na.rm = T), .SDcols = c(cols)]
  draws[, paste0(variable, '_lower') := matrixStats::rowQuantiles(as.matrix(.SD), probs = 0.025), .SDcols = c(cols)]
  draws[, paste0(variable, '_upper') := matrixStats::rowQuantiles(as.matrix(.SD), probs = 0.975), .SDcols = c(cols)]
  
  if (remove_draws) {
    draws[, c(cols) := NULL]
  }
  
  return(draws)
}

## All margin stats calculation function
make_margin_stats <- function(draws_dt) {
  
  data <- copy(draws_dt)
  
  for (draw in 1:500) {
    data[, paste0('intensive_lost_income_per_caregiver', draw) := get(paste0('intensive_lost_income', draw)) / get(paste0('total_caregivers', draw))]
    data[, paste0('extensive_lost_income_per_caregiver', draw) := get(paste0('extensive_lost_income', draw)) / get(paste0('total_caregivers', draw))]
    data[, paste0('intensive_lost_hours_per_caregiver', draw) := get(paste0('intensive_lost_hours', draw)) / get(paste0('total_caregivers', draw))]
    data[, paste0('extensive_lost_hours_per_caregiver', draw) := get(paste0('extensive_lost_hours', draw)) / get(paste0('total_caregivers', draw))]
    data[, paste0('care_hours_per_caregiver', draw) := get(paste0('care_hours', draw)) / get(paste0('total_caregivers', draw))]
  }
  
  data[, (draw_pc_cols) := lapply(.SD, function(x) {
    x[is.na(x) | is.infinite(x)] <- 0
    x
  }), .SDcols = draw_pc_cols]
  
  data <- make_stats(data, 'intensive_lost_income_per_caregiver', T)
  data <- make_stats(data, 'extensive_lost_income_per_caregiver', T)
  data <- make_stats(data, 'intensive_lost_hours_per_caregiver', T)
  data <- make_stats(data, 'extensive_lost_hours_per_caregiver', T)
  
  data <- make_stats(data, 'care_hours_per_caregiver', T)
  data <- make_stats(data, 'care_hours', T)
  
  data <- make_stats(data, 'intensive_lost_income')
  data <- make_stats(data, 'extensive_lost_income')
  data <- make_stats(data, 'intensive_lost_hours')
  data <- make_stats(data, 'extensive_lost_hours')
  
  data <- make_stats(data, 'total_caregivers')
  
  for (draw in 1:500) {
    data[, paste0('aggregate_lost_income', draw) := get(paste0('intensive_lost_income', draw)) + get(paste0('extensive_lost_income', draw))]
    data[, paste0('aggregate_lost_hours', draw) := get(paste0('intensive_lost_hours', draw)) + get(paste0('extensive_lost_hours', draw))]
    data[, paste0('aggregate_lost_income_per_caregiver', draw) := get(paste0('aggregate_lost_income', draw)) / get(paste0('total_caregivers', draw))]
    data[, paste0('aggregate_lost_hours_per_caregiver', draw) := get(paste0('aggregate_lost_hours', draw)) / get(paste0('total_caregivers', draw))]
    
    data[, paste0('intensive_lost_income', draw) := NULL]
    data[, paste0('extensive_lost_income', draw) := NULL]
    data[, paste0('intensive_lost_hours', draw) := NULL]
    data[, paste0('extensive_lost_hours', draw) := NULL]
    data[, paste0('total_caregivers', draw) := NULL]
  }
  
  data[, c(paste0('aggregate_lost_income_per_caregiver', 1:500), paste0('aggregate_lost_hours_per_caregiver', 1:500)) := lapply(.SD, function(x) {
    x[is.na(x) | is.infinite(x)] <- 0
    x
  }), .SDcols = c(paste0('aggregate_lost_income_per_caregiver', 1:500), paste0('aggregate_lost_hours_per_caregiver', 1:500))]
  
  # Calculate stats for aggregate of margins
  data <- make_stats(data, 'aggregate_lost_income', T)
  data <- make_stats(data, 'aggregate_lost_hours', T)
  data <- make_stats(data, 'aggregate_lost_income_per_caregiver', T)
  data <- make_stats(data, 'aggregate_lost_hours_per_caregiver', T)
  
  return(data)
}

###############################################################################
###                                                                     #######
###                   Aggregate and update stats
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
  
  ## Read in and merge draws
  dt1 <- data.table(arrow::read_feather(paste0(data_dir, 'FILEPATH', cause, '.feather')))
  dt2 <- data.table(arrow::read_feather(paste0(data_dir, 'FILEPATH', cause, '.feather')))
  dt <- merge(dt1, dt2, by = c('cause_name', 'location_id', 'year_id', 'age_group_id', 'sex_id'), all.x = T)
  rm(dt1, dt2)
  dt3 <- data.table(arrow::read_feather(paste0(data_dir, 'FILEPATH', cause, '.feather')))
  dt3 <- dt3[, c('cause_name', 'location_id', 'year_id', 'age_group_id', 'sex_id', paste0('total_caregivers', 1:500), paste0('care_hours', 1:500)), with = F]
  dt <- merge(dt, dt3, by = c('cause_name', 'location_id', 'year_id', 'age_group_id', 'sex_id'), all.x = T)
  rm(dt3)
  
  ## Aggregate all age and all sex
  ##-------------------------------------------
  # All age
  age <- copy(dt)
  age[, age_group_id := 22]
  age <- age[, lapply(.SD, sum, na.rm = T),
             .SDcols = draw_cols,
             by = .(cause_name, location_id, year_id, age_group_id, sex_id)]
  dt <- rbind(dt, age)
  rm(age)
  
  # All sex
  sex <- copy(dt)
  sex[, sex_id := 3]
  sex <- sex[, lapply(.SD, sum, na.rm = T),
             .SDcols = draw_cols,
             by = .(cause_name, location_id, year_id, age_group_id, sex_id)]
  dt <- rbind(dt, sex)
  rm(sex)
  
  # Calculate stats - country-specific
  stats <- make_margin_stats(dt)
  
  ## Aggregate GBD location levels
  ##-------------------------------------------
  # Global
  global <- copy(dt)
  global[, location_id := 1]
  global <- global[, lapply(.SD, sum, na.rm = T),
                   .SDcols = draw_cols,
                   by = .(cause_name, location_id, year_id, age_group_id, sex_id)]
  aggregate <- copy(global)
  rm(global)
  
  # Super-regions
  sr <- copy(dt[location_id %in% locs[level == 3]$location_id])
  sr <- merge(sr, locs[, .(location_id, super_region_id)], by = 'location_id', all.x = T)
  sr[, location_id := super_region_id]
  sr <- sr[, lapply(.SD, sum, na.rm = T),
           .SDcols = draw_cols,
           by = .(cause_name, location_id, year_id, age_group_id, sex_id)]
  aggregate <- rbind(aggregate, sr)
  rm(sr)
  
  # Regions
  r <- copy(dt[location_id %in% locs[level == 3]$location_id])
  r <- merge(r, locs[, .(location_id, region_id)], by = 'location_id', all.x = T)
  r[, location_id := region_id]
  r <- r[, lapply(.SD, sum, na.rm = T),
         .SDcols = draw_cols,
         by = .(cause_name, location_id, year_id, age_group_id, sex_id)]
  aggregate <- rbind(aggregate, r)
  rm(r)
  
  # Calculate stats - GBD location levels
  aggregate <- make_margin_stats(aggregate)
  stats <- rbind(stats, aggregate)
  rm(aggregate)
  
  ## Aggregate income groups
  ##-------------------------------------------
  ig <- copy(dt[location_id %in% locs[level == 3]$location_id])
  ig <- merge(ig, locs[, .(location_id, income_group)], by = 'location_id', all.x = T)
  ig <- ig[, lapply(.SD, sum, na.rm = T),
           .SDcols = draw_cols,
           by = .(cause_name, income_group, year_id, age_group_id, sex_id)]
  ig <- make_margin_stats(ig)
  stats <- rbind(stats, ig, fill = T)
  rm(dt, ig)
  
  ## Cleaning and formatting
  ##-------------------------------------------
  
  # Update cause-specific stats data
  # Reshape long by variable
  stats <- melt.data.table(stats,
                           id.vars = c('cause_name', 'location_id', 'income_group', 'year_id', 'age_group_id', 'sex_id'),
                           variable.factor = F)
  
  # Add column for stat and adjust variable column
  stats[, stat := str_extract(variable, "[^_]+$")]
  stats[, variable := sub("_[^_]*$", "", variable)]
  
  # Reshape wide by stat
  stats <- dcast.data.table(stats,
                            cause_name + location_id + income_group + year_id + age_group_id + sex_id + variable ~ stat,
                            value.var = 'value')
  
  # Add and adjust columns
  stats[, level := ifelse(grepl('per_caregiver', variable), 'per_caregiver', 'total')]
  stats[, variable := sub('_per_caregiver', '', variable)]
  stats[!variable %in% c('total_caregivers', 'care_hours'), margin := sub("_.*", "", variable)]
  stats[!variable %in% c('total_caregivers', 'care_hours'), variable := sub("^[^_]*_", "", variable)]
  
  # Order rows
  stats <- stats[order(cause_name, location_id, income_group, year_id, sex_id, age_group_id, variable, margin, level)]
  
  # Save
  arrow::write_feather(stats[variable == 'lost_income'], paste0(data_dir, 'FILEPATH', cause, '.feather'))
  arrow::write_feather(stats[variable == 'lost_hours'], paste0(data_dir, 'FILEPATH', cause, '.feather'))
  arrow::write_feather(stats[variable %in% c('total_caregivers', 'care_hours')], paste0(data_dir, 'FILEPATH', cause, '.feather'))
  rm(stats)
  
}

## End of Script ##