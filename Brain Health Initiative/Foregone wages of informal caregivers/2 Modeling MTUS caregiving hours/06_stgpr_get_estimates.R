#### #----#                    Docstring                    #----# ####
#' Title:    06_stgpr_get_estimates.R
#' Project:  IHME Brain Health Initiative
#' Purpose : Pull and save out estimates from STGPR for MTUS weekly 
#' caregiving and work hours
#'     
#' Author: USERNAME
#' Last Updated: 09/15/2025
#---------------------------------------------------------------------#

pacman::p_load(data.table, dplyr, cli, plotrix, xlsx, writexl, arrow, stringr)
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

## Filepaths
data_path <- 'FILEPATH'
config_file_path <- paste0(data_path, 'FILEPATH')

## Source functions
source('FILEPATH')
source('FILEPATH')
source('FILEPATH')
source('FILEPATH')
source('FILEPATH')

## Pull location hierarchy
locs <- get_location_metadata(location_set_id = ID, release_id = gbd_release_id)

## Function to read, melt, and subset columns from ST-GPR outputs
read_stgpr_draws <- function(x){
  data <- data.table(read_parquet(x))
  year <- as.numeric(str_extract(x, "(?<=year_id=)[0-9A-Za-z]{4}"))
  sex <- as.numeric(str_extract(x, "(?<=sex_id=)[0-9A-Za-z]{1}"))
  data <- data[location_id %in% locs[level == 3 | parent_id == 80]$location_id]
  data[, `:=` (year_id = year, sex_id = sex, metric_id = NULL, point_estimate = NULL, source = NULL, `__index_level_0__` = NULL)]
  setcolorder(data, c('location_id', 'year_id', 'age_group_id', 'sex_id', paste0('draw_', 0:999)))
  return(data)
}

#---------------------------------------------------------------------#

cat("\n\n")
cat(col_br_green("\t#####################################\n"),
    col_br_green("\t#### BEGIN Analysis file work #######\n"),
    col_br_green("\t#####################################\n\n"))

################### #----# Main #----# ###################

#### ## Read in Data ## ####
cat(paste0(" Read in Data \n"))
#---------------------------------------------------------------------#

## SET model index ID
model_index <- 15:17

## Read in config and get run id
config <- fread(config_file_path)
run_id <- config[model_index_id %in% model_index]$run_id

## Read in draws
## List all files in results directory
results_dir1 <- paste0("FILEPATH", run_id[1], "FILEPATH")
files1 <- list.files(results_dir1, recursive = T, full.names = T)
results_dir2 <- paste0("FILEPATH", run_id[2], "FILEPATH")
files2 <- list.files(results_dir2, recursive = T, full.names = T)
results_dir3 <- paste0("FILEPATH", run_id[3], "FILEPATH")
files3 <- list.files(results_dir3, recursive = T, full.names = T)

## Only keep relevant years
files1 <- files1[grepl(paste0('year_id=', years, collapse = '|'), files1)]
files2 <- files2[grepl(paste0('year_id=', years, collapse = '|'), files2)]
files3 <- files3[grepl(paste0('year_id=', years, collapse = '|'), files3)]

## Read, melt, select from, and bind all results
draws1 <- rbindlist(lapply(files1, read_stgpr_draws))
draws2 <- rbindlist(lapply(files2, read_stgpr_draws))
draws3 <- rbindlist(lapply(files3, read_stgpr_draws))

## Add columns and append
draws1[, `:=` (variable = 'adult_hours', run_id = run_id[1])]
draws2[, `:=` (variable = 'child_hours', run_id = run_id[2])]
draws3[, `:=` (variable = 'all_hours', run_id = run_id[3])]
draws <- rbind(draws1, draws2, draws3)
rm(draws1, draws2, draws3)

## Update age group id 30 to 21 
draws[age_group_id == 30, age_group_id := 21]

## Save draws
arrow::write_feather(draws, paste0(data_path, 'FILEPATH'))

## Calculate mean and UI from draws
stats <- copy(draws)
stats[, mean := rowMeans(.SD, na.rm = T), .SDcols = paste0('draw_', 0:999)]
stats[, lower := matrixStats::rowQuantiles(as.matrix(.SD), probs = 0.025), .SDcols = paste0('draw_', 0:999)]
stats[, upper := matrixStats::rowQuantiles(as.matrix(.SD), probs = 0.975), .SDcols = paste0('draw_', 0:999)]

stats <- stats[, .(run_id, variable, location_id, year_id, age_group_id, sex_id, mean, lower, upper)]

## Write out point estimates
fwrite(stats, paste0(data_path, 'FILEPATH'))


#### ## Caregiver age matrix ## ####
cat(paste0(" Caregiver age matrix \n"))
#---------------------------------------------------------------------#

## draw columns
draw_cols <- paste0('draw_', 0:499)

## Subset first 500 draws and necessary years
draws <- draws[, c('location_id', 'year_id', 'age_group_id', 'sex_id', 'variable', draw_cols), with = F]
draws <- draws[year_id <= 2021]

## Get populations
population <- get_population(location_id = locs[level == 3]$location_id,
                             year_id = years,
                             age_group_id = 8:21,
                             sex_id = 1:2,
                             release_id = 16)

## Merge population on draws
draws <- merge(draws, population, by = c('location_id', 'year_id', 'age_group_id', 'sex_id'), all.x = T)

## Calculate total hours
draws[, c(draw_cols) := .SD * population, .SDcols = draw_cols]

## Aggregate care hours and population to all sex
draws <- draws[, lapply(.SD, sum),
               .SDcols = c(draw_cols, 'population'),
               by = .(location_id, year_id, age_group_id, variable)]

## Divide by aggregated population
draws[, c(draw_cols) := .SD / population, .SDcols = draw_cols]
draws[, population := NULL]

## Reshape wide by age group
draws <- dcast.data.table(melt.data.table(draws, measure.vars = draw_cols, variable.factor = F),
                          location_id + year_id + variable + variable.1 ~ age_group_id,
                          value.var = 'value')

## Calculate proportion
draws[, as.character(8:21) := .SD / rowSums(.SD), .SDcols = as.character(8:21)]

## Reshape wide by draw
draws <- dcast.data.table(melt.data.table(draws, measure.vars = as.character(8:21), variable.name = 'age_group_id', variable.factor = F),
                          location_id + year_id + age_group_id + variable ~ variable.1,
                          value.var = 'value')
draws[, age_group_id := as.numeric(age_group_id)]
setcolorder(draws, c('location_id', 'year_id', 'age_group_id', 'variable', draw_cols))
draws <- draws[order(location_id, year_id, age_group_id, variable)]
arrow::write_feather(draws, paste0(data_path, 'FILEPATH'))

## Calculate stats
draws[, mean := rowMeans(.SD, na.rm = T), .SDcols = draw_cols]
draws[, lower := matrixStats::rowQuantiles(as.matrix(.SD), probs = 0.025), .SDcols = draw_cols]
draws[, upper := matrixStats::rowQuantiles(as.matrix(.SD), probs = 0.975), .SDcols = draw_cols]
draws[, paste0('draw_', 0:499) := NULL]

fwrite(draws, paste0(data_path, 'FILEPATH'))

## End of Script ##