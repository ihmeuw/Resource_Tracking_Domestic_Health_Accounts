#### #----#                    Docstring                    #----# ####
#' Title:    07_age_and_sex_split_estimates.R
#' Project:  Brain Health Initiative
#' Purpose : Take literature-based adjusted estimates and split by age and sex to
#' produce disaggregated estimates by age and sex.
#'     
#' Author: USERNAME
#' Date: 08/13/2024 MMDDYYYY
#---------------------------------------------------------------------#

pacman::p_load(data.table, dplyr, cli, plotrix, xlsx, writexl, stringr, arrow, matrixStats)
rm(list = ls())

################### #----# ENVIRONMENT SETUP #----# ###################
cat(paste0(" Environment Setup \n"))

#----# Set up directory roots #----#
rm(list=ls())
if (!exists("code_repo"))  {
  code_repo <- unname(ifelse(Sys.info()['sysname'] == "Windows", "ADDRESS", paste0("ADDRESS", Sys.info()['user'][1], "ADDRESS")))
}

## Parameters and functions
source(paste0(code_repo, 'FILEPATH'))
source('FILEPATH')
source('FILEPATH')
source('FILEPATH')
source(paste0(dah.roots$h, "FILEPATH"))
date <- gsub('-', '_', Sys.Date())
dex_yr <- 2019
gbd_release_id <- 16
locs <- get_location_metadata(location_set_id = 35, release_id = gbd_release_id)[level == 3]

## Income groups
income_groups <- fread('FILEPATH')
income_groups <- income_groups[YEAR == 2019]
setnames(income_groups, 'ISO3_RC', 'ihme_loc_id')
income_groups <- merge(income_groups, locs[, .(ihme_loc_id, location_id)])

## draw columns
draw_cols <- paste0('Draw_', 1:500)

## Paths
data_path <- 'FILEPATH'
int_data_path <- paste0(data_path, 'FILEPATH')
fin_data_path <- paste0(data_path, 'FILEPATH')

#---------------------------------------------------------------------#

cat("\n\n")
cat(col_br_green("\t#####################################\n"),
    col_br_green("\t#### BEGIN Analysis file work #######\n"),
    col_br_green("\t#####################################\n\n"))

################### #----# Main #----# ###################

#### ## Process pre-adjusted direct spending estimates ## ####
cat(paste0(" Process pre-adjusted direct spending estimates \n"))
#---------------------------------------------------------------------#

## Read in BHI estimates
bhi_estimates <- fread(paste0(fin_data_path, 'FILEPATH'))

## Aggregate to remove payers
toc_cols <- c('AM', 'ED', 'HH', 'IP', 'NF', 'RX')
bhi_estimates <- bhi_estimates[, lapply(.SD, sum, na.rm = T), .SDcols = c(toc_cols),
                               by = .(location_id, year_id, sex_id, age_group_name, cause_name, incident_prevalent_cases)]

## Reshape long by type of care
bhi_estimates <- melt.data.table(bhi_estimates,
                                 measure.vars = toc_cols,
                                 variable.name = 'type_of_care',
                                 variable.factor = F)

## Aggregate and merge all age/all sex estimates back onto estimates
temp <- copy(bhi_estimates)
temp <- temp[, lapply(.SD, sum, na.rm = T), .SDcols = c('value', 'incident_prevalent_cases'),
             by = .(location_id, year_id, cause_name, type_of_care)]
setnames(temp, c('value', 'incident_prevalent_cases'), c('total_value', 'total_incident_prevalent_cases'))
bhi_estimates <- merge(bhi_estimates, temp, by = c('location_id', 'year_id', 'cause_name', 'type_of_care'), all.x = T)
rm(temp)

## Calculate scalar of age-sex specific spend per case to all age/all sex spend per case
bhi_estimates[, scalar := (value / incident_prevalent_cases) / (total_value / total_incident_prevalent_cases)]
bhi_estimates[, `:=` (value = NULL, total_value = NULL, total_incident_prevalent_cases = NULL)]

## Update conduct disorder RX values
temp <- copy(bhi_estimates[cause_name == 'Conduct disorder' & type_of_care != 'RX'])
temp <- temp[, .(scalar = mean(scalar, na.rm = T),
                 type_of_care = 'RX'),
             by = .(location_id, year_id, cause_name, sex_id, age_group_name, incident_prevalent_cases)]
bhi_estimates <- bhi_estimates[!(cause_name == 'Conduct disorder' & type_of_care == 'RX')]
bhi_estimates <- rbind(bhi_estimates, temp)
rm(temp)


#### ## Process adjusted direct spending estimates ## ####
cat(paste0(" Process adjusted direct spending estimates \n"))
#---------------------------------------------------------------------#

## Read in adjusted estimates
adjusted_estimates <- fread('FILEPATH')

## Keep necessary columns
adjusted_estimates <- adjusted_estimates[, c('location_id', 'year_id', 'cause_name', 'type_of_care', draw_cols), with = F]

## Update type of care column for merge
adjusted_estimates[, type_of_care := gsub('raked_mean_', '', type_of_care)]


#### ## Merge estimates and calculate final estimates ## ####
cat(paste0(" Merge, split, and save out final estimates \n"))
#---------------------------------------------------------------------#

bhi_estimates[grepl('Attention', cause_name), cause_name := 'Attention-deficit hyperactivity disorder']
adjusted_estimates[grepl('Attention', cause_name), cause_name := 'Attention-deficit hyperactivity disorder']
for (i in unique(bhi_estimates$cause_name)) {
  
  ## print statement
  print(paste0('Splitting and saving estimates of ', tolower(i)))
  
  ## Subset data
  temp_bhi <- copy(bhi_estimates[cause_name == i])
  bhi_estimates <- bhi_estimates[cause_name != i]
  temp_cal <- copy(adjusted_estimates[cause_name == i])
  adjusted_estimates <- adjusted_estimates[cause_name != i]
  
  ## Merge
  temp_bhi <- merge(temp_bhi, temp_cal, by = c('location_id', 'year_id', 'cause_name', 'type_of_care'), all.x = T)
  rm(temp_cal)
  
  ## Calculate final estimates
  temp_bhi[, c(draw_cols) := lapply(.SD, function(x) x * scalar * incident_prevalent_cases), .SDcols = draw_cols]
  
  ## Drop extra columns
  temp_bhi[, `:=` (incident_prevalent_cases = NULL, scalar = NULL)]
  
  ## Merge on super region name and income group
  temp_bhi <- merge(temp_bhi, locs[, .(location_id, super_region_name)], by = 'location_id', all.x = T)
  temp_bhi <- merge(temp_bhi, income_groups[, .(location_id, INC_GROUP)], by = 'location_id', all.x = T)
  
  ## Save out
  fwrite(temp_bhi, paste0(fin_data_path, 'FILEPATH', tolower(i), '.csv'))
  rm(temp_bhi)
  
}

rm(bhi_estimates, adjusted_estimates)


#### ## Pull causes, aggregate mental, neuro, and all cause, and save out ## ####
cat(paste0(" Pull causes, aggregate mental, neuro, and all cause, and save out \n"))
#---------------------------------------------------------------------#

## List datasets for each type of cause
estimate_datasets <- list.files(paste0(fin_data_path, 'FILEPATH'))
estimate_datasets <- estimate_datasets[!grepl('_mental|_neurological|all_cause|consolidated_data|summary', estimate_datasets)]
mental_datasets <- estimate_datasets[grepl('anxiety|attention|autism|bipolar|conduct|depressive|eating|use disorders|mental|schizophrenia|self', estimate_datasets)]
neuro_datasets <- estimate_datasets[!estimate_datasets %in% mental_datasets]
rm(estimate_datasets)

## Pull and aggregate mental datasets
mental_dt <- data.table()
for (dataset in mental_datasets) {
  
  ## Read in dataset
  temp <- fread(paste0(fin_data_path, 'FILEPATH', dataset))
  temp[, cause_name := 'Mental']
  
  ## Bind and aggregate
  mental_dt <- rbind(mental_dt, temp)
  rm(temp)
  mental_dt <- mental_dt[, lapply(.SD, sum, na.rm = T),
                         .SDcols = draw_cols,
                         by = .(location_id, super_region_name, INC_GROUP, year_id, age_group_name, sex_id, cause_name, type_of_care)]
  
}

fwrite(mental_dt, paste0(fin_data_path, 'FILEPATH'))


## Pull and aggregate neurological datasets
neuro_dt <- data.table()
for (dataset in neuro_datasets) {
  
  ## Read in dataset
  temp <- fread(paste0(fin_data_path, 'FILEPATH', dataset))
  temp[, cause_name := 'Neurological']
  
  ## Bind and aggregate
  neuro_dt <- rbind(neuro_dt, temp)
  rm(temp)
  neuro_dt <- neuro_dt[, lapply(.SD, sum, na.rm = T),
                       .SDcols = draw_cols,
                       by = .(location_id, super_region_name, INC_GROUP, year_id, age_group_name, sex_id, cause_name, type_of_care)]
  
}

fwrite(neuro_dt, paste0(fin_data_path, 'FILEPATH'))


## Aggregate mental and neuro and save out all cause
mental_dt[, cause_name := 'All cause']
neuro_dt[, cause_name := 'All cause']
all_cause_dt <- rbind(mental_dt, neuro_dt)
rm(mental_dt, neuro_dt)
all_cause_dt <- all_cause_dt[, lapply(.SD, sum, na.rm = T),
                             .SDcols = draw_cols,
                             by = .(location_id, super_region_name, INC_GROUP, year_id, age_group_name, sex_id, cause_name, type_of_care)]
fwrite(all_cause_dt, paste0(fin_data_path, 'FILEPATH'))
rm(all_cause_dt)

## End of script ##