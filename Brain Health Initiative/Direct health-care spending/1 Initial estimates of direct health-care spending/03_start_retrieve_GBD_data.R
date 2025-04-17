#### #----#                    Docstring                    #----# ####
#' Title:    03_start_retrieve_GBD_data.R
#' Project:  Resource Tracking - Brain Health Initiative (BHI)
#' Purpose : Submits requests to cluster to pull outputs from GBD
#'           for the DEX specific causes. Pulls population and fertility
#'           estimates from GBD.
#'     
#' Author: USERNAME
#' Date: 2023-02-28
#---------------------------------------------------------------------#

#----# Set up directory roots #----#
rm(list=ls())
if (!exists("code_repo"))  {
  code_repo <- unname(ifelse(Sys.info()['sysname'] == "Windows", "ADDRESS", paste0("ADDRESS", Sys.info()['user'][1], "ADDRESS")))
}

## Source functions
source(paste0(code_repo, 'FILEPATH'))
source(paste0(dah.roots$k, "FILEPATH"))
source(paste0(dah.roots$k, "FILEPATH"))
source(paste0(dah.roots$k, "FILEPATH"))
source(paste0(dah.roots$k, "FILEPATH"))
source(paste0(dah.roots$k, "FILEPATH"))

#----# Local CONSTANTS #----#
## Variable prep
data_yr <- 2019
currentDate <- format(Sys.time(), "%Y%m%d")
gbd_release_id <- 16
years <- 2000:data_yr

## Paths
bhi_share <- paste0('FILEPATH')
raw_data_path <- paste0(bhi_share, 'FILEPATH')
int_data_path <- paste0(bhi_share, 'FILEPATH')
fin_data_path <- paste0(bhi_share, 'FILEPATH')

## variables
user <- Sys.info()[7]
report_yr <- dah.roots$report_year
project <- "VARIABLE" 
output_path <- paste0(dah.roots$h, 'FILEPATH')
queue <- "VARIABLE"
job_threads <- 1
job_mem <- 150
runtime <- "03:00:00"

## Age group id vector
age_group_ids <- c(2,     # 0 to 6 days
                   3,     # 7 to 27 days
                   6,     # 5 to 9 years
                   7,     # 10 to 14 years
                   8,     # 15 to 19 years
                   9,     # 20 to 24 years
                   10,    # 25 to 29 years
                   11,    # 30 to 34 years
                   12,    # 35 to 39 years
                   13,    # 40 to 44 years
                   14,    # 45 to 49 years
                   15,    # 50 to 54 years
                   16,    # 55 to 59 years
                   17,    # 60 to 64 years
                   18,    # 65 to 69 years
                   19,    # 70 to 74 years
                   20,    # 75 to 79 years
                   30,    # 80 to 84 years
                   31,    # 85 to 89 years
                   32,    # 90 to 95 years
                   34,    # 2 to 4 years
                   235,   # 95+ years
                   238,   # 12 to 23 months
                   388,   # 1 to 5 months
                   389)   # 6 to 11 months


####----# Get DEX cause metadata #----####
cat('Get DEX cause metadata\n')
dex_cause_dt <- fread(paste0(raw_data_path, 'FILEPATH'))

## Get unique groups for the DEX causes
dex_acause_groups <- unique(dex_cause_dt$`acause group`)
dex_acause_groups <- dex_acause_groups[dex_acause_groups %ni% 'rei']


## Launch jobs
## Loops over dex cause groups
for (group in dex_acause_groups) {
  cat('submitting job for group: ', group)
  gbd_cause_ids <- unique(dex_cause_dt[`acause group` == group]$`GBD cause id`)
  cat('\n acause ids: ', gbd_cause_ids, '\n')
  mysub <- paste0("FILEPATH")
  system(mysub)
}

## Launch jobs
## Loops over dex cause groups
cat('Get DEX cause metadata\n')
dex_rei_dt <- fread(paste0(raw_data_path, 'FILEPATH'))
# get unique groups for the DEX causes
dex_rei_groups <- unique(dex_rei_dt[include == 1]$group)

for (rf in c('rf_tobacco', 'rf_hyperlipidemia', 'rf_hypertension', 'rf_obesity', 'cvd_hf')) {
  cat('submitting job for rf: ', rf)
  for (group in dex_rei_groups) {
    cat('\t submitting job for group: ', group, '\n')
    mysub <- paste0("FILEPATH")
    system(mysub)
  }
}

####----# Get location, population, fertility data #----####
cat('Get location, population, and fertility data\n')

## Pull location metadata 
location_metadata <- get_location_metadata(location_set_id = 35,
                                           release_id = gbd_release_id)

## Subset to 204 countries and territories
locs <- copy(location_metadata)
locs <- locs[level == 3]$location_id

## Pull age metadata
age_metadata <- get_age_metadata(release_id = gbd_release_id)


cat('\n Pulling population estimates \n')
## Pull population estimates from GBD
pops <- get_population(sex_id = c(1, 2),
                       year_id = years,
                       location_id = locs,
                       age_group_id = age_group_ids,
                       release_id = gbd_release_id)

## Merge on location id names and age group id names and sex id names
pops <- merge(pops, location_metadata[, .(location_id, location_name)], by = c('location_id'), all.x = T)
pops <- merge(pops, age_metadata[, .(age_group_id, age_group_name)], by = c('age_group_id'), all.x = T)
pops[, sex := ifelse(sex_id == 1, 'Male', 'Female')]

## Drop run_id column and aggregate age groups
pops[, run_id := NULL]
pops[age_group_id %in% c(31, 32, 235), `:=` (age_group_id = 160, age_group_name = '85 plus')]
pops[age_group_id %in% c(2, 3, 34, 238, 388, 389), `:=` (age_group_id = 1, age_group_name = '<5 years')]
pops <- pops[, .(population = sum(population, na.rm = T)),
             by = .(location_id, location_name, year_id, sex_id, sex, age_group_id, age_group_name)]

#------------------------------------#
cat('\n Pulling fertility estimates \n')
## Pull fertility estimates for all available age groups
fertility <- get_covariate_estimates(
  covariate_id = 13, # age-specific fertility rate
  location_id = locs,
  age_group_id = age_group_ids,
  year_id = years,
  release_id = gbd_release_id,
  sex_id = c(1, 2))

## Aggregating age groups
fertility[age_group_id %in% c(31, 32, 235), `:=` (age_group_id = 160, age_group_name = '85 plus')]
fertility[age_group_id %in% c(2, 3, 34, 238, 388, 389), `:=` (age_group_id = 1, age_group_name = '<5 years')]
fertility <- fertility[, .(mean_value = sum(mean_value, na.rm = T)),
                       by = .(location_id, year_id, sex_id, sex, age_group_id, age_group_name)]

## Merging with population estimates to calculate population of people giving birth/pregnant
fertility <- merge(fertility, pops, by = c('location_id', 'year_id', 'sex_id', 'sex', 'age_group_id', 'age_group_name'), all.x = T)
fertility[, mean_value := mean_value * population]

## Updating columns for consistency
fertility[, `:=` (acause = 'exp_well_pregnancy', cause_name = 'Pregnancy and postpartum care', cause_id = NA,
                  metric_id = 1, gbd_topic = 'covariate', gbd_topic_id = 'covariate - exp_well_pregnancy',
                  population = NULL)]

#------------------------------------#
cat('\n Creating Welfare cause datasets \n')

## Welfare causes
exp_causes_list <- c(
  'exp_donor-Donor',
  'exp_family_planning-Family planning',
  'exp_social_services-Social services',
  'exp_well_dental-Well dental',
  'exp_well_newborn-Well baby',
  'exp_well_person-Well person')

## Create welfare dataset
dt_exp <- CJ(acause = exp_causes_list,
             age_group_id = unique(pops$age_group_id),
             location_id = locs,
             year_id = years,
             sex_id = c(1, 2),
             cause_id = NA,
             metric_id = 1,
             gbd_topic = 'covariate')

## Fill in some columns
dt_exp[, cause_name := gsub('.*-', '', acause)]
dt_exp[, acause := gsub('-.*', '', acause)]
dt_exp[, gbd_topic_id := paste0('covariate - ', acause)]

## Merge with population data
dt_exp <- merge(dt_exp, pops, by = c('age_group_id', 'location_id', 'year_id', 'sex_id'), all.x = T)

## Assign # of prevalent cases as population
setnames(dt_exp, 'population', 'mean_value')
#---------------------------------------------------------------------#

#### ## Saving dataset ## ####
cat(' Saving dataset \n')

## Write out all population estimates csv
fwrite(pops, file = paste0(int_data_path, "FILEPATH"))
fwrite(fertility, file = paste0(int_data_path, "FILEPATH"))
fwrite(dt_exp, file = paste0(int_data_path, "FILEPATH"))
fwrite(rbind(fertility, dt_exp), file = paste0(int_data_path, "FILEPATH"))

## Write out all population estimates feather
arrow::write_feather(pops, paste0(int_data_path, "FILEPATH"))
arrow::write_feather(fertility, paste0(int_data_path, "FILEPATH"))
arrow::write_feather(dt_exp, paste0(int_data_path, "FILEPATH"))
arrow::write_feather(rbind(fertility, dt_exp), paste0(int_data_path, "FILEPATH"))

## End of Script##