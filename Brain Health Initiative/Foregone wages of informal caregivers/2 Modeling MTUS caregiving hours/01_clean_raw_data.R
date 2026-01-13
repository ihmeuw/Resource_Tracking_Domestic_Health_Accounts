#### #----#                    Docstring                    #----# ####
#' Title:    01_clean_raw_mtus_data.R
#' Project:  BHI - Brain Health Initiative
#' Purpose: Read in, clean/process, and aggregate MTUS data 
#'
#' Author: USERNAME
#' Date: 02/10/2025
#---------------------------------------------------------------------#

#----# Set up directory roots #----#
rm(list=ls())
if (!exists("code_repo"))  {
  code_repo <- unname(ifelse(Sys.info()['sysname'] == "Windows", "ROOT", paste0("ROOT", Sys.info()['user'][1], "ROOT")))
}

## Source functions
source(paste0(code_repo, 'FILEPATH'))
source(paste0(code_repo, 'FILEPATH'))
source(paste0(dah.roots$k, "FILEPATH"))
source(paste0(dah.roots$k, "FILEPATH"))
source(paste0(dah.roots$k, "FILEPATH"))
source(paste0(dah.roots$k, "FILEPATH"))

#----# Local CONSTANTS #----#
## variable prep
data_yr <- 2023
gbd_release_id <- ID
years <- 1990:(data_yr)
locs <- get_location_metadata(location_set_id = ID, release_id = gbd_release_id)[level == 3]
ages <- get_age_metadata(age_group_set_id = ID, release_id = gbd_release_id)

## Paths
bhi_share <- paste0('FILEPATH')
data_path <- paste0(bhi_share, 'FILEPATH')

###-----------------------------------------------------------------
###------- Read in and process files
###-----------------------------------------------------------------

## Read in data
dt <- fread(paste0('FILEPATH'))

## Remove diaries with propwt of 0 as per MTUS
dt <- dt[PROPWT != 0]

## Get year from sample
dt[, year_id := as.numeric(substr(SAMPLE, start = 3, 6))]

## Assign ISO3 codes
dt[, ihme_loc_id := '']
dt[COUNTRY == 'AM', ihme_loc_id := 'ARM']
dt[COUNTRY == 'AT', ihme_loc_id := 'AUT']
dt[COUNTRY == 'BG', ihme_loc_id := 'BGR']
dt[COUNTRY == 'CA', ihme_loc_id := 'CAN']
dt[COUNTRY == 'FI', ihme_loc_id := 'FIN']
dt[COUNTRY == 'FR', ihme_loc_id := 'FRA']
dt[COUNTRY == 'HU', ihme_loc_id := 'HUN']
dt[COUNTRY == 'IL', ihme_loc_id := 'ISR']
dt[COUNTRY == 'IT', ihme_loc_id := 'ITA']
dt[COUNTRY == 'KR', ihme_loc_id := 'KOR']
dt[COUNTRY == 'NL', ihme_loc_id := 'NLD']
dt[COUNTRY == 'ES', ihme_loc_id := 'ESP']
dt[COUNTRY == 'UK', ihme_loc_id := 'GBR']
dt[COUNTRY == 'US', ihme_loc_id := 'USA']
dt[COUNTRY == 'ZA', ihme_loc_id := 'ZAF']

## Subset data to relevant ages and add age group ids
dt <- dt[AGE >= 15]
age_crosswalk <- copy(ages[age_group_id %in% c(8:20)])

dt[, `:=` (age_group_id = 0, age_group_name = '')]
for (row in 1:nrow(age_crosswalk)) {
  dt[AGE >= age_crosswalk$age_group_years_start[row] & AGE < age_crosswalk$age_group_years_end[row],
     `:=` (age_group_id = age_crosswalk$age_group_id[row], age_group_name = age_crosswalk$age_group_name[row])]
}
dt[AGE >= 80, `:=` (age_group_id = 30, age_group_name = '80+')]

## Rename sex column
setnames(dt, 'SEX', 'sex_id')

## Convert time use variables to hours space
time_use_cols <- c('adult_care_only', 'phys_med_child_care', 'travel_for_care')
dt[, c(time_use_cols) := .SD / 60, .SDcols = time_use_cols]

## Sum caregiving hours variables
dt[, care_hours := rowSums(.SD, na.rm = T), .SDcols = c('adult_care_only', 'phys_med_child_care', 'travel_for_care')]

## Set care hour ceiling
dt[adult_care_only > 16, adult_care_only := 16]
dt[phys_med_child_care > 16, phys_med_child_care := 16]
dt[care_hours > 16, care_hours := 16]

## Calculate values per person (weights at the individual level)
dt <- dt[, .(adult_hours = weighted.mean(adult_care_only, PROPWT, na.rm = T),
             child_hours = weighted.mean(phys_med_child_care, PROPWT, na.rm = T),
             all_hours = weighted.mean(care_hours, PROPWT, na.rm = T)),
         by = .(ihme_loc_id, year_id, HLDID, PERSID, sex_id, age_group_id)]

## Check for and remove people with more than one row of data
dt[, person := paste0(ihme_loc_id, ' ', year_id, ' ', HLDID, ' ', PERSID)]
check <- data.table(table(dt$person))
dt <- dt[!(person %in% check[N > 1]$V1)]
dt[, person := NULL]
rm(check)

## Calculate weekly hours of care and work from daily
dt[, paste0(c('adult', 'child', 'all'), '_hours') := .SD * 7, .SDcols = paste0(c('adult', 'child', 'all'), '_hours')]

## Reshape data long by variable
dt <- melt.data.table(dt,
                      id.vars = c('ihme_loc_id', 'year_id', 'HLDID', 'PERSID', 'sex_id', 'age_group_id'),
                      measure.vars = paste0(c('adult', 'child', 'all'), '_hours'),
                      variable.factor = F)

## Calculate means, variances, and sample size
dt <- dt[, .(mean = mean(value),
             variance = var(value),
             sample_size = .N),
         by = .(ihme_loc_id, year_id, sex_id, age_group_id, variable)]

## Adjust NA and zero variance values (one data point)
temp <- copy(dt[variance != 0])
temp <- temp[, .(variance_adjust = median(variance)),
             by = .(variable, sex_id, age_group_id)]
dt <- merge(dt, temp, by = c('variable', 'sex_id', 'age_group_id'), all.x = T)
dt[variance == 0, variance := variance_adjust]
dt[, variance_adjust := NULL]

## Assign NID
dt[, nid := 552] # NID for IPUMS

## Write out cleaned data
fwrite(dt, paste0(data_path, 'FILEPATH'))

## End of Script ##