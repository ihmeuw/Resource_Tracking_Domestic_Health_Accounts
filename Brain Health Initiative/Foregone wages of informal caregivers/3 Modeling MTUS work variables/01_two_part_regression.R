#### #----#                    Docstring                    #----# ####
#' Title:    01_two_part_regression.R
#' Project:  IHME Brain Health Initiative
#' Purpose : Model probability of employed status and work hours using 
#'           MTUS data and a two part regression
#'     
#' Author: USERNAME
#' Date: 05/21/2025
#' Last Updated: 09/02/2025
#---------------------------------------------------------------------#

pacman::p_load(data.table, dplyr, cli, plotrix, xlsx, writexl, lme4, MASS, ggplot2)
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

## Source functions
source('FILEPATH')
source('FILEPATH')
source("FILEPATH")
source("FILEPATH")

## Filepaths
data_path <- 'FILEPATH'
output_path <- 'FILEPATH'

## Lists
locs <- get_location_metadata(location_set_id = ID, release_id = gbd_release_id)
ages <- get_age_metadata(release_id = gbd_release_id)

#---------------------------------------------------------------------#

cat("\n\n")
cat(col_br_green("\t#####################################\n"),
    col_br_green("\t#### BEGIN Analysis file work #######\n"),
    col_br_green("\t#####################################\n\n"))

################### #----# Main #----# ###################

#### ## Processing and cleaning MTUS input data ## ####
cat(paste0(" Processing and cleaning MTUS input data \n"))
#---------------------------------------------------------------------#

## Read in MTUS input data
mtus_dt <- fread('FILEPATH')

## Keep necessary variables
mtus_dt <- mtus_dt[, .(COUNTRY, SAMPLE, HLDID, PERSID, DIARY, AGE, SEX, EMP, UNEMP, WORKHRS, PROPWT,
                       adult_care_only, phys_med_child_care, travel_for_care,
                       paid_work_main_FH, paid_work_main_NFH, paid_work_secondary)]

## Assign employed status
mtus_dt[, employed_status := ifelse(EMP == 0, 'Not in paid work',
                                    ifelse(EMP == 1, 'In paid work',
                                           ifelse(EMP == -8, 'Missing',
                                                  ifelse(EMP == -7, 'Could not be created', NA))))]

## Assign unemployment status
mtus_dt[, unemployment_status := ifelse(UNEMP == 0, 'Not unemployed',
                                        ifelse(UNEMP == 1, 'Unemployed',
                                               ifelse(UNEMP == -8, 'Missing',
                                                      ifelse(UNEMP == -9, 'Could not be created', NA))))]

## Fix work hours for missingness (not applicable, not asked, missing, or could not be created)
mtus_dt[WORKHRS < 0, WORKHRS := NA]

## Get year from sample
mtus_dt[, year_id := as.numeric(substr(SAMPLE, start = 3, 6))]

## Assign ISO3 codes
mtus_dt[, ihme_loc_id := '']
mtus_dt[COUNTRY == 'AM', ihme_loc_id := 'ARM']
mtus_dt[COUNTRY == 'AT', ihme_loc_id := 'AUT']
mtus_dt[COUNTRY == 'BG', ihme_loc_id := 'BGR']
mtus_dt[COUNTRY == 'CA', ihme_loc_id := 'CAN']
mtus_dt[COUNTRY == 'FI', ihme_loc_id := 'FIN']
mtus_dt[COUNTRY == 'FR', ihme_loc_id := 'FRA']
mtus_dt[COUNTRY == 'HU', ihme_loc_id := 'HUN']
mtus_dt[COUNTRY == 'IL', ihme_loc_id := 'ISR']
mtus_dt[COUNTRY == 'IT', ihme_loc_id := 'ITA']
mtus_dt[COUNTRY == 'KR', ihme_loc_id := 'KOR']
mtus_dt[COUNTRY == 'NL', ihme_loc_id := 'NLD']
mtus_dt[COUNTRY == 'ES', ihme_loc_id := 'ESP']
mtus_dt[COUNTRY == 'UK', ihme_loc_id := 'GBR']
mtus_dt[COUNTRY == 'US', ihme_loc_id := 'USA']
mtus_dt[COUNTRY == 'ZA', ihme_loc_id := 'ZAF']

## Subset data to relevant ages and assign GBD age groups
mtus_dt <- mtus_dt[AGE >= 15]
age_crosswalk <- copy(ages[age_group_id %in% c(8:20, 30:32, 235)])

mtus_dt[, `:=` (age_group_id = 0, age_group_name = '')]
for (row in 1:nrow(age_crosswalk)) {
  mtus_dt[AGE >= age_crosswalk$age_group_years_start[row] & AGE < age_crosswalk$age_group_years_end[row],
          `:=` (age_group_id = age_crosswalk$age_group_id[row], age_group_name = age_crosswalk$age_group_name[row])]
}

## Rename sex column
setnames(mtus_dt, 'SEX', 'sex_id')

## Drop "bad" diaries as indicated by MTUS
mtus_dt <- mtus_dt[PROPWT != 0]

## Convert time use variables from minutes to hours space
time_use_cols <- c('paid_work_main_FH', 'paid_work_main_NFH', 'paid_work_secondary',
                   'adult_care_only', 'phys_med_child_care', 'travel_for_care')
mtus_dt[, c(time_use_cols) := .SD / 60, .SDcols = time_use_cols]

## Aggregate caregiving and work hours variables
mtus_dt[, care_hours := rowSums(.SD, na.rm = T), .SDcols = c('adult_care_only', 'phys_med_child_care', 'travel_for_care')]
mtus_dt[, work_hours := rowSums(.SD, na.rm = T), .SDcols = c('paid_work_main_FH', 'paid_work_main_NFH', 'paid_work_secondary')]

## Set work hour and care hour ceilings
mtus_dt[care_hours > 16, care_hours := 16]
mtus_dt[work_hours > 16, work_hours := 16]

## Calculate caregiving age matrix
age_dt <- copy(mtus_dt)
age_dt <- age_dt[, .(adult_care_only = weighted.mean(adult_care_only, PROPWT, na.rm = T),
                     phys_med_child_care = weighted.mean(phys_med_child_care, PROPWT, na.rm = T),
                     travel_for_care = weighted.mean(travel_for_care, na.rm = T)),
                 by = .(ihme_loc_id, year_id, HLDID, PERSID, sex_id, age_group_name, age_group_id)]

# Check for multiple rows for one person and exclude (age changes and errors)
age_dt[, identifier := paste0(ihme_loc_id, '_', year_id, '_', HLDID, '_', PERSID)]
age_dt_check <- data.table(table(age_dt$identifier))[N > 1]
age_dt <- age_dt[!identifier %in% age_dt_check$V1]

## Reassign as binary variable
age_dt[, adult_care_only := ifelse(adult_care_only > 0, 1, 0)]
age_dt[, phys_med_child_care := ifelse(phys_med_child_care > 0, 1, 0)]
age_dt[, non_travel_care := ifelse(adult_care_only == 1 | phys_med_child_care == 1, 1, 0)]

## Build potential age matrices
care_cols <- c('adult_care_only', 'phys_med_child_care', 'non_travel_care')
age_dt <- age_dt[, .(adult_care_only = sum(adult_care_only),
                     phys_med_child_care = sum(phys_med_child_care),
                     non_travel_care = sum(non_travel_care),
                     N = .N),
                 by = .(ihme_loc_id, year_id, sex_id, age_group_id)]

## Aggregate 85+ age groups
temp1 <- copy(age_dt)
temp1[, c('adult_care_only', 'phys_med_child_care', 'non_travel_care') := .SD / N,
      .SDcols = c('adult_care_only', 'phys_med_child_care', 'non_travel_care')]
temp1[, N := NULL]
temp1 <- melt.data.table(temp1,
                         id.vars = c('ihme_loc_id', 'year_id', 'sex_id', 'age_group_id'),
                         variable.name = 'care_type',
                         variable.factor = F)
temp1 <- dcast.data.table(temp1, ihme_loc_id + year_id + care_type ~ age_group_id + sex_id,
                          value.var = 'value')
temp1[, other_ages := rowSums(.SD, na.rm = T), .SDcols = c(paste0(30:32, '_1'), paste0(30:32, '_2'))]
temp1[, c(paste0(30:32, '_1'), paste0(30:32, '_2')) := NULL]
to_calculate <- names(temp1)[!names(temp1) %in% c('ihme_loc_id', 'year_id', 'care_type')]
temp1[, total := rowSums(.SD, na.rm = T), .SDcols = to_calculate]
temp1[, c(to_calculate) := .SD / total, .SDcols = to_calculate]


temp1 <- melt.data.table(temp1,
                         id.vars = c('ihme_loc_id', 'year_id', 'care_type'),
                         measure.vars = to_calculate,
                         variable.name = 'age_group_id',
                         variable.factor = F)
temp1 <- dcast.data.table(temp1, ihme_loc_id + year_id + age_group_id ~ care_type, value.var = 'value')



temp2 <- temp1[, .(adult_care_only = sum(adult_care_only),
                   phys_med_child_care = sum(phys_med_child_care),
                   non_travel_care = sum(non_travel_care),
                   N = sum(N)),
               by = .(year_id, sex_id, age_group_name)]
temp3 <- temp2[, .(adult_care_only = sum(adult_care_only),
                   phys_med_child_care = sum(phys_med_child_care),
                   non_travel_care = sum(non_travel_care),
                   N = sum(N)),
               by = .(sex_id, age_group_name)]
temp4 <- temp3[, .(adult_care_only = sum(adult_care_only),
                   phys_med_child_care = sum(phys_med_child_care),
                   non_travel_care = sum(non_travel_care),
                   N = sum(N)),
               by = .(age_group_name)]

calculate_matrix <- function(dt) {
  
  non_care_cols <- names(dt)[!names(dt) %in% c(care_cols, 'N')]
  dt[, paste0(care_cols, '_prop') := .SD / N, .SDcols = care_cols]
  dcast_formula <- paste0(non_care_cols, collapse = ' + ')
  
}


## Calculate weighted mean for work and care hours per person (PROPWT is person-specific weighting)
caregiver_demographics <- copy(mtus_dt)
caregiver_demographics <- caregiver_demographics[age_group_name != '']
caregiver_demographics <- caregiver_demographics[, .(care_hours = weighted.mean(care_hours, PROPWT, na.rm = T),
                                                     work_hours = weighted.mean(work_hours, PROPWT, na.rm = T)),
                                                 by = .(ihme_loc_id, HLDID, PERSID, year_id, sex_id, age_group_name, employed_status,
                                                        unemployment_status, WORKHRS)]

## Update employed status (anyone with nonzero work hours regardless of employed status reported)
caregiver_demographics[work_hours > 0, employed_status := 'In paid work']

## Check for and remove people with more than one row of data
caregiver_demographics[, person := paste0(ihme_loc_id, ' ', year_id, ' ', HLDID, ' ', PERSID)]
check <- data.table(table(caregiver_demographics$person))
caregiver_demographics <- caregiver_demographics[!(person %in% check[N > 1]$V1)]
caregiver_demographics[, person := NULL]
rm(check)

## Make imputation method column
caregiver_demographics[, imputation_method := '']
caregiver_demographics[work_hours > 0, imputation_method := 'No imputation']
caregiver_demographics[work_hours == 0 & employed_status == 'Not in paid work', imputation_method := 'No imputation']

## Update employed status for unemployed
caregiver_demographics[work_hours == 0 & employed_status == 'In paid work' & unemployment_status == 'Unemployed',
                       `:=` (employed_status = 'Not in paid work', imputation_method = 'Unemployed status')]

## Update zero work hours people
caregiver_demographics[WORKHRS > 16 * 5, WORKHRS := 16 * 5]
caregiver_demographics[work_hours == 0 & employed_status == 'In paid work' & WORKHRS > 0,
                       `:=` (work_hours = WORKHRS / 7, imputation_method = 'WORKHRS')]

## Impute the remaining hours
imputation <- copy(caregiver_demographics[work_hours > 0])
imputation <- imputation[, .(work_hours_mean = mean(work_hours),
                             work_hours_median = median(work_hours)),
                         by = .(ihme_loc_id, year_id, sex_id, age_group_name)]

## Fix for two missing demographics
imputation_add <- copy(imputation[(ihme_loc_id == 'USA' & sex_id == 1 & age_group_name == '75 to 79' & year_id %in% c('1993', '1998')) | (ihme_loc_id == 'GBR' & sex_id == 1 & age_group_name == '75 to 79' & year_id == 2000)])
imputation_add[, year_id := 1995]
imputation_add <- imputation_add[, .(work_hours_mean = mean(work_hours_mean),
                                     work_hours_median = mean(work_hours_median)),
                                 by = .(ihme_loc_id, year_id, sex_id, age_group_name)]
imputation <- rbind(imputation, imputation_add)

caregiver_demographics <- merge(caregiver_demographics, imputation, by = c('ihme_loc_id', 'year_id', 'sex_id', 'age_group_name'), all.x = T)
caregiver_demographics[work_hours == 0 & employed_status == 'In paid work',
                       `:=` (work_hours = work_hours_median, imputation_method = 'Median work hours')]
caregiver_demographics[, `:=` (work_hours_median = NULL, work_hours_mean = NULL)]
rm(imputation, imputation_add)

## Merge on GBD location ID for future merging
caregiver_demographics <- merge(caregiver_demographics, locs[, .(ihme_loc_id, location_id)], by = 'ihme_loc_id', all.x = T)

## Pull in socio-demographic index from GBD
sdi <- get_covariate_estimates(covariate_id = ID,
                               year_id = min(caregiver_demographics$year_id):max(caregiver_demographics$year_id),
                               location_id = locs[level == 3]$location_id,
                               sex_id = 3,
                               age_group_id = 22,
                               release_id = gbd_release_id)
sdi <- sdi[, .(location_id, year_id, sdi = mean_value)]
caregiver_demographics <- merge(caregiver_demographics, sdi, by = c('location_id', 'year_id'), all.x = T)

## Pull and merge IHME labor force participation
lfp <- fread('FILEPATH')
lfp <- lfp[location_id %in% locs[level == 3]$location_id]
lfp <- lfp[, .(location_id, year_id, sex_id, age_group_name, lfp = val)]
lfp <- dcast.data.table(lfp, location_id + year_id + sex_id ~ age_group_name, value.var = 'lfp')
lfp[, `70 to 74` := 0.50 * `65 to 69`] ## Not modeled by IHME, assign to 50% of 65 to 69
lfp[, `75 to 79` := 0.25 * `65 to 69`] ## Not modeled by IHME, assign to 25% of 65 to 69
lfp <- melt.data.table(lfp,
                       id.vars = c('location_id', 'year_id', 'sex_id'),
                       variable.name = 'age_group_name',
                       variable.factor = F,
                       value.name = 'lfp')
caregiver_demographics <- merge(caregiver_demographics, lfp, by = c('location_id', 'year_id', 'age_group_name', 'sex_id'), all.x = T)

###############################################################################
###                                                                     #######
###                    Probability of employment
###                                                                     #######
###############################################################################

## Set employed status
caregiver_demographics[, work_status := ifelse(employed_status == 'In paid work', 1, 0)]

## Make sex a binary variable for modeling (male = 0, female = 1)
caregiver_demographics[, sex_id := sex_id - 1]

## Part one model formula
model_one_formula1 <- as.formula('work_status ~ sex_id*age_group_name + lfp + sdi*care_hours')
model_one_formula2 <- as.formula('work_status ~ sex_id*age_group_name + lfp*care_hours + sdi*care_hours') # Additional interaction between LFP and care hours

## Run models
model_one1 <- glm(model_one_formula1, family = binomial(link = "logit"), data = caregiver_demographics[employed_status != 'Missing'])
model_one2 <- glm(model_one_formula2, family = binomial(link = "logit"), data = caregiver_demographics[employed_status != 'Missing'])

## Model summary and fit statistics
summary(model_one1)
summary(model_one2)
performance::model_performance(model_one1)
performance::model_performance(model_one2)

###############################################################################
###                                                                     #######
###              Hours worked by employed population
###                                                                     #######
###############################################################################

## Model formula
model_two_formula <- as.formula('work_hours ~ sex_id*age_group_name + sdi*care_hours')

## Run models (difference with work hours variable and use of imputation)
model_two2 <- MASS::glm.nb(model_two_formula, data = caregiver_demographics[work_hours > 0 & imputation_method == 'No imputation'], link = "log")
model_two3 <- MASS::glm.nb(model_two_formula, data = caregiver_demographics[work_status == 1], link = "log")
caregiver_demographics[imputation_method %in% c('Median work hours', 'WORKHRS'), work_hours := 0]
model_two1 <- MASS::glm.nb(model_two_formula, data = caregiver_demographics[work_status == 1], link = "log")

## Model summary and fit statistics
summary(model_two1)
summary(model_two2)
summary(model_two3)
performance::model_performance(model_two1)
performance::model_performance(model_two2)
performance::model_performance(model_two3)

## Save out final models
saveRDS(model_one, paste0(data_path, 'FILEPATH'))
saveRDS(model_two, paste0(data_path, 'FILEPATH'))

## End of Script ##