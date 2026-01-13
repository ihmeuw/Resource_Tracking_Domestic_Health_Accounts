#### #----#                    Docstring                    #----# ####
#' Title:    02b_MTUS_custom_covariates.R
#' Project:  IHME Brain Health Initiative
#' Purpose: Create custom covariate file for ST-GPR
#'     
#' Author: USERNAME
#' Date: 02/10/2025
#' Last Updated: 
#---------------------------------------------------------------------#

pacman::p_load(data.table, dplyr, cli, plotrix, xlsx, writexl)
rm(list = ls())

################### #----# ENVIRONMENT SETUP #----# ###################
cat(paste0(" Environment Setup \n"))

#----# Set up directory roots #----#
rm(list=ls())
if (!exists("code_repo"))  {
  code_repo <- unname(ifelse(Sys.info()['sysname'] == "Windows", "ROOT", paste0("ROOT", Sys.info()['user'][1], "ROOT")))
}

## Functions
source('FILEPATH')
source('FILEPATH')
source('FILEPATH')
source('FILEPATH')

## Parameters
date <- gsub('-', '_', Sys.Date())
gbd_release_id <- ID
years <- 2000:2026

## Paths
bhi_share <- paste0('FILEPATH')
data_path <- paste0(bhi_share, 'FILEPATH')

#---------------------------------------------------------------------#

## Pull all country and subnational locations from GBD
locs <- get_location_metadata(location_set_id = ID, release_id = gbd_release_id)

## Pull in all required ages from GBD
ages <- get_age_metadata(age_group_set_id = ID, release_id = gbd_release_id)

## Make empty datasets
final <- CJ(location_id = locs[level >= 3]$location_id,
            year_id = years,
            sex_id = 1:2,
            age_group_id = c(8:20, 30))

national <- CJ(location_id = locs[level == 3]$location_id,
               year_id = years,
               sex_id = 1:2,
               age_group_id = c(8:20, 30))

## Merge ihme loc id on to deal with sub nationals
national <- merge(national, locs[, .(location_id, ihme_loc_id)], by = 'location_id', all.x = T)
final <- merge(final, locs[, .(location_id, ihme_loc_id)], by = 'location_id', all.x = T)
final[, country_ISO3 := substr(ihme_loc_id, 1, 3)]

##-----------------------------------------
## Age-specific proportions of population 
##-----------------------------------------

population <- get_population(location_id = locs[level == 3]$location_id,
                             year_id = years,
                             sex_id = 3,
                             age_group_id = c(22, 1, 6:8, 154),
                             release_id = ID)
population <- dcast.data.table(population, location_id + year_id ~ age_group_id, value.var = 'population')

population[, cv_proportion_under_10 := (`1` + `6`) / `22`]
population[, cv_proportion_under_15 := (`1` + `6` + `7`) / `22`]
population[, cv_proportion_under_20 := (`1` + `6` + `7` + `8`) / `22`]
population[, cv_proportion_65_plus := `154` / `22`]

temp <- copy(population[year_id == 2024])
temp[, year_id := 2025]
population <- rbind(population, temp)
temp[, year_id := 2026]
population <- rbind(population, temp)

population <- population[, .(location_id, year_id, cv_proportion_under_10, cv_proportion_under_15, cv_proportion_under_20, cv_proportion_65_plus)]

##---------------------
## IHME Labor force participation
##---------------------

lfp <- fread('FILEPATH')
lfp <- lfp[location_id %in% locs[level == 3]$location_id & year_id %in% years]
lfp <- lfp[, .(location_id, year_id, sex_id, age_group_id, lfp = val)]
lfp <- dcast.data.table(lfp, location_id + year_id + sex_id ~ age_group_id, value.var = 'lfp')
lfp[, `19` := 0.50 * `18`]
lfp[, `20` := 0.25 * `18`]
lfp[, `30` := 0]
lfp <- melt.data.table(lfp,
                       id.vars = c('location_id', 'year_id', 'sex_id'),
                       variable.name = 'age_group_id',
                       variable.factor = F,
                       value.name = 'cv_lfp')
lfp[, age_group_id := as.numeric(age_group_id)]

temp <- copy(lfp[year_id == 2024])
temp[, year_id := 2025]
lfp <- rbind(lfp, temp)
temp[, year_id := 2026]
lfp <- rbind(lfp, temp)
rm(temp)

lfp <- merge(lfp, locs[, .(location_id, ihme_loc_id)], by = 'location_id', all.x = T)


##-----------------------------------------
## Country-specific relative education (ages 15-79)
##-----------------------------------------

education <- get_covariate_estimates(covariate_id = 33,
                                     location_id = locs[level == 3]$location_id,
                                     year_id = years,
                                     sex_id = 1:2,
                                     age_group_id = c(8:20, 30:32, 235),
                                     release_id = ID)
education_pop <- get_population(location_id = locs[level == 3]$location_id,
                                year_id = years,
                                sex_id = 1:2,
                                age_group_id = c(8:20, 30:32, 235),
                                release_id = ID)
education <- merge(education, education_pop, by = c('location_id', 'year_id', 'age_group_id', 'sex_id'), all.x = T)

## Update age group 30 to be 80+
education[, mean_value := mean_value * population]
education[age_group_id >= 30, age_group_id := 30]
education <- education[, .(mean_value = sum(mean_value),
                           population = sum(population)),
                       by = .(location_id, year_id, age_group_id, sex_id)]
education[, mean_value := mean_value / population]

temp <- copy(education)
temp[, mean_value := mean_value * population]
temp <- temp[, .(mean_national = sum(mean_value),
                 population = sum(population)),
             by = .(location_id, year_id)]
temp[, mean_national := mean_national / population]
temp[, population := NULL]

education <- merge(education, temp, by = c('location_id', 'year_id'), all.x = T)
education[, cv_relative_education := mean_value / mean_national]

education <- merge(education, locs[, .(location_id, ihme_loc_id)], by = 'location_id', all.x = T)

temp <- copy(education[year_id == 2024])
temp[, year_id := 2025]
education <- rbind(education, temp)
temp[, year_id := 2026]
education <- rbind(education, temp)


education <- education[, .(ihme_loc_id, location_id, year_id, age_group_id, sex_id, cv_relative_education)]


##-----------------------------------------
## UN care hours
##-----------------------------------------

un_care <- fread(paste0(bhi_share, 'FILEPATH'))
un_care <- merge(un_care, locs[, .(location_id, ihme_loc_id)], by = 'location_id', all.x = T)
temp <- copy(un_care[age_group_id == 20])
temp[, age_group_id := 30]
un_care <- rbind(un_care, temp)

temp <- copy(un_care[year_id == 2024])
temp[, year_id := 2025]
un_care <- rbind(un_care, temp)
temp[, year_id := 2026]
un_care <- rbind(un_care, temp)

un_care <- un_care[, .(ihme_loc_id, year_id, age_group_id, sex_id, cv_un_care = mean)]


##---------------------
## Merge covariates
##---------------------

national <- merge(national, population, by = c('location_id', 'year_id'), all.x = T)
national <- merge(national, un_care, by = c('ihme_loc_id', 'year_id', 'age_group_id', 'sex_id'), all.x = T)
national <- merge(national, lfp, by = c('location_id', 'ihme_loc_id', 'year_id', 'age_group_id', 'sex_id'), all.x = T)
national <- merge(national, education, by = c('location_id', 'ihme_loc_id', 'year_id', 'age_group_id', 'sex_id'), all.x = T)
nrow(national[is.na(cv_proportion_65_plus) | is.na(cv_un_care) | is.na(cv_lfp) | is.na(cv_relative_education)])

national[, location_id := NULL]
setnames(national, 'ihme_loc_id', 'country_ISO3')

final <- merge(final, national, by = c('country_ISO3', 'year_id', 'sex_id', 'age_group_id'), all.x = T)
nrow(final[is.na(cv_proportion_65_plus) | is.na(cv_un_care) | is.na(cv_lfp) | is.na(cv_relative_education)])

## Write out file
fwrite(final, paste0(data_path, 'FILEPATH'))


## End of Script ##