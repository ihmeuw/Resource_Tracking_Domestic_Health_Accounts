################################################
#' @author USERNAME
#' 
#' @description Output spending per population at risk
#'              estimates logarithmic scale for SFMA
################################################
pacman::p_load(data.table, dplyr, reticulate, gtools, glmnet, ggplot2, parallel, lme4, AICcmodavg, readxl)
rm(list = ls())

cds_repo <- "ADDRESS"
source('FILEPATH')
source('FILEPATH')
source("FILEPATH")
source("FILEPATH")
source("FILEPATH")
source("FILEPATH")
source(paste0(cds_repo, "FILPATH"))

malaria_country_list <- fread('FILEPATH')

## *************************************************

## Fill in with malaria versions
malaria_version_total <- "NUMBER"
malaria_version_disaggregated <- "NUMBER"

## *************************************************

## Read in all financing source data
ghes <- fread(paste0('FILEPATH'))
ppp <- fread(paste0('FILEPATH'))
oop <- fread(paste0('FILEPATH'))

dah <- fread('FILEPATH')
dah <- dah[!program_area %in% c('fs_mal_dah', 'mal_dah_21')]
dah <- dah[, .(value = sum(value)),
           by = .(ihme_loc_id, year_id)]
dah <- get_location_id(dah)
dah[, ihme_loc_id := NULL]

## combine financing sources
dt <- merge(ghes, ppp, by = c('location_id', 'year_id', 'variable'), all.x = T)
dt <- merge(dt, oop, by = c('location_id', 'year_id', 'variable'), all.x = T)
dt <- merge(dt, dah, by = c('location_id', 'year_id'), all.x = T)
setnames(dt, 'value', 'mal_dah')

## sum to get total spending
dt[is.na(mal_dah), mal_dah := 0]
dt[, mal_the := mal_ghes + mal_ppp + mal_oop + mal_dah]
dt <- dt[, .(mal_the = mean(mal_the),
             mal_dah = mean(mal_dah),
             mal_ghes = mean(mal_ghes),
             mal_oop = mean(mal_oop)),
         by = .(location_id, year_id)]

# PAR - Population At Risk numbers from MAP
par <- data.table(
  read_excel(paste0("FILEPATH")))
all_locs <- get_location_metadata(location_set_id = 1, release_id = 9)[level == 3]
par <- merge(par, all_locs[, .(location_name, location_id, ihme_loc_id)], by = 'location_name', all.x = TRUE)
par[location_name %like% "Turkey", `:=`(location_id = 155, ihme_loc_id = 'TUR')]
par[location_name %like% "Côte d’Ivoire", `:=`(location_id = 205, ihme_loc_id = 'CIV')]
par[is.na(location_id)]
par <- par[!is.na(location_id)]

## merge PAR onto estimates
dt <- merge(dt, par, by = c('location_id', 'year_id'), all.x = T)

## vector of columns to update
to_update <- c('mal_the', 'mal_dah', 'mal_ghes', 'mal_oop')

## calculate spending per person at risk
dt[, c(to_update) := .SD / population_at_risk, .SDcols = to_update]

## calculate 10 year averages
dt[, year_group := ifelse(year_id %in% 2011:2020, '2011-2020',
                          ifelse(year_id %in% 2001:2010, '2001-2010', NA))]

## calculate spending per person at risk
dt[, c(to_update) := log(.SD), .SDcols = c(to_update)]

temp <- copy(dt)
temp <- temp[, .(mal_the_10_year = mean(mal_the)),
             by = .(location_id, location_name, ihme_loc_id, year_group)]
dt <- merge(dt, temp, by = c('location_id', 'location_name', 'ihme_loc_id', 'year_group'))

## keep relevant columns
dt <- dt[, .(location_id, year_id, year_group, mal_the, mal_dah, mal_ghes, mal_oop, mal_the_10_year, population_at_risk)]

## Disaggregated malaria spending
###-----------------------------------

## Government disaggregated estimates
ghes_disagg <- fread(paste0('FILEPATH'))
ghes_disagg <- ghes_disagg[, .(value = mean(value)),
                           by = .(location_id, year_id, value_code)]

## Calculate government proportions
ghes_disagg <- ghes_disagg[value_code %in% c('anti-malarial_medicines', 'insecticide_&_spraying_materials', 'itns_&_pbos')]
ghes_disagg <- dcast.data.table(ghes_disagg, location_id + year_id ~ value_code, value.var = 'value')
ghes_disagg[, prevention := `insecticide_&_spraying_materials` + `itns_&_pbos`]
ghes_disagg[, `insecticide_&_spraying_materials` := NULL]
ghes_disagg[, `itns_&_pbos` := NULL]

## DAH disaggregated estimates
dah <- fread('FILEPATH')
dah <- dah[!program_area %in% c('fs_mal_dah')]
dah <- get_location_id(dah)
dah <- dah[location_id %in% malaria_country_list$location_id]
dah[, ihme_loc_id := NULL]

dah.full <- data.table(expand.grid(location_id = unique(malaria_country_list$location_id), year_id = 2000:2020, program_area = unique(dah$program_area)))
dah <- merge(dah.full, dah, by = c('location_id', 'year_id', 'program_area'), all.x = T)
dah[is.na(value), value := 0]
dah <- dah[program_area %in% c('mal_treat_dah_21', 'mal_amr_dah_21', 'mal_con_irs_dah_21', 'mal_con_nets_dah_21', 'mal_con_oth_dah_21')]

## Calculate treatment
dah <- dcast.data.table(dah, location_id + year_id ~ program_area, value.var = 'value')
dah[, mal_treat_dah_21 := mal_treat_dah_21 + mal_amr_dah_21]
dah[, mal_amr_dah_21 := NULL]

## Calculate prevention
dah[, mal_prevention_dah_21 := mal_con_irs_dah_21 + mal_con_nets_dah_21 + mal_con_oth_dah_21]
dah[, mal_con_irs_dah_21 := NULL]
dah[, mal_con_nets_dah_21 := NULL]
dah[, mal_con_oth_dah_21 := NULL]


dah <- melt.data.table(dah, id.vars = c('location_id', 'year_id'),
                       variable.factor = F)
dah[, variable := gsub('mal_', '', variable)]
dah[, variable := gsub('_dah_21', '', variable)]
dah <- dcast.data.table(dah, location_id + year_id ~ variable, value.var = 'value')
setnames(ghes_disagg,
         c('anti-malarial_medicines'),
         c('treat'))

## Calculate total disaggregated values
dah.melt <- melt.data.table(dah, id.vars = c('location_id', 'year_id'), variable.factor = F)
ghes_disagg.melt <- melt.data.table(ghes_disagg, id.vars = c('location_id', 'year_id'), variable.factor = F)

disagg <- merge(dah.melt, ghes_disagg.melt, by = c('location_id', 'year_id', 'variable'))
disagg[, value := value.x + value.y]
disagg <- dcast.data.table(disagg, location_id + year_id ~ variable, value.var = 'value')
setnames(disagg, c('prevention', 'treat'), c('mal_prevention', 'mal_treat'))


## Merge spending datasets
dt <- merge(dt, disagg, by = c('location_id', 'year_id'))

## Calculate focus area spending per PAR
dt[, mal_prevention := log(mal_prevention / population_at_risk)]
dt[, mal_treat := log(mal_treat / population_at_risk)]

## write out dataset
fwrite(dt, 'FILEPATH')
fwrite(unique(dt[year_id %in% c(2010, 2020)]), 'FILEPATH')

## END ---------------------------------------------------------------------------------