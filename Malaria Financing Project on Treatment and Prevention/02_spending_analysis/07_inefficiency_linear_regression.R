################################################
#' @author USERNAME
#' 
#' @description Running regressions for exemplar analyses with disaggregated government estimates
################################################
pacman::p_load(data.table, dplyr, reticulate, gtools, glmnet, ggplot2, parallel, lme4, AICcmodavg, readxl, stargazer)
rm(list = ls())

cds_repo <- "ADDRESS"
source('FILEPATH')
source('FILEPATH')
source("FILEPATH")
source("FILEPATH")
source("FILEPATH")
source("FILEPATH")
source(paste0(cds_repo, "FILEPATH"))

malaria_country_list <- fread('FILEPATH')

##----------------------------------------------
## Read in inefficiency estimates
##----------------------------------------------

filepath <- 'ADDRESS'

cf <- fread("FILEPATH")
inc <- fread("FILEPATH")

inc[, variable := 'inc_diff_10']
cf[, variable := 'cf_diff_10']

## Combine
inefficiency <- rbind(inc, cf, fill = T)
inefficiency <- inefficiency[ineff != 0]
inefficiency <- inefficiency[, .(location_id, year_id, ineff, variable)]
inefficiency <- get_region(inefficiency)

rm(inc, cf)

##----------------------------------------------
## Read in aggregated spending estimates
##----------------------------------------------

## Fill in with correct malaria versions
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
dt <- merge(ghes, ppp, by = c('location_id', 'year_id', 'variable'))
dt <- merge(dt, oop, by = c('location_id', 'year_id', 'variable'))
dt <- merge(dt, dah, by = c('location_id', 'year_id'), all.x = T)
setnames(dt, 'value', 'mal_dah')

## sum to get total spending
dt[is.na(mal_dah), mal_dah := 0]
dt[, mal_the := mal_ghes + mal_ppp + mal_oop + mal_dah]

## calculate proportions by financing source
dt <- melt.data.table(dt, id.vars = c('location_id', 'year_id', 'variable', 'mal_the'),
                      variable.factor = F)
dt[, year_group := ifelse(year_id %in% 2001:2010, '2001-2010',
                          ifelse(year_id %in% 2011:2020, '2011-2020', NA))]
dt <- dt[, .(value = sum(value),
             mal_the = sum(mal_the)),
         by = .(location_id, year_group, variable, variable.1)]

dt <- dt[, value := value / mal_the]

dt <- dt[, .(value = mean(value)),
         by = .(location_id, year_group, variable.1)]
dt <- dcast.data.table(dt, location_id + year_group ~ variable.1, value.var = 'value')

rm(ppp, oop, dah)

##----------------------------------------------
## Read in disaggregated spending estimates
##----------------------------------------------

## Government disaggregated estimates
ghes_disagg <- fread(paste0('FILEPATH'))
ghes_disagg <- ghes_disagg[, .(value = mean(value)),
                           by = .(location_id, year_id, value_code)]

## Merge on total government spending
ghes <- ghes[, .(mal_ghes = mean(mal_ghes)),
             by = .(location_id, year_id)]
ghes_disagg <- merge(ghes_disagg, ghes, by = c('location_id', 'year_id'))

## Calculate government proportions
ghes_disagg <- dcast.data.table(ghes_disagg, location_id + year_id ~ value_code, value.var = 'value')
ghes_disagg[, caiemepaopsm := `infrastructure_&_equipment` + `planning,_administration,_overheads` + `procurement_&_supply_management` + `communication_and_advocacy` + `monitoring_and_evaluation`]
ghes_disagg[, iepaopsm := `infrastructure_&_equipment` + `planning,_administration,_overheads` + `procurement_&_supply_management`]
ghes_disagg[, `infrastructure_&_equipment` := NULL]
ghes_disagg[, `planning,_administration,_overheads` := NULL]
ghes_disagg[, `procurement_&_supply_management` := NULL]
ghes_disagg[, prevention := `insecticide_&_spraying_materials` + `itns_&_pbos`]
ghes_disagg[, total_w_other := `anti-malarial_medicines` + diagnostics + `human_resources_&_technical_assistance` + caiemepaopsm + other + prevention]

## DAH disaggregated estimates
dah <- fread('FILEPATH')
dah <- dah[!program_area %in% c('fs_mal_dah', 'mal_dah_21')]
dah <- get_location_id(dah)
dah <- dah[location_id %in% malaria_country_list$location_id]
dah[, ihme_loc_id := NULL]

dah.full <- data.table(expand.grid(location_id = unique(malaria_country_list$location_id), year_id = 2000:2020, program_area = unique(dah$program_area)))
dah <- merge(dah.full, dah, by = c('location_id', 'year_id', 'program_area'), all.x = T)
dah[is.na(value), value := 0]

dah <- dcast.data.table(dah, location_id + year_id ~ program_area, value.var = 'value')
dah[, mal_treat_dah_21 := mal_treat_dah_21 + mal_amr_dah_21]
dah[, mal_amr_dah_21 := NULL]
dah[, mal_prevention_dah_21 := mal_con_irs_dah_21 + mal_con_nets_dah_21 + mal_con_oth_dah_21]
dah[, mal_hss_other_ccme_dah_21 := mal_hss_other_dah_21 + mal_comm_con_dah_21 + mal_hss_me_dah_21]
dah[, mal_total_w_all_other_dah_21 := mal_diag_dah_21 + mal_hss_hrh_dah_21 + mal_hss_other_ccme_dah_21 + mal_treat_dah_21 + mal_other_dah_21 + mal_prevention_dah_21]

dah <- melt.data.table(dah, id.vars = c('location_id', 'year_id'),
                       variable.factor = F)

dah[, variable := gsub('mal_', '', variable)]
dah[, variable := gsub('_dah_21', '', variable)]
dah <- dcast.data.table(dah, location_id + year_id ~ variable, value.var = 'value')
dah[, other := NULL]
dah[, con_oth := NULL]

setnames(dah, 
         colnames(dah)[3:length(dah)],
         c('communication_and_advocacy', 'insecticide_&_spraying_materials', 'itns_&_pbos', 'diagnostics', 'human_resources_&_technical_assistance',
           'monitoring_and_evaluation', 'iepaopsm', 'caiemepaopsm', 'prevention', 'total_no_other', 'total_w_other', 'total_w_prev_other', 'anti-malarial_medicines'))

## Calculate total disaggregated values
disagg <- rbind(ghes_disagg, dah)
disagg <- melt.data.table(disagg, id.vars = c('location_id', 'year_id'), variable.factor = F)
disagg <- disagg[, .(value = sum(value)),
                 by = .(location_id, year_id, variable)]
disagg <- dcast.data.table(disagg, location_id + year_id ~ variable, value.var = 'value')


setnames(disagg,
         colnames(disagg)[3:12],
         c('Anti-malarials', 'CAIEMEPAOPSM', 'Communication&advocacy', 'Diagnostics', 'Human resources', 'IEPAOPSM', 'IRS', 'ITNs', 'M&E', 'Prevention'))


disagg <- melt.data.table(disagg, id.vars = c('location_id', 'year_id', 'total_no_other', 'total_w_other', 'total_w_prev_other'), variable.factor = F)

##--------------------------------------------
disagg[, year_group := ifelse(year_id %in% 2001:2010, '2001-2010',
                              ifelse(year_id %in% 2011:2020, '2011-2020', NA))]
disagg[, proportion := value / total_w_other]
disagg <- disagg[, .(value = sum(value),
                     proportion = mean(proportion),
                     total_w_other = sum(total_w_other)),
                 by = .(location_id, year_group, variable)]

disagg[, value := value / total_w_other]


##--------------------------------------------

disagg <- dcast.data.table(disagg, location_id + year_group ~ variable, value.var = 'value')
disagg[, CAIEMEPAOPSM := IEPAOPSM + `Communication&advocacy` + `M&E`]

##---------------------------------------------------------
inefficiency[, year_group := ifelse(year_id == 2010, '2001-2010', '2011-2020')]

inefficiency <- merge(inefficiency, dt, by = c('location_id', 'year_group'))

inefficiency <- merge(inefficiency, disagg, by = c('location_id', 'year_group'))

other_cov <- fread('FILEPATH')
other_cov <- other_cov[, .(location_id, year_id, haqi)]
inefficiency <- merge(inefficiency, other_cov, by = c('location_id' , 'year_id'), all.x = T)

# government effectiveness
raw <- paste0("ADDRESS")
gov_eff <- setDT(read_xlsx(paste0(raw, "FILEPATH")))
gov_eff[Code == 'ZAR', Code := 'COD']
gov_eff[Code == 'TMP', Code := 'TLS']
gov_eff <- melt.data.table(gov_eff, id = c("Country/Territory", "Code"), value.name = "gov_effect_index", variable.name = "year_id")
setnames(gov_eff, c("Country/Territory", "Code"), c("location_name", "ihme_loc_id"))
gov_eff[, year_id := as.integer(as.character(year_id))]
gov_eff <- gov_eff[year_id %in% 2001:2020]
gov_eff[, year_id := ifelse(year_id %in% 2001:2010, 2010, 2020)]
gov_eff <- gov_eff[, .(gov_effect_index = mean(gov_effect_index, na.rm = T)),
                   by = .(location_name, ihme_loc_id, year_id)]

temp <- copy(gov_eff[location_name == 'Sudan' & year_id == 2010])
temp[, `:=` (location_name = 'South Sudan', ihme_loc_id = 'SSD')]
gov_eff <- gov_eff[!(location_name == 'South Sudan' & year_id == 2010)]
gov_eff <- rbind(gov_eff, temp)

# gdp
gdp <- setDT(read_feather('FILEPATH'))
gdp <- gdp[year %in% 2001:2020 & scenario == 0]

## Currency conversion
gdp <- currency_conversion(gdp,
                           col.loc = 'iso3',
                           col.value = paste0('draw_', 1:500),
                           currency = 'USD',
                           currency.year = 2022,
                           base.year = 2021,
                           base.unit = 'USD',
                           converter.version = 7)

## Calculate means from draws
gdp <- melt.data.table(gdp,
                       id.vars = c('iso3', 'year', 'scenario'),
                       variable.factor = F)
gdp[, year := ifelse(year %in% 2001:2010, 2010, 2020)]
gdp <- gdp[, .(value = mean(value, na.rm = T)),
           by = .(iso3, year, variable)]
gdp[, value := log(value)]
gdp <- gdp[, .(value = mean(value, na.rm = T)),
           by = .(iso3, year)]
setnames(gdp, c('iso3', 'year', 'value'), c('ihme_loc_id', 'year_id', 'gdp'))


gdp <- merge(gdp, gov_eff[, .(ihme_loc_id, year_id, gov_effect_index)], by = c('ihme_loc_id', 'year_id'), all.x = T)
gdp <- get_location_id(gdp)

## Merge
inefficiency <- merge(inefficiency, gdp, by = c('location_id', 'year_id'), all.x = T)


inc_ineff <- copy(inefficiency[variable == 'inc_diff_10'])
cf_ineff <- copy(inefficiency[variable == 'cf_diff_10'])

## Models
model.1 <- lm(ineff ~ mal_ghes + mal_dah + haqi + year_group, data = inc_ineff)
model.2 <- lm(ineff ~ Diagnostics + `CAIEMEPAOPSM` + `Prevention` + haqi + year_group, data = inc_ineff)
model.3 <- lm(ineff ~ mal_ghes + mal_dah + haqi + year_group, data = cf_ineff)
model.4 <- lm(ineff ~ Diagnostics + `CAIEMEPAOPSM` + `Anti-malarials` + haqi + year_group, data = cf_ineff)

## Sensitivity analysis 1
model.5 <- lm(ineff ~ mal_ghes + mal_dah + haqi + year_group, data = inc_ineff[super_region_name == 'Sub-Saharan Africa'])
model.6 <- lm(ineff ~ Diagnostics + `CAIEMEPAOPSM` + `Prevention` + haqi + year_group, data = inc_ineff[super_region_name == 'Sub-Saharan Africa'])
model.7 <- lm(ineff ~ mal_ghes + mal_dah + haqi + year_group, data = cf_ineff[super_region_name == 'Sub-Saharan Africa'])
model.8 <- lm(ineff ~ Diagnostics + `CAIEMEPAOPSM` + `Anti-malarials` + haqi + year_group, data = cf_ineff[super_region_name == 'Sub-Saharan Africa'])

## Sensitivity analysis 2
model.9 <- lm(ineff ~ mal_ghes + mal_dah + haqi + year_group + gdp + gov_effect_index, data = inc_ineff)
model.10 <- lm(ineff ~ Diagnostics + `CAIEMEPAOPSM` + `Prevention` + haqi + year_group + gdp + gov_effect_index, data = inc_ineff)
model.11 <- lm(ineff ~ mal_ghes + mal_dah + haqi + year_group + gdp + gov_effect_index, data = cf_ineff)
model.12 <- lm(ineff ~ Diagnostics + `CAIEMEPAOPSM` + `Anti-malarials` + haqi + year_group + gdp + gov_effect_index, data = cf_ineff)

## Output tables
directory <- 'ADDRESS'

## Models
stargazer(model.1, model.3, title = 'Linear regression on inefficiency', align = T, keep.stat = c('n'), no.space = T,
          single.row = T, digits = 3, column.labels = c('Inefficiency in incidence    ', 'Inefficiency in case fatality'),
          model.numbers = F, ord.intercepts = F, dep.var.labels.include = F,
          order = c(2, 1, 3, 4),
          covariate.labels = c('Development assistance proportion', 'Government spending proportion', 'HAQ index', '2011-2020 period'),
          omit = 'Constant',
          out = paste0(directory, 'FILEPATH'))

stargazer(model.2, model.4, title = 'Linear regression on inefficiency', align = T, keep.stat = c('n'), no.space = T,
          single.row = T, digits = 3, column.labels = c('Inefficiency in incidence    ', 'Inefficiency in case fatality'),
          model.numbers = F, ord.intercepts = F, dep.var.labels.include = F,
          order = c(4, 2, 1, 3, 5, 6),
          covariate.labels = c('Anti-malarial medicines', NA, NA, NA, 'HAQ index', '2011-2020 period'),
          omit = 'Constant',
          out = paste0(directory, 'FILEPATH'))

## Sensitivity analysis 1
stargazer(model.5, model.7, title = 'Linear regression on inefficiency', align = T, keep.stat = c('n'), no.space = T,
          single.row = T, digits = 3, column.labels = c('Inefficiency in incidence    ', 'Inefficiency in case fatality'),
          model.numbers = F, ord.intercepts = F, dep.var.labels.include = F,
          order = c(2, 1, 3, 4),
          covariate.labels = c('Development assistance proportion', 'Government spending proportion', 'HAQI', '2011-2020 period'),
          omit = 'Constant',
          out = paste0(directory, 'FILEPATH'))

stargazer(model.6, model.8, title = 'Linear regression on inefficiency', align = T, keep.stat = c('n'), no.space = T,
          single.row = T, digits = 3, column.labels = c('Inefficiency in incidence    ', 'Inefficiency in case fatality'),
          model.numbers = F, ord.intercepts = F, dep.var.labels.include = F,
          order = c(4, 2, 1, 3, 5, 6),
          covariate.labels = c('Anti-malarial medicines', NA, NA, NA, 'HAQI', '2011-2020 period'),
          omit = 'Constant',
          out = paste0(directory, 'FILEPATH'))

## Sensitivity analysis 2
stargazer(model.9, model.11, title = 'Linear regression on inefficiency', align = T, keep.stat = c('n'), no.space = T,
          single.row = T, digits = 3, column.labels = c('Inefficiency in incidence    ', 'Inefficiency in case fatality'),
          model.numbers = F, ord.intercepts = F, dep.var.labels.include = F,
          order = c(2, 1, 3, 4, 5, 6),
          covariate.labels = c('Development assistance proportion', 'Government spending proportion', 'HAQI', '2011-2020 period', 'GDP per capita', 'GEI'),
          omit = 'Constant',
          out = paste0(directory, 'FILEPATH'))

stargazer(model.10, model.12, title = 'Linear regression on inefficiency', align = T, keep.stat = c('n'), no.space = T,
          single.row = T, digits = 3, column.labels = c('Inefficiency in incidence    ', 'Inefficiency in case fatality'),
          model.numbers = F, ord.intercepts = F, dep.var.labels.include = F,
          order = c(4, 2, 1, 3, 5, 6, 7, 8),
          covariate.labels = c('Anti-malarial medicines', NA, NA, NA, 'HAQI', '2011-2020 period', 'GDP per capita', 'GEI'),
          omit = 'Constant',
          out = paste0(directory, 'FILEPATH'))

##END------------------------------------------------------------------------------