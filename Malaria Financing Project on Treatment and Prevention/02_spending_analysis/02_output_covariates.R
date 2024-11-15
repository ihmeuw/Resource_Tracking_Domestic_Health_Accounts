################################################
#' @author USERNAME
#' 
#' @description Output relevant covariates for SFMA
################################################
pacman::p_load(data.table, dplyr, reticulate, gtools, glmnet, ggplot2, parallel, lme4, AICcmodavg, readxl)
rm(list = ls())

cds_repo <- "ADDRESS"
source('FILENAME')
source('FILENAME')
source("FILENAME")
source("FILENAME")
source("FILENAME")
source("FILENAME")
source("FILENAME")
source("FILENAME")
source('FILENAME')
source(paste0(cds_repo, "FILENAME"))

malaria_country_list <- fread('FILENAME')

## *************************************************

covariates1 <- get_covariate_estimates(covariate_id = 1099,
                                       location_id = malaria_country_list$location_id,
                                       year_id = 2000:2020,
                                       release_id = 9)
covariates2 <- get_covariate_estimates(covariate_id = 854,
                                       location_id = malaria_country_list$location_id,
                                       year_id = 2000:2020,
                                       release_id = 9)
covariates3 <- get_covariate_estimates(covariate_id = 881,
                                       location_id = malaria_country_list$location_id,
                                       year_id = 2000:2020,
                                       release_id = 9)

## Pull GDP per capita
gdp <- get_gdp()
gdp <- gdp[year %in% 2000:2020,
           .(gdp_pc = mean(data_var)),
           by = .(ihme_loc_id = iso3, year_id = year)]
gdp[, log_gdp_pc := log(gdp_pc)]

## Pull population for under 5
pops <- get_population(age_group_id = c('22', '1'),
                       year_id = 2000:2020,
                       location_id = malaria_country_list$location_id,
                       gbd_round_id = 7,
                       decomp_step = 'iterative')
pops <- dcast.data.table(pops, location_id + year_id ~ age_group_id, value.var = 'population')
pops[, under_5_prop := `1` / `22`]
pops <- pops[, .(location_id, year_id, under_5_prop)]


for (loc_id in malaria_country_list$location_id) {

  morb.1 <- get_draws('cause_id',
                    345,
                    location_id = loc_id,
                    year_id = 2000:2018,
                    source = 'como',
                    gbd_round_id = 6,
                    sex_id = 3,
                    age_group_id = 22,
                    measure_id = 6,
                    metric_id = 3,
                    decomp_step = 'step5')
  
  morb.2 <- get_draws("modelable_entity_id",
                    3055,
                    location_id = loc_id,
                    year_id = 2019:2020,
                    source = "epi",
                    release_id = 9,
                    version_id = 746086,
                    sex_id = 3,
                    age_group_id = 22,
                    measure_id = 6,
                    metric_id = 3)
  
  morb <- rbind(morb.1, morb.2, fill = T)
  rm(morb.1, morb.2)
  
  if (loc_id == 6) {
    morb_final <- copy(morb)
    rm(morb)
  } else {
    morb_final <- rbind(morb_final, morb)
    rm(morb)
  }
  
  
}


for (loc_id in malaria_country_list$location_id) {
  
  mort.1 <- get_draws('cause_id',
                      345,
                      location_id = loc_id,
                      year_id = 2000:2018,
                      source = 'codcorrect',
                      version_id = 313,
                      gbd_round_id = 7,
                      sex_id = 3,
                      age_group_id = 22,
                      measure_id = 1,
                      metric_id = 1,
                      decomp_step = 'step3')
  to_keep <- c('location_id', 'year_id', colnames(mort.1)[colnames(mort.1) %like% 'draw_'])
  mort.1 <- mort.1[, to_keep, with = F]
  
  mort.2 <- get_draws("cause_id",
                    345,
                    location_id = loc_id,
                    year_id = 2021,
                    source = "codem",
                    release_id = 9,
                    version_id = 729251,
                    measure_id = 1,
                    metric_id = 1)
  mort.3 <- get_draws("cause_id",
                      345,
                      location_id = loc_id,
                      year_id = 2021,
                      source = "codem",
                      release_id = 9,
                      version_id = 729254,
                      measure_id = 1,
                      metric_id = 1)
  mort.2 <- rbind(mort.2, mort.3)
  rm(mort.3)
  cols <- colnames(mort.2)[!colnames(mort.2) %like% 'draw_']
  mort.2 <- melt.data.table(mort.2,
                            id.vars = cols,
                            variable.factor = F)
  mort.2 <- mort.2[, .(value = sum(value)),
                   by = .(location_id, year_id, variable)]
  mort.2 <- dcast.data.table(mort.2,
                             location_id + year_id ~ variable,
                             value.var = 'value')
  
  mort <- rbind(mort.1, mort.2)
  rm(mort.1, mort.2)
  
  if (loc_id == 6) {
    mort_final <- copy(mort)
    rm(mort)
  } else {
    mort_final <- rbind(mort_final, mort)
    rm(mort)
  }
  
  
}

all_pops <- get_population(age_group_id = 22,
                       year_id = 2000:2020,
                       location_id = malaria_country_list$location_id,
                       gbd_round_id = 7,
                       decomp_step = 'iterative')
all_pops <- all_pops[, .(location_id, year_id, population)]

morb_final <- merge(morb_final, all_pops, all.x = T)
measurecols <- colnames(morb_final)[colnames(morb_final) %like% 'draw_']
morb_final <- melt.data.table(morb_final,
                              id.vars = c('location_id', 'year_id', 'population'),
                              measure.vars = measurecols,
                              variable.factor = F)
morb_final[, value := value * population]
morb_final[, population := NULL]
mort_final <- melt.data.table(mort_final,
                              id.vars = c('location_id', 'year_id'),
                              variable.factor = F)
morb_final[, measure_name := 'incidence']
mort_final[, measure_name := 'deaths']

outcomes <- rbind(morb_final, mort_final)
rm(morb_final, mort_final)

outcomes <- merge(outcomes[measure_name == 'incidence'], outcomes[measure_name == 'deaths'],
                  by = c('location_id', 'year_id', 'variable'))

outcomes[, case_fatality := value.y / value.x]
outcomes[is.na(case_fatality)| is.infinite(case_fatality), case_fatality := 0]

outcomes <- merge(outcomes, par, by = c('location_id', 'year_id'))
outcomes[, incidence := value.x / population_at_risk]


outcomes <- outcomes[, .(location_id, year_id, variable, case_fatality, incidence)]


# pull in remaining covariates
covs1 <- get_covariate_estimates(age_group_id = 22,
                                location_id = malaria_country_list$location_id,
                                year_id = 2000:2020,
                                gbd_round_id = 7,
                                covariate_id = 71,
                                decomp_step = 'iterative')
covs2 <- get_covariate_estimates(age_group_id = 22,
                                location_id = malaria_country_list$location_id,
                                year_id = 2000:2020,
                                gbd_round_id = 7,
                                covariate_id = 463,
                                decomp_step = 'iterative')
covs3 <- get_covariate_estimates(age_group_id = 22,
                                 location_id = malaria_country_list$location_id,
                                 year_id = 2000:2020,
                                 gbd_round_id = 7,
                                 covariate_id = 127,
                                 decomp_step = 'iterative')
covs4 <- get_covariate_estimates(age_group_id = 22,
                                 location_id = malaria_country_list$location_id,
                                 year_id = 2000:2020,
                                 gbd_round_id = 7,
                                 covariate_id = 3,
                                 decomp_step = 'iterative')

covs <- rbind(covs1, covs2, covs3, covs4)
covs <- covs[, .(location_id, year_id, covariate_name_short, mean_value)]
covs <- dcast.data.table(covs, location_id + year_id ~ covariate_name_short, value.var = 'mean_value')
setnames(covs,
         c('maternal_educ_yrs_pc', 'mean_temperature', 'rainfall_pop_weighted', 'abs_latitude'),
         c('mat_edu', 'temp', 'rainfall', 'latitude'))

# Pull in life expectancy
life_exp <-  get_life_table(location_id = malaria_country_list$location_id,
                            year_id = 2000:2020,
                            release_id = 9,
                            sex_id = 3,
                            age_group_id = 28,
                            life_table_parameter_id = 5)
life_exp <- life_exp[, .(location_id, year_id, life_exp = mean)]

# Pull in GEI
gei <- data.table(read_excel('FILEPATH'))
gei[Code == 'ZAR', Code := 'COD']
gei[Code == 'TMP', Code := 'TLS']
gei <- gei[Code %in% malaria_country_list$ihme_loc_id]
setnames(gei, 'Code', 'ihme_loc_id')
gei <- get_location_id(gei)

for (year in 2010:2000) {
  
  year_to_update <- as.character(year)
  year_half <- as.character(year + 1)
  year_third <- as.character(year + 2)
  year_sixth <- as.character(year + 3)
  gei[is.na(get(year_to_update)), eval(year_to_update) := eval(get(year_half))*(1/2) + eval(get(year_third))*(1/3) + eval(get(year_sixth))*(1/6)]
  
}

gei <- melt.data.table(gei, id.vars = c('ihme_loc_id', 'Country/Territory', 'location_id'),
                       variable.factor = F, variable.name = 'year_id')
gei[, year_id := as.numeric(year_id)]
gei <- gei[, .(location_id, year_id, gei = value)]


# merge all datasets
covariates <- covariates[location_id %in% malaria_country_list$location_id]
dt <- merge(covariates, pops, by = c('location_id', 'year_id'))
dt <- merge(dt, covs, by = c('location_id', 'year_id'))
dt <- merge(dt, gdp, by = c('location_id', 'year_id'))
dt <- merge(dt, life_exp, by = c('location_id', 'year_id'))
dt <- merge(dt, gei, by = c('location_id', 'year_id'))
dt <- merge(outcomes, dt, by = c('location_id', 'year_id'))


fwrite(dt, 'FILEPATH')


## END -------------------------------------------------------------------------