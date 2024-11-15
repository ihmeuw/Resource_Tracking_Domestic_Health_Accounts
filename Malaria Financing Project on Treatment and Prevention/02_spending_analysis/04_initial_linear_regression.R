################################################
#' @author USERNAME
#' 
#' @description Run initial linear regression analyses
################################################
pacman::p_load(data.table, dplyr, reticulate, gtools, glmnet, ggplot2, parallel, lme4, MASS, AICcmodavg, readxl, stargazer)
rm(list = ls())

cds_repo <- "ADDRESS"
source('FILEPATH')
source('FILEPATH')
source("FILEPATH")
source("FILEPATH")
source("FILEPATH")
source("FILEPATH")
source(paste0(cds_repo, "FILEPATH"))
source(paste0(cds_repo, "FILEPATH"))

malaria_country_list <- fread('FILEPATH')
directory <- 'ADDRESS'

##-------------------------------------------------------------------------

## Read in spending, covariates, and outcome estimates
spending <- fread(paste0(directory, 'FILEPATH'))
incidence <- fread(paste0(directory, 'FILEPATH'))
case_fatality <- fread(paste0(directory, 'FILEPATH'))

## Keep relevant columns
incidence <- incidence[, .(location_id, ihme_loc_id, year_id, incidence)]
case_fatality <- case_fatality[, .(location_id, ihme_loc_id, year_id, case_fatality)]

## Merge outcomes
dt <- merge(incidence, case_fatality, by = c('location_id', 'ihme_loc_id', 'year_id'), all = T)

## Merge spending estimates onto outcome estimates
dt <- merge(dt, spending, by = c('location_id', 'year_id'))

## Run each linear regression for each outcome and spending pair

model.1 <- lm(incidence ~ mal_the + ihme_loc_id,
              data = dt[!is.na(incidence)])
model.2 <- lm(incidence ~ mal_ghes + ihme_loc_id,
              data = dt[!is.na(incidence)])
model.3 <- lm(incidence ~ mal_dah + ihme_loc_id,
              data = dt[!(is.na(incidence)|is.infinite(mal_dah))])

model.4 <- lm(case_fatality ~ mal_the + ihme_loc_id,
              data = dt[!is.na(case_fatality)])
model.5 <- lm(case_fatality ~ mal_ghes + ihme_loc_id,
              data = dt[!is.na(case_fatality)])
model.6 <- lm(case_fatality ~ mal_dah + ihme_loc_id,
              data = dt[!(is.na(case_fatality)|is.infinite(mal_dah))])


stargazer(model.1, model.2, model.3, model.4, model.5, model.6, title = 'Initial linear regressions', align = F,
          dep.var.labels = c('Incidence', 'Case fatality'), keep.stat = c('n', 'rsq', 'adj.rsq'), no.space = F,
          covariate.labels = c('Total spending', 'Government spending', 'Development assistance', unique(dt$ihme_loc_id)),
          digits = 3, keep = c('mal_the', 'mal_ghes', 'mal_dah'), column.sep.width = '15pt',
          out = paste0(directory, 'FILEPATH'))


##END-----------------------------------------------------------------------