################################################
#' @author USERNAME
#' 
#' @description Output outcome datasets without 
#' zeros and lowest 5% removed with covariates
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
source(paste0(cds_repo, "FILEPATH"))
source(paste0(cds_repo, "FILEPATH"))

malaria_country_list <- fread('FILEPATH')

##----------------------------------------------------------------------------------
## Read in data
##----------------------------------------------------------------------------------

## Read in covariates and draw level outcomes
covariates <- fread('FILEPATH')

##----------------------------------------------------------------------------------
## Outcome estimates
##----------------------------------------------------------------------------------

## Subset to incidence and case fatality values
outcomes <- copy(covariates)
outcomes <- outcomes[, .(location_id, year_id, variable, case_fatality, incidence)]

## Take natural log of outcome values
outcomes[, c('case_fatality', 'incidence') := log(.SD), .SDcols = c('case_fatality', 'incidence')]
outcomes <- melt.data.table(outcomes,
                            id.vars = c('location_id', 'year_id', 'variable'),
                            variable.factor = F)

## Set lowest 1% of non-infinite values to 1st percentile (deal with zero values)
incidence_threshold <- round(quantile(outcomes[variable.1 == 'incidence' & !is.infinite(value)]$value, 0.01), 3)
case_fatality_threshold <- round(quantile(outcomes[variable.1 == 'case_fatality' & !is.infinite(value)]$value, 0.01), 3)

outcomes[variable.1 == 'incidence' & value < incidence_threshold, value := incidence_threshold]
outcomes[variable.1 == 'case_fatality' & value < case_fatality_threshold, value := case_fatality_threshold]

## Subset to relevant years (10 year difference)
outcomes <- outcomes[year_id %in% c(2000, 2010, 2020)]

## Reshape wide by year for subtraction
outcomes <- dcast.data.table(outcomes, location_id + variable + variable.1 ~ year_id, value.var = 'value')

## Calculate 10 year differences
outcomes[, `2020 value` := `2020` - `2010`]
outcomes[, `2010 value` := `2010` - `2000`]

## Calculate means and variances by draw
outcomes <- outcomes[, .(`2010` = mean(`2010`),
                         `2010 value` = mean(`2010 value`),
                         `2010 variance` = var(`2010 value`),
                         `2020` = mean(`2020`),
                         `2020 value` = mean(`2020 value`),
                         `2020 variance` = var(`2020 value`)),
                     by = .(location_id, variable.1)]

## Reassign variances of zero
outcomes[`2010 variance` == 0, `2010 variance` := 0.001]
outcomes[`2020 variance` == 0, `2020 variance` := 0.001]

## Remove countries with zero incident cases (or lower than threshold) from 2000-2020
outcomes <- outcomes[!(`2010` == 0 & `2020` == 0)]

## Mark datapoints with incidence of 0 in 2020
outcomes[variable.1 == 'incidence' & `2020` < -13.2, `2020 value` := NA]
outcomes[variable.1 == 'case_fatality' & `2020` < -11.1, `2020 value` := NA]

outcomes[, `2010` := NULL]
outcomes[, `2020` := NULL]

## Reshape long by mean value
outcomes <- melt.data.table(outcomes,
                            id.vars = c('location_id', 'variable.1', '2010 variance', '2020 variance'),
                            variable.name = 'year_id',
                            variable.factor = F)

## Remove values where there were only incident cases/deaths in 2000
outcomes[, year_id := gsub(' value', '', year_id)]
outcomes <- outcomes[value != 0]

## Set correct variances
outcomes[, variance := ifelse(year_id == '2010', `2010 variance`, `2020 variance`)]

## Subset to relevant columns and rename
outcomes <- outcomes[, .(location_id, year_id, variable = variable.1, value, variance)]

##----------------------------------------------------------------------------------
## Covariates
##----------------------------------------------------------------------------------

## Subset to relevant columns
covariates <- covariates[, .(location_id, year_id, haqi, urbanicity, under_5_prop, mat_edu, temp, gdp_pc, life_exp, gei, latitude, rainfall, region_inc_mean)]

## All draws are the same; remove excess rows
covariates <- unique(covariates)

## Set as 10 year groups
covariates[, year_group := ifelse(year_id %in% 2011:2020, '2011-2020',
                                  ifelse(year_id %in% 2001:2010, '2001-2010', NA))]

## Reshape long by covariates
covariates <- melt.data.table(covariates,
                              id.vars = c('location_id', 'year_id', 'year_group'),
                              variable.name = 'covariate',
                              variable.factor = F)

## Calculate log values for skewed covariates
covariates[covariate %in% c('temp', 'gdp_pc', 'latitude', 'rainfall'),
           value := log(value)]

## Calculate mean covariate value by year group
covariates <- covariates[, .(value = mean(value)),
                         by = .(location_id, year_group, covariate)]
covariates[, year_id := ifelse(year_group == '2001-2010', 2010,
                               ifelse(year_group == '2011-2020', 2020, NA))]

## Subset to relevant columns and remove 2000 values
covariates <- covariates[!is.na(year_group),
                         .(location_id, year_id, covariate, value)]
covariates[covariate == 'region_inc_mean' & value == 0,
           value := min(covariates[covariate == 'region_inc_mean' & value != 0]$region_inc_mean)]

## Reshape wide by covariates
covariates <- dcast.data.table(covariates, location_id + year_id ~ covariate, value.var = 'value')

##----------------------------------------------------------------------------------
## Final dataset
##----------------------------------------------------------------------------------

outcomes[, year_id := as.numeric(year_id)]
outcomes <- merge(outcomes, covariates, by = c('location_id', 'year_id'), all.x = T)

##----------------------------------------------------------------------------------
## Write out datasets
##----------------------------------------------------------------------------------

fwrite(outcomes[variable == 'incidence'], 'FILEPATH')
fwrite(outcomes[variable == 'case_fatality'], 'FILEPATH')

##END-----------------------------------------------------------------------------------------