################################################
#' @author USERNAME
#' 
#' @description Running regressions for exemplar analyses with disaggregated government estimates
################################################
pacman::p_load(data.table, dplyr, reticulate, gtools, glmnet, ggplot2, parallel, lme4, AICcmodavg, readxl, ggpubr)
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

### -------------------------------------------
## Country exemplars
### -------------------------------------------

## Read in inefficiency estimates
cf <- fread("FILEPATH")
inc <- fread("FILEPATH")

## Label estimates datasets
cf[, outcome_measure := 'case fatality']
inc[, outcome_measure := 'incidence']

## Bind datasets
dt <- rbind(cf, inc)

## Get ISO3 codes and country names
dt <- get_ihme_loc(dt)
dt <- get_location_name(dt)

## Remove trimmed data
dt <- dt[y_adj >= y_adj_hat]

## Calculate mean inefficiency values by location and outcome measure
dt <- dt[, .(ineff = mean(ineff)),
         by = .(location_name, outcome_measure)]

## Order ascending by outcome measure
dt <- dt[order(outcome_measure, ineff)]

## Top country exemplars
inc_exemplars <- dt[outcome_measure == 'incidence']$location_name[1:3]
cf_exemplars <- dt[outcome_measure == 'case fatality']$location_name[1:3]

## Round inefficiency values
dt[, ineff := round(ineff, 3)]

## Keep exemplars
dt <- dt[(outcome_measure == 'incidence' & location_name %in% inc_exemplars) | (outcome_measure == 'case fatality' & location_name %in% cf_exemplars)]

## Output table
fwrite(dt, 'FILEPATH')

## END--------------------------------------------