##################################################
# USERNAME
# 
# prepare for covariates for covariate selection Malaria government
##################################################

rm(list = ls())
pacman::p_load(data.table, dplyr, tidyr, stringr)

source("FILEPATH")

library(lme4)
require(ggplot2)
require(parallel)
library(data.table)

years <- 2000:2020
location_set_id <- 22
gbd_round_id <- 7
# ==========================================================================================
setwd("ADDRESS")
source("FILEPATH")
location <- get_location_metadata(location_set_id = location_set_id, gbd_round_id = gbd_round_id)
loc <- location[level == 3 | parent_id == 95, .(ihme_loc_id, location_id, location_name, super_region_name, region_name)]
level_3 <- location[level == 3, .(ihme_loc_id, location_id, location_name, super_region_name, region_name)]

source("FILEPATH")
population <- get_population(
  location_set_id = location_set_id, gbd_round_id = gbd_round_id,
  age_group_id = 22, sex_id = 3, year_id = years, location_id = loc$location_id, decomp_step = 'iterative'
)
pop <- population[, .(location_id, year_id, population)]


# get non-malaria and malaria specific covariates
cov_ids <- c(7, 
             8,
             1099, 
             854, 
             143, 
             881, 
             1097, 
             57)

mal_cov_ids <- c(183)

cov_ids <- c(cov_ids, mal_cov_ids)
source("FILEPATH")
covar <- lapply(
  cov_ids,
  function(x)
    get_covariate_estimates(
      gbd_round_id = gbd_round_id,
      covariate_id = x,
      age_group_id = 22,
      sex_id = 3,
      year_id = years,
      location_id = loc$location_id,
      decomp_step = 'iterative'
    )
)
covar <- lapply(
  covar,
  function(x)
    subset(x,
           select = c("covariate_name_short", "location_id", "year_id", "mean_value")
    )
)

covar <- rbindlist(covar)

covar <- dcast.data.table(covar, formula = location_id + year_id ~ covariate_name_short, value.var = 'mean_value')

setnames(covar, c("ANC1_coverage_prop",
                  "ANC4_coverage_prop", 
                  "haqi", 
                  "LDI_pc",
                  "malaria_lysenko_1_prop",
                  "prop_urban",
                  "SBA_coverage_prop", 
                  "sdi",
                  "universal_health_coverage"), 
         c("anc1",
           "anc4",
           "haqi", 
           "ldi_pc",
           "malaria_lysenko_1",
           "prop_urban",
           "sba",
           "sdi",
           "uhc"))

# get malaria incidence rates and prevalence rates
source("FILEPATH")
mal_inc_prev1 <- get_outputs("cause", cause_id=345, metric_id=3, measure_id=c(5), gbd_round_id = 6,
                            location_id = unique(loc$location_id),
                            year_id=2000:2019,
                            age_group_id = 22, sex_id=3, decomp_step = 'step5')
mal_inc_prev2 <- mal_inc_prev1[year_id == 2019]
mal_inc_prev2[, year_id := 2020]
mal_inc_prev1 <- rbind(mal_inc_prev1, mal_inc_prev2)
mal_inc_prev1 <- mal_inc_prev1[, .(measure_name, location_id, year_id, val)]


incidence <- fread('FILEPATH')
draw_cols <- colnames(incidence)[colnames(incidence) %like% 'draw']
incidence <- melt.data.table(incidence, id.vars = c('location_id', 'year_id'), measure.vars = draw_cols)
incidence <- incidence[location_id %in% unique(mal_inc_prev1$location_id),
                       .(val = mean(value)),
                       by = .(location_id, year_id)]
incidence[, measure_name := 'Incidence']

mal_inc_prev <- rbind(mal_inc_prev1, incidence)


mal_inc_prev <- mal_inc_prev[,.(measure_name, location_id, year_id, val)]
mal_inc_prev <- mal_inc_prev %>% spread(measure_name, val) %>% data.table
setnames(mal_inc_prev,c('Incidence','Prevalence'),c('mal_inc','mal_prev'))

# merge main covariates with incidence and prevalence
covar <- merge(covar, mal_inc_prev, by = c('location_id','year_id'), all.x = T)

setDT(covar)

#-------------------------------------------------------------------------------------------------

#===================================================================
#                       FIX NAs and scales
#===================================================================
# check summary on each covariate
lapply(covar[, -1:-2], summary)

# check for NAs
covar[is.na(uhc)]

# fix uhc for GBR - adding subnational and weighted by population
uhc_95 <- merge(pop, covar, by = c("location_id", "year_id"))
uhc_95 <- uhc_95[location_id %in% c(4749, 433, 434, 4636)]
uhc_95 <- uhc_95[,
                 .(location_id, 
                   year_id, 
                   population, 
                   uhc = uhc * population), 
                 by = year_id]
uhc_95[,uhc := sum(uhc) / sum(population)]

covar[location_id == 95, uhc := unique(uhc_95$uhc)]

# adjust haqi scale
covar[, haqi := haqi / 100] 


# =======================================================================================================
#                                         prep and transform malaria covariates
# =======================================================================================================

# collect irs, itn, act from gpr outputs
irs_itn <- fread('FILEPATH')

act <- fread('FILEPATH')
setnames(act, 'act_cov_final','act')

intervention <- merge(irs_itn, act, by = c('location_id','year_id'))

# ========================================

covariate <- merge(covar, level_3, by = c('location_id'))
covariate <- merge(covariate, intervention, by = c('location_id','year_id'), all.x = T)

# logit transform applicable covariates
vars <- c(
  "act", 'malaria_lysenko_1',
  "anc1", "anc4", "haqi", "prop_urban", "sba",
  "sdi", "uhc",'mal_inc','mal_prev'
)


trans_vars <- paste("logit", vars, sep = "_")
covariate[, eval(trans_vars) := lapply(
  covariate[, vars, with = F],
  function(x) logit_trans(x)
)]

covariate[, log_ldi_pc := log(ldi_pc)]

#########################################################################################################
#                                   PREP A SQUARE SET FOR STGPR INPUT
#########################################################################################################

location <- rbind(location)

## prep square
loc_sq <- location[level>=3,.(location_id,ihme_loc_id, location_name, region_name, super_region_name)]
year_id <- 2000:2020
sq_dt <- data.table(crossing(year_id, loc_sq))

#============================================================================

## prep covariates to merge
covariate_sq <- copy(covariate)

## select columns of interest
cov <- colnames(covariate_sq)
cov <- cov[grepl('logit|log',cov)]
covariate_sq <- covariate_sq[,c("year_id",
                                "location_id", 
                                "ihme_loc_id",
                                "location_name",
                                "region_name",
                                "super_region_name", 
                                cov), with = F]

## merge data and square
covariate_sq <- merge(sq_dt,covariate_sq, 
                      by = c("year_id",
                             "location_id", 
                             "ihme_loc_id",
                             "location_name",
                             "region_name",
                             "super_region_name"), all.x = T)

## mark sub-saharan africa and non sub-saharan africa
covariate_sq[super_region_name == 'Sub-Saharan Africa',is_ssa:=1]
covariate_sq[is.na(is_ssa),is_ssa:=0]

## ---------------
## fill subnats
## ---------------

## Get parent loc at country level
covariate_sq[,parent_loc := str_extract(ihme_loc_id, "[:upper:]{3}")]

## get country-level data
parent <- covariate_sq[parent_loc == ihme_loc_id, c("parent_loc", "year_id", cov), with = F]

## delete original data columns
covariate_sq[, c(cov) := NULL]

## merge parent data back in by parent column
covariate_sq <- merge(covariate_sq, parent, by = c("year_id", "parent_loc")) 

## remove parent loc
covariate_sq[,parent_loc := NULL]

## -------------
## write data
## -------------
fwrite(covariate_sq, "FILEPATH")

## End of Script ##
