########################################################################################
## For ST-GPR, Generate input data (path_to_data.csv)
## Required columns: location_id, year_id, age_group_id=22, val, sample_size=1000, 
##                   measure=proportion, is_outlier=0, nid=50505, variance=?
## Author: Emilie Maddison
## Date: 20 May 2020
########################################################################################
## -------------------------------------------------------------------------------------
## 1. Set up environment
## -------------------------------------------------------------------------------------

## Clear environment
rm(list = ls())

## Set filepaths
if (Sys.info()[1] == "Linux") {
  j <-FILEPATH
  h <-FILEPATH
  k <-FILEPATH
} else if (Sys.info()[1] == "Windows") {
  j <- FILEPATH
  h <- FILEPATH
  k <- FILEPATH
}

code.dir <- FILEPATH
out.dir <- FILEPATH

## Set today's date
date1 <- format(Sys.time(), "%Y%m%d")

source(paste0(FILEPATH, "helper_functions.R"))
source(paste0(FILEPATH, "get_covariate_estimates.R"))
source(paste0(FILEPATH, "get_location_metadata.R"))
## -------------------------------------------------------------------------------------
## 2. Read in OOP scalar data
## -------------------------------------------------------------------------------------

dt <- fread(paste0(out.dir, "oop_scalar/private_immunization_utilization_rate_no_region_fill_", date1, ".csv"))
print(paste0(out.dir, "oop_scalar/private_immunization_utilization_rate_no_region_fill_", date1, ".csv"))

## -------------------------------------------------------------------------------------
## 3. Run covariate selection
## -------------------------------------------------------------------------------------

## Covariate selection - OOP as a fraction of THE
oop_frac <- get_covariate_estimates(gbd_round_id = 6,
                               decomp_step = 'step4',
                               covariate_id = 1093)

loc_ids <- get_location_metadata(location_set_id = 1 , gbd_round_id = 6)
oop_frac <- merge(oop_frac, loc_ids[, .(location_id, ihme_loc_id)], by = 'location_id', all.x = T) 
setnames(oop_frac, "mean_value", "frac_oop_hexp")
names(dt)
dt_oop_frac <- merge(dt, oop_frac[, .(ihme_loc_id, year_id, frac_oop_hexp)], 
                by = c('ihme_loc_id', 'year_id'), all = T)

summary(lm((pct_private) ~ frac_oop_hexp, data = dt_oop_frac))
## A higher OOP fraction predicts a lower percent of immunizations in private facilities...
## This is counterintuitive, but the effect is insignificant 

## Covariate selection - OOP as a fraction of THE
haqi <- get_covariate_estimates(gbd_round_id = 6,
                               decomp_step = 'step4',
                               covariate_id = 1099)

##
haqi <- merge(haqi, loc_ids[, .(location_id, ihme_loc_id)], by = 'location_id', all.x = T) 
setnames(haqi, "mean_value", "haqi")
names(dt)
dt_haqi <- merge(dt, haqi[year_id %in% c(2000:2017) & !is.na(ihme_loc_id), .(ihme_loc_id, year_id, haqi)], 
                by = c('ihme_loc_id', 'year_id'), all = T)

summary(lm(pct_private ~ haqi, data = dt_haqi))

##
dt_oop_frac <- dt_oop_frac[ihme_loc_id %in% locs$ihme_loc_id,]
dt_oop_haqi <- merge(dt_oop_frac, haqi[, .(ihme_loc_id, year_id, haqi)], 
                 by = c('ihme_loc_id', 'year_id'), all = T)

summary(lm(pct_private ~ frac_oop_hexp + haqi, data = dt_oop_haqi))

## Covariate selection - GDP per capita
gdppc <- get_covariate_estimates(gbd_round_id = 6,
                                decomp_step = 'step4',
                                covariate_id = 851)

gdppc <- merge(gdppc, loc_ids[, .(location_id, ihme_loc_id)], by = 'location_id', all.x = T) 
setnames(gdppc, "mean_value", "gdppc")
names(gdppc)

dt_gdppc <- merge(dt, gdppc[year_id %in% c(2000:2017) & !is.na(ihme_loc_id), .(ihme_loc_id, year_id, gdppc)], 
                 by = c('ihme_loc_id', 'year_id'), all = T)
summary(lm(pct_private ~ gdppc, data = dt_gdppc))

## Covariate selection - LDI per capita
ldi_pc <- get_covariate_estimates(gbd_round_id = 6,
                                 decomp_step = 'step4',
                                 covariate_id = 57)

ldi_pc <- merge(ldi_pc, loc_ids[, .(location_id, ihme_loc_id)], by = 'location_id', all.x = T) 
setnames(ldi_pc, "mean_value", "ldipc")
names(ldi_pc)

dt_ldipc <- merge(dt, ldi_pc[year_id %in% c(2000:2017) & !is.na(ihme_loc_id), .(ihme_loc_id, year_id, ldipc)], 
                  by = c('ihme_loc_id', 'year_id'), all.x = T)
summary(lm(pct_private ~ ldipc, data = dt_ldipc))

## gdp_pc has the lowest R squared and lowest p-value (besides frac_oop_hexp, which is weird)
## -------------------------------------------------------------------------------------
## 4. Format data
## -------------------------------------------------------------------------------------

dt2 <- dt[, val := (pct_private)]

adj_val <- dt2[, 0.01 * median(val)]
dt2[, val := val + 0.01 * median(val)]

dt2 <- dt2[, variance := var(val)]

dt2 <- get_location_id(dt2)
dt2[, age_group_id := 22]
dt2[, sex_id := 3]
dt2[, sample_size := 1000]
dt2[, measure := "proportion"]
dt2[, is_outlier := 0]
dt2[, nid := 50505]

dt2 <- dt2[, .(location_id, year_id, age_group_id, sex_id, sample_size, measure, is_outlier, 
              nid, val, variance)]

## check that all variance values exist
nrow(dt2[is.na(variance)]) == 0
## -------------------------------------------------------------------------------------
## 5. Save data
## -------------------------------------------------------------------------------------

fwrite(dt2, paste0("FILEPATH/oop_scalar_",
                   date1, ".csv"))
