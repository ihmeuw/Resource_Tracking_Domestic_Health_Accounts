########################################################################################
## Prepare volume data
## Emilie Maddison
## May 17, 2020
########################################################################################
## -----------------------------------------------------------------------------------##
## 1. Set up workspace
## -----------------------------------------------------------------------------------##
## Clear environment
rm(list = ls())

## Set filepaths
if (Sys.info()[1] == "Linux") {
  j <- FILEPATH 
  h <- FILEPATH 
  k <- FILEPATH 
} else if (Sys.info()[1] == "Windows") {
  j <- FILEPATH
  h <- FILEPATH
  k <- FILEPATH
}

## Set directories
code.dir <- FILEPATH
in.dir <- FILEPATH
out.dir <- FILEPATH

## Source functions
library(data.table)
source(paste0(FILEPATH, "helper_functions.R"))
source("FILEPATH/utility.r")
source(paste0(FILEPATH, "get_population.R"))
library(ggplot2)

## Today's date
date1 <- format(Sys.time(), "%Y%m%d")

## -----------------------------------------------------------------------------------##
## 2. Read in required data
## -----------------------------------------------------------------------------------##
## Create list of countries we are estimating for (135 L, LM, UM income countries)
locs_lm <- get_ig(locs)
locs_lm <- locs_lm[income_group != 'H',]

## Pull ST-GPR data from best models, for the ~40 countries with no volume data##
dt_hpv1 <- get_data(151160)
dt_ipv1 <- get_data(151163)
dt_je1 <- get_data(151166)
dt_mena1 <- get_data(151169)
dt_mr1 <- get_data(151172)
dt_pcv1 <- get_data(151175)
dt_penta1 <- get_data(151178)
dt_rvv1 <- get_data(151181)
dt_yf1 <- get_data(151184)

## Read in raw data ##
raw_dt1 <- fread(paste0(in.dir, "volume/total_volume_vaccines.csv"))
setnames(raw_dt1, "Index", "ihme_loc_id")

raw_dt1 <- get_location_id(raw_dt1)
head(raw_dt1)
unique(raw_dt1$Vaccine)
names(raw_dt1)

raw_dt1[Vaccine %in% c('Measles 1st', 'Measles 2nd', 'Measles', 'Rubella'), Vaccine := 'MR']
raw_dt <- raw_dt1[, .(volume = sum(volume)), .(location_id, Vaccine, year_id) ]
raw_dt <- raw_dt[!is.na(location_id),]

length(unique(raw_dt$location_id))
raw_locs <- unique(raw_dt$location_id)

## Subset raw data to each of the 9 vaccines
raw_dt_hpv <-   raw_dt[Vaccine == "HPV", ]
raw_dt_ipv <-   raw_dt[Vaccine == "IPV", ]
raw_dt_je <-    raw_dt[Vaccine == "JE", ]
raw_dt_mena <-  raw_dt[Vaccine == "MenA", ]
raw_dt_mr <-    raw_dt[Vaccine == "MR", ]
raw_dt_pcv <-   raw_dt[Vaccine == "PCV", ]
raw_dt_penta <- raw_dt[Vaccine == "Penta", ]
raw_dt_rvv <-   raw_dt[Vaccine == "RVV", ]
raw_dt_yf <-    raw_dt[Vaccine == "YF", ]

## Find which super-regions have 0 reported units of vaccine purchased. Use this to 
## overrule St-GPR models. For example, Yellow Fever vaccines are not used in every 
## country, but St-GPR doesn't know that. We will assume that in super-regions where
## sum(volume) == 0 for the entire time series, no country in that region buys that 
## vaccine.
find_zero_regions <- function(raw_dt1) {
  dt_test <- get_region(raw_dt1)
  dt_test <- dt_test[, .(region_volume = sum(volume)), .(super_region_name)]
}

raw_dt_hpv_summary <- find_zero_regions(raw_dt_hpv) #all but NAME
raw_dt_ipv_summary <- find_zero_regions(raw_dt_ipv) # no zeros
raw_dt_je_summary <- find_zero_regions(raw_dt_je) #SAEAO, SA
raw_dt_mena_summary <- find_zero_regions(raw_dt_mena) #NAME, SSA
raw_dt_mr_summary <- find_zero_regions(raw_dt_mr) #no zeros
raw_dt_pcv_summary <- find_zero_regions(raw_dt_pcv) #no zeros
raw_dt_penta_summary <- find_zero_regions(raw_dt_penta) #no zeros
raw_dt_rvv_summary <- find_zero_regions(raw_dt_rvv) # no zeros
raw_dt_yf_summary <- find_zero_regions(raw_dt_yf) # NAME, SSA

## -----------------------------------------------------------------------------------##
## 3. Create final dataset using raw data where available ##
## Steps: Keep modeled data only where raw data does not exist,
##        Transform into volume space using Under-15 population,
##        Set to zero doses where the super-region total raw doses = 0, and 
##        Append raw data where it exists
## -----------------------------------------------------------------------------------##

pops <- get_population(location_id = 'all',
                       age_group_id = c(1, 6, 7),
                       year_id = c(2000:2018),
                       gbd_round_id = 6,
                       decomp_step = 'step4')
pops1 <- merge(pops, locs, by = 'location_id', all.y = T)
pops1 <- pops1[, .(population = sum(population)), .(location_id, year_id)]

# dt1 <- dt_hpv1
# raw_dt_summary <- raw_dt_hpv_summary
# raw_dt1 <- raw_dt_hpv
# vac_name <- "HPV"

transform_to_volume <- function(dt1, raw_dt_summary, raw_dt1, vac_name) {
  dt <- get_region(dt1)
  dt[, Vaccine := vac_name]
  # dt <- dt[(!location_id %in% raw_locs) & (location_id %in% locs_lm$location_id), ]
  dt <- dt[(location_id %in% locs_lm$location_id), ]
  dt <- merge(dt, pops1, by = c('location_id', 'year_id'), all.x = T)
  
  ##Un-log transform and remove zero padding
  dt[, `:=`(gpr_mean = exp(gpr_mean),
            gpr_lower = exp(gpr_lower),
            gpr_upper = exp(gpr_upper))]
  adj_val <- min(dt$gpr_mean)
  dt[, `:=`(gpr_mean = gpr_mean - adj_val,
            gpr_lower = gpr_lower - adj_val,
            gpr_upper = gpr_upper - adj_val)]
  
  dt[, `:=`(volume1 = gpr_mean * population,
            volume_lower1 = gpr_lower * population,
            volume_upper1 = gpr_upper * population)]
  dt <- merge(dt, raw_dt_summary, by = 'super_region_name', all.x = T)
  dt[region_volume == 0, `:=`(volume1 = 0,
                              volume_lower1 = 0,
                              volume_upper1 = 0)]
  # dt[volume < 0, volume := 0]
  dt <- dt[, .(location_id, year_id, Vaccine, volume1, volume_lower1, volume_upper1)]
  # dt <- rbind(dt, raw_dt1)
  dt2 <- merge(dt, raw_dt1, by = c('location_id','Vaccine','year_id'), all.x = T)
  return(dt)
}

dt_hpv <- transform_to_volume(dt_hpv1, raw_dt_hpv_summary, raw_dt_hpv, "HPV")
dt_ipv <- transform_to_volume(dt_ipv1, raw_dt_ipv_summary, raw_dt_ipv, "IPV")
dt_je <- transform_to_volume(dt_je1, raw_dt_je_summary, raw_dt_je, "JE")
dt_mena <- transform_to_volume(dt_mena1, raw_dt_mena_summary, raw_dt_mena, "MenA")
dt_mr <- transform_to_volume(dt_mr1, raw_dt_mr_summary, raw_dt_mr, "MR")
dt_pcv <- transform_to_volume(dt_pcv1, raw_dt_pcv_summary, raw_dt_pcv, "PCV")
dt_penta <- transform_to_volume(dt_penta1, raw_dt_penta_summary, raw_dt_penta, "Penta")
dt_rvv <- transform_to_volume(dt_rvv1, raw_dt_rvv_summary, raw_dt_rvv, "RVV")
dt_yf <- transform_to_volume(dt_yf1, raw_dt_yf_summary, raw_dt_yf, "YF")

## -----------------------------------------------------------------------------------##
## 4. Test data
## to make sure all countires and years are present, and that all values are positive
## -----------------------------------------------------------------------------------##

check_shape <- function(dt1) {
length(unique(dt1$location_id)) == 135
length(unique(dt1$year_id)) == 19
min(dt1$volume) >= 0
}

check_shape(dt_hpv)
check_shape(dt_ipv)
check_shape(dt_je)
check_shape(dt_mena)
check_shape(dt_mr)
check_shape(dt_pcv)
check_shape(dt_penta)
check_shape(dt_rvv)
check_shape(dt_yf)

## Create aggregated dataset
dt_volume_all <- rbind(dt_hpv, dt_ipv, dt_je, dt_mena, dt_mr, dt_pcv, dt_penta, dt_rvv, dt_yf)
dt_volume_all <- get_ihme_loc(dt_volume_all)

## Remove clutter
rm(dt_hpv1, dt_ipv1, dt_mena1, dt_mr1, dt_pcv1, dt_penta1, dt_rvv1, dt_yf1)
rm(raw_dt_hpv_summary, raw_dt_ipv_summary, raw_dt_mena_summary, raw_dt_mr_summary, 
   raw_dt_pcv_summary, raw_dt_penta_summary, raw_dt_rvv_summary, raw_dt_yf_summary)

## -----------------------------------------------------------------------------------##
## 6. Write Final dataset
## -----------------------------------------------------------------------------------##

fwrite(dt_volume_all, paste0(in.dir, "volume/135_total_volume_vaccines_", date1, ".csv"))


## END OF SCRIPT ##
