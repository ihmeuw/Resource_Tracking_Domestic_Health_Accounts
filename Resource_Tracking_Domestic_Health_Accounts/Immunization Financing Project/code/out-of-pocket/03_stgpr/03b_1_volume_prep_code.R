########################################################################################
## Generate path_to_data.csv
## location_id, year_id, age_group_id=22, val, sample_size=1000, measure=proportion, 
## is_outlier=0, nid=50505, variance=?
## Author: Emilie Maddison
## Date: 14 May 2020
## Last edited: 24 June 2020
########################################################################################
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

code.dir <- FILEPATH
out.dir <- FILEPATH

source(paste0(FILEPATH, "helper_functions.R"))
source(paste0(FILEPATH, "get_population.R"))

## Today's date
date1 <- format(Sys.time(), "%Y%m%d")

## -------------------------------------------------------------------------------------

dt1 <- fread(paste0(out.dir, "total_volume_vaccines.csv"))
head(dt1)
unique(dt1$vaccine)
names(dt1)

# dt1[vaccine %in% c('Measles', 'MR'), vaccine := 'MR']

dt <- dt1[, .(volume = sum(volume)), .(location_id, ihme_loc_id, vaccine, year_id) ]
## Drop Tuvalu because it isn't in the GBD 2019 hierarchy
dt <- dt[ihme_loc_id != 'TUV',]
nrow(dt[is.na(location_id)]) == 0

dt_mena <- dt[vaccine == "MenA", ]
dt_hpv <- dt[vaccine == "HPV", ]
dt_pcv <- dt[vaccine == "PCV", ]
dt_rvv <- dt[vaccine == "RVV", ]
dt_yf <- dt[vaccine == "YF", ]
dt_je <- dt[vaccine == "JE", ]
dt_penta <- dt[vaccine == "Penta", ]
dt_meas <- dt[vaccine == "Measles", ]
dt_mr <- dt[vaccine == "MR", ]
dt_ipv <- dt[vaccine == "IPV", ]

dt_list <- list(dt_mena, dt_hpv, dt_pcv, dt_rvv, dt_yf, dt_je, dt_penta, dt_meas, dt_mr, dt_ipv)
# dt2 <- dt_hpv

prep_data <- function(dt2) {
  setnames(dt2, "volume", "val")
  dt2 <- dt2[, val := val + 0.01 * mean(val)]
  dt2 <- dt2[, val := log(val)]
  dt2 <- dt2[, .(variance = var(val), val, year_id, vaccine), 
             .(location_id, ihme_loc_id)]

  dt2[, age_group_id := 22]
  dt2[, sex_id := 3]
  dt2[, sample_size := 1000]
  dt2[, measure := "continuous"]
  dt2[, is_outlier := 0]
  dt2[, nid := 50505]

  dt2 <- dt2[, .(vaccine, location_id, year_id, age_group_id, sex_id, sample_size, 
                 measure, is_outlier, nid, val, variance)]
  
  fwrite(dt2, 
         paste0("FILEPATH/log_volume_",
                unique(dt2$vaccine), "_",
                date1, ".csv"))
}

r <- lapply(dt_list, prep_data)

## -------------------------------------------------------------------------------------

### Transform into proportion space
pops <- get_population(location_id = 'all',
                       age_group_id = c(1, 6, 7),
                       year_id = c(2000:2017),
                       gbd_round_id = 6,
                       decomp_step = 'step4')

pops1 <- merge(pops, locs, by = 'location_id', all.y = T)
pops1 <- pops1[, .(population = sum(population)), .(location_id, year_id)]

dt_prop <- merge(dt, pops1, by = c('location_id', 'year_id'), all.x = T)
dt_prop[, prop := volume/population]

dt2_mena <- dt_prop[vaccine == "MenA", ]
dt2_hpv <- dt_prop[vaccine == "HPV", ]
dt2_pcv <- dt_prop[vaccine == "PCV", ]
dt2_rvv <- dt_prop[vaccine == "RVV", ]
dt2_yf <- dt_prop[vaccine == "YF", ]
dt2_je <- dt_prop[vaccine == "JE", ]
dt2_penta <- dt_prop[vaccine == "Penta", ]
##
dt2_meas <- dt_prop[vaccine == "Measles", ]
dt2_mr <- dt_prop[vaccine == "MR", ]
dt2_ipv <- dt_prop[vaccine == "IPV", ]

dt_list <- list(dt2_mena, dt2_hpv, dt2_pcv, dt2_rvv, dt2_yf, dt2_je, dt2_penta, dt2_meas, dt2_mr, dt2_ipv)
# dt2 <- copy(dt2_mr)
prep_prop_data <- function(dt2) {
  setnames(dt2, "prop", "val")
  dt2 <- dt2[, val := val + 0.01 * mean(val)]
  dt2 <- dt2[, val := log(val)]
  dt2 <- dt2[, .(variance = var(val), val, year_id, vaccine), 
             .(location_id, ihme_loc_id)]

  dt2[, age_group_id := 22]
  dt2[, sex_id := 3]
  dt2[, sample_size := 1000]
  dt2[, measure := "proportion"]
  dt2[, is_outlier := 0]
  dt2[, nid := 50505]

  dt2 <- dt2[, .(vaccine, location_id, year_id, age_group_id, sex_id, sample_size, 
                 measure, is_outlier, nid, val, variance)]

  fwrite(dt2, 
         paste0("FILEPATH/log_volume_proportion_",
                unique(dt2$vaccine), "_", 
                date1, ".csv"))
}

s <- lapply(dt_list, prep_prop_data)

## End Of Script ##