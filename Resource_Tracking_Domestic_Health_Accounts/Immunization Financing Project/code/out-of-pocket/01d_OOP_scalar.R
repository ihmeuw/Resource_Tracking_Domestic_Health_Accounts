########################################################################################
## Out-of_pocket scalar
## Author: Emilie Maddison, ermadd@uw.edu
## Date: 7 May 2020
## Description: 
########################################################################################
##--------------------------------------------------------------------------------------
## 1. Set up environment 
##--------------------------------------------------------------------------------------
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

code.dir <- paste0(FILEPATH)
out.dir <- paste0(FILEPATH)

## source packages and functions
require(data.table)
library(readxl)
#require(classInt, lib.loc = FILEPATH)
source(paste0(FILEPATH, "helper_functions.R"))

## Set today's date
date1 <- format(Sys.time(), "%Y%m%d")

##--------------------------------------------------------------------------------------
## 2. Read in data sources
##--------------------------------------------------------------------------------------
## Private immunization utilization rate (from literature)
## Read in data
imm_rate1 <- data.table(
  read_excel(paste0(FILEPATH, "percent_private_vaccines_from_lit_20200429.xlsx")))

imm_rate <- get_ig(imm_rate1)
## Drop ngo data, since out-of-pocket spending is unlikely at these facilities
imm_rate <- imm_rate[profit_type != 'ngo' & 
                        urban_type != 'urban' &
                        vaccine_specific == 0 &
                        exclude != 1 &
                        income_group != 'H',]


## Merge on super region
imm_rate <- get_region(imm_rate)
imm_rate[ihme_loc_id == 'R7', super_region_name := 	"Latin America and Caribbean"]

## Calculate regional "% of immunizations in private facilities"
imm_rate_reg <- imm_rate[, .(pct_imm = mean(pct_private)), .(super_region_name)]
# imm_rate_iso <- imm_rate[, .(pct_imm = mean(pct_private)), .(ihme_loc_id)]
imm_rate[, pct_imm := mean(pct_private), .(super_region_name)]

setnames(imm_rate, 'year_end', 'year_id')
imm_rate[, year_id := as.integer(year_id)]

##--------------------------------------------------------------------------------------
## Private facility utilization rate (from DHS)
## Read in data
private_rate1 <- fread(paste0(FILEPATH, "private_facility_use_data.csv"))
private_rate <- get_region(private_rate1)

## Drop NAs, then calculate regional "% of care in private facilities" 
## (from diahrea/fever care-seeking questions in DHS)
private_rate <- private_rate[(!is.na(private_facility_use_prop)),]
private_rate_reg <- private_rate[, .(pct_fac = mean(private_facility_use_prop)), 
                                 .(super_region_name)]
# private_rate_iso <- private_rate[, .(pct_fac = mean(private_facility_use_prop)), 
#                                  .(ihme_loc_id)]
private_rate[, pct_fac := mean(private_facility_use_prop), .(super_region_name)]

##--------------------------------------------------------------------------------------
## 3. Calculate facility scalar
##--------------------------------------------------------------------------------------
## Merge the two sources so that we can calculate the facility scalar
imm_prv_rate <- merge(imm_rate_reg, private_rate_reg, by = 'super_region_name', all.x = T)
imm_prv_rate[, fac_scalar := pct_imm / pct_fac]
# imm_prv_rate_iso <- merge(imm_rate_iso, private_rate_iso, by = 'ihme_loc_id', all.x = T)
# imm_prv_rate_iso[, fac_scalar := pct_imm / pct_fac]

names(imm_rate)
names(private_rate)

# imm_tot_rate <- merge(imm_rate[, .(ihme_loc_id, year_id, super_region_name, pct_private)],
#            private_rate[, .(ihme_loc_id, year_id, super_region_name, private_facility_use_prop)], 
#            by = c('ihme_loc_id', 'super_region_name'),
#            all = T)

imm_tot_rate <- rbind(imm_rate[, .(ihme_loc_id, year_id, super_region_name, pct_private)],
                      private_rate[, .(ihme_loc_id, year_id, super_region_name, private_facility_use_prop)],
                      fill = T)
imm_tot_rate <- merge(imm_tot_rate, imm_prv_rate[, .(super_region_name, fac_scalar)], 
                      by = 'super_region_name', all = T)

##--------------------------------------------------------------------------------------
## 4. Fill immunizations using scalar, and save data
##--------------------------------------------------------------------------------------
## 
imm_tot_rate[is.na(pct_private), pct_private := private_facility_use_prop * fac_scalar]
                                       

# imm_tot_rate <- merge(imm_tot_rate, locs[, .(ihme_loc_id, super_region_name)], 
#                       by = c('ihme_loc_id', 'super_region_name'), all = T)
# imm_tot_rate <- merge(imm_tot_rate, imm_prv_rate, by = 'super_region_name', all = T)
# imm_tot_rate[is.na(pct_private), pct_private := pct_imm.y]
# imm_tot_rate2 <- imm_tot_rate[, .(pct_private_imm = mean(pct_private)), .(ihme_loc_id)]

##--------------------------------------------------------------------------------------
## Write out filled "private immunization utilization rate"
fwrite(imm_tot_rate, paste0(FILEPATH, "private_immunization_utilization_rate_no_region_fill_", date1, ".csv"))

## END OF FILE