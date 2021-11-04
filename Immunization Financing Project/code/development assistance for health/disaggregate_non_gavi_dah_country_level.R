## #############################################################################
## Project: Immunization Financing
## Purpose: Disaggregate all non-Gavi data
## Author: 
## Last edited: 11/03/2021
## #############################################################################

## #############################################################################
## SETUP
## #############################################################################

## parameters
rm(list = ls())
date <- format(Sys.Date(), '%Y%m%d')
million <- 1e6
billion <- 1e9

## Environment
if (Sys.info()[1] == "Linux") {
  j <- "FILEPATH"
  h <- paste0("FILEPATH",Sys.info()[7], "/")
  k <- "FILEPATH"
}else if (Sys.info()[1] == "Windows") {
  j <- "FILEPATH"
  h <- "FILEPATH"
  k <- "FILEPATH"
}

## Filepaths
OUT <- paste0(j,"FILEPATH")
FIN <- paste0(j,"FILEPATH")

## Libraries and sources
pacman::p_load(data.table, dplyr, feather, reshape2, ggplot2, readstata13, gridExtra, RColorBrewer, openxlsx, grid)
source(paste0(h, "FILEPATH/helper_functions.R"))

## User Inputs
years <- 2000:2017

## Load data
## read in data subsetted to years of interest
asdb <- fread(paste0(FIN,"FILEPATH/AsDB_vaccine_DAH.csv"))[YEAR %in% years]
crs <- fread(paste0(FIN,"FILEPATH/CRS_ImmFin_Data.csv"))[YEAR %in% years]
idb <- fread(paste0(FIN,"FILEPATH/IDB_vaccine_DAH.csv"))[YEAR %in% years]
ngo <- fread(paste0(FIN,"FILEPATH/NGO_ImmFin_Data.csv"))[YEAR %in% years]
wb <- fread(paste0(FIN,"FILEPATH/WB_vaccine_DAH.csv"))[YEAR %in% years]
bmgf <- fread(paste0(FIN, "FILEPATH/BMGF_ImmFin_Data.csv"))[YEAR %in% years]
usf <- fread(paste0(FIN, "FILEPATH/US_FOUND_ImmFin_Data.csv"))[YEAR %in% years]
unicef <- fread(paste0(FIN, "FILEPATH/UNICEF_ImmFin_Data.csv"))[YEAR %in% years]
who <- fread(paste0(FIN, "FILEPATH/WHO_ImmFin_Data.csv"))[YEAR %in% years]

## #############################################################################
## DISAGGREGATE
## #############################################################################

## inkind
asdb_inkind <- asdb[, .(inkind = sum(INKIND)),
                    by = .(YEAR)]
idb_inkind <- idb[, .(inkind = sum(INKIND)),
                  by = .(YEAR)]
wb_inkind <- wb[, .(inkind = sum(INKIND)),
                by = .(YEAR)]
crs_inkind <- crs[INKIND == 1,
                  .(inkind = sum(nch_cnv_DAH_19)),
                  by = .(YEAR)]
ngo_inkind <- ngo[INKIND == 1,
                  .(inkind = sum(nch_cnv_DAH_19)),
                  by = .(YEAR)]
bmgf_inkind <- bmgf[INKIND == 1,
                    .(inkind = sum(nch_cnv_DAH_19)),
                    by = .(YEAR)]
usf_inkind <- usf[INKIND == 1,
                  .(inkind = sum(nch_cnv_DAH_19)),
                  by = .(YEAR)]
unicef_inkind <- unicef[INKIND == 1,
                        .(inkind = sum(nch_cnv_DAH_19)),
                        by = .(YEAR)]
who_inkind <- who[INKIND == 1, 
                  .(inkind = sum(nch_cnv_DAH_19)),
                  by = .(YEAR)]

## bind inkind objects together & aggregate by year
inkind <- rbind(asdb_inkind, idb_inkind, wb_inkind, crs_inkind, ngo_inkind, 
                usf_inkind, bmgf_inkind, unicef_inkind, who_inkind)
inkind[is.na(inkind), inkind := 0]
inkind <- inkind[, .(inkind = sum(inkind),
                     vaccine = 0,
                     delivery = 0,
                     vd_other = 0,
                     routine = 0,
                     supplementary = 0,
                     sr_other = 0,
                     rd = 0),
                 by = .(YEAR)]
inkind <- inkind[order(YEAR, na.last = T)]


## remove inkind values and change column names to component names
asdb <- asdb[, .(YEAR, ihme_loc_id = ISO3_RC, 
                 routine = rout_DAH_19, supplementary = supp_DAH_19, sr_other = rs_other_DAH_19,
                 vaccine = comm_DAH_19, delivery = deli_DAH_19, vd_other = dc_other_DAH_19,
                 rd = 0)]
asdb <- asdb[, channel := "AsDB"]

idb <- idb[, .(YEAR, ihme_loc_id = ISO3_RC, 
               routine = rout_DAH_19, supplementary = supp_DAH_19, sr_other = rs_other_DAH_19,
               vaccine = comm_DAH_19, delivery = deli_DAH_19, vd_other = dc_other_DAH_19,
               rd = 0)]
idb <- idb[, channel := "IDB"]

wb <- wb[, .(YEAR, ihme_loc_id = ISO3_RC, 
             routine = rout_DAH_19, supplementary = supp_DAH_19, sr_other = rs_other_DAH_19,
             vaccine = comm_DAH_19, delivery = deli_DAH_19, vd_other = dc_other_DAH_19,
             rd = 0)]
wb <- wb[, channel := "WB"]

crs <- crs[INKIND == 0,
           .(YEAR, ihme_loc_id = ISO3_RC, 
             routine = nch_cnv_rout_DAH_19, supplementary = nch_cnv_supp_DAH_19, 
             sr_other = nch_cnv_rs_other_DAH_19, 
             vaccine = nch_cnv_comm_DAH_19, delivery = nch_cnv_deli_DAH_19,
             vd_other = nch_cnv_dc_other_DAH_19,
             rd = nch_cnv_rd_DAH_19)]
crs <- crs[, channel := "CRS"]

ngo <- ngo[INKIND == 0, 
           .(YEAR, ihme_loc_id = ISO3_RC, 
             routine = nch_cnv_rout_DAH_19, supplementary = nch_cnv_supp_DAH_19,
             sr_other = nch_cnv_rs_other_DAH_19, vaccine = nch_cnv_comm_DAH_19, 
             delivery = nch_cnv_deli_DAH_19, vd_other = nch_cnv_dc_other_DAH_19,
             rd = nch_cnv_rd_DAH_19)]
ngo <- ngo[, channel := "NGO"]

bmgf <- bmgf[INKIND == 0,
             .(YEAR, ihme_loc_id = ISO3_RC, 
               routine = nch_cnv_rout_DAH_19, supplementary = nch_cnv_supp_DAH_19,
               sr_other = nch_cnv_rs_other_DAH_19, vaccine = nch_cnv_comm_DAH_19, 
               delivery = nch_cnv_deli_DAH_19, vd_other = nch_cnv_dc_other_DAH_19,
               rd = nch_cnv_rd_DAH_19)]
bmgf <- bmgf[, channel := "BMGF"]

usf <- usf[INKIND == 0,
           .(YEAR, ihme_loc_id = ISO3_RC, 
             routine = nch_cnv_rout_DAH_19, supplementary = nch_cnv_supp_DAH_19,
             sr_other = nch_cnv_rs_other_DAH_19, vaccine = nch_cnv_comm_DAH_19, 
             delivery = nch_cnv_deli_DAH_19, vd_other = nch_cnv_dc_other_DAH_19,
             rd = nch_cnv_rd_DAH_19)]
usf <- usf[, channel := "US_FOUNDS"]

unicef <- unicef[INKIND == 0,
                 .(YEAR, ihme_loc_id = ISO3_RC, 
                   routine = nch_cnv_rout_DAH_19, supplementary = nch_cnv_supp_DAH_19,
                   sr_other = nch_cnv_rs_other_DAH_19, vaccine = nch_cnv_comm_DAH_19, 
                   delivery = nch_cnv_deli_DAH_19, vd_other = nch_cnv_dc_other_DAH_19,
                   rd = nch_cnv_rd_DAH_19)]
unicef <- unicef[, channel := "UNICEF"]

who <- who[INKIND == 0,
           .(YEAR, ihme_loc_id = ISO3_RC, 
             routine = nch_cnv_rout_DAH_19, supplementary = nch_cnv_supp_DAH_19,
             sr_other = nch_cnv_rs_other_DAH_19, vaccine = nch_cnv_comm_DAH_19, 
             delivery = nch_cnv_deli_DAH_19, vd_other = nch_cnv_dc_other_DAH_19,
             rd = nch_cnv_rd_DAH_19)]
who <- who[, channel := "WHO"]

## bind together objects
non_gavi_data <- rbind(asdb, idb, wb, crs, ngo, bmgf, usf, unicef, who)
non_gavi_data[is.na(non_gavi_data)] <- 0
non_gavi_data <- non_gavi_data[ihme_loc_id == "Unallocable", ihme_loc_id := "QZA"]
non_gavi_data <- non_gavi_data[ihme_loc_id == "USA", ihme_loc_id := "QZA"]

## aggregate data by year, income group, and super region
non_gavi_data <- non_gavi_data[, .(vaccine = sum(vaccine),
                                   delivery = sum(delivery),
                                   vd_other = sum(vd_other),
                                   routine = sum(routine),
                                   supplementary = sum(supplementary),
                                   sr_other = sum(sr_other),
                                   rd = sum(rd),
                                   inkind = 0),
                               by = .(YEAR, ihme_loc_id)]

## bind data and inkind data
non_gavi_data <- rbind(non_gavi_data, inkind, fill = T)

## transform data wide to long
non_gavi_data <- melt.data.table(non_gavi_data, id.vars = c('YEAR', 'ihme_loc_id'), variable.name = 'component')

## write out data
fwrite(non_gavi_data, paste0(FIN,"non_gavi_data_country_level.csv"))
fwrite(non_gavi_data, paste0(FIN,"FILEPATH/non_gavi_data_country_level_",date,".csv"))

## END OF FILE ##