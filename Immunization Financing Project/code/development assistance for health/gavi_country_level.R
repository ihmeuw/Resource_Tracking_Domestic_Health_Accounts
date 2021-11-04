## #############################################################################
## Project: Immunization Financing
## Purpose: Disaggregate Gavi data country level
## Author:
## Last edited: 11/03/2021
## #############################################################################

## #############################################################################
## SETUP
## #############################################################################

## parameters
rm(list = ls())
date <- format(Sys.Date(),'%Y%m%d')
billion <- 1e9
million <- 1e6

## Environment
if (Sys.info()[1] == "Linux"){
  j <- "FILEPATH"
  h <- paste0("FILEPATH", Sys.info()[7], "/")
  k <- "FILEPATH"
} else if (Sys.info()[1] == "Windows"){
  j <- "FILEPATH"
  h <- "FILEPATH"
  k <- "FILEPATH"
}

## Filepaths
OUT <- paste0(j,"FILEPATH")
FIN <- paste0(j,"FILEPATH")

## Libraries and sources
pacman::p_load(data.table, dplyr, feather, reshape2, ggplot2, readstata13, 
               gridExtra, RColorBrewer, openxlsx, grid)
source("FILEPATH/helper_functions.R")

## User Inputs
years <- 2000:2017

## Load data
gavi_data <- data.table(read.dta13(paste0(j, "FILEPATH/P_GAVI_INTPDB_FGH2019_IMFIN.dta")))
deflator <- data.table(read.dta13(paste0(j, "FILEPATH/imf_usgdp_deflators_1019.dta")))
tca_gavi_data <- fread(paste0(j, "FILEPATH/Gavi_TCA_DAH.csv"))

inkind_data <- data.table(read.dta13(paste0(j, "FILEPATH/P_GAVI_ADB_PDB_FGH2019.dta")))[INKIND == 1]

## #############################################################################
## DISAGGREGATE
## #############################################################################

## Filter
gavi_data <- gavi_data[YEAR %in% years & DISBURSEMENT_yr_paid > 0, 
                       .(YEAR, ihme_loc_id = ISO3_RC, PROGRAM, DISBURSEMENT_yr_paid)]

## component program lists
vaccine_list <- c("Ebola EPI Recovery Plan", "Vaccine Introduction Grant", "HPV", "HPV Demo", "HepB mono", "Hib mono", "IPV", "JEV", "Measles", "Measles SIA", "Measles-Rubella", "Meningitis A",
                  "Meningitis A - campaign", "Meningitis A - mini catch-up campaign", "Penta", "Pneumo", "Rotavirus", "Tetra DTP-HepB", "Tetra DTP-Hib", "Yellow Fever", "Yellow Fever - campaign",
                  "Additional Intro Support", "HPV MAC", "IPV Catch-up RI", "JEV-Routine", "MR 1st and 2nd dose", "MR 1st dose", "MR 2nd dose", "MR-Catch-up campaign", "MR-Follow-up campaign",
                  "Measles 1st and 2nd dose", "Measles-Catch-up campaign", "Measles-Follow-up campaign", "Penta - campaign", "Pneumo - campaign", "TCV Outbreak", "Td - campaign", "MNT", "Meningitis",
                  "Polio", "UNICEF - vaccine stockpile", "Vaccine Independence Initiative", "IPV-Catch-up campaign", "INS")

delivery_list <- c("CCEOP", "CSO Type A", "CSO Type B", "HPV Demo - cash support", "Graduation grant", "HSS", "Product Switch Grant", "JEV - Operational costs", "MR - Operational costs", 
                   "MR-Catch-up campaign op.costs", "MR-Follow-up campaign op.costs", "Measles SIA - Operational costs", "Measles-Follow-up campaign op.costs", "Meningitis A - mini catch-up op.costs",
                   "Meningitis A - operational costs", "Operational costs", "YF - Operational costs", "Injection Safety Devices", "HPV MAC - Op costs", "TCV - Outbreak op. costs", "MNT - operational costs",
                   "Measles - operational costs", "Measles-Rubella - operational costs", "Meningitis - operational costs", "Merck - Adv purchase comm - Ebola", "Mongolia - Vodafone", "Polio - operational costs",
                   "UNICEF - CCEOP", "WHO - Debt Relief Study", "WHO - Vaccine pilot implementation", "WHO - operational costs", "WHO - vaccine stockpile", "Yellow Fever - operational costs",
                   "IPV-Catch-up campaign op.costs", "ISS")

routine_list <- c("CCEOP", "CSO Type A", "CSO Type B", "HPV Demo - cash support", "HSS", "INS", "Product Switch Grant", "Vaccine Introduction Grant", "HPV", "HPV Demo", "HepB mono", "Hib mono", "IPV", "JEV",
                  "Measles", "Measles-Rubella", "Meningitis A", "Penta", "Pneumo", "Rotavirus", "Tetra DTP-HepB", "Tetra DTP-Hib", "Yellow Fever", "JEV - Operational costs", "MR - Operational costs",
                  "Meningitis A - operational costs", "Operational costs", "YF - Operational costs", "Injection Safety Devices", "HPV MAC - Op costs", "TCV - Outbreak op. costs", "Additional Intro Support",
                  "HPV MAC", "JEV-Routine", "MR 1st and 2nd dose", "MR 1st dose", "MR 2nd dose", "Measles 1st and 2nd dose", "TCV Outbreak", "MNT", "MNT - operational costs", "Measles - operational costs",
                  "Measles-Rubella - operational costs", "Meningitis", "Meningitis - operational costs", "Mongolia - Vodafone", "Polio", "Polio - operational costs", "UNICEF - CCEOP",
                  "UNICEF - vaccine stockpile", "Vaccine Independence Initiative", "WHO - Debt Relief Study", "WHO - operational costs", "WHO - vaccine stockpile", "Yellow Fever - operational costs", "ISS")

supplementary_list <- c("Ebola EPI Recovery Plan", "Graduation grant", "Measles SIA", "Meningitis A - campaign", "Meningitis A - mini catch-up campaign", "Yellow Fever - campaign", "MR-Catch-up campaign op.costs",
                        "MR-Follow-up campaign op.costs", "Measles SIA - Operational costs", "Measles-Follow-up campaign op.costs", "Meningitis A - mini catch-up op.costs", "IPV Catch-up RI", "MR-Catch-up campaign",
                        "MR-Follow-up campaign", "Measles-Catch-up campaign", "Measles-Follow-up campaign", "Penta - campaign", "Pneumo - campaign", "Td - campaign", "Merck - Adv purchase comm - Ebola",
                        "WHO - Vaccine pilot implementation", "IPV-Catch-up campaign", "IPV-Catch-up campaign op.costs") 

## flag programs
gavi_data[, `:=` (vaccine = ifelse(PROGRAM %in% vaccine_list, DISBURSEMENT_yr_paid, 0),
                  delivery = ifelse(PROGRAM %in% delivery_list, DISBURSEMENT_yr_paid, 0),
                  routine = ifelse(PROGRAM %in% routine_list, DISBURSEMENT_yr_paid, 0),
                  supplementary = ifelse(PROGRAM %in% supplementary_list, DISBURSEMENT_yr_paid, 0))]

## hard code in specific GIN ebola data
gavi_data[ihme_loc_id == "GIN" & YEAR == 2015 & PROGRAM == "Ebola EPI Recovery Plan",
          `:=` (vaccine = vaccine - 1900000 - 334518,
                supplementary = supplementary - 1900000 - 334518)]
gavi_data <- rbind(gavi_data, data.table(ihme_loc_id = "GIN", YEAR = 2015, 
                                         PROGRAM = "Ebola EPI Recovery Plan",
                                         DISBURSEMENT_yr_paid = 2234518, 
                                         vaccine = 0, delivery = 1, routine = 0, 
                                         supplementary = 1))  

## bind gavi data and tca gavi data
gavi_data <- rbind(gavi_data, tca_gavi_data, fill = T)

## get income groups and super regions
gavi_data <- get_ig(get_region(gavi_data), fgh2018 = T)


## aggregate gavi data by year, income group, and gbd super region
gavi_data <- gavi_data[, .(vaccine = sum(vaccine),
                           delivery = sum(delivery),
                           vd_other = 0,
                           routine = sum(routine),
                           supplementary = sum(supplementary),
                           sr_other = 0,
                           inkind = 0),
                       by = .(YEAR, ihme_loc_id)]

## aggregate gavi inkind data by year
setnames(inkind_data, "ISO3_RC", "ihme_loc_id")
inkind_data <- inkind_data[YEAR %in% years & nch_cnv_DAH > 0,
                           .(inkind = sum(nch_cnv_DAH),
                             vaccine = 0,
                             delivery = 0,
                             vd_other = 0,
                             routine = 0,
                             supplementary = 0,
                             sr_other = 0),
                           by = .(YEAR, ihme_loc_id)]

## bind gavi data and gavi inkind data
gavi_data <- rbind(gavi_data, inkind_data, fill = T)

## deflate to 2019 USD
gavi_data <- merge(gavi_data, deflator[, .(YEAR, GDP_deflator_2019)], by = c("YEAR"))
gavi_data[, c('vaccine', 'delivery', 'routine', 'supplementary', 'inkind') := .SD / GDP_deflator_2019,
          .SDcols = c('vaccine', 'delivery', 'routine', 'supplementary', 'inkind')]
gavi_data[, GDP_deflator_2019 := NULL]

## transform data wide to long
gavi_data <- melt.data.table(gavi_data, id.vars = c('YEAR', 'ihme_loc_id'), 
                  variable.name = 'component')

## write out data
fwrite(gavi_data, paste0(FIN,'gavi_data_country_level.csv'))
fwrite(gavi_data, paste0(FIN,'FILEPATH/gavi_data_country_level_', date, '.csv'))

## END OF FILE ##