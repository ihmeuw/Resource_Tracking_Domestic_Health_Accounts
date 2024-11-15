## ---------------------------------------------------------------------------------------------------------------------
## DAH Adjustment of WHO categorized Malaria spending data
## Author: USERNAME
## Date: 25 May 2022
## Description: Use proportions from IHME's Malaria DAH estimates, to subtract WHO-reported DAH spending from
## WHO-reported categorized spending data, in order to estimate `Categorized Government Malaria Spending`
## ---------------------------------------------------------------------------------------------------------------------
## 1. Environment Prep ## ----------------------------------------------------------------------------------------------
# System prep
rm(list=ls())

if (!exists("code_repo"))  {
  code_repo <- unname(ifelse(Sys.info()['sysname'] == "Windows", 
                             "ADDRESS", 
                             paste0("ADDRESS")))
}

adate <- Sys.Date()

## Source functions
library(stringr)
library(data.table)
source(paste0(code_repo,'FILEPATH'))
source(paste0(code_repo,'FILEPATH'))

## Set FGH round ID
get_fgh_round_ids()
fgh_round_id <- 13

## 2. Read in raw data ## ----------------------------------------------------------------------------------------------
## WHO data with proportions
who_all_dt <- fread("FILEPATH")
## Cleaned WHO categorized spending data (constant USD 2021)
who_dt <- fread(paste0('FILEPATH'))
## IHME DAH categorized data
adb_pdb0 <- fread(paste0("FILEPATH"))
adb_pdb <- copy(adb_pdb0)

## 3. Prep column name dictionary ## -----------------------------------------------------------------------------------

dictionary <- data.table(who_name = c("Human Resources",
                                      "ITNs/PBOs", 
                                      "Insecticide & spraying materials", 
                                      "Diagnostics", 
                                      "Anti-malarial medicines", 
                                      "Procurement & supply management", 
                                      "Infrastructure & equipment", 
                                      "Communication and advocacy", 
                                      "Monitoring and Evaluation", 
                                      "Planning, administration, overheads", 
                                      "Other"), 
                         dah_name = c('mal_hss_hrh_DAH_21',
                                      'mal_con_nets_DAH_21', 
                                      'mal_con_irs_DAH_21',
                                      'mal_diag_DAH_21', 
                                      'mal_med_DAH_21',
                                      'mal_proc_DAH_21',
                                      'mal_infra_DAH_21',
                                      'mal_comm_con_DAH_21', 
                                      'mal_hss_me_DAH_21',
                                      'mal_plan_DAH_21',
                                      'mal_all_other_DAH_21'))

## 4. Prep WHO dataset -------------------------------------------------------------------------------------------------
## Subset WHO data to those with both categorized values and government spending values
who_all_dt1 <- who_all_dt[disaggregated_total > 0 & government_contribution > 0]
names(who_all_dt1)

## Merge on categorized spending dataset
who_comb_dt <- merge(who_dt, who_all_dt1, by = c('ihme_loc_id', 'year_id'), all.y = T)

## Scale categorized values so that they sum to total contributions
who_comb_dt[, value_adj := value / total_cat * contribution_total]
setnames(who_comb_dt, 'variable', 'who_name')

## 5. Prep ADB PDB: IHME DAH data ## -----------------------------------------------------------------------------------
adb_pdb <- adb_pdb[!(ELIM_CH == 1 | ELIM_DONOR == 1)]

names(adb_pdb)

## Isolate malaria program areas
adb_pdb <- adb_pdb[, .(ISO3_RC, YEAR,
                       mal_diag_DAH_21, mal_con_nets_DAH_21, mal_con_irs_DAH_21, mal_con_oth_DAH_21, mal_treat_DAH_21,
                       mal_comm_con_DAH_21, mal_amr_DAH_21, mal_other_DAH_21, 
                       mal_hss_other_DAH_21, 
                       mal_hss_hrh_DAH_21,
                       mal_hss_me_DAH_21, mal_DAH_21)]

adb_pdb <- adb_pdb[, lapply(.SD, sum, na.rm=TRUE), by = c('ISO3_RC', 'YEAR')]
adb_pdb <- adb_pdb[mal_DAH_21 > 0,]

## proc/infra/plan fractions 
hss_other_frac <- who_comb_dt[who_name %in% c('Procurement & supply management', 'Planning, administration, overheads',
                            'Infrastructure & equipment')]
hss_other_frac <- hss_other_frac[, .(value_adj = sum(value_adj, na.rm = T)), .(who_name, super_region_name)]
hss_other_frac <- hss_other_frac[, `:=`(total = sum(value_adj, na.rm = T)), .(super_region_name)]
hss_other_frac[, frac := value_adj / total]
hss_other_frac <- dcast(hss_other_frac, super_region_name ~ who_name, value.var = 'frac')

## Create groups of interest, align WHO and IHME data categories
setnames(adb_pdb, 'ISO3_RC', 'ihme_loc_id')
adb_pdb <- get_region(adb_pdb)

adb_pdb <- merge(adb_pdb, hss_other_frac, by = 'super_region_name')

adb_pdb[, `:=`(mal_med_DAH_21 = mal_treat_DAH_21 + mal_amr_DAH_21, 
               mal_all_other_DAH_21 = mal_con_oth_DAH_21 + mal_other_DAH_21)]

adb_pdb[, `:=`(mal_proc_DAH_21 = mal_hss_other_DAH_21 * `Procurement & supply management`, 
               mal_infra_DAH_21 = mal_hss_other_DAH_21 * `Infrastructure & equipment`, 
               mal_plan_DAH_21 = mal_hss_other_DAH_21 * `Planning, administration, overheads`)]

stopifnot(round(sum(adb_pdb$mal_proc_DAH_21) + sum(adb_pdb$mal_infra_DAH_21) + sum(adb_pdb$mal_plan_DAH_21), 0) ==
           round(sum(adb_pdb$mal_hss_other_DAH_21), 0))

adb_pdb[, `:=`(mal_treat_DAH_21 = NULL, mal_amr_DAH_21 = NULL, mal_con_oth_DAH_21 = NULL, mal_other_DAH_21 = NULL,
               mal_hss_other_DAH_21 = NULL, `Procurement & supply management` = NULL,
               `Infrastructure & equipment` = NULL, `Planning, administration, overheads` = NULL)]

setnames(adb_pdb, 'ihme_loc_id', "ISO3_RC")
adb_pdb <- adb_pdb[, .(ISO3_RC, YEAR,
                       mal_diag_DAH_21, mal_con_nets_DAH_21, mal_con_irs_DAH_21,
                       mal_all_other_DAH_21, mal_med_DAH_21,
                       mal_comm_con_DAH_21,
                       mal_proc_DAH_21, mal_infra_DAH_21, mal_plan_DAH_21,
                       mal_hss_hrh_DAH_21,
                       mal_hss_me_DAH_21,
                       mal_DAH_21)]

## Create fractions
adb_pdb1 <- adb_pdb[, lapply(.SD, '/', mal_DAH_21),  by = c('ISO3_RC', 'YEAR')]
adb_pdb1[, total := rowSums(.SD), .SDcols = 3:13]
stopifnot(round(adb_pdb1$mal_DAH_21, 2) == round(adb_pdb1$total, 2))

summary(adb_pdb1$mal_DAH_21)
summary(adb_pdb1$total)

fwrite(adb_pdb1, 'FILEPATH')

## Reshape to align with WHO dataset
adb_pdb1[, `:=`(mal_DAH_21 = NULL, total = NULL)]
adb_pdb1 <- data.table::melt(adb_pdb1, 
                             id.vars = c('ISO3_RC', 'YEAR'), 
                             variable.name = 'dah_name', 
                             value.name = 'dah_frac')
adb_pdb1[is.na(dah_frac), dah_frac := 0]
setnames(adb_pdb1, c('ISO3_RC', 'YEAR'), c('ihme_loc_id', 'year_id'))

adb_pdb1 <- merge(adb_pdb1, dictionary, by = 'dah_name')

## 6. Adjust WHO data using IHME DAH data ## ---------------------------------------------------------------------------

who_comb_dt1 <- merge(who_comb_dt, adb_pdb1, by = c('ihme_loc_id', 'year_id', 'who_name'), all.x = T)
who_comb_dt1 <- who_comb_dt1[!is.na(value)]
who_comb_dt1[is.na(dah_frac), dah_frac := 0]

## IHME DAH adjustment
## Calculate DAH spending to subtract
who_comb_dt1[, dah_subtract := donor_contribution_total * dah_frac]
## Calculate government spending by removing DAH
## This will be larger than reported to WHO if DAH categorized spending is larger than categorized spending

who_comb_dt1[, value_ghes := value_adj - dah_subtract]
who_comb_dt1[value_ghes < 0, value_ghes := 0]

## WHO GHES adjustment
who_comb_dt1[, `:=`(sum_value_ghes = sum(value_ghes)), .(ihme_loc_id, year_id)]
who_comb_dt1[, value_ghes_adj := value_ghes / sum_value_ghes * government_contribution]

who_comb_dt1[, `:=`(total_cat = NULL)]

who_adjusted_dt <- who_comb_dt1[, .(ihme_loc_id, year_id, who_name, super_region_name, Country, dah_name, 
                                    value_ghes = value_ghes_adj)]


## 7. Write out dataset ## ---------------------------------------------------------------------------------------------
fwrite(who_adjusted_dt, paste0('FILEPATH'))

## End of Script ## 