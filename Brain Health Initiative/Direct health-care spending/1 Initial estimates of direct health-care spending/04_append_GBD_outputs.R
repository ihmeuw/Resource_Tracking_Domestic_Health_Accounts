#### #----#                    Docstring                    #----# ####
#' Title:    04_append_GBD_outputs.R
#' Project:  Resource Tracking - Brain Health Initiative (BHI)
#' Purpose : Append datasets pulled from GBD for causes and risk factors
#' from DEX specific acauses.
#'           
#'     
#' Author: USERNAME
#' Date: 2023-02-28
#---------------------------------------------------------------------#

#----# Set up directory roots #----#
rm(list=ls())
if (!exists("code_repo"))  {
  code_repo <- unname(ifelse(Sys.info()['sysname'] == "Windows", "ADDRESS", paste0("ADDRESS", Sys.info()['user'][1], "ADDRESS")))
}

## Source functions
source(paste0(code_repo, 'FILEPATH'))
source(paste0(dah.roots$k, "FILEPATH"))
source(paste0(dah.roots$k, "FILEPATH"))
source(paste0(dah.roots$k, "FILEPATH"))

#----# Local CONSTANTS #----#
## Variable prep
data_yr <- 2019
currentDate <- format(Sys.time(), "%Y%m%d")
gbd_release_id <- 16
years <- 2000:(data_yr)

## Paths
bhi_share <- paste0('FILEPATH')
raw_data_path <- paste0(bhi_share, 'FILEPATH')
int_data_path <- paste0(bhi_share, 'FILEPATH')
fin_data_path <- paste0(bhi_share, 'FILEPATH')


###~~~~~ list ~~~~~###

incidence_causes <- c(
  '_enteric_all', '_ntd',
  'diptheria', 'encephalitis', 'hepatitis_a', 'hepatitis_b', 'hepatitis_c', 'hepatitis_e', 'hiv', 'infectious', 
  'inj_animal', 'inj_disaster', 'inj_drowning', 'inj_electrocution', 'inj_falls', 'inj_fires', 'inj_foreign', 'inj_homicide',
  'inj_mech', 'inj_non_disaster', 'inj_othunintent', 'inj_poisoning', 'inj_suicide', 'inj_trans', 'inj_war_execution', 'inj_war_warterror',
  'lri',
  'maternal_abort_ectopic', 'maternal_abort_mis', 'maternal_hem', 'maternal_hiv', 'maternal_htn', 'maternal_indirect',
  'maternal_late', 'maternal_obstruct', 'maternal_other', 'maternal_sepsis', 'maternal_diabetes', 'maternal_cardiomyopathy',
  'measles', 'meningitis', 
  'neo_bladder', 'neo_bone', 'neo_brain', 'neo_breast', 'neo_cervical', 'neo_colorectal', 'neo_esophageal', 'neo_eye',
  'neo_gallbladder', 'neo_hodgkins', 'neo_kidney', 'neo_larynx', 'neo_leukemia', 'neo_liver', 'neo_lung', 'neo_lymphoma',
  'neo_melanoma', 'neo_meso', 'neo_mouth', 'neo_myeloma', 'neo_nasopharynx', 'neo_neuro', 'neo_nmsc', 'neo_other_benign',
  'neo_other_cancer', 'neo_otherpharynx', 'neo_ovarian', 'neo_pancreas', 'neo_prostate', 'neo_stomach', 'neo_testicular',
  'neo_thyroid', 'neo_tissue_sarcoma', 'neo_uterine',
  'neonatal_enceph', 'neonatal_hemolytic', 'neonatal_other', 'neonatal_preterm', 'neonatal_sepsis',
  'otitis', 'resp_pneum', 'std', 'tb', 'tetanus', 'uri', 'varicella', 'whooping') 

#---------------------------------------------------------------------#

cat("\n\n")
cat(green("\t############################################\n"),
    green("\t#######   BEGIN append_GBD_outputs   #######\n"),
    green("\t############################################\n\n"))


################### #----# Main #----# ###################
####----# Get Data #----####
cat('Get Data\n')

## Reading exp dataset with welfare datasets
## to be appended at the end
dt_exp <- fread(paste0(int_data_path, "FILEPATH"))
dt_exp <- dt_exp[year_id %in% years]
dt_exp[, `:=` (dex_acause = acause, rei_acause_frct = 1)]

## Read in DEX/GBD cause map
cat('Get DEX cause metadata\n')
dex_cause_dt <- fread(paste0(raw_data_path, 'FILEPATH'))
colnames(dex_cause_dt) <- tolower(gsub(" ", "_", names(dex_cause_dt)))
dex_rei_dt <- copy(dex_cause_dt)

## Subset REI causes and reassign GBD causes to DEX causes where necessary
dex_cause_dt <- dex_cause_dt[acause_group != 'rei']
dex_cause_dt[, dex_cause_id := ifelse(is.na(reassigned_id), gbd_cause_id, reassigned_id)]
dex_rei_dt <- dex_rei_dt[acause_group == 'rei']

## Vectors of GBD output files
all <- list.files(paste0(int_data_path, 'FILEPATH'))
all <- all[grepl('csv', all) & !grepl('all_', all)]
risk_files <- all[grepl('rei', all)]
cause_files <- all[!grepl('rei', all)]

id_cols <- c('age_group_id', 'cause_id', 'location_id', 'metric_id', 'sex_id', 'year_id', 
             'acause', 'age_group_name', 'cause_name', 'location_name', 'measure', 'sex')
cols_to_keep <-  c(id_cols, 'mean')

## Launch jobs
## Loops over dex cause groups
dt_cause <- data.table()
gcount <- length(cause_files)
for (group in cause_files) {
  cat(gcount, ' files remaining\n', 'getting data for group: ', gsub('.csv', '', group), '\n')
  temp <- fread(paste0(int_data_path, 'FILEPATH', group))
  
  temp[age_group_id %in% c(5, 28), `:=` (age_group_id = 1, age_group_name = '<5 years')]
  temp <- temp[, .(mean = sum(mean, na.rm = T)),
               by = id_cols]
  temp <- dcast.data.table(temp, 
                           formula = age_group_id + cause_id + location_id + metric_id + sex_id + year_id + 
                             acause + age_group_name + cause_name + location_name + sex ~ measure,
                           value.var = 'mean')
  temp[, `:=` (gbd_topic = 'cause', gbd_topic_id = paste0('cause - ', cause_name), gbd_acause_id = cause_id)]
  temp <- merge(temp, dex_cause_dt[, .(gbd_cause_id, dex_acause, dex_cause_id)], by.x = c('cause_id'), by.y = c('gbd_cause_id'), all.x = T)
  dt_cause <- rbind(dt_cause, temp, fill = T)
  gcount <- gcount - 1
  rm(temp)
}


dt_rei <- data.table()
gcount <- length(risk_files)
for (group in risk_files) {
  cat(gcount, ' files remaining\n', 'getting data for group: ', gsub('.csv', '', group), '\n')
  temp <- fread(paste0(int_data_path, 'FILEPATH', group))
  
  temp[age_group_id %in% c(5, 28), `:=` (age_group_id = 1, age_group_name = '<5 years')]
  temp <- temp[, .(mean = sum(mean, na.rm = T)),
               by = c(id_cols, 'rei_id', 'rei', 'rei_name')]
  
  temp <- dcast.data.table(temp, 
                           formula = age_group_id + cause_id + location_id + metric_id + sex_id + year_id + 
                             acause + age_group_name + cause_name + location_name + sex + rei + rei_name + rei_id ~ measure,
                           value.var = 'mean')
  
  temp[, `:=` (gbd_topic = rei, gbd_topic_id = paste0('rei - ', rei_name), gbd_acause_id = rei_id)]
  temp[, `:=` (rei = NULL, rei_name = NULL)]
  temp <- merge(temp, dex_rei_dt[, .(gbd_cause_id, dex_acause)], by.x = c('rei_id'), by.y = c('gbd_cause_id'), all.x = T)
  temp[, dex_cause_id := cause_id]
  dt_rei <- rbind(dt_rei, temp, fill = T)
  gcount <- gcount - 1
  rm(temp)
}


#---------------------------------------------------------------------#

#### ## Computing fraction reallocation for risk factors to acause ## ####
cat(' Computing fraction reallocation for risk factors to acause \n')

## Cause dataset should be 1:1 to creating a column setting fraction value to 1
dt_cause[, rei_acause_frct := 1]

## Assigning applicable mean values by incidence or prevalence
dt_cause[, measure := '']
dt_cause[acause %in% incidence_causes, `:=` (mean_value = ifelse(!is.na(incidence), incidence, daly), measure = ifelse(!is.na(incidence), 'incidence', 'daly'))]
dt_cause[is.na(mean_value), `:=` (mean_value = ifelse(!is.na(prevalence), prevalence, daly), measure = ifelse(!is.na(prevalence), 'prevalence', 'daly'))]
dt_cause[mean_value < 0, mean_value := 0]

## Computing the fraction each (year-location-age-sex) risk factor has for their acauses
## creating a single fraction for risk causes from the prevalence or dalys
## heart failure is an impairment, so we use prevalence
## the other four rei are risks, so we use dalys
dt_rei[, measure := '']
dt_rei[, `:=` (mean_value = ifelse(gbd_topic == 'imp_hf', prevalence, daly), measure = ifelse(gbd_topic == 'imp_hf', 'prevalence', 'daly'))]
dt_rei[mean_value < 0, mean_value := 0]

## Calculating year-location-age-sex risk factor fraction
dt_rei[, tot_ylas_rf_mean := lapply(.SD, sum, na.rm = T), by = c('year_id', 'location_id', 'age_group_id', 'sex_id', 'rei_id'),
       .SDcols = c('mean_value')]
dt_rei[, frct_ylas_rf_value := mean_value / tot_ylas_rf_mean]

## Creating a single fraction for risk causes from the prevalence or dalys
dt_rei[, rei_acause_frct := if_else(!is.na(frct_ylas_rf_value), frct_ylas_rf_value, 0)]

## Removing columns used for computing, no longer needed
dt_rei[, c('frct_ylas_rf_value', 'tot_ylas_rf_mean') := NULL]

#---------------------------------------------------------------------#

#### ## Saving dataset ## ####
cat(' Saving dataset \n')

## Write out GBD cause and rei datasets
fwrite(dt_cause, file = paste0(int_data_path, "FILEPATH"))
fwrite(dt_rei, file = paste0(int_data_path, "FILEPATH"))
fwrite(rbind(dt_cause, dt_rei, dt_exp, fill = T), file = paste0(int_data_path, "FILEPATH"))

## Write out as feather files
arrow::write_feather(dt_cause, paste0(int_data_path, "FILEPATH"))
arrow::write_feather(dt_rei, paste0(int_data_path, "FILEPATH"))
arrow::write_feather(rbind(dt_cause, dt_rei, dt_exp, fill = T), paste0(int_data_path, "FILEPATH"))

## End of Script##