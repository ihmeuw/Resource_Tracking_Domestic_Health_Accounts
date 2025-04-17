#### #----#                    Docstring                    #----# ####
#' Title: 07_finalize_bh_causes.R
#' Project:  Resource Tracking - Brain Health Initiative (BHI)
#' Purpose : Disaggregate spending on encephalitis
#' and create a binary indicator for brain health causes
#'           
#' Author: USERNAME
#' Date: 2024-03-08
#---------------------------------------------------------------------#


#----# Set up directory roots #----#
rm(list=ls())
if (!exists("code_repo"))  {
  code_repo <- unname(ifelse(Sys.info()['sysname'] == "Windows", "ADDRESS", paste0("ADDRESS", Sys.info()['user'][1], "ADDRESS")))
}

## Source functions
source(paste0(code_repo, 'FILEPATH'))
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


#---------------------------------------------------------------------#

cat("\n\n")
cat(green("\t################################################\n"),
    green("\t#######   BEGIN 6_finalize_bh_causes.R   #######\n"),
    green("\t################################################\n\n"))


################### #----# Main #----# ###################
####----# Read and process data #----####
cat(' Read and process data\n')

## Read in raked estimates
estimates <- fread(paste0(int_data_path, 'FILEPATH'))

####----# Label brain health causes #----####
cat(' Label brain health causes\n')

## Brain health causes
brain_health_causes <- c(332,    #  meningitis
                         337,    #  encephalitis
                         543,    #  neuro_dementia
                         544,    #  neuro_parkinsons
                         545,    #  neuro_epilepsy
                         546,    #  neuro_ms
                         554,    #  neuro_neurone
                         559,    #  mental_schizo
                         567,    #  mental_unipolar
                         570,    #  mental_bipolar
                         571,    #  mental_anxiety
                         572,    #  mental_eating
                         575,    #  mental_pdd
                         578,    #  mental_adhd
                         579,    #  mental_conduct
                         582,    #  mental_id
                         585,    #  mental_other
                         560,    #  mental_alcohol
                         562,    #  mental_drug_opioids
                         566,    #  mental_drug_other
                         477,    #  neo_brain
                         718,    #  inj_suicide
                         494,    #  cvd_stroke
                         972)    #  neuro_headach

## Create binary indicator
estimates[, bh_cause := ifelse(cause_id %in% brain_health_causes, 1, 0)]

## Update mental_drug_other cause_name
estimates[cause_name == 'Other substance use disorders', `:=` (cause_name = 'Non-opioid drug use disorders', cause_id = 9999)]

####----# Add currency information, order and sort columns #----####
cat(' Add currency information, order, and sort columns\n')

## Add currency columns
estimates[, `:=` (currency = 'USD', currency_year = 2021, currency_converter_version = 7)]

## Rename mean_value column to provide more context
setnames(estimates, 'mean_value', 'incident_prevalent_cases')

## Reorder columns
setcolorder(estimates,
            c('location_id', 'location_name',
              'year_id',
              'sex_id', 'sex',
              'age_group_id', 'age_group_name',
              'cause_id', 'acause', 'cause_name', 'bh_cause',
              'metric_id', 'incident_prevalent_cases',
              'AM', 'DV', 'ED', 'HH', 'IP', 'NF', 'RX',
              'currency', 'currency_year', 'currency_converter_version'))

## Sort data
estimates <- estimates[order(location_id, year_id, sex_id, age_group_id, cause_id)]


####----# Save out final estimates #----####
cat(' Save out final estimates\n')

## Write out all cause dataset
fwrite(estimates, paste0(fin_data_path, 'FILEPATH'))

## Write out brain health cause dataset
fwrite(estimates[bh_cause == 1], paste0(fin_data_path, 'FILEPATH', currentDate, '.csv'))
fwrite(estimates[bh_cause == 1], paste0(fin_data_path, 'FILEPATH'))

## End of Script ##