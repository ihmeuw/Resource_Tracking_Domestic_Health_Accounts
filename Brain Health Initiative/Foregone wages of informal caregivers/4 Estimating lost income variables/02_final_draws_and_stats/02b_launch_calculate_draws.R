#### #----#                    Docstring                    #----# ####
#' Title:    02b_launch_calculate_draws.R
#' Project:  Brain Health Initiative
#' Purpose : Launch calculate_draws.R script in a loop by cause
#'     
#' Author: USERNAME
#' Date: 03/11/2025
#' Last Updated: 09/30/2025
#---------------------------------------------------------------------#

pacman::p_load(data.table, dplyr, arrow)
rm(list = ls())

################### #----# ENVIRONMENT SETUP #----# ###################
cat(paste0(" Environment Setup \n"))

#----# Set up directory roots #----#
if (!exists("code_repo"))  {
  code_repo <- unname(ifelse(Sys.info()['sysname'] == "Windows", "ROOT", paste0("ROOT", Sys.info()['user'][1], "ROOT")))
}

## Parameters
gbd_release_id <- ID

## Source functions
source('FILEPATH')

## Filepaths
bhi_share <- 'FILEPATH'
data_path <- paste0(bhi_share, 'FILEPATH')

## Lists
causes <- get_cause_metadata(cause_set_id = ID, release_id = gbd_release_id)
bhi_causes <- causes[parent_id %in% c(542, 558, 561) | acause %in% c('cvd_stroke', 'encephalitis', 'inj_suicide', 'meningitis', 'mental_alcohol', 'neo_brain')]
bhi_causes <- bhi_causes[!cause_id %in% c(557, 563:566)]
bhi_causes <- bhi_causes[, .(cause_id, cause_name)]
bhi_causes <- rbind(bhi_causes, data.table(cause_id = 9999,
                                           cause_name = 'Non-opioid drug use disorders'))

## CLUSTER variables
user <- Sys.info()[7]
project <- "PROJECT_NAME" 
output_path <- paste0(data_path, 'FILEPATH')
queue <- "QUEUE_NAME"
job_threads <- 1
job_mem <- 30
runtime <- "00:45:00"

#---------------------------------------------------------------------#

## Launch jobs
## Loops over causes
for (cause in bhi_causes$cause_id) {
  cat('submitting job for cause ID', cause, ':', bhi_causes[cause_id == cause]$cause_name, ' ')
  mysub <- paste0("sbatch -e ", output_path, cause, "FILEPATH -o ", output_path, cause, "FILEPATH -J bhi_", cause, " -C archive -p ", queue, " -A PROJECT_NAME -c ",
                  job_threads, " --mem ", job_mem, "G -t ", runtime, " ", 
                  "FILEPATH -s FILEPATH", user, "FILEPATH ", cause)
  system(mysub)
}

## End of Script ##