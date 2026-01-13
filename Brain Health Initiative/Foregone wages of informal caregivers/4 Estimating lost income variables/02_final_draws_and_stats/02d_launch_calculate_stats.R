#### #----#                    Docstring                    #----# ####
#' Title:    02d_launch_calculate_stats.R
#' Project:  Brain Health Initiative
#' Purpose : Launch calculate_stats.R script in a loop by cause
#'     
#' Author: USERNAME
#' Date: 03/11/2025
#' Last Updated: 09/30/2025
#---------------------------------------------------------------------#

pacman::p_load(data.table, dplyr, cli, plotrix, arrow, stringr)
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
bhi_causes <- bhi_causes[!cause_id %in% c(557, 563:566), .(cause_id, cause_name)]
bhi_causes[, cause_id := as.character(cause_id)]
bhi_causes <- rbind(bhi_causes,
                    data.table(cause_id = c('9999', 'mental', 'neuro', 'all_cause'),
                               cause_name = c('Non-opioid drug use disorders', 'Mental causes', 'Neurological causes', 'All causes')))

## CLUSTER SLURM variables
user <- Sys.info()[7]
project <- "PROJECT_NAME" 
output_path <- paste0(data_path, 'FILEPATH')
queue <- "QUEUE_NAME"
job_threads <- 1
job_mem <- 35
runtime <- "01:30:00"

#---------------------------------------------------------------------#

## Launch jobs
## Loops over causes
for (cause in bhi_causes$cause_id) {
  cat('submitting job for cause ID', cause, ':', bhi_causes[cause_id == cause]$cause_name, ' ')
  mysub <- paste0("sbatch -e ", output_path, cause, "FILEPATH -o ", output_path, cause, "FILEPATH -J bhi_stats_", cause, " -C archive -p ", queue, " -A PROJECT_NAME -c ",
                  job_threads, " --mem ", job_mem, "G -t ", runtime, " ", 
                  "FILEPATH -s FILEPATH", user, "FILEPATH ", cause)
  system(mysub)
}

## End of Script ##