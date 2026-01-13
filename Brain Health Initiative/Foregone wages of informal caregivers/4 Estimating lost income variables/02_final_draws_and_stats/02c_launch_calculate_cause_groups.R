#### #----#                    Docstring                    #----# ####
#' Title:    02c_launch_calculate_cause_groups.R
#' Project:  IHME Brain Health Initiative
#' Purpose : Launch calculate_cause_groups.R script in a loop by analysis
#'
#' Author: USERNAME
#' Date: 03/11/2025
#' Last Updated: 09/30/2025
#---------------------------------------------------------------------#

################### #----# ENVIRONMENT SETUP #----# ###################
cat(paste0(" Environment Setup \n"))

#----# Set up directory roots #----#
rm(list=ls())
if (!exists("code_repo"))  {
  code_repo <- unname(ifelse(Sys.info()['sysname'] == "Windows", "ROOT", paste0("ROOT", Sys.info()['user'][1], "ROOT")))
}

## Variable prep
currentDate <- format(Sys.time(), "%Y%m%d")
gbd_release_id <- ID
years <- 2000:2021

## Paths
bhi_share <- 'FILEPATH'
data_path <- paste0(bhi_share, 'FILEPATH')

## CLUSTER SLURM variables
user <- Sys.info()[7]
project <- "PROJECT_NAME" 
output_path <- paste0(data_path, 'FILEPATH')
queue <- "QUEUE_NAME"
job_threads <- 1
job_mem <- 30
runtime <- "00:30:00"

#---------------------------------------------------------------------#

## Launch jobs
## Loops over causes
for (version in c('reg', 'sens1', 'sens2')) {
  cat('submitting job for version:', version, ' ')
  mysub <- paste0("sbatch -e ", output_path, version, "FILEPATH -o ", output_path, version, "FILEPATH -J bhi_all_cause_", version, " -C archive -p ", queue, " -A PROJECT_NAME -c ",
                  job_threads, " --mem ", job_mem, "G -t ", runtime, " ", 
                  "FILEPATH -s FILEPATH", user, "FILEPATH ", version)
  system(mysub)
}

## End of Script ##