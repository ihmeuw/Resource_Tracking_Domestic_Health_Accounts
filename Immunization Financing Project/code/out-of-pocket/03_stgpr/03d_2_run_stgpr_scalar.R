########################################################################################
## Register and launch an ST-GPR model from R to estimate OOP scalar
#
# Description: Sending ImFIn OOP models to ST-GPR
#
# Author: Matthew Schneider (adapted from Brandon Cummingham and Hayley Tymeson)
# Last edited: Emilie Maddison (ermadd@uw.edu), 20 May 2020

########################################################################################

## Clear environment
rm(list = ls())

## Set paths to directories
if (Sys.info()[1] == "Linux") {
  j <- FILEPATH
  h <- FILEPATH
}else if (Sys.info()[1] == "Windows") {
  j <- FILEPATH
  h <- FILEPATH
}
central_root <- FILEPATH
setwd(central_root)

## load ST-GPR launch functions
source('FILEPATH/register.R')
source('FILEPATH/sendoff.R')

## -------------------------------------------------------------------------------------

## Loop over all model_ids you wish to run
runlist = list()
for (i in c(20:21)) { 

  # i = 1
  
  ####################################
  # Set arguments
  ####################################
  
  path_to_config <- "FILEPATH/oop_scalar_config.csv"
  # Arguments
  me_name <- "imfin_oop_scalar"

  my_model_id <- i
  project <- 'proj_fgh'
  
  #number of parallelizations! More parallelizations --> faster (if cluster is emtpy). I usually do 50-100.
  nparallel <- 50
  
  #number of slots for each ST or GP job. You can profile by looking at a specific ST job through qacct.
  slots <- 5
  
  # Logs will be saved in
  # FILEPATH
  
  ####################################
  # Register an ST-GPR model
  ####################################
  
  run <- register_stgpr_model(
    path_to_config = path_to_config,
    model_index_id = my_model_id
  )
  
  # Submit ST-GPR model for your newly-created run_id!
  stgpr_sendoff(run, project, nparallel = nparallel) #,log_path = logs)
  
  ##creating a list of run_id's which you just launched
  run.dict <- data.frame(model_id = i, run_id = run)
  runlist[[i]] <- run.dict
}

## -------------------------------------------------------------------------------------

## Create dicitonary list of run_ids and model_ids
run.dictionary <- do.call(rbind, runlist)
run.dictionary$date <- format(Sys.time(), "%Y%m%d") ##Today's date
run.dictionary$status <- ''

## Read in existing dictionary and append newest run_ids and model_ids
output.dir <- FILEPATH
complete.dictionary <- read.csv(paste0(output.dir,
                                       "oop_scalar_runid_modelid_dictionary.csv"))
complete.dictionary <- rbind(complete.dictionary, run.dictionary)

## Write out dictionary which links model ids, run ids, date, and status.
write.csv(complete.dictionary, 
          paste0(output.dir, "oop_scalar_runid_modelid_dictionary.csv"), 
          row.names = FALSE)
