################################################################################
# Takes config file for LIS data and sends job to ST-GPR
# for ST-GPR
# Script "a" is for the version of the model that contains wages disaggregated by
# age, whereas script "b" is for wage data that has been re-weighted to the 
# average across ages.
# Author: Elye Bliss
# Date: Aug 22, 2023
################################################################################


# Clean the environment 
rm(list=ls()) 


# Register and run a model 
source('FILEPATH/public.R')

# Specify cluster arguments for launch
cluster_project = "proj_fgh"
nparallel = 50 #number of parallelizations
slots = 5 #number of slots for each ST or GP job. You can profile by looking at a specific ST job through qacct.
logs <- 'FILEPATH'
holdouts = 0 # Keep at 0 unless you want to run cross-validation
draws = 100 
model_num = 6 # update this to be the model_index_id you want to use from the config file


# Register an ST-GPR model using a config for non-decom run.
st_gpr_dir <- "FILEPATH"
run_id <- register_stgpr_model(paste0(st_gpr_dir,'LIS_age_config.csv'), 
                               model_index_id = model_num)
run_id # Note run_id for reference, run_id <- 208351

# Submit ST-GPR model for your newly-created run_id!
stgpr_sendoff(run_id, cluster_project)

# continuously check ST-GPR status until it is done: 
stgpr_status = get_model_status(version_id = run_id)
while(stgpr_status == 2){
  stgpr_status = get_model_status(version_id = run_id)
  Sys.sleep(30) # wait 30 seconds in between checks 
}

# read_draws
## list all files in results directory
results_dir <- paste0("FILEPATH/", run_id, "/draws_temp_0/")
files <- list.files(results_dir)

## function to read, melt, and select
read_stgpr_draws <- function(x){
  draw <- fread(x)
  draw <- melt(draw, id.vars = c("location_id", "year_id", "age_group_id", "sex_id"))
  setDT(draw)
  draw <- draw[, .(location_id, year_id, variable, value)]
  return(draw)
}

## read, melt, select from, and bind all results
results <- rbindlist(lapply(paste0(results_dir, files), read_stgpr_draws))
# Because the results of this model fit has different levels for
# each age group, the results are so big that R might crash. Requesting 200G of 
# memory seemed to resolve the problem.

fwrite(results, paste0('FILEPATH',
                       model_num,
                       '.csv'), row.names = FALSE)

# Reading outputs from models:  Allowed values: 'stage1', 'spacetime', 'gpr', 'final'.
rates_stgpr <- get_estimates(run_id, 'final')

fwrite(rates_stgpr,paste0('FILEPATH',
                          model_num,
                          '.csv'), row.names = FALSE)
