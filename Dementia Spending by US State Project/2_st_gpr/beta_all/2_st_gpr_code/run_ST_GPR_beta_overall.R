##########################################################################
### Author: Michael Breshock
### Date: 08/04/2023
### Project: US dementia spending
### Purpose: run ST-GPR for diagnosis rates (beta, overall rate, living condition ignored)
##########################################################################
# Clean the environment 

rm(list=ls()) 


library(data.table)
st_gpr_dir <- "FILEPATH"

# Register and run a model 

source('FILEPATH/public.R')


# Specify cluster arguments for launch
cluster_project = "proj_fgh" # "ihme_general"
nparallel = 100 #number of parallelizations
slots = 5 #number of slots for each ST or GP job. You can profile by looking at a specific ST job through qacct.
logs <- 'FILEPATH'
holdouts = 0 # Keep at 0 unless you want to run cross-validation
draws = 100 
model_num = 6 # update this to be the model_index_id you want to use from the config file

# Register an ST-GPR model using a config for non-decom run. 
run_id <- register_stgpr_model(paste0(st_gpr_dir,'beta_overall_config.csv'), model_index_id = model_num)
run_id

# Submit ST-GPR model for your newly-created run_id!
stgpr_sendoff(run_id, cluster_project,
              nparallel = nparallel,
              log_path = logs)

# check model's status
get_model_status(version_id = run_id)

# read_draws
## list all files in results directory
results_dir <- "FILEPATH"
files <- list.files(results_dir)

## function to read, melt, and select
read_stgpr_draws <- function(x){
  draw <- fread(x)
  draw <- melt(draw, id.vars = c("location_id", "year_id", "age_group_id", "sex_id"))
  draw <- draw[, .(location_id, year_id, variable, value)]
  return(draw)
}

## read, melt, select from, and bind all results
results <- rbindlist(lapply(paste0(results_dir, files), read_stgpr_draws))

fwrite(results,
       paste0('FILEPATH', model_num, '.csv'), 
       row.names = FALSE)

# Reading outputs from models
dx_rates_stgpr <- get_estimates(run_id, 'final')

fwrite(dx_rates_stgpr,
       paste0('FILEPATH', model_num, '.csv'),
       row.names = FALSE)

