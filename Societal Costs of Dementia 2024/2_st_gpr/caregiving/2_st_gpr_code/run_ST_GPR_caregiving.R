##########################################################################
### Author: Michael Breshock
### Date: 09/12//2023
### Project: Global dementia spending
### Purpose: run ST-GPR for caregiving hours
##########################################################################
# Clean the environment 

rm(list=ls()) 

library(data.table)
st_gpr_dir <- "FILEPATH"

# Register and run a model 

source('FILEPATH/public.R')


# Specify cluster arguments for launch
cluster_project = "proj_fgh" 
nparallel = 100 #number of parallelizations
slots = 5 #number of slots for each ST or GP job. You can profile by looking at a specific ST job through qacct.
logs <- '/share/temp/sgeoutput/mrb123/errors'
holdouts = 0 # Keep at 0 unless you want to run cross-validation
model_num = 15 # update this to be the model_index_id you want to use from the config file

# Register an ST-GPR model using a config for non-decom run. 
run_id <- register_stgpr_model(paste0(st_gpr_dir,'caregiving_config.csv'), 
                               model_index_id = model_num)
run_id

# Submit ST-GPR model for your newly-created run_id!
stgpr_sendoff(run_id, cluster_project,
              nparallel = nparallel,
              log_path = logs)

# check model's status
get_model_status(version_id = run_id)

# continuously check ST-GPR status until it is done: 
stgpr_status = get_model_status(version_id = run_id)
while(stgpr_status == 2){
  Sys.sleep(30) # wait 30 seconds in between checks 
  stgpr_status = get_model_status(version_id = run_id)
}

# read_draws
## list all files in results directory
results_dir <- paste0("FILEPATH", run_id, "/draws_temp_0/")
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

# reverse logit transform on st-gpr draws: 
inverse_scaled_logit <- function(y,a,b){
  # Function to reverse scaled_logit transformation
  result <- ((b-a)*exp(y))/(1+exp(y))+a
  return(result) 
}
results[, care_hours := pmin(inverse_scaled_logit(value, 0, 169),168)] # also cap at 168

fwrite(results,
       paste0('FILEPATH', model_num, '.csv'), 
       row.names = FALSE)

# Reading outputs from models
cg_hours_stgpr <- get_estimates(run_id, 'final')

cg_hours_stgpr[, ":=" (care_hours = pmin(inverse_scaled_logit(val, 0, 169),168),
                       care_hours_lower = pmin(inverse_scaled_logit(lower, 0, 169),168),
                       care_hours_upper = pmin(inverse_scaled_logit(upper, 0, 169),168))]

fwrite(cg_hours_stgpr,
       paste0('FILEPATH', model_num, '.csv'),
       row.names = FALSE)
