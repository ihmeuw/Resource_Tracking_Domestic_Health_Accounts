##########################################################################
### Author: Michael Breshock
### Date: 09/29/2023
### Project: Global dementia spending
### Purpose: run ST-GPR for caregiver gender
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
logs <- 'FILEPATH'
model_num = 5 # update this to be the model_index_id you want to use from the config file

# Register an ST-GPR model using a config for non-decom run. 
run_id <- register_stgpr_model(paste0(st_gpr_dir,'gender_config.csv'), 
                               model_index_id = model_num)
run_id

# Submit ST-GPR model for your newly-created run_id
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
gender_stgpr <- get_estimates(run_id, 'final')

fwrite(gender_stgpr,
       paste0('FILEPATH', model_num, '.csv'),
       row.names = FALSE)
