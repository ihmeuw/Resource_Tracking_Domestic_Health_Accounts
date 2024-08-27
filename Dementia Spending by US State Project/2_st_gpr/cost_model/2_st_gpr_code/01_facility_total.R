# Takes config file for facility (institutional) care type and sends job to ST-GPR
# Author: Elye Bliss with outline from previous pipeline
# Date: 08/04/2023

# Clean the environment 
rm(list=ls()) 


source('FILEPATH/public.R')

# Specify cluster arguments for launch
cluster_project = "proj_fgh"
nparallel = 50 #number of parallelizations
slots = 5 #number of slots for each ST or GP job. You can profile by looking at a specific ST job through qacct.
logs <- 'FILEPATH'
holdouts = 0 # Keep at 0 unless you want to run cross-validation
draws = 100 #Either 0 or 1000 - run with 0 for test runs, 1000 for upload-ready runs
model_num = 8 # 

# Register an ST-GPR model
st_gpr_dir <- "FILEPATH"

run_id <- register_stgpr_model(paste0(st_gpr_dir, "facility_cost_total_config.csv"), model_index_id = model_num)
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
  setDT(draw)
  draw <- draw[, .(location_id, year_id, variable, value)]
  return(draw)
}

## read, melt, select from, and bind all results
results <- rbindlist(lapply(paste0(results_dir, files), read_stgpr_draws))

head(results)

fwrite(results,
          paste0('FILEPATH',
                 model_num,'.csv'), 
       row.names = FALSE)


# Reading outputs from models:  Allowed values: 'stage1', 'spacetime', 'gpr', 'final'.
rates_stgpr <- get_estimates(run_id, 'final')

fwrite(rates_stgpr,
          paste0(j_root,'Project/IRH/Global_dementia_2023/data/STGPR/cost/st_gpr_output/facility/facility_means',
                 model_num,'.csv'), 
       row.names = FALSE)







