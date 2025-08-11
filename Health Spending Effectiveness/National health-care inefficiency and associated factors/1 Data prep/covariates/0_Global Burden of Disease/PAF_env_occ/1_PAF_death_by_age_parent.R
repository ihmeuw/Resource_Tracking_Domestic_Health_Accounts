##########################################################################
### Author: USERNAME
### Date: 08/14/2024
### Project: Health Spending Effectiveness 
### Purpose: Parent script for extracting population attributable fraction (PAF) and death draws by age group in parallel.
###          Calls 0_get_PAF_death_by_age_child.R in a loop over all locations individually.
###########################################################################

# Clean the environment 
rm(list=ls()) 

library(data.table)
library(stringr)

# set up file path roots
if (Sys.info()["sysname"] == "Linux") {
  j_root <- "FILEPATH" 
  h_root <- "FILEPATH"
  l_root <- "FILEPATH"
  functions_dir <- "FILEPATH"
} else { 
  j_root <- "FILEPATH"
  h_root <- "FILEPATH"
  l_root <- "FILEPATH"
  functions_dir <- "FILEPATH"
}

# set up locations to loop over: 
source(paste0(functions_dir,"get_location_metadata.R"))
# get location IDs from metadata; location_set_id=35: -> GBD Model results
locs <- get_location_metadata(location_set_id=35, 
                              release_id = 16)  # GBD 2023
# extract country location IDs from the location set (country = level 3)
loc_ids <- locs[level == 3]$location_id 

# Point to child script to run for each location
child_script = "FILEPATH/0_get_PAF_death_by_age_child.R"

# Update output_log and error_log to personal directory in slurmoutput
all_jobs <- data.table()
iter = 0 # var to print loop iteration number
for (loc in loc_ids) {
  job_name <- paste0('PAF_draws_',loc)
  output_log <- paste0('FILEPATH/%x.o%j') # %x puts the job name in the log file
  error_log <- paste0('FILEPATH/%x.e%j') # %j puts the job number in the log file
  
  sbatch_str <- paste("sbatch -J",job_name,"--mem=10G -c 2 -A proj_integrated_analytics -t 02:00:00 -p long.q",
                      "-o ", output_log," -e", error_log, 
                      "FILEPATH/execR.sh -i FILEPATH/ihme_rstudio_4421.img",
                      "-s ", child_script, loc, sep=" ")
  
  # submit job while simulatenously saving job number
  message_output <- paste0(system(sbatch_str,intern=TRUE))
  iter = iter + 1
  print(paste("Running Location ID", loc, "with loop iteration", iter))
  print(message_output)
  # Link location_id to job numbers submitted
  job_no <- str_extract_all(message_output,'[0-9]*',simplify = TRUE)
  job_no <- as.integer(job_no[job_no!=''])
  job_df = data.table(location_id = loc, job_id = job_no)
  all_jobs <- rbind(all_jobs, job_df)
  
  Sys.sleep(5.0) # sleep for 5 seconds to avoid overloading the scheduler
}
