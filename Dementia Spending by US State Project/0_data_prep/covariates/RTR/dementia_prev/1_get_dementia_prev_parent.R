################################################################################
# Parent script for running interpolate function for dementia prevalence in parallel
# Loops over each location_id and pulls prevalence for that location 1990-2024
# Author: Michael Breshock
# Date: 04/17/2024
################################################################################

# Clean the environment 
rm(list=ls()) 

library(data.table)
library(stringr)

# set up locations to loop over: 
source("FILEPATH/get_location_metadata.R")
# get location IDs from metadata; location_set_id=22: -> covariate computation
locs <- get_location_metadata(location_set_id=22, 
                              release_id = 16)  # GBD 2023
# extract country location IDs from the location set (country = level 3)
loc_ids <- locs[level == 3]$location_id 
# add Hong Kong (not level 3 in GBD location hierarchy)
HK_id <- 354
loc_ids <- c(loc_ids, HK_id)

# Point to child script to run for each location. This may cause an error if 
# your directory is configured in a different way. Edit file location as needed.
child_script = "FILEPATH/0_data_prep/covariates/RTR/get_dementia_prev_child.R"

# Update output_log and error_log to personal directory in slurmoutput
all_jobs <- data.table()
for (loc in loc_ids) {
  job_name <- paste0('interp_dem_prev',loc)
  output_log <- paste0('FILEPATH/%x.o%j') # %x puts the job name in the log file
  error_log <- paste0('FILEPATH/%x.e%j') # %j puts the job number in the log file
  
  sbatch_str <- paste("sbatch -J",job_name,"--mem=10G -c 5 -A ACCOUNT -t 02:00:00 -p PARTITION",
                      "-o ",output_log," -e",error_log, 
                      "FILEPATH/execR.sh -i FILEPATH/ihme_rstudio_4222.img",
                      "-s ", child_script, loc,sep=" ")
  
  # submit job while simulatenously saving job number
  message_output <- paste0(system(sbatch_str,intern=TRUE))
  print(message_output)
  # Link location_id to job numbers submitted
  job_no <- str_extract_all(message_output,'[0-9]*',simplify = TRUE)
  job_no <- as.integer(job_no[job_no!=''])
  job_df = data.table(location_id = loc, job_id = job_no)
  all_jobs <- rbind(all_jobs, job_df)
  
  Sys.sleep(5.0) # sleep for 5 seconds to avoid overloading the scheduler
}
