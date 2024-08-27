################################################################################
# Calls shell script to loop over each location ID, and run child script(s) for 
# each in parallel. Change/comment child script as needed
# Author: Elye Bliss
# Date: Sep 27, 2023
################################################################################

# Clean the environment 
rm(list=ls()) 

library(data.table)
library(stringr)



output_dir = "FILEPATH"

# Get locations to loop over
locs <- fread(file = paste0(j_root, "FILEPATH/location_set_22_metadata.csv"))
locs <- locs[level==3]$location_id # 204 countries


# Point to child script to run for each location. 
child_script <- paste0("FILEPATH/2_forecast_scenarios_child.R")

# Update output_log and error_log to personal directory in slurmoutput
all_jobs <- c()
job_name <- 'forecast_alt'
for (lid in locs) {
  
  output_log <- paste0('FILEPATH','_',lid)
  error_log <- paste0('FILEPATH','_',lid)
  
  qsub_str <- paste("sbatch -J",job_name,"--mem=50G -c 6 -A ACCOUNT -t 02:00:00 -p PARTITION",
                    "-o ",output_log," -e",error_log, 
                    "FILEPATH/execR.sh -i FILEPATH/ihme_rstudio_4221.img",
                    "-s ", child_script, lid,sep=" ")
  
  system(qsub_str)
  
  # Link location ids to job numbers submitted
  message_output <- paste0(system(qsub_str,intern=TRUE))
  job_no <- str_extract_all(message_output,'[0-9]*',simplify = TRUE)
  job_no <- as.integer(job_no[job_no!=''])
  all_jobs <- c(all_jobs,job_no)
  Sys.sleep(5.0)
}

