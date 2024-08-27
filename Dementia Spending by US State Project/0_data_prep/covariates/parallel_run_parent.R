################################################################################
# Calls shell script to loop over each location ID, and run child script(s) for 
# each in parallel. Change/comment child script as needed
# Author: Elye Bliss
# Date: Sep 27, 2023
################################################################################

# Clean the environment 
rm(list=ls()) 

library(data.table)

# Get locations to loop over
required_GBD_locs <- fread('FILEPATH/required_GBD_locs.csv')
locs <- required_GBD_locs$location_id

output_dir = "FILEPATH/"

####################### Find missing output locs ###############################
# If this script gets run and only some countries are missing, this next section
# determines which countries are missing and can be used to only rerun those.
# Skip this section if it's the first time running the script.

output_files <- list.files(output_dir)
search_draws <- str_extract_all(output_files,'[0-9]*',simplify = TRUE)
search_draws <- search_draws[search_draws!=""]
search_draws <- as.integer(search_draws)
locs <- setdiff(locs,search_draws) # Get difference in locations, i.e. missing.

################################################################################

# Point to child script to run for each location. 
child_script <- "FILEPATH/0_data_prep/covariates/get_prevalence_by_age_child.R"

job_name <- 'get_draws_prevalence'

for (lid in locs) {
  
  # Update output_log and error_log to personal directory in slurmoutput
  output_log <- paste0('FILEPATH/%x.o%j','_',lid)
  error_log <- paste0('FILEPATH/%x.e%j','_',lid)
  
  qsub_str <- paste("sbatch -J",job_name,"--mem=50G -c 6 -A ACCOUNT -t 02:00:00 -p PARTITION",
                    "-o ",output_log," -e",error_log, 
                    "FILEPATH/execR.sh -i FILEPATH/ihme_rstudio_4221.img",
                    "-s ", child_script, lid,sep=" ")
  
  system(qsub_str)
  
  Sys.sleep(5.0)
}

# Check number of files in output directory
62+2-length(list.files(output_dir))
