################################################
#' @author USERNAME
#' 
#' @description Launch ST-GPR & save results for Malaria government STGPR
################################################

rm(list = ls())

## *************************************************

malaria_version <- "NAME"

## *************************************************

## source STGPR functions
central_root <- 'ADDRESS'
setwd(central_root)
source('FILEPATH')
source('FILEPATH')
source('FILEPATH')

## set the model_index_id from the config (specifies which model to launch)
setwd("ADDRESS")

## get config
config <- paste0(getwd(), "FILEPATH")

## examine config
config_data <- fread(config)

value_codes <- unique(config_data[version == malaria_version]$me_name)

setwd(central_root)

for (i in 1:length(value_codes)) {
  
  code <- value_codes[i]
  ## pick index to run
  index_id <- config_data[version == malaria_version & me_name == code, model_index_id]
  
  ## register model and get run_id
  run_id <- register_stgpr_model(config, model_index_id = index_id)
  
  ## launch model
  stgpr_sendoff(run_id, 'proj_fgh')
  
  ## Successful run: create run info data for use later
  config_info <- fread(config)
  config_info <- config_info[model_index_id == index_id]
  config_info[, run_id := run_id]
  config_info[, date_run := format(Sys.Date(),'%m%d%y')]
  config_info[, time_run := format(Sys.time(), '%H:%M')]
  
  setwd("ADDRESS")
  
  ## append to run_info_file
  if(!file.exists(paste0(getwd(), "FILEPATH"))){
    fwrite(config_info, "FILEPATH")
  }else{
    runinfo <- fread("FILEPATH")
    runinfo <- rbind(runinfo, config_info, use.names = T, fill = T)
    fwrite(runinfo, "FILEPATH")
  }
  
  max_loop <- length(value_codes)
  
}

## End of Script ##