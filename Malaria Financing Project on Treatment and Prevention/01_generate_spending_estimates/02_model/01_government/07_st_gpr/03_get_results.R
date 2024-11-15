################################################
#' @author USERNAME
#' 
#' @description Save results for inpatient cases Malaria government STGPR
################################################
pacman::p_load(data.table, dplyr)
rm(list = ls())

## *************************************************

malaria_version <- "NAME"

## *************************************************

## set working directory 
setwd("ADDRESS")

## read run info
runinfo <- fread("FILEPATH")
run_id <- "NUMBER"

## *******************************************
run_id <- runinfo[version == malaria_version, run_id]
## *******************************************

## list all files in results directory
results_dir <- paste0("FILEPATH")
files <- list.files(results_dir)

## function to read, melt, and select
read_stgpr_draws <- function(x){
  draw <- fread(x)
  draw <- melt.data.table(draw, id.vars = c("location_id", "year_id", "age_group_id", "sex_id"))
  draw <- draw[, .(location_id, year_id, variable, value)]
  return(draw)
}

## read, melt, select from, and bind all results
results <- rbindlist(lapply(paste0(results_dir, files), read_stgpr_draws))

## write outputs
fwrite(results, paste0(getwd(), "FILEPATH"))

## End of Script ##