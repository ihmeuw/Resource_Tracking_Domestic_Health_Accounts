########################################################################################
## Make OOP scalar line graph plots of ST-GPR results
## Author: Emilie Maddison
## Date: 17 May 2020
## Description: Produces PDFs, with each page representing the countries within
##              each super-region. Time series line graphs
########################################################################################
## -------------------------------------------------------------------------------------
## 1. Set up environment
## -------------------------------------------------------------------------------------

## Clear environment
rm(list = ls())

## Set filepaths
if (Sys.info()[1] == "Linux") {
  j <- FILEPATH
  h <- FILEPATH
  k <- FILEPATH
} else if (Sys.info()[1] == "Windows") {
  j <- FILEPATH
  h <- FILEPATH
  k <- FILEPATH
}

code.dir <- FILEPATH

out.dir <- FILEPATH
# out.dir <- FILEPATH

## Load libraries
source(paste0(FILEPATH, "helper_functions.R"))
## St-GPR functions
source("FILEPATH/utility.r")
library(ggplot2)

## Today's date
# date1 <- format(Sys.time(), "%Y%m%d")
# date1 <- as.character(Sys.Date())
date1 <- "20200628"

## -------------------------------------------------------------------------------------
## 2. Prep data
## -------------------------------------------------------------------------------------

## 135 locations of interest
lm_locs <- get_ig(locs)
lm_locs <- lm_locs[income_group != 'H']

## Read in dictionary
run_dictionary <- fread(paste0(FILEPATH,
                               'oop_scalar_runid_modelid_dictionary.csv'))

## Read in config file to get stage1 model parameters
config_file <- fread("FILEPATH/oop_scalar_config.csv")

config_file <- config_file[, .(model_version, model_index_id, stage_1_model_formula)]

run_dictionary <- fread(paste0(out.dir, "oop_scalar_runid_modelid_dictionary.csv"))
run_dictionary2 <- data.table(merge(run_dictionary, config_file,
                        by.x = 'model_id', by.y = 'model_index_id',
                        all.x = T))

## Subset to today's runs
# run_dictionary2 <- run_dictionary2[date == date1, ]
run_dictionary2 <- run_dictionary2[model_id == 20, ]

## -------------------------------------------------------------------------------------
## Pull draw-level St-GPR results
# run_id = 151160
# loc_id = 10
draw_cols <- paste0('draw_', c(0:999))

prep_data <- function(run_id) {
  dt = data.table()
  for (loc_id in locs$location_id) {
    a = fread(paste0(FILEPATH, run_id, FILEPATH, loc_id, '.csv'))
    dt = rbind(dt, a, fill = T)
  }
  dt <- melt(dt, 
             id.vars = c('location_id', 'year_id'),
             measure.vars = draw_cols,
             variable.name = 'draw')
  return(dt)
}

## Read in St-GPR data
draw_data <- prep_data(run_dictionary2$run_id)

## Read in raw (pre-St-GPR data)
raw_data <- fread(paste0("FILEPATH/oop_scalar_",
                         date1, ".csv"))

## Delete the tiny adjustment made to fix zeros for logit space
adj_val <- min(raw_data$val)
## Subtract the adjustment value we added pre-ST-GPR to deal with zeros
draw_data[, `:=`(value = value - adj_val)]

## Write out data
fwrite(draw_data,
       paste0("FILEPATH/oop_scalar_stgpr_draws_",
              date1, ".csv"))

## END OF SCRIPT ##