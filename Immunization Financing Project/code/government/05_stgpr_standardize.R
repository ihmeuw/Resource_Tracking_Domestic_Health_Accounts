## #############################################################################
## Author:
## Project: Immunization financing
## Purpose: Add required ST-GPR columns for modelling and calculate variance for standardized data
##          
## Inputs: 04_ghes_data.csv (output data from step 04_cooks_distance.R)
##
## Outputs: 05_ghes_COMPONENT_data.csv by component (same input data, but with ST-GPR required columns 
##          calculated variance)
##          
## Last update: 11/03/2021
## #############################################################################
pacman::p_load(tidyverse, data.table, dplyr, feather, reshape2, ggplot2)

rm(list = ls())

## Defining j, h, and k 
if (Sys.info()[1] == "Linux"){
  j <- "FILEPATH"
  h <- paste0("FILEPATH", Sys.info()[7])
  k <- "FILEPATH"
} else if (Sys.info()[1] == "Windows"){
  j <- "FILEPATH"
  h <- "FILEPATH"
  k <- "FILEPATH"
}


## input and output data folder
data_folder <- "FILEPATH"

## read in cleaned script 04 data
data <- fread(paste0(data_folder, "FILEPATH/04_ghes_data.csv"))

## date
date <- format(Sys.Date(), '%m%d%y')


## keep relevant columns
data <- data[, .(location_id,
                 year_id,
                 val = proportion,
                 is_outlier,
                 source,
                 component)]

## add other required ST-GPR columns for modelling
data[, `:=` (sample_size = 1000,
             measure_id = 18,
             nid = 12345,
             age_group_id = 22,
             sex_id = 3)]


## immunization components to loop through
components <- c('vaccine', 'routine', 'delivery', 'supplementary', 'immunization')

## loop through components, calculate variance, create variance column
for (i in components) {
  
  # keep data for specific immunization component
  loop_data <- data[component == i]
  
  # calculate variance without outliered datapoints
  loop_data[, variance := var(loop_data[is_outlier == 0]$val)]

  # increase variance for FSP/cMYP and IDCC datapoints
  loop_data[source %in% c('FSP/cMYP', 'IDCC'),
            variance := variance * 2]

  # remove unnecessary columns
  loop_data[, c('source', 'component') := NULL]

  # save out data for ST-GPR by component
  fwrite(loop_data, paste0(data_folder, 'FILEPATH/05_ghes_', i, '_data_dsv.csv'))

}
