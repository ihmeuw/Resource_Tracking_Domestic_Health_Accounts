# Create institutional diagnosis rate forecasts
# Author: Michael Breshock
# Date: 11/07/2023

# clear environment
rm(list = ls())

# load libraries
library(dplyr)
library(data.table)
library(tidyr)


# load beta care setting scale factors: 
scale_factors = fread("FILEPATH/care_setting_scale_factor.csv")

## load forecasted beta draws - reference scenario:
# initialize projected draws file path: 
projected_files = "FILEPATH"
# reference scenario path:
beta_path = paste0(projected_files, "projected_draws/")
# create list of all beta file paths for each location 
beta_files = paste0(beta_path, 
                    list.files(beta_path, 
                               pattern = "^beta_all_projections_loc_.*.*[0-9]\\.csv$"))
# read in each overall beta forecast (each location has its own file)
beta = rbindlist(lapply(beta_files, fread, 
                        select = c("location_id", "year_id", "draw", "data_var")
                        )
                 )

## accelerated beta scenario: 
# accelerated beta path: 
accel_beta_path = paste0(projected_files, "alt_projection_draws/accelerated_beta/")
accel_beta_files = paste0(accel_beta_path, 
                          list.files(accel_beta_path, 
                                     pattern = "^beta_all_projections_loc_.*.*[0-9]\\.csv$"))
accel_beta = rbindlist(lapply(accel_beta_files, fread, 
                              select = c("location_id", "year_id", "draw", "data_var")
                              )
                       )

# create reference scenario beta_inst: 
setnames(scale_factors, old = "variable", new = "draw")
beta_inst = merge(beta, scale_factors[,.(draw, inst_scale)], by = "draw")
# calculate beta_inst by applying scale factor to beta
setnames(beta_inst, old = "data_var", new = "beta")
beta_inst[, beta_inst := pmin(beta*inst_scale, 1)] # cap at 1 - 100% diagnosis rate

# create accelerated beta scenario beta_inst: 
accel_beta_inst = merge(accel_beta, scale_factors[,.(draw, inst_scale)], by = "draw")
# calculate beta_inst by applying scale factor to beta
setnames(accel_beta_inst, old = "data_var", new = "beta")
accel_beta_inst[, beta_inst := pmin(beta*inst_scale, 1)]

# save out beta inst dataframes for each scenario: 
beta_inst_out = beta_inst[,.(year_id, location_id, draw, inst_scale, beta_inst)]
fwrite(beta_inst_out,
       file = paste0(projected_files, "beta_inst/reference_beta_inst_forecast.csv"))

accel_inst_out = accel_beta_inst[,.(year_id, location_id, draw, inst_scale, beta_inst)]
fwrite(accel_inst_out,
       file = paste0(projected_files, "beta_inst/accelerated_beta_inst_forecast.csv"))
