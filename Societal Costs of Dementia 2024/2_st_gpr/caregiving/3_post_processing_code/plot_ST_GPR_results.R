# Plot Caregiving ST-GPR Results 
# Author: Michael Breshock
# Date: 09/13/2023

# clear environment
rm(list = ls())

# load libraries
library(ggplot2)
library(dplyr)
library(data.table)
library(tidyr)


# model index number used for st_gpr run configuration - dont forget to update!
model_num = 15

# read in data
raw = fread("FILEPATH/04.17.24.cg_data.csv")
stage1 = fread("FILEPATH/04.17.24.cg_predictions.csv")
final = fread(paste0("FILEPATH/cg_hours_means_run_", model_num, ".csv"))

# change column names to allow for data to be combined: 
setnames(raw, old = "val", new = "logit_hours")
setnames(stage1, old = "cv_custom_stage_1", new = "logit_hours")
setnames(final, old = "val", new = "logit_hours")

# add data source indicator column and lower/upper to raw & stage1
raw[, ":=" (source = "raw", lower = NA, upper = NA)]
stage1[, ":=" (source = "stage1", lower = NA, upper = NA)]
final[, source := "ST-GPR"]

# select just the columns needed for plotting: 
raw_df = raw[, .(year_id, location_id, logit_hours, lower, upper, source)]
stage1_df = stage1[, .(year_id, location_id, logit_hours, lower, upper, source)]
final_df = final[, .(year_id, location_id, logit_hours, lower, upper, source)]

# combine raw/stage1 inputs with ST-GPR final output
cg_df = rbind(raw_df, stage1_df, final_df)

# inverse logit transform data: 
inverse_scaled_logit <- function(y,a,b){
  # Function to reverse scaled_logit transformation
  result <- ((b-a)*exp(y))/(1+exp(y))+a
  return(result) 
}
# inverse transform while capping at 168
cg_df[, ":=" (cg_hours = pmin(inverse_scaled_logit(logit_hours, 0, 169),168), 
              lower = pmin(inverse_scaled_logit(lower, 0, 169),168), 
              upper = pmin(inverse_scaled_logit(upper, 0, 169),168))]


###################### plot time series for all 204 countries ######################
# read in location metadata to get country location IDs: 
source(paste0(functions_dir,"get_location_metadata.R"))
# get location IDs from metadata
locs <- get_location_metadata(location_set_id = 22, # covariate computation
                              release_id = 16) 

country_ids = locs[level == 3] # level 3 -> countries

# merge in location names to diagnosis data: 
cg_named = merge(cg_df, country_ids[,.(location_id, location_name)], 
                 by = "location_id")

# see range of country level estimates: 
range(cg_named$cg_hours)
range(cg_named[source == "ST-GPR"]$lower)
range(cg_named[source == "ST-GPR"]$upper)

# plot all countries with raw data and save to single pdf: 
output_dir = "FILEPATH"
pdf(paste0(output_dir,"global_cg_hours_plots_run_",model_num,".pdf")) # remember to update the run number or old plots may be overwritten
for(country in unique(raw$location_id)){
  country_df = cg_named[location_id == country]
  p = ggplot(country_df, aes(x = year_id, y = cg_hours, 
                             ymin = lower, ymax = upper, color = source)) +
    geom_line() + geom_point() + ylim(0,168) +
    geom_ribbon(data = country_df[source == "ST-GPR"], alpha=0.15) +
    labs(x = "Year", y = "Caregiving Hours",
         title = paste(unique(country_df$location_name), 
                       "Caregiving Hours 1990-2019"))
  print(p)
}
dev.off()

# plot all countries without raw data and save to single pdf: 
countries_no_raw = country_ids[!(location_id %in% unique(raw$location_id))]
pdf(paste0(output_dir,"global_cg_hours_plots_run_",model_num,"_no_raw.pdf")) # remember to update the run number or old plots may be overwritten
for(country in countries_no_raw$location_id){
  country_df = cg_named[location_id == country]
  p = ggplot(country_df, aes(x = year_id, y = cg_hours, 
                             ymin = lower, ymax = upper, color = source)) +
    geom_line() + geom_point() + ylim(0,168) +
    geom_ribbon(data = country_df[source == "ST-GPR"], alpha=0.15) +
    labs(x = "Year", y = "Caregiving Hours",
         title = paste(unique(country_df$location_name), 
                       "Caregiving Hours 1990-2019"))
  print(p)
}
dev.off()


# make map: 
source('FILEPATH/gbd2023_map.R')
 
# extract 2019 data: 
map_df = cg_named[year_id == 2019 & source == "ST-GPR"]
# change variable name for map function: 
setnames(map_df, old = "cg_hours", new = "mapvar")
# see values to choose limits:
range(map_df$mapvar) # 34.18449 - 154.90975
limits = c(seq(20, 168, 20),168)
# initialize file path to save pdf to: 
map_output_dir = paste0(output_dir,"global_cg_hours_map_run_",model_num,"_RTR.pdf")

gbd_map(data=map_df,
        limits,
        sub_nat="none",
        inset=FALSE,
        na.color = 'white',
        title="Mean Weekly Caregiving Hours per Case 2019",
        legend.title="Caregiving Hours",
        fname = map_output_dir)

