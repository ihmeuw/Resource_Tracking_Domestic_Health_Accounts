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
model_num = 5

# read in data
raw = fread("FILEPATH/04.18.24.gender_data.csv")
stage1 = fread("FILEPATH/04.18.24.gender_predictions.csv")
final = fread(paste0("FILEPATH", model_num, ".csv"))

# change column names to allow for data to be combined: 
setnames(raw, old = "val", new = "percent_female")
setnames(stage1, old = "cv_custom_stage_1", new = "percent_female")
setnames(final, old = "val", new = "percent_female")

# add data source indicator column and lower/upper to raw & stage1
raw[, ":=" (source = "raw", lower = NA, upper = NA)]
stage1[, ":=" (source = "stage1", lower = NA, upper = NA)]
final[, source := "ST-GPR"]

# select just the columns needed for plotting: 
raw_df = raw[, .(year_id, location_id, percent_female, lower, upper, source)]
stage1_df = stage1[, .(year_id, location_id, percent_female, lower, upper, source)]
final_df = final[, .(year_id, location_id, percent_female, lower, upper, source)]

# combine raw/stage1 inputs with ST-GPR final output
cg_df = rbind(raw_df, stage1_df, final_df)

# set limit on caregiving hours: 
# cg_df[, upper := pmin(upper, 168)]
# cg_df[, cg_hours := pmin(cg_hours, 168)]

###################### plot time series for all 204 countries ######################
# read in location metadata to get country location IDs: 
source(paste0(functions_dir,"get_location_metadata.R"))
# get location IDs from metadata
locs <- get_location_metadata(location_set_id = 22, # covariate computation
                              release_id = 16) # GBD 2023
country_ids = locs[level == 3] # level 3 -> countries

# merge in location names to diagnosis data: 
cg_named = merge(cg_df, locs[,.(location_id, location_name)], by = "location_id")

# plot all countries with raw data and save to single pdf: 
output_dir = "FILEPATH"
pdf(paste0(output_dir,"global_gender_plots_run_0",model_num,".pdf")) 
for(country in unique(raw$location_id)){
  country_df = cg_named[location_id == country]
  p = ggplot(country_df, aes(x = year_id, y = percent_female, 
                             ymin = lower, ymax = upper, color = source)) +
    geom_line() + geom_point() + ylim(0,1) +
    geom_ribbon(data = country_df[source == "ST-GPR"], alpha=0.15) +
    labs(x = "Year", y = "Percent Female Caregivers",
         title = paste(unique(country_df$location_name), 
                       "Percent Female Caregivers 1990-2019"))
  print(p)
}
dev.off()

# plot all countries without raw data and save to single pdf: 
countries_no_raw = country_ids[!(location_id %in% unique(raw$location_id))]
pdf(paste0(output_dir,"global_gender_plots_run_0",model_num,"_no_raw.pdf")) # remember to update the run number or old plots may be overwritten
for(country in countries_no_raw$location_id){
  country_df = cg_named[location_id == country]
  p = ggplot(country_df, aes(x = year_id, y = percent_female, 
                             ymin = lower, ymax = upper, color = source)) +
    geom_line() + geom_point() + ylim(0,1) +
    geom_ribbon(data = country_df[source == "ST-GPR"], alpha=0.15) +
    labs(x = "Year", y = "Percent Female Caregivers",
         title = paste(unique(country_df$location_name), 
                       "Percent Female Caregivers 1990-2019"))
  print(p)
}
dev.off()
