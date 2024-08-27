################################################################################
# Plot ST-GPR output for the gamma model
# Author: Elye Bliss with outline from previous pipeline
# Date: 09/12/2023
################################################################################

# clear environment
rm(list = ls())

# load libraries
library(ggplot2)
library(dplyr)
library(data.table)
library(tidyr)


# model index number used for st_gpr run configuration - dont forget to update!
model_num = 6

# read in data
raw = fread("FILEPATH/12_11_23_gamma_data.csv")
stage1 = fread("FILEPATH/12_11_23_gamma_pred.csv")
final = fread(paste0(j_root, "FILEPATH", model_num, "_means.csv"))

# change column names to allow for data to be combined: 
setnames(raw, old = "val", new = "gamma")
setnames(stage1, old = "cv_custom_stage_1", new = "gamma")
setnames(final, old = "val", new = "gamma")

# add data source indicator column and lower/upper to raw & stage1
raw[, ":=" (source = "raw", lower = NA, upper = NA)]
stage1[, ":=" (source = "stage1", lower = NA, upper = NA)]
final[, source := "ST-GPR"]

# select just the columns needed for plotting: 
raw_df = raw[, .(year_id, location_id, gamma, lower, upper, source)]
stage1_df = stage1[, .(year_id, location_id, gamma, lower, upper, source)]
final_df = final[, .(year_id, location_id, gamma, lower, upper, source)]

# combine raw/stage1 inputs with ST-GPR final output
gamma_df = rbind(raw_df, stage1_df, final_df)

###################### plot time series ########################################

# read in location metadata to get country location IDs: 
locs = fread("FILEPATH/location_set_22_metadata.csv")
country_ids = locs[level == 3 | location_id == 1] # level 3 -> countries, location_id 1 -> global

# merge in location names to diagnosis data: 
gamma_named = merge(gamma_df, locs[,.(location_id, location_name)], by = "location_id")


# plot only countries that contain some raw data, and save to single pdf: 
countries_with_raw = unique(gamma_named[source=='raw',]$location_id)

output_dir = "FILEPATH"
pdf(paste0(output_dir,"global_gamma_plots_run_with_raw",model_num,".pdf")) 
for(country in country_ids[location_id %in% countries_with_raw,]$location_id){
  # Ensure raw data is last so that it overlays other points/lines and is visible
  country_df_model =  gamma_named[location_id == country& source %in% c('stage1','ST-GPR')]
  country_df_raw = gamma_named[location_id == country& source =='raw']
  country_df <- rbind(country_df_model,country_df_raw) 
  p = ggplot(country_df, aes(x = year_id, y = gamma, 
                             ymin = lower, ymax = upper, color = source)) +
    geom_line() + geom_point() +
    geom_ribbon(data = country_df[source == "ST-GPR"], alpha=0.15) +
    ylim(0,NA)+ # always include 0 in y limit 
    labs(x = "Year", y = "Gamma",
         title = paste(unique(country_df$location_name), 
                       "Institution rates 1990-2019"))
  print(p)
}
dev.off()


# plot only countries without any raw data, and save to single pdf: 
countries_without_raw = unique(gamma_named[!(location_id %in% countries_with_raw),]$location_id)

# plot all countries NOT containing raw data, and save to single pdf: 
output_dir = "FILEPATH"
pdf(paste0(output_dir,"global_gamma_plots_run_without_raw",model_num,".pdf")) # remember to update the run number or old plots may be overwritten
for(country in country_ids[location_id %in% countries_without_raw,]$location_id){
  country_df =  gamma_named[location_id == country]
  p = ggplot(country_df, aes(x = year_id, y = gamma, 
                             ymin = lower, ymax = upper, color = source)) +
    geom_line() + geom_point() +
    geom_ribbon(data = country_df[source == "ST-GPR"], alpha=0.15) +
    ylim(0,NA)+ # always include 0 in y limit 
    labs(x = "Year", y = "Gamma",
         title = paste(unique(country_df$location_name), 
                       "Institution rates 1990-2019"))
  print(p)
}
dev.off()
