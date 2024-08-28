# Plot facility results. Script adapted from 
# https://stash.ihme.washington.edu/projects/FRH/repos/alzheimers/browse/Global_dementia_2023/2_st_gpr/1_beta/3_post_processing_code/plot_ST_GPR_results.R?at=refs%2Fheads%2Fmb_global_beta
# for the cost model
# Date: 08/14/2023

# clear environment
rm(list = ls())

# load libraries
library(ggplot2)
library(dplyr)
library(data.table)
library(tidyr)

# model index number used for st_gpr run configuration - dont forget to update!
model_num = 8

# read in data
raw = fread("FILEPATH/11_09_23_facility_data.csv")
stage1 = fread("FILEPATH/11_09_23_facility_pred.csv")
final = fread("FILEPATH", model_num, ".csv")

# change column names to allow for data to be combined: 
setnames(raw, old = "val", new = "spending")
setnames(stage1, old = "cv_custom_stage_1", new = "spending")
setnames(final, old = "val", new = "spending")

# add data source indicator column and lower/upper to raw & stage1
raw[, ":=" (source = "raw", lower = NA, upper = NA)]
stage1[, ":=" (source = "stage1", lower = NA, upper = NA)]
final[, source := "ST-GPR"]

# select just the columns needed for plotting: 
raw_df = raw[, .(year_id, location_id, spending, lower, upper, source)]
stage1_df = stage1[, .(year_id, location_id, spending, lower, upper, source)]
final_df = final[, .(year_id, location_id, spending, lower, upper, source)]

# combine raw/stage1 inputs with ST-GPR final output
spending_df = rbind(raw_df, stage1_df, final_df)

###################### plot time series ########################################

# read in location metadata to get country location IDs: 
locs = fread("FILEPATH/location_set_22_metadata.csv")
country_ids = locs[level == 3 | location_id == 1] # level 3 -> countries, location_id 1 -> global

# merge in location names to diagnosis data: 
spending_named = merge(spending_df, locs[,.(location_id, location_name)], by = "location_id")


# plot only countries that contain some raw data, and save to single pdf: 
countries_with_raw = unique(spending_named[source=='raw',]$location_id)

output_dir = "FILEPATH"
pdf(paste0(output_dir,"global_spending_facility_plots_run_with_raw",model_num,".pdf")) 
for(country in country_ids[location_id %in% countries_with_raw,]$location_id){
  country_df =  spending_named[location_id == country]
  p = ggplot(country_df, aes(x = year_id, y = spending, 
                             ymin = lower, ymax = upper, color = source)) +
    geom_line() + geom_point() +
    geom_ribbon(data = country_df[source == "ST-GPR"], alpha=0.15) +
    ylim(0,NA)+ # always include 0 in y limit 
    labs(x = "Year", y = "Spending (facility)",
         title = paste(unique(country_df$location_name), 
                       "Overall Spending 1990-2019"))
  print(p)
}
dev.off()


# plot only countries without any raw data, and save to single pdf: 
countries_without_raw = unique(spending_named[!(location_id %in% countries_with_raw),]$location_id)

# plot all countries NOT containing raw data, and save to single pdf: 
output_dir = "FILEPATH"
pdf(paste0(output_dir,"global_spending_facility_plots_run_without_raw",model_num,".pdf")) 
for(country in country_ids[location_id %in% countries_without_raw,]$location_id){
  country_df =  spending_named[location_id == country]# spending_named[location_id == 102]
  p = ggplot(country_df, aes(x = year_id, y = spending, 
                             ymin = lower, ymax = upper, color = source)) +
    geom_line() + geom_point() +
    geom_ribbon(data = country_df[source == "ST-GPR"], alpha=0.15) +
    ylim(0,NA)+ # always include 0 in y limit 
    labs(x = "Year", y = "Spending (facility)",
         title = paste(unique(country_df$location_name), 
                       "Overall Spending 1990-2019"))
  print(p)
}
dev.off()
