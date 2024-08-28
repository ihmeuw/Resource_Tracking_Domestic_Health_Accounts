# Plot ILO results. Script adapted from 
# https://stash.ihme.washington.edu/projects/FRH/repos/alzheimers/browse/Global_dementia_2023/2_st_gpr/1_beta/3_post_processing_code/plot_ST_GPR_results.R?at=refs%2Fheads%2Fmb_global_beta
# for the income model
# Date: 08/15/2023

# clear environment
rm(list = ls())

# load libraries
library(ggplot2)
library(dplyr)
library(data.table)
library(tidyr)

# model index number used for st_gpr run configuration - dont forget to update!
model_num = 1

# read in data
raw = fread("FILEPATH/08_11_23_income_data.csv")
stage1 = fread("FILEPATH/08_11_23_income_pred.csv")
final = fread(paste0("FILEPATH", model_num, ".csv"))

# change column names to allow for data to be combined: 
setnames(raw, old = "val", new = "income")
setnames(stage1, old = "cv_custom_stage_1", new = "income")
setnames(final, old = "val", new = "income")

# add data source indicator column and lower/upper to raw & stage1
raw[, ":=" (source = "raw", lower = NA, upper = NA)]
stage1[, ":=" (source = "stage1", lower = NA, upper = NA)]
final[, source := "ST-GPR"]

# select just the columns needed for plotting: 
raw_df = raw[, .(year_id, location_id, sex_id, income, lower, upper, source)]
stage1_df = stage1[, .(year_id, location_id,sex_id,  income, lower, upper, source)]
final_df = final[, .(year_id, location_id,sex_id,  income, lower, upper, source)]

# combine raw/stage1 inputs with ST-GPR final output
income_df = rbind(raw_df, stage1_df, final_df)

###################### plot time series ########################################

# read in location metadata to get country location IDs: 
locs = fread("FILEPATH/location_set_22_metadata.csv")
country_ids = locs[level == 3 | location_id == 1] # level 3 -> countries, location_id 1 -> global

# merge in location names to diagnosis data: 
income_named = merge(income_df, locs[,.(location_id, location_name)], by = "location_id")

# Separate graphs of predictions by sex. Make below list to include in titles.
sex_id_list = list("1"="Males","2"= "Females")

# plot all countries and save to single pdf: 
output_dir = "FILEPATH"
pdf(paste0(output_dir,"global_income_plots_run_",model_num,".pdf")) # remember to update the run number or old plots may be overwritten

# Use income_named[location_id == 102 & sex_id==sex] as test case
for(country in country_ids$location_id){
  for (sex in 1:2){
    country_df =  income_named[location_id == country & sex_id==sex]  
    
    sex_string = sex_id_list[as.character(sex)][[1]]
    
    p = ggplot(country_df, aes(x = year_id, y = income, 
                               ymin = lower, ymax = upper, color = source)) +
      geom_line() + geom_point() +
      geom_ribbon(data = country_df[source == "ST-GPR"], alpha=0.15) +
      ylim(0,NA)+ # always include 0 in y limit 
      labs(x = "Year", y = "Income",
           title = paste(unique(country_df$location_name),
                         sex_string,
                         "Overall Income 1990-2019"))
    print(p)    
  }

}
dev.off()

# plot only countries that contain some raw data, and save to single pdf: 
countries_with_raw = unique(income_named$location_id[income_named$source=='raw'])

output_dir = "FILEPATH"
pdf(paste0(output_dir,"global_income_plots_run_with_raw",model_num,".pdf")) # remember to update the run number or old plots may be overwritten

for(country in country_ids$location_id[country_ids$location_id %in% countries_with_raw]){
  for (sex in 1:2){
    country_df =  income_named[location_id == country & sex_id==sex]  
    
    sex_string = sex_id_list[as.character(sex)][[1]]
    
    p = ggplot(country_df, aes(x = year_id, y = income, 
                               ymin = lower, ymax = upper, color = source)) +
      geom_line() + geom_point() +
      geom_ribbon(data = country_df[source == "ST-GPR"], alpha=0.15) +
      ylim(0,NA)+ # always include 0 in y limit 
      labs(x = "Year", y = "Income",
           title = paste(unique(country_df$location_name),
                         sex_string,
                         "Overall Income 1990-2019"))
    print(p)    
  }
  
}
dev.off()

