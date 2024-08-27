################################################################################
# Plots results of LIS output from ST-GPR
# Author: Elye Bliss (adpated from previous scripts)

# Date: Aug 22, 2023
################################################################################


# clear environment
rm(list = ls())

# load libraries
library(ggplot2)
library(dplyr)
library(data.table)
library(tidyr)


# model index number used for st_gpr run configuration - dont forget to update!
model_num = 4

# read in data
raw = fread("FILEPATH/10_31_23_LIS.csv")
stage1 = fread("FILEPATH/10_31_23_LIS_pred.csv")
final = fread(paste0('FILEPATH',
                     model_num,
                     '.csv'))

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

# merge in location names 
income_named = merge(income_df, locs[,.(location_id, location_name)], by = "location_id")

# Separate graphs of predictions by sex. Make below list to include in titles.
sex_id_list = list("1"="Males","2"= "Females")


# plot only countries that contain some raw data, and save to single pdf: 
countries_with_raw = unique(income_named$location_id[income_named$source=='raw'])

output_dir = "FILEPATH"
pdf(paste0(output_dir,"global_income_LIS_plots_run_with_raw",model_num,".pdf")) 

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

# plot all countries NOT containing any raw data, and save to single pdf: 
countries_without_raw = unique(income_named[!(location_id %in% countries_with_raw),]$location_id)

pdf(paste0(output_dir,"global_income_LIS_plots_run_without_raw",model_num,".pdf")) # remember to update the run number or old plots may be overwritten
for(country in country_ids[location_id %in% countries_without_raw,]$location_id){
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
dev.off()