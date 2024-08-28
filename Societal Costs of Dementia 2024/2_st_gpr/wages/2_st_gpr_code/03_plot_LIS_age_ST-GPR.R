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

# Shared function used for age_group_ids
source(paste0(functions_dir,"get_ids.R"))

# model index number used for st_gpr run configuration - dont forget to update!
model_num = 6

# read in data
raw = fread("FILEPATH/10_27_23_LIS_age.csv")
stage1 = fread("FILEPATH/10_27_23_LIS_age_pred.csv")
final = fread("FILEPATH", model_num, ".csv")

# change column names to allow for data to be combined: 
setnames(raw, old = "val", new = "income")
setnames(stage1, old = "cv_custom_stage_1", new = "income")
setnames(final, old = "val", new = "income")

# add data source indicator column and lower/upper to raw & stage1
raw[, ":=" (source = "raw", lower = NA, upper = NA)]
stage1[, ":=" (source = "stage1", lower = NA, upper = NA)]
final[, source := "ST-GPR"]

# select just the columns needed for plotting: 
raw_df = raw[, .(year_id, location_id, sex_id, age_group_id, income, lower, upper, source)]
stage1_df = stage1[, .(year_id, location_id,sex_id, age_group_id, income, lower, upper, source)]
final_df = final[, .(year_id, location_id,sex_id, age_group_id, income, lower, upper, source)]

# combine raw/stage1 inputs with ST-GPR final output
income_df = rbind(raw_df, stage1_df, final_df)

###################### plot time series ########################################

# read in location metadata to get country location IDs: 
locs = fread("FILEPATH/location_set_22_metadata.csv")
country_ids = locs[level == 3 | location_id == 1] # level 3 -> countries, location_id 1 -> global

# merge in location names 
income_named = merge(income_df, locs[,.(location_id, location_name)], by = "location_id")

# merge in age-group names
age_group_ids <-get_ids('age_group')
income_named <- merge(income_named,age_group_ids,by='age_group_id',all_x=T)

# Separate graphs of predictions by sex. Make below list to include in titles.
sex_id_list = list("1"="Males","2"= "Females")

# plot only countries that contain some raw data, and save to single pdf: 
countries_with_raw <-  unique(income_named$location_id[income_named$source=='raw'])


output_dir = "FILEPATH"
pdf(paste0(output_dir,"global_income_LIS_age_plots_run_with_raw",model_num,".pdf")) # remember to update the run number or old plots may be overwritten

for(country in country_ids$location_id[country_ids$location_id %in% countries_with_raw]){
  for (sex in 1:2){ 
    # only plot for age groups that also contain raw data for give country
    country_df_outer =  income_named[location_id == country & sex_id==sex] 
    
    ages_with_raw = unique(country_df_outer$age_group_name[country_df_outer$source=='raw'])
    
  
    for (age in ages_with_raw){
 
      country_df <- country_df_outer[age_group_name==age]
      
      sex_string = sex_id_list[as.character(sex)][[1]]
      
      p = ggplot(country_df, aes(x = year_id, y = income, 
                                   ymin = lower, ymax = upper, color = source)) +
        geom_line() + 
        geom_point() +
        geom_ribbon(data = country_df[source == "ST-GPR"], 
                    alpha=0.15)+
        ylim(0,NA)+ # always include 0 in y limit 
        labs(x = "Year", y = "Income",
             title = paste(unique(country_df$location_name),
                           sex_string,age,
                           "Overall Income 1990-2019"))
      print(p)    
    }

  }
  
}
dev.off()

# plot all countries NOT containing any raw data, and save to single pdf. Exclude
# non-country plots
countries_without_raw <- unique(income_named[!(location_id %in% countries_with_raw)&
                                               (location_id %in% country_ids[level==3,]$location_id),]$location_id)

ages <- unique(income_named$age_group_name) # plot all age groups for this pdf

pdf(paste0(output_dir,"global_income_LIS_age_plots_run_without_raw",model_num,".pdf")) 
for(country in country_ids[location_id %in% countries_without_raw,]$location_id){
  for (sex in 1:2){ 

    for (age in ages){
      country_df =  income_named[location_id == country & sex_id==sex&
                                   age_group_name==age]  
      
      sex_string = sex_id_list[as.character(sex)][[1]]
      
      p = ggplot(country_df, aes(x = year_id, y = income, 
                                   ymin = lower, ymax = upper, color = source))+
        geom_line() + geom_point() +
        geom_ribbon(data = country_df[source == "ST-GPR"], alpha=0.15) +
        ylim(0,NA)+ # always include 0 in y limit 
        labs(x = "Year", y = "Income",
             title = paste(unique(country_df$location_name),
                           sex_string,age,
                           "Overall Income 1990-2019"))
      print(p)      
    }
    }

}
dev.off()