# Plot Beta ST-GPR Results 
# Author: Michael Breshock
# Date: 08/11/2023

# clear environment
rm(list = ls())

# load libraries
library(ggplot2)
library(dplyr)
library(data.table)
library(tidyr)



# model index number used for st_gpr run configuration
model_num = 6

# read in data
raw = fread("FILEPATH/09.18.23.beta_all_data.csv")
stage1 = fread("FILEPATH/09.18.23.beta_all_predictions.csv")
final = fread(paste0("FILEPATH/dx_rate_all_means_run_0", model_num, ".csv"))

# change column names to allow for data to be combined: 
setnames(raw, old = "val", new = "dx_rate")
setnames(stage1, old = "cv_custom_stage_1", new = "dx_rate")
setnames(final, old = "val", new = "dx_rate")

# add data source indicator column and lower/upper to raw & stage1
raw[, ":=" (source = "raw", lower = NA, upper = NA)]
stage1[, ":=" (source = "stage1", lower = NA, upper = NA)]
final[, source := "ST-GPR"]

# select just the columns needed for plotting: 
raw_df = raw[, .(year_id, location_id, dx_rate, lower, upper, source)]
stage1_df = stage1[, .(year_id, location_id, dx_rate, lower, upper, source)]
final_df = final[, .(year_id, location_id, dx_rate, lower, upper, source)]

# combine raw/stage1 inputs with ST-GPR final output
dx_df = rbind(raw_df, stage1_df, final_df)

###################### plot time series for all 204 countries ######################
# read in location metadata to get country location IDs: 
locs = fread("FILEPATH/location_set_22_metadata.csv")
country_ids = locs[level == 3 | location_id == 1] # level 3 -> countries, location_id 1 -> global

# merge in location names to diagnosis data: 
dx_named = merge(dx_df, locs[,.(location_id, location_name)], by = "location_id")

# plot all countries and save to single pdf: 
output_dir = "FILEPATH"
pdf(paste0(output_dir,"global_dx_rate_plots_run_0",model_num,".pdf")) # remember to update the run number or old plots may be overwritten
for(country in country_ids$location_id){
  country_df = dx_named[location_id == country]
   p = ggplot(country_df, aes(x = year_id, y = dx_rate, 
                              ymin = lower, ymax = upper, color = source)) +
     geom_line() + geom_point() + ylim(0,1) +
     geom_ribbon(data = country_df[source == "ST-GPR"], alpha=0.15) +
     labs(x = "Year", y = "Diagnosis Rate",
          title = paste(unique(country_df$location_name), 
                        "Overall Diagnosis Rates 1990-2019"))
   print(p)
}
dev.off()

  

