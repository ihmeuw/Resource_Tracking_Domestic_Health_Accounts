################################################################################
# Pull and clean extractions related to care settings from care_setting_extraction_2023_08_16.xlsx
# Author: Elye Bliss
# Date: Aug 24, 2023
################################################################################

# Clean the environment 
rm(list=ls()) 

library(data.table)
library(readxl)

############################# Clean new data ###################################

# Read raw data from latest excel file:
new_data <- read_excel("FILEPATH/care_setting_extraction_2023_11_03.xlsx",
                       sheet = "Sheet1") 
setDT(new_data)

# Remove first row which contains explanations of variables
new_data <- new_data[-1, ]

# Add index column for downstream merging
new_data[,index_col := 1:nrow(new_data)]

# Add location_id using location_name_id
locs = fread(paste0(j_root, "FILEPATH/location_set_22_metadata.csv"))
new_data[,ihme_loc_id := tstrsplit(location_name_id,'\\|')[[2]]] # id is after the |
new_data[,ihme_loc_id := str_trim(ihme_loc_id)] # SVN has leading white space
new_data <- merge(new_data,locs[,.(location_id,ihme_loc_id)],by='ihme_loc_id',)


# Convert to proper data types
new_data[,":=" (year_published = as.integer(year_published),
                year_start = as.integer(year_start),
                year_end = as.integer(year_end),
                `institution percent (gamma)` = as.numeric(`institution percent (gamma)`),
                sample_size = as.integer(sample_size))]

setnames(new_data,old='institution percent (gamma)',new='gamma')

# Note, 6 observations from the new data have NA values for gamma. 
# Confirmed that these should be removed
new_data <- new_data[!is.na(gamma)]

# Several rows contain values for studies spanning multiple years. Taking the 
# midpoint (rounding down) of the year span
new_data[,year_id := NA_integer_]
new_data[year_start==year_end,year_id := year_end]
# Integer division to take midpoint of range and round down
new_data[year_start!=year_end,year_id := (year_end+year_start)%/%2] 

################### Prepare resolved previous data  ############################
prev_data <- read_excel("FILEPATH/care_settings_template_v1.xlsx",
                        sheet = "care_settings_template")
setDT(prev_data)

# Last two columns are notes
prev_data <- prev_data[,1:(ncol(prev_data)-2)]
prev_data[,gamma:= as.numeric(Institution_per/100)]
setnames(prev_data,old='Year',new='year_id')
prev_data[,ihme_loc_id := tstrsplit(location_name_id,'\\|')[[2]]] # id is after the |
prev_data <- merge(prev_data,locs[,.(location_id,ihme_loc_id)],by='ihme_loc_id')
prev_data[,loc_year := paste(location_id,year_id,sep="-")]

# source ID doesn't mean the same thing in the old files as it does in the new files
# use new file to map from the old source_id + source_tab to the new index
prev_data <- prev_data[, full_index := paste0(source_id,source_tab)]

prev_source_ids <- fread("FILEPATH/paola_review_source_ids.csv")
#create full index using old_index_number and old_index_letter
prev_source_ids <- prev_source_ids[, full_index := paste0(old_index_number,old_index_letter)]

# merge prev_cost_data and prev_source_ids by full_index
prev_data <- merge(prev_data, prev_source_ids, by = "full_index", all.x = TRUE)
prev_data[, source_id := NULL]
setnames(prev_data,"index","source_id")

##################### Merge prev data and new data #############################

# Merge prev data and new data
prev_data <- prev_data[,.(ihme_loc_id,source_id,source_tab,location_name_id,
                          year_id,gamma, location_id)]

new_data <- new_data[,.(ihme_loc_id,NID, source_id,location_name_id, gamma, sample_size,
                        location_id,year_id)]

gamma_dt <- rbind(prev_data,new_data,fill=T) #94 obs
gamma_dt[is.na(NID),NID := 999999]

########################### Merge covariates ###################################

covars <- fread("FILEPATH/all_covars_1990_2019.csv")
covars[,year_id:=as.integer(year_id)]

gamma_dt <- merge(gamma_dt,covars,by=c('year_id','location_id'))

# Add index
gamma_dt[,n:= 1:nrow(gamma_dt)]

# Save full data set in Intermediate folder
fwrite(gamma_dt,"FILEPATH/00_combined_with_prev_RTR.csv")

