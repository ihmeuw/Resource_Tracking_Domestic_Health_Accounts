##########################################################################
# Title:  03_create_model_data_all_age.R
# Author: USERNAME
# Date: 2025 03 13
# Project:  BHI - Brain Health Initiative
# Purpose : To extract, clean, and standardize literature data on informal 
#           caregiving hours and create initial modeling dataset. Output file will 
#           be used in 04_create_covariates.R script.
##########################################################################

# Setting up environment
rm(list=ls())

## Source functions
source("FILEPATH/get_location_metadata.R")
source("FILEPATH/get_age_metadata.R")
source("FILEPATH/get_population.R")
source("FILEPATH/get_outputs.R")

# file paths
file_dir <- "FILEPATH"
un_caregiver_data_path <- "FILEPATH/06_stgpr_estimates.csv"

# Import data
un_caregiver_dt <- fread(un_caregiver_data_path)

# Data info
#----# Local CONSTANTS #----#
## variable prep
data_yr <- 2021
gbd_release_id <- 16
years <- 2000:(data_yr)
converter_version <- 7.0
currency_convert_yr <- 2021
locs <- get_location_metadata(location_set_id = 35, release_id = gbd_release_id)[level == 3]
ages <- get_age_metadata(age_group_set_id = 24, release_id = gbd_release_id)

# Age data
####----# Lists #----####
cat(' Lists\n')

## Age crosswalk
age_info <- copy(ages[, c('age_group_id', 'age_group_name', 'age_group_years_start', 'age_group_years_end'), with = F])
age_info <- age_info[age_group_id %in% 6:30]
age_info <- rbind(age_info, data.table(age_group_id = c(1, 160), age_group_name = c('Under 5', '85 plus'), age_group_years_start = c(0, 85), age_group_years_end = c(5, 125)))

all_ages <- data.table(age = 0:125, age_group_id = c(sort(rep(c(1, 6:20, 30), 5)), rep(160, 41)))
all_ages <- merge(all_ages, age_info, by = 'age_group_id', all.x = T)

# Read in files & assign caregiver age group IDs
## Get file names
extraction_filepaths <- list.files('FILEPATH')
extraction_filepaths <- extraction_filepaths[!extraction_filepaths %in% c('archive')]

# Literature data reshaping
## Create empty dataset to fill in by cause
dt <- data.table()

for (path in extraction_filepaths) {
  
  # Read in extractions
  temp <- fread(paste0('FILEPATH', path))
  
  # Remove any blank rows (should catch all)
  temp <- temp[!((is.na(pmid) | pmid == '') & location == '')]
  
  # Assign cause name for data organization
  temp[, cause_name := gsub('.csv', '', path)]
  temp[cause_name == 'ADHD', cause_name := 'Attention-deficit/hyperactivity disorder']
  
  # Fix differences in column names
  if ('avg_caregiver_age_range' %in% names(temp)) {setnames(temp, 'avg_caregiver_age_range', 'avg_caregiver_age')}
  if ('caregiver_age_min' %in% names(temp)) {setnames(temp, 'caregiver_age_min', 'min_caregiver_age')}
  if ('caregiver_age_max' %in% names(temp)) {setnames(temp, 'caregiver_age_max', 'max_caregiver_age')}
  if ('patient_age_average' %in% names(temp)) {setnames(temp, 'patient_age_average', 'avg_patient_age')}
  if ('patient_age_min' %in% names(temp)) {setnames(temp, 'patient_age_min', 'min_patient_age')}
  if ('patient_age_max' %in% names(temp)) {setnames(temp, 'patient_age_max', 'max_patient_age')}
  if ('n_male' %in% names(temp)) {setnames(temp, 'n_male', 'n_caregiver_male')}
  if ('n_female' %in% names(temp)) {setnames(temp, 'n_female', 'n_caregiver_female')}
  
  ## Update if no patient age or publication start/end data available
  if ('avg_patient_age' %in% names(temp) == F) {temp[, avg_patient_age := NA]}
  if ('min_patient_age' %in% names(temp) == F) {temp[, min_patient_age := NA]}
  if ('max_patient_age' %in% names(temp) == F) {temp[, max_patient_age := NA]}
  if ('data_collection_year_start' %in% names(temp) == F) {temp[, data_collection_year_start := NA]}
  if ('data_collection_year_end' %in% names(temp) == F) {temp[, data_collection_year_end := NA]}
  
  ## Keep relevant columns
  temp <- temp[, c('pmid', 'location', 'cause_name', 'publication_year', 'data_collection_year', 'data_collection_year_start', 'data_collection_year_end',
                   'avg_caregiver_age', 'min_caregiver_age', 'max_caregiver_age', 'avg_patient_age', 'min_patient_age', 'max_patient_age', 'n_caregiver_male',
                   'n_caregiver_female', 'hours_informal_care_wk', 'care_type', 'caregiver_sample_size', 'notes', 'percent_receiving_informal_care', 'estimation_method'), with = F]
  
  ## MS Ireland study fix
  if (path == 'Multiple sclerosis.csv') {
    temp[pmid == 29320900, `:=` (n_caregiver_male = NA, n_caregiver_female = NA)]
    
  }
  
  ## Delete duplicate rows
  temp <- unique(temp)
  
  # Fix max caregiver ages that are just everything (don't really understand how this value occurs but whatever)
  temp[max_caregiver_age %in% c('50+', '99'), max_caregiver_age := NA]
  temp[min_caregiver_age %in% c('18', '30') & path != 'Stroke.csv', min_caregiver_age := NA]
  temp[min_caregiver_age %in% c(18, 20, 21) & !pmid %in% c(25952926, 22531368), min_caregiver_age := NA]
  temp[max_patient_age %in% c('90+'), max_patient_age := '90']
  
  # Alcohol disorders fix
  if (path == 'Alcohol disorders.csv') {
    temp[caregiver_sex == 'male', caregiver_sample_size := n_male]
    temp[caregiver_sex == 'female', caregiver_sample_size := n_female]
    temp[, caregiver_sex := NULL]
  }
  
  ## Brain cancer fix
  if (path == 'Brain and central nervous system cancer.csv') {
    temp <- temp[!(pmid == 27624465 & is.na(hours_informal_care_wk))]
  }
  
  ## Motor neuron disease/MS fix
  if (path %in% c('Motor neuron disease.csv', 'Multiple sclerosis.csv')) {
    temp[pmid == 35652417, `:=` (avg_patient_age = 57.3, min_patient_age = 26, max_patient_age = 83, hours_informal_care_wk = hours_informal_care_wk * 7)]
    temp[pmid == 21804344, hours_informal_care_wk := hours_informal_care_wk * 7]
    
    temp_fix <- copy(temp[pmid %in% c(35652417, 21804344)])
    temp <- temp[!pmid %in% c(35652417, 21804344)]
    keep_cols <- names(temp_fix)
    keep_cols <- keep_cols[!keep_cols %in% c('caregiver_sample_size', 'hours_informal_care_wk')]
    temp_fix <- temp_fix[, .(hours_informal_care_wk = weighted.mean(hours_informal_care_wk, caregiver_sample_size, na.rm = T)),
                         by = keep_cols]
    temp_fix[, caregiver_sample_size := n_caregiver_male + n_caregiver_female]
    temp <- rbind(temp, temp_fix)
    rm(temp_fix, keep_cols)
  }
  
  ## Stroke USA, Nigeria study fix
  if (path == 'Stroke.csv') {
    temp[pmid == 30929619, `:=` (min_caregiver_age = 30, max_caregiver_age = 89)]
    temp[pmid == 30734330, `:=` (min_caregiver_age = 17, max_caregiver_age = 66, min_patient_age = 23, max_patient_age = 83)]
    temp[pmid == 27387990, `:=` (max_patient_age = 91)]
    temp[pmid == 12084872, `:=` (max_patient_age = 91)]
    temp[pmid == 29153121, `:=` (max_patient_age = 86)]
    temp[pmid == 29236217, `:=` (min_caregiver_age = 30, max_caregiver_age = 93)]
    temp[pmid == 20487482, `:=` (min_caregiver_age = 22, max_caregiver_age = 82, min_patient_age = 47, max_patient_age = 103)]
  }
  
  ## Autism Brazil study fix
  if (path == 'Autism spectrum disorders.csv') {
    temp_fix <- copy(temp[pmid == 35613240])
    temp <- temp[pmid != 35613240]
    temp_fix[, `:=` (min_caregiver_age = 23, max_caregiver_age = 56, n_caregiver_female = 51)]
    temp_fix[hours_informal_care_wk == 24.5, hours_informal_care_wk := 4*7]
    temp_fix[hours_informal_care_wk == 87.5, hours_informal_care_wk := 12*7]
    keep_cols <- names(temp_fix)
    keep_cols <- keep_cols[!keep_cols %in% c('caregiver_sample_size', 'hours_informal_care_wk')]
    temp_fix <- temp_fix[, .(hours_informal_care_wk = weighted.mean(hours_informal_care_wk, caregiver_sample_size, na.rm = T)),
                         by = keep_cols]
    temp_fix[, caregiver_sample_size := n_caregiver_female]
    temp <- rbind(temp, temp_fix)
    rm(temp_fix, keep_cols)
    
  }
  
  
  # Adjust blanks to NAs for code
  temp[min_caregiver_age == '', min_caregiver_age := NA]
  temp[max_caregiver_age == '', max_caregiver_age := NA]
  temp[avg_caregiver_age == '', avg_caregiver_age := NA]
  
  # Fix rows where only minimum or maximum is present
  temp[is.na(min_caregiver_age) & !is.na(max_caregiver_age) & !cause_name %in% c('Alcohol use disorders', 'Idiopathic epilepsy'), max_caregiver_age := NA]
  temp[is.na(max_caregiver_age) & !is.na(min_caregiver_age) & !cause_name %in% c('Alcohol use disorders', 'Idiopathic epilepsy'), min_caregiver_age := NA]
  
  # Make all caregiver age columns numeric
  temp[, min_caregiver_age := as.numeric(min_caregiver_age)]
  temp[, max_caregiver_age := as.numeric(max_caregiver_age)]
  
  # Round any minimum or maximum age values (check these in raw data because this makes very little practical sense)
  temp[, min_caregiver_age := round(min_caregiver_age, 0)]
  temp[, max_caregiver_age := round(max_caregiver_age, 0)]
  
  ## Mark rows with ranges from the original source
  temp[, caregiver_age_range_available := 0]
  temp[!is.na(min_caregiver_age) & !is.na(max_caregiver_age) & pmid != 27998922, caregiver_age_range_available := 1]
  temp[cause_name == 'Brain and central nervous system cancer' & pmid == 27624465 & care_type != 'all', caregiver_age_range_available := 0]
  temp[cause_name == "Parkinson's disease" & pmid == 24682360 & caregiver_sample_size == 18, caregiver_age_range_available := 0]
  temp[cause_name == 'Stroke' & pmid == 29236217 & care_type == 'ADL', caregiver_age_range_available := 0]
  temp[cause_name == 'Stroke' & pmid %in% c(27387990, 12084872, 29153121), caregiver_age_range_available := 0]
  
  ## Keep extractions as is if no caregiver age data available --------------------------------------------
  dt <- rbind(dt, temp)
  rm(temp)
  
}

# Additional data cleaning
## Assign mental or neurological
dt[, cause_group := ifelse(cause_name %in% c("Alzheimer's disease and other dementias",
                                             'Brain and central nervous system cancer',
                                             'Idiopathic epilepsy',
                                             'Meningitis',
                                             'Motor neuron disease',
                                             'Multiple sclerosis',
                                             "Parkinson's disease",
                                             'Stroke'), 'Neurological', 'Mental')]

## Fix location names
setnames(dt, 'location', 'location_name')
dt[location_name %in% c('UK', 'England', 'UK (London)'), location_name := 'United Kingdom']
dt[location_name %in% c('United States', 'Alabama', 'North Carolina, United States', 'Ohio', 'USA'), location_name := 'United States of America']
dt[location_name %in% c('Austrailia'), location_name := 'Australia']
dt[location_name %in% c('Czech Republic'), location_name := 'Czechia']
dt[location_name %in% c('Hungry'), location_name := 'Hungary']
dt[location_name %in% c('Korea', 'South Korea'), location_name := 'Republic of Korea']
dt[location_name %in% c('Russia'), location_name := 'Russian Federation']
dt[location_name %in% c('Turkey'), location_name := 'Türkiye']
dt[pmid == 28134026 & hours_informal_care_wk == 26.7, location_name := 'Türkiye']
dt[pmid == 24936758, location_name := 'Türkiye']

# Fix data_collection_year
# fixing range years
dt[data_collection_year == "1997\x961999", data_collection_year := 1999]
dt[data_collection_year == "2002-2003", data_collection_year := 2003]
dt[data_collection_year == "2004-2017", data_collection_year := 2017]
dt[data_collection_year == "2007-2009", data_collection_year := 2009]
dt[data_collection_year == "2009-2010", data_collection_year := 2010]
dt[data_collection_year == "2015-2016", data_collection_year := 2016]
dt[data_collection_year == "2016-2017", data_collection_year := 2017]
dt[, data_collection_year := as.integer(data_collection_year)]

# missing values
dt[, data_diff := publication_year - data_collection_year]
mean_yr_diff <- floor(mean(dt$data_diff, na.rm = T))

# replacing missing data collection years with pub year 
dt[is.na(data_collection_year), data_collection_year := publication_year - mean_yr_diff]

## Create caregiver min and max values for data using means by care group
temp <- copy(dt[caregiver_age_range_available == 1])
temp <- temp[, .(min_caregiver_age_average = mean(min_caregiver_age),
                 max_caregiver_age_average = mean(max_caregiver_age)),
             by = .(cause_group)]

dt <- merge(dt, temp, by = 'cause_group', all.x = T)
dt[is.na(min_caregiver_age), min_caregiver_age := min_caregiver_age_average]
dt[is.na(max_caregiver_age), max_caregiver_age := max_caregiver_age_average]
save_dt <- copy(dt)

# adding location id
dt <- merge(dt, locs[, .(location_name, location_id)], by = 'location_name', all.x = T)

## Keep necessary columns and data with hours
dt <- dt[, c('pmid', 'location_name', 'location_id', 'cause_name', 'publication_year', 'data_collection_year', 'data_collection_year_start', 'data_collection_year_end',
             'min_caregiver_age', 'max_caregiver_age', 'n_caregiver_male', 'n_caregiver_female', 'hours_informal_care_wk', 'care_type', 'notes', 'percent_receiving_informal_care', 'estimation_method'), with = F]

dt <- dt[!is.na(hours_informal_care_wk)]

## Expand caregiver age groups
dt[, `:=` (min_caregiver_age = round(min_caregiver_age, 0),
           max_caregiver_age = round(max_caregiver_age, 0))]

dt[, included_ages := as.character(Map(function(a,b) paste0(a:b, collapse = ','), min_caregiver_age, max_caregiver_age))]

# Reading in population data for weighted means
pop_wt_dt <- get_population(location_id = locs$location_id,
                            year_id = years,
                            age_group_id = c(8:20),
                            sex_id = c(1, 2),
                            release_id = gbd_release_id 
)

# adding ihme loc id 
pop_wt_dt <- merge(pop_wt_dt, locs[, .(location_id, ihme_loc_id)], by = "location_id")


# condensing columns
pop_wt_dt <- pop_wt_dt[, .(ihme_loc_id, 
                           location_id,
                           year_id, 
                           age_group_id,
                           sex_id,
                           population)]

# Adding population weight value to UN caregiver dt
un_caregiver_dt <- merge(un_caregiver_dt, pop_wt_dt, by = c('location_id', 'year_id', 'age_group_id', 'sex_id'), all.x = T)
nrow(un_caregiver_dt[is.na(population) & year_id <= 2021]) 
# 0

# Create UN aggregated mean hours of caregiving for model data
# Copy dt
agg_dt <- copy(dt)

# adding Index column
agg_dt[, ID := 1:nrow(agg_dt)]

# storing ID list
IDs <- agg_dt$ID

# creating empty list to store data
my_list <- list()

# looping though ID list and creating agg mean from UN data
for (i in IDs) {
  # individual row temp dt
  temp_dt <- agg_dt[ID == i]
  
  # storing location
  temp_loc <- temp_dt[ID == i, location_id]
  
  # storing year
  temp_year <- temp_dt[ID == i, data_collection_year]
  
  # storing ages
  temp_ages <- as.numeric(unlist(strsplit(unique(temp_dt$included_ages), ',')))
  temp_age_ids <- all_ages[age %in% temp_ages, age_group_id]
  
  # creating aggregated means hours of care
  temp_mean_hrs <- weighted.mean(un_caregiver_dt[location_id == temp_loc & year_id == temp_year & age_group_id %in% temp_age_ids]$mean, 
                                 un_caregiver_dt[location_id == temp_loc & year_id == temp_year & age_group_id %in% temp_age_ids]$population, 
                                 na.rm = T)
  
  temp_dt[, un_mean_hrs := temp_mean_hrs]
  
  # storing each row in list
  # list name
  list_name <- paste0(c(i, temp_loc, temp_year), collapse = "_")
  # adding to list
  my_list[[list_name]] <- copy(temp_dt)
}

# rbinding list
final_dt <- rbindlist(my_list)

# creating year from data collection year
final_dt[, year_id := data_collection_year]

# only retain relevant years
final_dt <- final_dt[year_id %in% years]

# Removing/adjusting papers after data vetting and model iterations
# # Brazil papers
# adjusting extracted values from Brazil MS paper due to only 30% of people were receiving informal care
final_dt[pmid == 30673707 & cause_name == "Multiple sclerosis" & location_name == "Brazil", hours_informal_care_wk := hours_informal_care_wk * 0.3]

# removing Autism paper from Brazil due to using bucket values for extracted values (ex.hours of care 0-8)
final_dt <- final_dt[!(pmid == 35613240 & cause_name == "Autism spectrum disorders" & location_name == "Brazil")]

# Saving out model dataset
fwrite(final_dt, "FILEPATH/caregiving_all_ages_model_dt.csv")
