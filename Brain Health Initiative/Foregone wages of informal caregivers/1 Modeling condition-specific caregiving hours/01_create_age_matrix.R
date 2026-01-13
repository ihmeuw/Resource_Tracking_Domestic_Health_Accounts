##########################################################################
# Title:    01_create_age_matrix.R
# Author: USERNAME
# Date: 2024-12-10 
# Project:  BHI - Brain Health Initiative
# Purpose: Read in and clean/process informal care extractions 
##########################################################################

#----# Set up directory roots #----#
## Source functions
source('FILEPATH/utils.R')
source('FILEPATH/currency_conversion.R')
source("FILEPATH/get_location_metadata.R")
source("FILEPATH/get_age_metadata.R")
source("FILEPATH/get_population.R")
source("FILEPATH/get_outputs.R")

#----# Local CONSTANTS #----#
## variable prep
data_yr <- 2019
gbd_release_id <- 16
years <- 2000:(data_yr)
converter_version <- 7.0
currency_convert_yr <- 2021
locs <- get_location_metadata(location_set_id = 35, release_id = gbd_release_id)[level == 3]
ages <- get_age_metadata(age_group_set_id = 24, release_id = gbd_release_id)

####----# Lists #----####
cat(' Lists\n')

## Age crosswalk
age_info <- copy(ages[, c('age_group_id', 'age_group_name', 'age_group_years_start', 'age_group_years_end'), with = F])
age_info <- age_info[age_group_id %in% 6:30]
age_info <- rbind(age_info, data.table(age_group_id = c(1, 160), age_group_name = c('Under 5', '85 plus'), age_group_years_start = c(0, 85), age_group_years_end = c(5, 125)))

all_ages <- data.table(age = 0:125, age_group_id = c(sort(rep(c(1, 6:20, 30), 5)), rep(160, 41)))
all_ages <- merge(all_ages, age_info, by = 'age_group_id', all.x = T)


###-----------------------------------------------------------------
###------- Read in files & assign caregiver age group IDs
###-----------------------------------------------------------------


## Get file names
extraction_filepaths <- list.files('FILEPATH')
extraction_filepaths <- extraction_filepaths[!extraction_filepaths %in% c('archive')]

## Create empty dataset to fill in by cause
dt <- data.table()

for (path in extraction_filepaths) {
  
  # Read in extractions
  temp <- fread(paste0('FILEPATH/', path))
  
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
                   'n_caregiver_female', 'hours_informal_care_wk', 'care_type', 'caregiver_sample_size'), with = F]
  
  ## MS Ireland study fix
  if (path == 'Multiple sclerosis.csv') {
    temp[pmid == 29320900, `:=` (n_caregiver_male = NA, n_caregiver_female = NA)]
    
  }
  
  ## Delete duplicate rows
  temp <- unique(temp)
  
  # Fix max caregiver ages
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
    
    temp_fix <- copy(temp[pmid %in% c(35652417, 21804344, 21957864, 24453784)])
    temp <- temp[!pmid %in% c(35652417, 21804344, 21957864, 24453784)]
    keep_cols <- names(temp_fix)
    keep_cols <- keep_cols[!keep_cols %in% c('caregiver_sample_size', 'hours_informal_care_wk')]
    temp_fix <- temp_fix[, .(hours_informal_care_wk = weighted.mean(hours_informal_care_wk, caregiver_sample_size, na.rm = T)),
                         by = keep_cols]
    temp_fix[, caregiver_sample_size := n_caregiver_male + n_caregiver_female]
    temp <- rbind(temp, temp_fix)
    rm(temp_fix, keep_cols)
  }
  
  ## MS Brazil study fix
  if (path == 'Multiple sclerosis.csv') {
    temp_fix <- copy(temp[pmid == 30673707])
    temp <- temp[pmid != 30673707]
    temp_fix[, patient_sample_size := 0]
    temp_fix[hours_informal_care_wk == 46.2, patient_sample_size := 81]
    temp_fix[hours_informal_care_wk == 51.8, patient_sample_size := 94]
    temp_fix[hours_informal_care_wk == 95.9, patient_sample_size := 35]
    keep_cols <- names(temp_fix)
    keep_cols <- keep_cols[!keep_cols %in% c('patient_sample_size', 'hours_informal_care_wk')]
    temp_fix <- temp_fix[, .(hours_informal_care_wk = weighted.mean(hours_informal_care_wk, patient_sample_size, na.rm = T)),
                         by = keep_cols]
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
  
  # Round any minimum or maximum age values
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
dt[pmid %in% c(24936758, 28134026), location_name := 'Türkiye']
  

## Create caregiver min and max values for data using means by care group
temp <- copy(dt[caregiver_age_range_available == 1])
temp <- temp[, .(min_caregiver_age_average = mean(min_caregiver_age),
                 max_caregiver_age_average = mean(max_caregiver_age)),
             by = .(cause_group)]
dt <- merge(dt, temp, by = 'cause_group', all.x = T)
dt[is.na(min_caregiver_age), min_caregiver_age := min_caregiver_age_average]
dt[is.na(max_caregiver_age), max_caregiver_age := max_caregiver_age_average]
save_dt <- copy(dt)

## Keep necessary columns and data with hours
dt <- dt[, c('pmid', 'location_name', 'cause_name', 'publication_year', 'data_collection_year', 'data_collection_year_start', 'data_collection_year_end',
             'min_caregiver_age', 'max_caregiver_age', 'n_caregiver_male', 'n_caregiver_female', 'hours_informal_care_wk', 'care_type'), with = F]
dt <- dt[!is.na(hours_informal_care_wk)]


## Expand caregiver age groups
dt[, `:=` (min_caregiver_age = round(min_caregiver_age, 0),
           max_caregiver_age = round(max_caregiver_age, 0))]

dt[, included_ages := as.character(Map(function(a,b) paste0(a:b, collapse = ','), min_caregiver_age, max_caregiver_age))]
cols <- names(dt)

dt <- cbind(dt, setDT(tstrsplit(as.character(dt$included_ages), ",", fixed = T))[])

dt <- melt.data.table(dt,
                      id.vars = cols,
                      variable.factor = F,
                      value.name = 'age',
                      na.rm = T)

dt[, age := as.numeric(age)]

dt <- merge(dt, all_ages[, .(age, age_group_id)], by = 'age', all.x = T)

dt[, c('age', 'variable', 'included_ages') := NULL]
dt <- unique(dt)

## Probability matrices for caregiver and patient ages (patient age should add to 1)
dt <- copy(save_dt)
dt <- dt[, .(cause_group, pmid, location_name, cause_name, data_collection_year, data_collection_year_start, data_collection_year_end, min_caregiver_age,
             max_caregiver_age, min_patient_age, max_patient_age, caregiver_age_range_available)]
dt[max_patient_age == '', max_patient_age := NA]
dt <- dt[caregiver_age_range_available == 1 & !is.na(min_patient_age) & !is.na(max_patient_age)]
dt <- unique(dt)


## Keep necessary columns
dt <- dt[, c('cause_group', 'min_caregiver_age', 'max_caregiver_age', 'min_patient_age', 'max_patient_age'), with = F]
dt[, max_patient_age := as.numeric(max_patient_age)]

## Round ages
to_round <- names(dt)[grepl('age', names(dt))]
dt[, c(to_round) := round(.SD, 0), .SDcols = to_round]


## Create row for each caregiver age group
dt[, included_ages := as.character(Map(function(a,b) paste0(a:b, collapse = ','), min_caregiver_age, max_caregiver_age))]
cols <- names(dt)

dt <- cbind(dt, setDT(tstrsplit(as.character(dt$included_ages), ",", fixed = T))[])

dt <- melt.data.table(dt,
                        id.vars = cols,
                        variable.factor = F,
                        value.name = 'age',
                        na.rm = T)

dt[, age := as.numeric(age)]

dt <- merge(dt, all_ages[, .(age, age_group_id)], by = 'age', all.x = T)

dt[, c('age', 'variable', 'included_ages') := NULL]
dt <- unique(dt)

setnames(dt, 'age_group_id', 'age_group_id_caregiver')

## Create row for each patient age group
dt[, included_ages := as.character(Map(function(a,b) paste0(a:b, collapse = ','), min_patient_age, max_patient_age))]
cols <- names(dt)

dt <- cbind(dt, setDT(tstrsplit(as.character(dt$included_ages), ",", fixed = T))[])

dt <- melt.data.table(dt,
                        id.vars = cols,
                        variable.factor = F,
                        value.name = 'age',
                        na.rm = T)

dt[, age := as.numeric(age)]

dt <- merge(dt, all_ages[, .(age, age_group_id)], by = 'age', all.x = T)

dt[, c('age', 'variable', 'included_ages') := NULL]
dt <- unique(dt)

setnames(dt, 'age_group_id', 'age_group_id_patient')


## Remove min and max columns
dt <- dt[, c('cause_group', 'age_group_id_caregiver', 'age_group_id_patient'), with = F]

## Reshape wide to create matrix (should default to length)
dt[, value := 1]
dt[, cause_group := 'All BHI causes']
dt <- dcast.data.table(dt, cause_group + age_group_id_patient ~ age_group_id_caregiver, value.var = 'value')


## Create final table to merge on for all possible caregiver and cause group combinations
final_dt <- CJ(cause_group = c('All BHI causes'),
               age_group_id_patient = unique(dt$age_group_id_patient))
final_dt <- merge(final_dt, dt, by = c('cause_group', 'age_group_id_patient'), all.x = T)
final_dt[is.na(final_dt)] <- 0

## Create proportional matrices
to_update <- names(final_dt)[!names(final_dt) %in% c('cause_group', 'age_group_id_patient')]
final_dt_proportion <- copy(final_dt)
final_dt_proportion[, c(to_update) := round(.SD / rowSums(.SD, na.rm = T) * 100, 1), .SDcols = to_update]
final_dt_proportion[is.na(final_dt_proportion)] <- 0

## Save out matrices
fwrite(final_dt_proportion,'FILEPATH/01_all_cause_matrix_no_average_values.csv')
