################################################################################
### Author: Elye Bliss
### Date: 11/30/2023
### Project: Global dementia spending
### Purpose: Prep dementia prevalence forecasts for 9 new GBD countries not 
### previously forecasted

### Overall steps:
# 1. calculate AROC for each age group for all countries in the missing countries' 
#    regions 
# 2. use the means for each as the missing countries' prevalence rates (still by 
#    age group) 
# 3. apply to those countries actual populations to get counts and populations by 
#    age group 
# 4. reweight to get overall prevalence across age groups
################################################################################

rm(list=ls()) 

library(data.table)

library("metR", lib.loc = "FILEPATH")

### Load required data files and helper functions

# locations
locs <- fread(file = "FILEPATH/location_set_22_metadata.csv")
loc_ids = locs[level==3]$location_id

# load internal AROC helper function
source('FILEPATH/helper_functions.R')

# get age groups
source("FILEPATH/get_ids.R")
age_group_ids <-get_ids('age_group')

# get estimates by age group and forecasts for 9 missing countries (added since 2017)
missing_locs <- fread('FILEPATH/missing_locs.csv')

## 2031 - 2050 prevalence draws:
# each country has its own file
prev_path2 ="FILEPATH"
prev_files2 = paste0(prev_path2, 
                     list.files(prev_path2, 
                                pattern = "^collapseddraws_.*.*[0-9]\\.rds$"))

# get lists of required age groups and required countries
example_file <- readRDS(prev_files2[1])
age_group_ids[age_group_id %in% unique(example_file$age_group_id)]
# age_group_id   age_group_name
# 1:           13         40 to 44
# 2:           14         45 to 49
# 3:           15         50 to 54
# 4:           16         55 to 59
# 5:           17         60 to 64
# 6:           18         65 to 69
# 7:           19         70 to 74
# 8:           20         75 to 79
# 9:           22         All Ages
# 10:           27 Age-standardized
# 11:           30         80 to 84
# 12:           31         85 to 89
# 13:           32         90 to 94
# 14:          235          95 plus

# 'All Ages' and 'Age-standardized' not needed
required_ages <- age_group_ids[age_group_id %in% unique(example_file[!(age_group_id %in% c(22,27))]$age_group_id)]

# save required age_group_ids for parellization script below
fwrite(required_ages,'FILEPATH/required_age_group_ids.csv')

# get required countries
missing_locs_regions <- unique(missing_locs$region_name)
required_locs <- locs[region_name %in% missing_locs_regions]

# save required locations for parellization script below
required_GBD_locs <- rbind(required_locs[,.(location_id,location_name)],missing_locs[,.(location_id,location_name)])
fwrite(required_GBD_locs,paste0(j_root,'FILEPATH/required_GBD_locs.csv'))

# get the 2050 forecasts for each age group for all countries in the regions of 
# the new countries

# limit prev_files2 to required country file list 
search_files <- function(location_id,file_list,filename_stem){
  
  expression <- paste0('.*',filename_stem,'_',location_id,'\\.rds')
  
  files_result <- str_extract_all(file_list,expression,simplify = TRUE)
  files_result <- files_result[files_result!=""]
  
  return(files_result)
}

required_files <- sapply(required_locs$location_id,search_files,prev_files2,'collapseddraws')

all_locs_forecasts_2050 <- rbindlist(lapply(required_files, readRDS))[(year_id == 2050)&
                                                                        (age_group_id %in% required_ages$age_group_id)]

all_locs_forecasts_2050[,num := NULL] #only using prevalence

# get the 2019 levels for all required locs as well as missing locs
path_2019 = "FILEPATH/"
path_2019_files = paste0(path_2019, 
                         list.files(path_2019, 
                                    pattern = "^prev_by_age_[0-9]*\\.csv$"))

all_locs_gbd_2019 <- rbindlist(lapply(path_2019_files, fread)) # files in wide format

# melt to long format
all_locs_gbd_2019 <- all_locs_gbd_2019[,-c('metric_id','model_version_id','measure_id',
                                           'modelable_entity_id','version_id')]

all_locs_gbd_2019 <- melt(all_locs_gbd_2019,
                          id.vars = c('location_id',"age_group_id",'sex_id','year_id'))
setDT(all_locs_gbd_2019)
setnames(all_locs_gbd_2019,old=c('variable','value'),new=c('draw','prev'))

# draws from GBD are formatted differently, and from 0-999 rather than 1-1000
all_locs_gbd_2019[,draw:=as.integer(gsub('draw_','', draw))]
all_locs_gbd_2019[,draw:=draw+1]

### validate that all countries required are accounted for

# Required but not pulled from gbd 2019
# required_GBD_locs$location_id contains missing countries as well as countries
# of their region. all_locs_gbd_2019 are the successfully-loaded 2019 prevalence 
# values.
setdiff(unique(required_GBD_locs$location_id),unique(all_locs_gbd_2019$location_id))
#integer(0)

# Exists as forecasts but not found in forecasted data
# required_locs$location_id contains just the countries of the regions of the 
# missing countries (for whom 2050 forecasts were already made). all_locs_forecasts_2050
# contains the successfully-loaded forecasts for those countries.
setdiff(unique(required_locs$location_id),unique(all_locs_forecasts_2050$location_id))
# integer(0)

# calculate AROC across all required countries
data <- rbind(all_locs_forecasts_2050,all_locs_gbd_2019[location_id %in% unique(all_locs_forecasts_2050$location_id)]) 

setnames(data, old = "prev", new = "data_var") # naming required by function 

# Selecting needed data years and renaming variables as required by helper function 
data <- merge(data,locs[,.(location_id,location_name)],on='location_id')

# calculating aroc between 2000 and 2019 
data_aroc <- create_metric(data,
                           id_var = c('year_id','sex_id','age_group_id', 'location_id', 'location_name'), 
                           aroc_years = c(2019, 2050))
setnames(data_aroc, old = 'aroc_output', new = 'aroc')

# get mean by region
data_aroc <- merge(data_aroc,locs[,.(location_id,region_name)],on='location_id')

# getting mean aroc by region (also by sex and age group)
aroc_means <- data_aroc[,.(mean_aroc=mean(aroc)),by=.(sex_id,age_group_id,region_name)]


### get 2019 estimates for new countries

# project prevalence rates based off of 2019 levels for new countries
prev_forecasts_by_age <- all_locs_gbd_2019[location_id %in% missing_locs$location_id]
prev_forecasts_by_age <- merge(prev_forecasts_by_age,missing_locs[,.(location_id,region_name)],on='location_id')
prev_forecasts_by_age <- merge(prev_forecasts_by_age,aroc_means,by=c('sex_id','age_group_id','region_name'))
setnames(prev_forecasts_by_age,'mean_aroc','aroc')

# the below two vectors from 2019 data and placeholder data frame are used in the 
# loop, with a new table for each year added to prev_forecasts_by_age
arocs_2019 <- prev_forecasts_by_age$aroc
values_2019 <- prev_forecasts_by_age$prev
prev_forecasts_by_age_empty <- prev_forecasts_by_age[,.(location_id,age_group_id,sex_id,draw,region_name)]

for (year in 2020:2050){
  
  data_temp <- copy(prev_forecasts_by_age_empty)

  # set year_id to projected year
  data_temp[,year_id:=year]
  
  # Derive AROC between 2019 and projected year for all draws (arocs_2019 is
  # an array of length equal to number of draws)
  data_temp[,aroc :=(1 + arocs_2019)^(year-2019)]
  
  # Use derived AROCs to project values for projected year for all draws 
  # (values_2019 is also an array of length equal to number of draws)
  data_temp[,prev := aroc*values_2019]
  
  prev_forecasts_by_age <- rbind(prev_forecasts_by_age,data_temp)
  
}

range(prev_forecasts_by_age$prev) 
# [1] 0.000034356 0.665003741

# load population forecast data: 
forecast_file = "FILEPATH/population.nc"
GlanceNetCDF(forecast_file)
pop_forecast = ReadNetCDF(forecast_file, 
                          subset = list(year_id = c(2019,2050), 
                                        location_id = missing_locs$location_id,
                                        age_group_id = required_ages$age_group_id)) 
pop_forecast[,scenario:= NULL] # not used and all equal to 0

# use mean population to take estimates out of draw space
pop_forecast <- pop_forecast[,.(population = mean(population)),by=.(location_id,
                                                                    age_group_id,
                                                                    sex_id,
                                                                    year_id)]

# file pulled has extra location_ids in it
pop_forecast <- pop_forecast[location_id %in% missing_locs$location_id]

setDT(pop_forecast)
pop_forecast[,":=" (location_id= as.integer(location_id),
                    age_group_id= as.integer(age_group_id),
                    sex_id = as.integer(sex_id),
                    year_id = as.integer(year_id))]

pop_forecast <- merge(pop_forecast,missing_locs[,.(location_id,region_name)],on='location_id')

### Merge data and create age_group_id 22 forecasts
prev_forecasts_by_age <- merge(prev_forecasts_by_age,pop_forecast,by=c('location_id','age_group_id','sex_id','year_id','region_name'))

# get prevalence counts
prev_forecasts_by_age[,total_cases := prev*population]

# reweight across age groups and sexes
prev_forecasts_missing_locs <- prev_forecasts_by_age[,.(total_cases = sum(total_cases),
                                                        population = sum(population)),
                                                     by=.(location_id,
                                                          region_name,
                                                          year_id,
                                                          draw)]
prev_forecasts_missing_locs[,":=" (prevalence = total_cases/population)]

prev_forecasts_missing_locs[,":=" (region_name = NULL,
                                   population = NULL)]

# check results
range(prev_forecasts_missing_locs$total_cases)
#[1]    6.214365 1233.952600
range(prev_forecasts_missing_locs$prevalence)
#[1] 0.004222367 0.069667468

### save updated data

# load previous
prev_sum <- fread("FILEPATH/dementia_prevalence_forecast_draws.csv")

range(prev_sum$total_cases)
#[1]       92.26231 65529475.74573
range(prev_sum$prevalence)
#[1] 0.0009091325 0.0755248737

# merge and save
combined <- rbind(prev_sum,prev_forecasts_missing_locs[year_id>2019])
fwrite(combined,"FILEPATH/dementia_prevalence_forecast_draws_locs_included.csv")


