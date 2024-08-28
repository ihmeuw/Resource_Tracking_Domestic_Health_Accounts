# Clean and Combine old+new Extraction Data for Beta (Diagnosis + Treatment rate)
# Author: Michael Breshock
# Date: 07/25/2023

# Clear environment
rm(list = ls())

# Loading libraries
library(data.table)
library(dplyr)
library(tidyr)
library(readxl)

# load new raw diagnosis extraction data: 
new_data = data.table(read_excel("FILEPATH/diagnosis extraction 2023_06_30.xlsx",
                                 sheet = "Extraction"))[-1] # first row is description of columns - not needed
# load old raw diagnosis extraction data: 
old_data = fread("FILEPATH/19.06.05.dementia_dx_extraction_template.csv")

## fix new extraction data: 
# convert sample size to numeric: 
new_data[, sample_size := as.numeric(sample_size)]

# create totals for Aldus source: 
# extract one row to use as template
aldus_total = new_data[source_id == "Aldus" & sex == "male"]
# replace cases and sample_size numbers by aggregating sex
aldus_total$cases = sum(new_data[source_id == "Aldus" & notes == "sex split"]$cases)
aldus_total$sample_size = sum(new_data[source_id == "Aldus" & notes == "sex split"]$sample_size)
aldus_total$sex = "all"
aldus_total$notes = "total"
# combine aldus totals with data: 
new_data = rbind(new_data, aldus_total)

# filter to just totals or institution/community splits (or missing notes): 
new_data = new_data[notes %in% c("total", "living situation split") | is.na(notes)]

# fix year variable for Ura Japan paper: 
# paper was published in 2020, but submitted in 2019 -> want to label it as 2019 data not 2020
# doing this so it doesn't get cut when merged with covariate data that ends at 2019 
new_data[source_id == "Ura"]$year_end = 2019

## add dementia diagnosis gap to new data: 
# Dementia diagnosis gap (ddgn) = fraction of dementia cases that are diagnosed
new_data[, dementia_diagnosis_gap := cases/sample_size*100] # multiplying by 100 to match Paola's percentage format 
# cases = number of diagnosed cases
# sample size = total number of people categorized with dementia

# add missing columns to old data: 
colnames(new_data)[!(colnames(new_data) %in% colnames(old_data))]
old_data[, ":=" (study_name = NA, cases = NA)]

# row bind old and new data together 
data = rbind(old_data, new_data)

## Clean data:
data[, year_published := as.numeric(year_published)] #making numeric
data[living_conditions == "institution", living_conditions := "Institution"] #cleaning typos

# Getting ISO code from variable location_name_id
data[, ihme_loc_id:= tstrsplit(location_name_id, "|", fixed = TRUE, keep = 2)] 

# Cleaning year_id variable
data[, year_id := year_end]
data[is.na(year_end), year_id := year_published]
data<- data[year_id >= 1990] #removing rows prior to 1990 (cuts one row)

# cleaning dementia diagnosis gap variable
# making the dementia_diagnosis_gap numeric and a fraction
data[initials != "vi", dementia_diagnosis_gap := # only need to do this for Paola's numbers
       tstrsplit(dementia_diagnosis_gap, "%", fixed = TRUE, keep = 1)] 
data[, ddgn:= as.numeric(dementia_diagnosis_gap)/100] #ddgn dementia diagnosis gap numeric

# Diagnosis over dementia cases
#Converting all units to diagnosed over dementia cases

#no dx over dementia cases
data[ reported_value == 'no diagnosed over dementia cases', clean_value := 1 - ddgn]
data[ reported_value == 'no diagnosed over dementia cases', clean_units := 'dx_over_dem_cases']

#dx but unware over dementia cases
data[ reported_value == 'diagnosed but unware over dementia cases', clean_value := ddgn]
data[ reported_value == 'diagnosed but unware over dementia cases', clean_units := 'dx_over_dem_cases']

#dx and aware over dementia cases
data[ reported_value == 'diagnosed and aware over dementia cases', clean_value := ddgn]
data[ reported_value == 'diagnosed and aware over dementia cases', clean_units := 'dx_over_dem_cases']

#diagnosed over dementia cases
data[ reported_value == 'diagnosed over dementia cases', clean_value := ddgn]
data[ reported_value == 'diagnosed over dementia cases', clean_units := 'dx_over_dem_cases']

# Treatment rate over diagnosed dementia cases
# Converting all units to treated cases over dementia cases

# Treatment over dementia cases 
data[ reported_value == 'trx over dementia cases', clean_value := ddgn]
data[ reported_value == 'trx over dementia cases', clean_units := 'trx_over_dem_cases']

#treatment over dementia cases 
data[ reported_value == 'no trx over dementia cases', clean_value := 1 - ddgn]
data[ reported_value == 'no trx over dementia cases', clean_units := 'trx_over_dem_cases']

data[, .N, by = clean_units] #Just checking that everything worked out

#dropping data points that I couldn't standardize (n = 2) 
data <- data[!(is.na(clean_units))]

############## CLEANING EXTRACTION DATA / REMOVING INCONSITENCIES ##############

# look at duplicates: 
data_dups = data[duplicated(data[,.(location_name_id, year_id, living_conditions, source_id)])]
# most of these are due to severity splits / splits indicated in the 
# "what_was_measured_DNU" column 

## example: source #5 - USA community rates -> there are three values provided 
# one of these is "diagnosed but unware", one is "diagnosed and aware" 
# and the other is the overall diagnosis rate, the sum of the other two. 
# we just need the overall observation, so cutting the other two

# first confirming that this source is the only one with a "aware vs unaware" split:
nrow(data[reported_value == "diagnosed but unware over dementia cases"]) # = 1
nrow(data[reported_value == "diagnosed and aware over dementia cases"]) # = 1
# confirmed

# removing the aware vs unaware split: 
data_clean = data[!(reported_value %in% 
                      c("diagnosed but unware over dementia cases",
                        "diagnosed and aware over dementia cases"))]

## source #35 - Germany community rates -> severity split and overall rate provided
# the overall rate seems to be miscalculated 
# -> should be a weighted average of the severity split data
germ35_val = data[source_id == 35 & severity == ""]$clean_value
germ35_sev = data[source_id == 35 & severity != ""]
germ35_sev_mean = weighted.mean(germ35_sev$clean_value, w = germ35_sev$sample_size)
germ35_val == germ35_sev_mean # FALSE
# checking sample size: 
germ35_sev_n = sum(germ35_sev$sample_size)
germ35_sev_n == data[source_id == 35 & severity == ""]$sample_size # FALSE 

# fixing source 35 data: 
data_clean[source_id == 35 & severity == "", 
           ":=" (clean_value = germ35_sev_mean, 
                 sample_size = germ35_sev_n)]

# looking at severity split data to check that there is always an overall estimate 
# recode severity column for more readability
data[severity == "" | is.na(severity), severity := "overall"]

# pivot wider so that there is one row per source: 
data_wide = data[,.(source_id, ihme_loc_id, year_id, severity, 
                    living_conditions, clean_value, clean_units)] %>%
  pivot_wider(names_from = severity, values_from = clean_value)
setDT(data_wide)
# see if any sources are missing overall diagnosis rate: 
data_wide[overall == "NULL"] # zero cases -> not losing any sources here

# set NA values in "severity" column from Vinnie's extractions to blank string ("")
# to be consistent with data from Paola's extractions
data_clean[is.na(severity), severity := ""]

# filter to just diagnosis rate data, remove severity splits,
# remove data missing location: 
data_clean = data_clean[clean_units == "dx_over_dem_cases" & severity == "" & 
                          location_name_id != ""]

# remove AD specific diagnosis rate from source 30
data_clean = data_clean[what_was_measured_DNU != "of undiagnosed have AD"]

# look for duplicates: 
data_clean[,.(source_id, year_id, ihme_loc_id, clean_value, living_conditions)] %>%
  group_by(source_id, year_id, ihme_loc_id, clean_value, living_conditions) %>%
  summarise(n = n(), .groups = "drop") %>% 
  filter(n > 1L)
# 1 duplicate:
# source_id = 8, year_id = 2011, location = GBR, living_condition = Institution

# removing one of the duplicate rows: 
data_clean = data_clean[!(source_id == 8 & year_id == 2011 & ihme_loc_id == "GBR" & 
                            reported_value == "no diagnosed over dementia cases")]
# the other row had reported_value = "diagnosed over dementia cases"

# saving clean data to csv: 
fwrite(data_clean, file = 
         "FILEPATH/00_clean_combined_data.csv")

