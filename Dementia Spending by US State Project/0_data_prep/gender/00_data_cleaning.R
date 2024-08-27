##########################################################################
### Author: Michael Breshock
### Date: 09/18/2023
### Project: Global dementia spending
### Purpose: data cleaning for caregiving gender extraction
##########################################################################

rm(list=ls())

library(dplyr)
library(data.table)
library(tibble)

# this file updated by Amy Lastuka on 04/11/2024: 
cg_extraction <- fread("FILEPATH/caregiving extraction 2024_04_11.csv")

# cut the Zhu paper because I am not clear on whether everyone had dementia at baseline
cg_extraction <- cg_extraction[citation != "Zhu",]

# cut papers with multiple locations
cg_extraction <- cg_extraction[!location %like% ";",]

# cut papers for Hong Kong (not considered a country in GBD -> sad face)
cg_extraction <- cg_extraction[location != "Hong Kong"]

# cut Montgomery 2018 source from Japan -> Amy found that this was not a representative sample
cg_extraction <- cg_extraction[citation != "Montgomery"]

# remove Hurd paper -> this source just used for attributable fraction
# does not have data on caregiver gender
cg_extraction <- cg_extraction[citation != "Hurd"] 

# remove Cantu paper - includes paid and unpaid care time
cg_extraction <- cg_extraction[citation != "Cantu"] 

# remove Melaka paper - focuses on just frontotemporal dementia 
cg_extraction <- cg_extraction[citation != "Melaka"] 

# remove data missing location info and institutional values
cg_extraction = cg_extraction[location != "" & care_setting == "community"]
# lose one observation missing location 
# lose 6 institutional observations

# recode all "total" care_type observations to be blank
cg_extraction[care_type == "total", care_type := ""]

# change estimate method column name: 
setnames(cg_extraction, old = "estimation method", new = "estimate_method")

# filter to non-NA percent female values
sex = cg_extraction[!is.na(percent_female_cg)]

# view range of percent female values
range(sex$percent_female_cg) # 0.5230 81.6087
# see which source is > 1
sex[percent_female_cg > 1] # Nobili 2004 Italy
# fix this value to be in percent: 
sex[percent_female_cg > 1, percent_female_cg := percent_female_cg/100]
# sanity check: 
range(sex$percent_female_cg) # 0.523 - 0.950
sex[citation == "Nobili"]$percent_female_cg

#################### summarize severity/care type splits #######################
# extract all sources that already have overall estimates:
sex_overall = sex[severity == "" & care_type == ""]

# extract all severity and care type split data: 
sex_split = anti_join(sex, sex_overall)
sex_split <- sex_split %>% data.table() %>% as_tibble() %>% as.data.table()
# ^ have to do this for some reason to get the following command to work

# filter to just sources that don't already have an overall estimate: 
sex_split_fix = sex_split[!(citation %in% unique(sex_overall$citation))]

# check that sample size isn't missing: 
nrow(sex_split_fix[is.na(sample_size)]) # 0 -> phew! 

# first summarize data that is just care type splits: 
care_type_splits = sex_split_fix[severity == "" & care_type != ""]
care_type_sum = care_type_splits[,.(percent_female_cg = weighted.mean(percent_female_cg,
                                                                      w = sample_size),
                                    sample_size = sum(sample_size)), 
                                 by = c("citation", "location", "year_published", 
                                        "year_end", "estimate_method", 
                                        "time_period", "condition", 
                                        "page_number")]
# add columns so it can be combined with extraction data:
care_type_sum[, ":=" (severity = "", 
                      care_setting = "community", 
                      care_type = "", 
                      notes = "summarized from care type splits")]

# next summarize data that is just severity splits: 
severity_splits = sex_split_fix[severity != "" & care_type == ""]
severity_sum = severity_splits[,.(percent_female_cg = weighted.mean(percent_female_cg, w = sample_size),
                                  sample_size = sum(sample_size)), 
                               by = c("citation", "location", "year_published", 
                                      "year_end", "estimate_method", 
                                      "time_period", "condition", 
                                      "page_number")]
# add columns so it can be combined with extraction data:
severity_sum[, ":=" (severity = "", 
                     care_setting = "community", 
                     care_type = "", 
                     notes = "summarized from severity splits")]

# finally summarize data that is both severity and care type split: 
both_splits = sex_split_fix[severity != "" & care_type != ""]
# first summarize care types: 
both_sum_type = both_splits[,.(percent_female_cg = weighted.mean(percent_female_cg, w = sample_size),
                               sample_size = sum(sample_size)), 
                            by = c("citation", "location", "year_published", 
                                   "year_end", "estimate_method", 
                                   "time_period", "condition", 
                                   "page_number", "severity")]
both_sum = both_sum_type[,.(percent_female_cg = weighted.mean(percent_female_cg, w = sample_size),
                            sample_size = sum(sample_size)), 
                         by = c("citation", "location", "year_published", 
                                "year_end", "estimate_method", 
                                "time_period", "condition", 
                                "page_number")]
# add columns so it can be combined with extraction data:
both_sum[, ":=" (severity = "", 
                 care_setting = "community", 
                 care_type = "", 
                 notes = "summarized from both severity and care type splits")]

## bind all overall data together:
# remove extra columns from original data set: 
delete_cols = colnames(sex_overall)[!(colnames(sex_overall) %in% colnames(both_sum))] # use this to see which columns are missing
sex_overall[, (delete_cols) := NULL]

sex_sum = rbind(sex_overall, care_type_sum, severity_sum, both_sum)

# check that no splits are left in the data:
nrow(sex_sum[severity != "" | care_type != ""]) # 0

# create year_id column
sex_sum[, year_published := as.numeric(year_published)]
sex_sum[, year_id := ifelse(is.na(year_end), year_published, year_end)]

# look for duplicates: 
sex_sum[,.(citation, year_id, location, percent_female_cg, severity, 
                care_setting, care_type)] %>%
  group_by(citation, year_id, location, percent_female_cg, severity, 
           care_setting, care_type) %>%
  summarise(n = n(), .groups = "drop") %>% 
  filter(n > 1L)
# zero duplicates

nrow(sex_sum[is.na(sample_size)]) # 0
## no longer have missing sample sizes, but leaving this here in case that changes: 
# fill missing sample size with mean:
# mean_n = round(mean(sex_sum$sample_size, na.rm = T))
# sex_sum[is.na(sample_size), sample_size := mean_n]

# create standard error estimate for % female caregivers
# using eqn for standard error of binomial variable sqrt (pq/n)
sex_sum[, cg_sex_st_err := sqrt(percent_female_cg*(1 - percent_female_cg)/sample_size)]

# load location metadata: 
locs <- fread(file = "FILEPATH/location_set_22_metadata.csv")

# merge in GBD location IDs: 
setnames(sex_sum, old = "location", new = "location_name")
name_test = merge(sex_sum, locs[,.(location_id, location_name)], 
                  all.x = T, by = "location_name")

# see where location name is wrong: 
unique(name_test[is.na(location_id)]$location_name)
# Iran, Korea, Taiwan, UK, US

# get GBD location names from location set data: 
locs[location_name %like% "Iran"]$location_name # Iran (Islamic Republic of)
locs[location_name %like% "Korea"]$location_name # Republic of Korea
locs[location_name %like% "Taiwan"]$location_name # Taiwan (Province of China)
locs[location_name %like% "United Kingdom"]$location_name # United Kingdom
locs[location_name %like% "United States"]$location_name # United States of America

# change location names in extraction data:
sex_sum[, location_name := 
               case_when(location_name == "Iran" ~ "Iran (Islamic Republic of)",
                         location_name == "Korea" ~ "Republic of Korea",
                         location_name == "Taiwan" ~ "Taiwan (Province of China)",
                         location_name == "UK" ~ "United Kingdom",
                         location_name == "US" ~ "United States of America", 
                         TRUE ~ location_name)]

# merge location IDs now that names have been fixed: 
clean_all = merge(sex_sum, locs[,.(location_id, location_name)], 
                  all.x = T, by = "location_name")

# check that naming fix worked: 
nrow(clean_all[is.na(location_id)]) # zero NAs! 

# some sources in the extraction data do not have year_start or year_end data
# the year_id for these sources gets assigned from the year_published 
# this results in some sources having year_id > 2019 (later than we are making estimates)
nrow(clean_all[year_id > 2019]) # 2
table(clean_all[year_id > 2019]$citation) # Cheung, Xu
# for these sources we did further exploration for the year the study 
# was conducted in hopes of preserving input data 

# fix year IDs for sources published in 2020 or later: 
clean_all[citation == "Cheung", year_id := 2018] # IRB approval 2017 & 2018
clean_all[citation == "Xu", year_id := 2001] # REACH II study baseline 
# https://med.stanford.edu/oafc/projects/reach.html

# double check all years are 2019 or earlier now: 
nrow(clean_all[year_id > 2019]) # 0

# save out clean data with estimates summarized for overall severity and care type (no splits)
fwrite(clean_all, file = "FILEPATH/00_clean_data.csv")

