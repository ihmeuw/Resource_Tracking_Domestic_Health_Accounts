##########################################################################
### Author: Amy Lastuka
### Date: 11/06/2023
### Project: Global dementia spending
### Purpose: data cleaning for caregiving attributable fraction. The only
###          cleaning needed for this metric is to combine the rows where
###          attributable fraction was reported by severity and/or care type
##########################################################################

rm(list=ls())

library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(ggpubr)

cg_extraction <- fread("FILEPATH/caregiving extraction 2023_10_17.csv")
# change estimate method column name: 
setnames(cg_extraction, old = "estimation method", new = "estimate_method")

cg_data <- cg_extraction[!is.na(attributable_fraction),]

#################### summarize severity/care type splits #######################
# extract all sources that already have overall estimates:
cg_overall = cg_data[severity == "" & care_type == ""]

# extract all severity and care type split data: 
cg_split = anti_join(cg_data, cg_overall)
cg_split_fix = cg_split[!citation %in% cg_overall$citation] 

# there are only two papers that are split - one is split by severity only and one is split by both severity and care activity 

# first, summarize data that is just severity splits: 
severity_splits = cg_split_fix[severity != "" & care_type == ""]
severity_sum = severity_splits[,.(attributable_fraction = weighted.mean(attributable_fraction, w = sample_size), 
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


# summarize data that is both severity and care type split: 
# first, need to combine the care type fractions - this is a 
# weighted average based on caregiving hours, not on sample size 
# it doesn't matter what units the caregiving hours are in (days, weeks, etc) as long as they
# are all the same within a given source
# second, proceed with the severity combination
both_splits = cg_split_fix[severity != "" & care_type != ""]
# first summarize care types (don't add sample size here, it's the same sample): 
both_sum_type = both_splits[,.(attributable_fraction = weighted.mean(attributable_fraction, w = value),
                               sample_size = mean(sample_size)), 
                            by = c("citation", "location", "year_published", 
                                   "year_end", "estimate_method", 
                                   "time_period", "condition", 
                                   "severity")]
both_sum = both_sum_type[,.(attributable_fraction = weighted.mean(attributable_fraction, w = sample_size), 
                            sample_size = sum(sample_size)), 
                         by = c("citation", "location", "year_published", 
                                "year_end", "estimate_method", 
                                "time_period", "condition")]
# add columns so it can be combined with extraction data:
both_sum[, ":=" (severity = "", 
                 care_setting = "community", 
                 care_type = "", 
                 notes = "summarized from both severity and care type splits")]

## bind all overall data together:
# remove extra columns from original data set: 
delete_cols = colnames(cg_overall)[!(colnames(cg_overall) %in% colnames(both_sum))] # use this to see which columns are missing
cg_overall[, (delete_cols) := NULL]

comb_overall = rbind(cg_overall, severity_sum, both_sum)

# check that no splits are left in the data:
nrow(comb_overall[severity != "" | care_type != ""]) # 0


fwrite(comb_overall, file = "FILEPATH/00_clean_caregiving_attributable_fraction.csv")

