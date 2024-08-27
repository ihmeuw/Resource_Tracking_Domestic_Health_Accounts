##########################################################################
### Author: Amy Lastuka, Michael Breshock
### Date: 08/15/2023
### Project: Global dementia spending
### Purpose: data cleaning for caregiving hours extraction
##########################################################################

rm(list=ls())

library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(ggpubr)

# this file updated on 04/11/2024: 
cg_extraction <- fread(paste0('FILEPATH/caregiving extraction 2024_04_11.csv'))

# cut the Zhu paper because I am not clear on whether everyone had dementia at baseline
cg_extraction <- cg_extraction[citation != "Zhu"]

# cut papers with multiple locations
cg_extraction <- cg_extraction[!location %like% ";",]

# cut papers for Hong Kong (not considered a country in GBD -> sad face)
cg_extraction <- cg_extraction[location != "Hong Kong"]

# remove Hurd paper -> this source just used for attributable fraction
# does not have data on caregiving hours
cg_extraction <- cg_extraction[citation != "Hurd"] 

# remove Cantu paper - includes paid and unpaid care time
cg_extraction <- cg_extraction[citation != "Cantu"] 

# remove Melaka paper - focuses on just frontotemporal dementia 
cg_extraction <- cg_extraction[citation != "Melaka"] 

# create standard error estimate for % female caregivers
# using eqn for standard error of binomial variable sqrt (pq/n)
cg_extraction[, cg_sex_st_err := sqrt(percent_female_cg*(1 - percent_female_cg)/sample_size)]

# create overall hours estimate for Trepel source - reported by severity split but not overall:
trepel_sev_split = cg_extraction[citation == "Trepel" & severity != ""]

cg_extraction[citation == "Trepel" & severity == "",
              ":=" (sample_size = sum(trepel_sev_split$sample_size),
                    value = weighted.mean(trepel_sev_split$value,
                                          w = trepel_sev_split$sample_size)
                    )]

# remove data missing location info and institutional values
cg_extraction = cg_extraction[location != "" & care_setting == "community"]
# lose one observation missing location 
# lose 6 institutional observations

# recode all "total" care_type observations to be blank
cg_extraction[care_type == "total", care_type := ""]

# change estimate method column name: 
setnames(cg_extraction, old = "estimation method", new = "estimate_method")

# # save copy of dataset to summarize split data later: 
# cg_copy = copy(cg_extraction)

#################### summarize severity/care type splits #######################
# extract all sources that already have overall estimates:
cg_overall = cg_extraction[severity == "" & care_type == ""]

# extract all severity and care type split data: 
cg_split = anti_join(cg_extraction, cg_overall)

# filter to just sources that don't already have an overall estimate: 
cg_split_fix = cg_split[!(citation %in% unique(cg_overall$citation))]
# check that sample size isn't missing: 
nrow(cg_split_fix[is.na(sample_size)]) # 0 -> phew! 

### preserve sources that only provide partial splits, ie: 
# ADL+IADL care types but not supervision
# mild+moderate severity but not severe 
# etc. 

# first view what levels of severity + care_type splits are provided for each source 
which_splits = cg_split_fix[, .(sev_levels = paste(severity, collapse = " "), 
                                care_levels = paste(care_type, collapse = " ")), 
                            by = citation]

# create functions to label split levels as "all", "some", or "none" 
# all = has all split levels 
# some = has only some of the split levels 
# none = has no splits

# function will be applied to each observation of `which_splits$sev_levels`
# the input will be a single character string containing 
# all split levels present for each source/citation
severity_split_levels <- function(x){
  # check to see if each level is present in the split data
  mild = grepl("mild", x)
  moderate = grepl("moderate", x)
  severe = grepl("severe", x)
  num_levels = sum(mild, moderate, severe)
  # if all levels are present, then num_levels == 3
  if (num_levels == 3) {
    label = "all"
  } else if (num_levels == 0) { # if no levels are present, then num_levels == 0
    label = "none" 
  } else { # if num_levels is between 0 & 3, then some levels are present
    label = "some"
  }
  return(label)
}

# create similar function for care type splits: 
# function will be applied to each observation of `which_splits$care_levels`
# the input will be a single character string containing 
# all split levels present for each source/citation
care_type_split_levels <- function(x){
  # check to see if each level is present in the split data
  ADL = grepl('(?<!I)ADL', x, perl=TRUE) 
  # ^ regular expression used here to prevent grepl returning TRUE for "IADL"
  IADL = grepl("IADL", x)
  supervision = grepl("supervision", x)
  num_levels = sum(ADL, IADL, supervision)
  # if all levels are present, then num_levels == 3
  if (num_levels == 3) {
    label = "all"
  } else if (num_levels == 0) { # if no levels are present, then num_levels == 0
    label = "none" 
  } else { # if num_levels is between 0 & 3, then some levels are present
    label = "some"
  }
  return(label)
}

# label each source's severity splits as "all", "some", or "none"
which_splits[, sev_label := sapply(sev_levels, severity_split_levels)]
# label each source's care_type splits as "all", "some", or "none"
which_splits[, care_label := sapply(care_levels, care_type_split_levels)]

# label each source based on which splits need to be summarized 
# and which splits should preserved

# summarize just severity splits: 
# sev_label == "all" & care_label != "all"
# in this case, either the source provided no care type splits or 
# only some of the care type split levels. In both scenarios only the 
# severity can be summarized. If some care type splits are present,
# an overall severity estimate will be created for each care type split
# number of sources like this:
nrow(which_splits[sev_label == "all" & care_label != "all"]) # 4

# summarize just care type splits: 
# sev_label != "all" & care_label == "all"
# in this case, either the source provided no severity splits or 
# only some of the severity split levels. In both scenarios only the 
# care type can be summarized. If some severity splits are present,
# an overall care type estimate will be created for each severity split
# number of sources like this:
nrow(which_splits[sev_label != "all" & care_label == "all"]) # 9

# summarize both splits: 
# sev_label == "all" & care_label == "all"
# in this case, all of the split levels are provided for both 
# severity (mild, moderate, severe) and care type (ADL, IADL, supervision). 
# Thus, both splits can be summarized to provide a single overall estimate 
# for all severities and all care types. 
# number of sources like this:
nrow(which_splits[sev_label == "all" & care_label == "all"]) # 2

# preserve splits: 
# sev_label != "all" & care_label != "all"
# these sources do not provide all split levels for either severity or care type
# so they cannot be accurately summarized. The splits provided will be 
# preserved and used as factor variables in the stage 1 model for ST-GPR
# number of sources like this:
nrow(which_splits[sev_label != "all" & care_label != "all"]) # 12
# RTR update: The Nigeria paper falls under this category ^ 

# add variable indicating what kind of summary/preservation needs to be done
# for each source for easier filtering later on: 
which_splits[, action := 
               case_when(
                 (sev_label == "all" & care_label != "all") ~ "severity",
                 (sev_label != "all" & care_label == "all") ~ "care type", 
                 (sev_label == "all" & care_label == "all") ~ "both", 
                 TRUE ~ "preserve")
             ]
table(which_splits$action)
# both  care type   preserve    severity 
# 2           9          12           4 
# this matches what we found above with nrow()

# create lists of each source/citation name by action category
# this will make filtering to each subset easier later on
sum_severity = which_splits[action == "severity"]$citation
sum_care_type = which_splits[action == "care type"]$citation
sum_both = which_splits[action == "both"]$citation
preserve_sources = which_splits[action == "preserve"]$citation

# first summarize data that is just care type splits: 
care_type_splits = cg_split_fix[citation %in% sum_care_type]
care_type_sum = care_type_splits[,.(value = sum(value), 
                                    sample_size = sum(sample_size)), 
                                 by = c("citation", "location", "year_published", 
                                        "year_end", "estimate_method", "severity",
                                        "time_period", "condition", 
                                        "page_number")]
# add columns so it can be combined with extraction data:
care_type_sum[, ":=" (care_setting = "community", 
                      care_type = "", 
                      notes = "summarized from care type splits")]


# next summarize data that is just severity splits: 
severity_splits = cg_split_fix[citation %in% sum_severity]
severity_sum = severity_splits[,.(value = weighted.mean(value, w = sample_size), 
                                  sample_size = sum(sample_size)), 
                               by = c("citation", "location", "year_published", 
                                      "year_end", "estimate_method", "care_type",
                                      "time_period", "condition", 
                                      "page_number")]
# add columns so it can be combined with extraction data:
severity_sum[, ":=" (care_setting = "community", 
                     severity = "", 
                     notes = "summarized from severity splits")]


# finally summarize data that is both severity and care type split: 
both_splits = cg_split_fix[citation %in% sum_both]
# first summarize care types: 
both_sum_type = both_splits[,.(value = sum(value), 
                               sample_size = sum(sample_size)), 
                            by = c("citation", "location", "year_published", 
                                   "year_end", "estimate_method", "severity",
                                   "time_period", "condition",
                                   "page_number")]
# then summarize severities:
both_sum = both_sum_type[,.(value = weighted.mean(value, w = sample_size), 
                            sample_size = sum(sample_size)), 
                         by = c("citation", "location", "year_published", 
                                "year_end", "estimate_method", 
                                "time_period", "condition", 
                                "page_number")]
# add columns so it can be combined with extraction data:
both_sum[, ":=" (care_setting = "community",
                 severity = "", 
                 care_type = "", 
                 notes = "summarized from both severity and care type splits")]

preserve_splits = cg_split_fix[citation %in% preserve_sources]

## bind all overall data + preserved split data together:
# remove extra columns from overall data and preserved split data: 
delete_cols = colnames(cg_overall)[!(colnames(cg_overall) %in% colnames(both_sum))] # use this to see which columns are missing
cg_overall[, (delete_cols) := NULL]
preserve_splits[, (delete_cols) := NULL]


comb_overall = rbind(cg_overall, care_type_sum, 
                     severity_sum, both_sum, preserve_splits)

# fix year published column 
comb_overall[year_published == "2002(2)", year_published := "2002"]
comb_overall[, year_published := as.numeric(year_published)]
# create year_id column
comb_overall[, year_id := ifelse(is.na(year_end), year_published, year_end)]

# look for duplicates: 
comb_overall[,.(citation, year_id, location, value, severity, 
                 care_setting, care_type)] %>%
  group_by(citation, year_id, location, value, severity, 
           care_setting, care_type) %>%
  summarise(n = n(), .groups = "drop") %>% 
  filter(n > 1L)
# zero duplicates

# see all the time units in extraction data: 
table(comb_overall$time_period) # day, month, week, year

# convert all hours into per-week units:
comb_overall[, weekly_hours := NA_real_] # initialize variable
comb_overall[time_period == "day", weekly_hours := value*7] # 7 days per week
comb_overall[time_period == "month", weekly_hours := value*7/30.5] # using average days per month: 30.5
comb_overall[time_period == "year", weekly_hours := value/52] # 52 weeks per year
comb_overall[time_period == "week", weekly_hours := value] # already in weekly units

# check that all values are converted: 
sum(is.na(comb_overall$weekly_hours)) # 0 
# check if any estimates are over 168 hours per week
nrow(comb_overall[weekly_hours > 168]) # 1
# Cavallo -> reported 24.46 hours per day
# setting max of 168 hours per week (24 hours per day)
comb_overall[, weekly_hours := pmin(weekly_hours, 168)]
# check if any estimates <= zero 
nrow(comb_overall[weekly_hours <= 0]) # 0

# load location metadata: 
locs <- fread("FILEPATH/location_set_22_metadata.csv")

# merge in GBD location IDs: 
setnames(comb_overall, old = "location", new = "location_name")
name_test_all = merge(comb_overall, locs[,.(location_id, location_name)], 
                  all.x = T, by = "location_name")

# see where location name is mismatching: 
unique(name_test_all[is.na(location_id)]$location_name)
# Iran, Korea, Taiwan, UK, US

# get GBD location names from location set data: 
locs[location_name %like% "Iran"]$location_name # Iran (Islamic Republic of)
locs[location_name %like% "Korea"]$location_name # Republic of Korea
locs[location_name %like% "Taiwan"]$location_name # Taiwan (Province of China)
locs[location_name %like% "United Kingdom"]$location_name # United Kingdom
locs[location_name %like% "United States"]$location_name # United States of America

# change location names in extraction data:
comb_overall[, location_name := 
                case_when(location_name == "Iran" ~ "Iran (Islamic Republic of)",
                          location_name == "Korea" ~ "Republic of Korea",
                          location_name == "Taiwan" ~ "Taiwan (Province of China)",
                          location_name == "UK" ~ "United Kingdom",
                          location_name == "US" ~ "United States of America", 
                          TRUE ~ location_name)]

# merge location IDs now that names have been fixed: 
clean_all = merge(comb_overall, locs[,.(location_id, location_name)], 
                   all.x = T, by = "location_name")

# check that naming fix worked: 
nrow(clean_all[is.na(location_id)]) # zero NAs! 

# some sources in the extraction data do not have year_start or year_end data
# the year_id for these sources gets assigned from the year_published 
# this results in some sources having year_id > 2019 (later than we are making estimates)
nrow(clean_all[year_id > 2019]) # 3
table(clean_all[year_id > 2019]$citation) # Cheung, Chuakhamfoo, Xu
# for these sources we did further exploration for the year the study 
# was conducted in hopes of preserving input data 

# fix year IDs for sources published in 2020 or later: 
clean_all[citation == "Cheung", year_id := 2018] # IRB approval 2017 & 2018
clean_all[citation == "Chuakhamfoo", year_id := 2017] 
# survey conducted in 2017 according to paper describing survey results: 
# https://bmjopen.bmj.com/content/bmjopen/10/3/e032637.full.pdf
clean_all[citation == "Xu", year_id := 2001] # REACH II study baseline 
# https://med.stanford.edu/oafc/projects/reach.html

# double check all years are 2019 or earlier now: 
nrow(clean_all[year_id > 2019]) # 0

### explore sources that provide both per dyad & per patient estimates: 
clean_wide = clean_all[,.(year_id, location_name, citation, 
                          sample_size, estimate_method, weekly_hours)] %>%
  pivot_wider(names_from = estimate_method, values_from = weekly_hours)
setDT(clean_wide)
setnames(clean_wide, old = c("per dyad", "per patient"), 
         new = c("per_dyad", "per_patient"))
# filter to sources that provide estimates for both dyads and patients: 
both_est = clean_wide[per_dyad != "NULL" & per_patient != "NULL"]
# unlist variables
both_est[, ":=" (per_dyad = unlist(per_dyad), 
                 per_patient = unlist(per_patient))]
# add labels for plotting
both_est[, plot_lab := paste(citation, location_name, year_id)]

# scatter plot of per patient vs per dyad estimates: 
pdf("FILEPATH/patient_vs_dyad_scatter.pdf")
p = ggplot(both_est,aes(x=per_dyad,y=per_patient)) +
  geom_point(show.legend = FALSE) +
  geom_smooth(method ='lm') +
  theme_minimal() + xlim(40, 70) +
  labs(title = "Per Patient vs Per Dyad Estimates from the same source") + 
  stat_cor(method = "pearson", label.x = 50, label.y = 50) + 
  geom_text(label = both_est$plot_lab, nudge_x = 2.5, nudge_y = 1) + 
  stat_regline_equation()
print(p)
dev.off()

# try for cases where we have per dyad and per patient estimates for the 
# same country/year - ignoring citation/source: 
clean_wide2 = clean_all[,.(year_id, location_name, 
                           estimate_method, weekly_hours)] %>%
  pivot_wider(names_from = estimate_method, values_from = weekly_hours)
setDT(clean_wide2)
setnames(clean_wide2, old = c("per dyad", "per patient"), 
         new = c("per_dyad", "per_patient"))

both_est2 = clean_wide2[per_dyad != "NULL" & per_patient != "NULL"]
both_est2[, ":=" (per_dyad = as.numeric(per_dyad), 
                  per_patient = as.numeric(per_patient))]
both_est2[, ratio := per_patient/per_dyad]
# this method only adds two more observations when ignoring citation/source
# the ratios from these two are much larger than the ratios coming from the 
# observations that are from the same source, so going to stick with
# just the ratios that are from the same source (both_est)

# create regression model for predicting per_patient from per_dyad estimates
ratio_model = lm(per_patient ~ per_dyad, data = both_est)
summary(ratio_model)

# test accuracy of regression vs mean ratio:
# create ratio
both_est[, ratio := per_patient/per_dyad]
patient_ratio = mean(both_est$ratio)
# run regression predictions: 
both_est[, lm_pred := predict(ratio_model, data = both_est)]
# run ratio predictions: 
both_est[, ratio_pred := per_dyad*patient_ratio]
# see abs difference for each: 
both_est[, ":=" (lm_diff = abs(lm_pred - per_patient), 
                 ratio_diff = abs(ratio_pred - per_patient))]

# calculate prediction accuracy for both methods:
lm_RMSE = sqrt(mean((both_est$lm_pred - both_est$per_patient)^2))
ratio_RMSE = sqrt(mean((both_est$ratio_pred - both_est$per_patient)^2))

# print model accuracy metrics comparison:
cat("Regression RMSE:", lm_RMSE, "\nMean Ratio RMSE:", ratio_RMSE)
# regression is better according to RMSE 

#### try leave one out method for Out of Sample Validation: 
# initialize data.table to store all results in
all_results <- data.table(
  model = character(),
  MSE = double(),
  RMSE = double(),
  MAE = double(),
  fold = integer()
)

for (i in 1:nrow(both_est)) {
  # split into train and test
  test <- both_est[i]
  train <- anti_join(both_est, test)
  
  
  pt_model <- lm(per_patient ~ per_dyad, data = train)
  pt_ratio <- mean(train$ratio)
  
  test$pt_model <- predict(pt_model, test)
  test$pt_ratio <- test$per_dyad * pt_ratio
  
  results <- data.table(test)[,.(per_patient, pt_model, pt_ratio)]
  
  results <- melt(results,id.vars='per_patient')
  setnames(results,old=c('variable','value'),new=c('model','predictions'))
  results <- results[,.(MSE = mean((predictions - per_patient)^2),
                        RMSE = sqrt(mean((predictions - per_patient)^2)),
                        MAE = mean(abs(predictions - per_patient))),
                     by=model]
  
  results$fold <- i
  all_results <- rbind(all_results, results)
}

# Take average of k results:
manual_CV_results <- all_results[, .(
  avg_MSE = mean(MSE),
  avg_RMSE = mean(RMSE),
  avg_MAE = mean(MAE)
), by = model]

# Order by avg_RMSE
manual_CV_results <- manual_CV_results[order(avg_RMSE, decreasing = FALSE), ]
manual_CV_results
# ratio is better according to leave one out - out of sample prediction RMSE 

# extract just per dyad estimates
per_dyad = clean_all[estimate_method == "per dyad"]
# extract just per patient estimates 
per_patient = clean_all[estimate_method == "per patient"]

# remove source/location/year combos that already have a per patient estimate
per_dyad_fix = per_dyad[!paste(per_dyad$citation,per_dyad$location_name,per_dyad$year_id) 
                        %in% paste(per_patient$citation,per_patient$location_name,per_patient$year_id)]

# see range of hours before conversion: 
range(per_dyad_fix$weekly_hours)

# convert per dyad estimates to per patient using ratio 
per_dyad_fix[, ":=" (weekly_hours = pmin(weekly_hours*patient_ratio, 168), # cap at 168 hrs/week
                     estimate_method = "per patient")]

# see range of hours after conversion: 
range(per_dyad_fix$weekly_hours)

# combine all per patient estimates back together: 
clean_all_patient = rbind(per_patient, per_dyad_fix)

# confirm that all estimates are in per patient now: 
table(clean_all_patient$estimate_method) # now have 83 per patient estimates

# check for duplicates: 
sum(duplicated(clean_all_patient, by = c('citation', 'year_id', 'location_name', 
                                     'weekly_hours', "estimate_method")))
# zero duplicates

# save out clean data with all estimates in per-patient metrics
# and split data summarized where possible
fwrite(clean_all_patient, "FILEPATH/00_clean_data_per_patient.csv")
