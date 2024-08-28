### Author: Michael Breshock
### Date: 01/12/2024
### Project: Global dementia spending
### Purpose: Create Results Table for GHDx

rm(list=ls()) 

library(data.table)
library(dplyr)
library("metR", lib.loc = "FILEPATH")

# load cost estimates: 
current = fread("FILEPATH/final_cost_global_draws_by_loc_RTR_sens_2024_05_06.csv")
baseline = fread("FILEPATH/reference/forecast_costs_all_draws_RTR_2024-05-07.csv")
accel_beta = fread("FILEPATH/accelerated_beta/forecast_costs_all_draws_RTR_2024-05-07.csv")
accel_cost = fread("FILEPATH/accelerated_cost/forecast_costs_all_draws_RTR_2024-05-07.csv")
accel_gamma = fread("FILEPATH/accelerated_gamma/forecast_costs_all_draws_RTR_2024-05-07.csv")
decel_gamma = fread("FILEPATH/decelerated_gamma/forecast_costs_all_draws_RTR_2024-05-07.csv")

# load population forecast data: 
# get location IDs from metadata
locs <- fread("FILEPATH/location_set_22_metadata.csv")
loc_ids <- locs[level == 3]$location_id

forecast_file = "FILEPATH/population.nc"
pop_forecast = ReadNetCDF(forecast_file, 
                          subset = list(year_id = c(2020:2050), 
                                        location_id = loc_ids, 
                                        age_group_id = c(2:20, 30:32, 235), # all ages
                                        scenario = 0))[location_id %in% loc_ids] # reference scenario

# summarize country population draws for all ages: 
country_pop = pop_forecast[,.(population = sum(population)), 
                           by = c("year_id", "location_id")]

## calculate per case costs: 
current[, ":=" (direct_pc = attributable_direct_all / prev_counts, 
                indirect_pc = attributable_caregiving_all / prev_counts, 
                scenario = "current")]

baseline[, ":=" (direct_pc = attributable_direct_all / total_cases, 
                 indirect_pc = attributable_caregiving_all / total_cases, 
                 scenario = "baseline")]

accel_beta[, ":=" (direct_pc = attributable_direct_all / total_cases, 
                   indirect_pc = attributable_caregiving_all / total_cases, 
                   scenario = "accelerated_beta")]

accel_cost[, ":=" (direct_pc = attributable_direct_all / total_cases, 
                   indirect_pc = attributable_caregiving_all / total_cases,
                   scenario = "accelerated_cost")]

accel_gamma[, ":=" (direct_pc = attributable_direct_all / total_cases, 
                    indirect_pc = attributable_caregiving_all / total_cases, 
                    scenario = "accelerated_gamma")]

decel_gamma[, ":=" (direct_pc = attributable_direct_all / total_cases, 
                    indirect_pc = attributable_caregiving_all / total_cases,
                    scenario = "decelerated_gamma")]

forecasts = rbind(baseline, accel_beta, accel_cost, accel_gamma, decel_gamma)


# summarize draws: 
current_sum = current[,.(direct_pc = mean(direct_pc), 
                         direct_pc_lower = quantile(direct_pc, 0.025), 
                         direct_pc_upper = quantile(direct_pc, 0.975), 
                         indirect_pc = mean(indirect_pc), 
                         indirect_pc_lower = quantile(indirect_pc, 0.025), 
                         indirect_pc_upper = quantile(indirect_pc, 0.975), 
                         dem_cases = mean(prev_counts), 
                         population = mean(population)), 
                      by = c("year_id", "location_id")]

forecasts_sum = forecasts[,.(direct_pc = mean(direct_pc), 
                             direct_pc_lower = quantile(direct_pc, 0.025), 
                             direct_pc_upper = quantile(direct_pc, 0.975), 
                             indirect_pc = mean(indirect_pc), 
                             indirect_pc_lower = quantile(indirect_pc, 0.025), 
                             indirect_pc_upper = quantile(indirect_pc, 0.975), 
                             dem_cases = mean(total_cases)), 
                        by = c("year_id", "location_id", "scenario")]

# merge in population forecasts with costs: 
forecasts_sum = merge(forecasts_sum, country_pop, by = c("year_id", "location_id"))


## process current cost estimates: 
# merge in location names: 
current_costs = merge(current_sum, 
                      locs[,.(location_id, location_name, ihme_loc_id,
                              region_id, region_name, 
                              super_region_id, super_region_name)], 
                      by = "location_id")

# melt to long format:
current_costs_long = melt(current_costs, 
                          id.vars = c("year_id", "location_id", "location_name",
                                      "ihme_loc_id", "region_id", "region_name", 
                                      "super_region_id", "super_region_name", 
                                      "dem_cases", "population"), 
                          measure = list(val = c("direct_pc", # creating three value columns
                                                 "indirect_pc"),
                                         upper = c("direct_pc_upper", 
                                                   "indirect_pc_upper"),
                                         lower = c("direct_pc_lower",
                                                   "indirect_pc_lower")), 
                          variable.name = "model_output")

# recode model_output: 
current_costs_long[, model_output := case_when(model_output == 1 ~ "Direct cost per case", 
                                               TRUE ~ "Indirect cost per case")]
# order based on location first, then year
current_costs_ord = current_costs_long[order(location_id, year_id)]

# change variable names for clarity: 
setnames(current_costs_ord, 
         old = c("ihme_loc_id", 
                 "region_id", 
                 "region_name", 
                 "super_region_id", 
                 "super_region_name", 
                 "dem_cases"), 
         new = c("location_iso_code", 
                 "gbd_region_id", 
                 "gbd_region_name", 
                 "gbd_super_region_id", 
                 "gbd_super_region_name", 
                 "dementia_cases"))

# just need years after 2000
costs2019 = current_costs_ord[year_id >= 2000 & year_id <= 2019]

# save out table: 
fwrite(costs2019, file = "FILEPATH/IHME_DEMENTIA_SPENDING_2000_2019_DATA_Y2024M06D04.csv")

## process forecast estimates: 
# merge in location names: 
forecast_costs = merge(forecasts_sum, 
                      locs[,.(location_id, location_name, ihme_loc_id,
                              region_id, region_name, 
                              super_region_id, super_region_name)], 
                      by = "location_id")

# melt to long format:
forecast_costs_long = melt(forecast_costs, 
                          id.vars = c("year_id", "location_id", "location_name",
                                      "ihme_loc_id", "region_id", "region_name", 
                                      "super_region_id", "super_region_name", 
                                      "dem_cases", "population", "scenario"), 
                          measure = list(val = c("direct_pc", # creating three value columns
                                                 "indirect_pc"),
                                         upper = c("direct_pc_upper", 
                                                   "indirect_pc_upper"),
                                         lower = c("direct_pc_lower",
                                                   "indirect_pc_lower")), 
                          variable.name = "cost_type")

# create model_output variable: 
forecast_costs_long[, model_output := 
                      case_when((cost_type == 1 & scenario == "baseline") ~ 
                                  "Direct cost per case (baseline)",
                                (cost_type == 1 & scenario == "accelerated_beta") ~ 
                                  "Direct cost per case (accelerated diagnosis rate)",
                                (cost_type == 1 & scenario == "accelerated_cost") ~ 
                                  "Direct cost per case (accelerated unit cost)",
                                (cost_type == 1 & scenario == "accelerated_gamma") ~ 
                                  "Direct cost per case (accelerated nursing home-based care rate)",
                                (cost_type == 1 & scenario == "decelerated_gamma") ~ 
                                  "Direct cost per case (decelerated nursing home-based care rate)",
                                (cost_type == 2 & scenario == "baseline") ~ 
                                  "Indirect cost per case (baseline)",
                                (cost_type == 2 & scenario == "accelerated_beta") ~ 
                                  "Indirect cost per case (accelerated diagnosis rate)",
                                (cost_type == 2 & scenario == "accelerated_cost") ~ 
                                  "Indirect cost per case (accelerated unit cost)",
                                (cost_type == 2 & scenario == "accelerated_gamma") ~ 
                                  "Indirect cost per case (accelerated nursing home-based care rate)",
                                TRUE ~ "Indirect cost per case (decelerated nursing home-based care rate)")]

# order based on location first, then year
forecast_costs_ord = forecast_costs_long[order(location_id, year_id)]

# remove unnecessary columns now:
forecast_costs_ord[,cost_type := NULL]
forecast_costs_ord[,scenario := NULL]

# change variable names for clarity: 
setnames(forecast_costs_ord, 
         old = c("ihme_loc_id", 
                 "region_id", 
                 "region_name", 
                 "super_region_id", 
                 "super_region_name", 
                 "dem_cases"), 
         new = c("location_iso_code", 
                 "gbd_region_id", 
                 "gbd_region_name", 
                 "gbd_super_region_id", 
                 "gbd_super_region_name", 
                 "dementia_cases"))

fwrite(forecast_costs_ord, file = "FILEPATH/IHME_DEMENTIA_SPENDING_2020_2050_DATA_Y2024M06D04.csv")
