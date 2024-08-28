##########################################################################
### Author: Michael Breshock
### Date: 10/05/2023
### Project: Global dementia spending
### Purpose: aggregate forecast cost components 
##########################################################################

rm(list=ls()) # clear environment 
gc() # clear unused memory (this script is memory intensive)


library(data.table)
library(stringr)

# loop through each scenario and run forecasting for each: 
scenarios = c(1:4)

for (s in scenarios) {
  ## initialize which forecast scenario is being run: 
  # scenario = 0 -> Reference Scenario (baseline)
  # scenario = 1 -> Accelerated Beta
  # scenario = 2 -> Accelerated Gamma
  # scenario = 3 -> Accelerated Cost (facility and community)
  # scenario = 4 -> Decelerated Gamma
  scenario = s
  
  
  ####################### Load Cost Component Forecast Draws #####################
  # get country location IDs: 
  locs <- fread(file = 'FILENAME/location_set_22_metadata.csv')
  loc_ids = locs[level==3]$location_id
  
  # dementia prevalence: 
  prev = fread("FILENAME/dementia_prevalence_forecast_draws_locs_included.csv")
  
  ## Sensitivity analysis: 
  # add 85th percentile and 15th percentile variables to prevalence data
  prev[, ":=" (threshold15 = quantile(total_cases, 0.15), 
               threshold85 = quantile(total_cases, 0.85)), 
       by = c("year_id", "location_id")]
  # create dementia cases variables where: 
  # 1. 15th percentile is the maximum estimate
  # 2. 85th percentile is the minimum estimate
  prev[, ":=" (total_cases15 = pmin(total_cases, threshold15), 
               total_cases85 = pmax(total_cases, threshold85))]
  # pmin takes the minimum value between total cases and threshold15 
  # so if total cases < threshold15, pmin returns total cases 
  # if total cases > threshold15, pmin returns threshold15
  # pmax takes the maximum value between total cases and threshold85
  # so if total cases > threshold85, pmax returns total cases 
  # if total cases < threshold85, pmax returns threhsold85
  
  # initialize projected draws file path: 
  projected_files = 'FILEPATH'
  
  # diagnosis rate - beta (overall): 
  beta_folder = ifelse(scenario == 1, 
                       "alt_projection_draws/accelerated_beta/",
                       "projected_draws/")
  beta_path = paste0(projected_files, beta_folder)
  paste0("beta path: ", beta_path) # sanity check
  
  beta_files = paste0(beta_path, 
                      list.files(beta_path, 
                                 pattern = "^beta_all_projections_loc_.*.*[0-9]\\.csv$"))
  beta = rbindlist(lapply(beta_files, fread, 
                          select = c("location_id", "year_id", "draw", "data_var")
  )
  )
  
  # save out intermediate file for beta forecasts
  beta_output_filename = ifelse(scenario == 1, "FILEPATH/accelerated_beta_forecast.csv",
                                "FILEPATH/reference_beta_forecast.csv")
  
  fwrite(beta, beta_output_filename)
  
  # gamma - probability of living in an institution/community given that you have a diagnosis
  if (scenario == 2) {
    gamma_folder = "alt_projection_draws/accelerated_gamma/"
  }else if (scenario == 4) {
    gamma_folder = "alt_projection_draws/decelerated_gamma/"
  }else {
    gamma_folder = "projected_draws/"
  }
  gamma_path = paste0(projected_files, gamma_folder)
  paste0("gamma path: ", gamma_path) # sanity check
  
  gamma_files = paste0(gamma_path, 
                       list.files(gamma_path, 
                                  pattern = "^gamma_projections_loc_.*.*[0-9]\\.csv$"))
  gamma = rbindlist(lapply(gamma_files, fread,
                           select = c("location_id", "year_id", "draw", "data_var")
  )
  )
  
  # save out intermediate file for gamma forecasts
  if(scenario == 2){
    gamma_output_filename <- "FILEPATH/accelerated_gamma_forecast.csv"
  }else if(scenario == 4){
    gamma_output_filename <- "FILEPATH/decelerated_gamma_forecast.csv"
  }else{
    gamma_output_filename <- "FILEPATH/reference_gamma_forecast.csv"
  }
  
  fwrite(gamma, gamma_output_filename)
  
  # theta - no diagnosed with dementia over dementia residents in nursing homes
  theta_path = paste0(projected_files, "theta/")
  # load in theta forecasts based on scenario 
  if (scenario == 1){ # accelerated beta
    theta_file = paste0(theta_path, "theta_for_accelerated_beta_forecast.csv")
  } else if (scenario == 2){ # accelerated gamma
    theta_file = paste0(theta_path, "theta_for_accelerated_gamma_forecast.csv")
  } else if (scenario == 4){ # decelerated gamma
    theta_file = paste0(theta_path, "theta_for_decelerated_gamma_forecast.csv")
  } else { # reference or accelerated cost 
    theta_file = paste0(theta_path, "reference_theta_forecast.csv")
  }
  paste0("theta path: ", theta_file) # sanity check
  
  theta = fread(theta_file)
  
  # Community direct costs: 
  cost_com_folder = ifelse(scenario == 3, 
                           "alt_projection_draws/accelerated_cost_com/",
                           "projected_draws/")
  cost_com_path = paste0(projected_files, cost_com_folder)
  paste0("cost community path: ", cost_com_path) # sanity check
  
  cost_com_files = paste0(cost_com_path, 
                          list.files(cost_com_path, 
                                     pattern = "^cost_com_projections_loc_.*.*[0-9]\\.csv$"))
  cost_com = rbindlist(lapply(cost_com_files, fread))
  
  # Facility direct costs: 
  cost_fac_folder = ifelse(scenario == 3, 
                           "alt_projection_draws/accelerated_cost_fac/",
                           "projected_draws/")
  cost_fac_path = paste0(projected_files, cost_fac_folder)
  paste0("cost facility path: ", cost_fac_path)
  
  cost_fac_files = paste0(cost_fac_path, 
                          list.files(cost_fac_path, 
                                     pattern = "^cost_fac_projections_loc_.*.*[0-9]\\.csv$"))
  cost_fac = rbindlist(lapply(cost_fac_files, fread))
  
  # Caregiving Hours: 
  care_hours_path = paste0(projected_files, "projected_draws/")
  care_hours_files = paste0(care_hours_path, 
                            list.files(care_hours_path, 
                                       pattern = "^caregiving_projections_loc_.*.*[0-9]_RTR\\.csv$"))
  care_hours = rbindlist(lapply(care_hours_files, fread))
  
  
  # Wages: 
  wage_path = paste0(projected_files, "projected_draws/")
  # male:
  male_wage_files = paste0(wage_path, 
                           list.files(wage_path, 
                                      pattern = "^income_projections_loc_.*.*[0-9]_1\\.csv$"))
  male_wage = rbindlist(lapply(male_wage_files, fread))
  
  # female:
  female_wage_files = paste0(wage_path, 
                             list.files(wage_path, 
                                        pattern = "^income_projections_loc_.*.*[0-9]_2\\.csv$"))
  female_wage = rbindlist(lapply(female_wage_files, fread))
  
  # Caregiver Gender (percent female):
  # using the 2019 draws here - assuming caregiver gender stays stable
  gender_path = "FILEPATH"
  gender_run = 5 # update this as needed
  gender = fread(paste0(gender_path, "gender_draws_run_0", gender_run, ".csv"))[year_id == 2019 & location_id %in% loc_ids]
  
  # Attributable fraction (direct cost): 
  af_path <- "FILEPATH"
  af <- fread(paste0(af_path,'attributable_fraction_draws_RTR_2024_04_24_logit.csv'))
  
  # Attributable fraction (caregiving hours):
  caregiving_af = fread(paste0(af_path, "caregiving_af_draws_2024_04_24.csv"))
  
  # Sensitivity Analysis: Old Attributable fraction values
  af_old = fread(paste0(af_path,'attributable_fraction_draws_2023_12_15.csv'))
  caregiving_af_old = fread(paste0(af_path, "caregiving_af_draws_2023_12_04.csv"))
  
  # Labor Force Participation: 
  lfp = fread("FILEPATH/lfp_forecast_draws_by_sex_2020_2050.csv")
  # load in sensitivity LFP data: 
  lfp_half = fread("FILEPATH/sensitivity_lfp_forecast_draws_by_sex_2020_2050.csv")
  
  
  ########################## Clean, Process and Merge Data #######################
  # align prevalence data draws with other draw names
  prev[, draw := draw - 1]
  prev[, draw := paste0("draw_",draw)]
  # change beta variable name: 
  setnames(beta, old = "data_var", new = "beta")
  # merge prevalence and beta: 
  data = merge(prev, beta[,.(year_id, location_id, draw, beta)], 
               by = c("year_id", "location_id", "draw"))
  
  # merge gamma: 
  setnames(gamma, old = "data_var", new = "gamma")
  data = merge(data, gamma[,.(year_id, location_id, draw, gamma)],
               by = c("year_id", "location_id", "draw"))
  
  # merge theta: 
  data = merge(data, theta[,.(year_id, location_id, draw, theta)],
               by = c("year_id", "location_id", "draw"))
  
  # calculate number of diagnosed cases:
  data[, dx := total_cases * beta]
  # number of undiagnosed cases: 
  data[, no_dx := total_cases * (1-beta)]
  
  ## Sensitivity Analysis: High & Low Prevalence: 
  data[, ":=" (dx15 = total_cases15*beta, 
               dx85 = total_cases85*beta, 
               no_dx15 = total_cases15*(1-beta), 
               no_dx85 = total_cases85*(1-beta))]
  
  # 3 of the 4 groups have non-zero costs:
  #   1. diagnosed in community (dx_comm)
  #   2. diagnosed in SNF (dx_inst)
  #   3. undiagnosed in SNF (no_dx_inst)
  #   
  
  # calculate number of diagnosed cases in institution vs community: 
  data[, dx_comm := dx * (1-gamma)]
  data[, dx_inst := dx * gamma]
  
  # Not diagnosed and in community
  data[, no_dx_comm := no_dx * (1 - theta)]
  # Not diagnosed and in institution
  data[, no_dx_inst := no_dx * theta]
  
  ## Sensitivity Analysis: High & Low Prevalence: 
  # calculate number of diagnosed cases in institution vs community: 
  data[, ":=" (dx_comm15 = dx15 * (1-gamma),
               dx_comm85 = dx85 * (1-gamma))]
  data[, ":=" (dx_inst15 = dx15 * gamma, 
               dx_inst85 = dx85 * gamma)]
  
  # Not diagnosed and in community
  data[, ":=" (no_dx_comm15 = no_dx15 * (1 - theta),
               no_dx_comm85 = no_dx85 * (1 - theta))]
  # Not diagnosed and in institution
  data[, ":=" (no_dx_inst15 = no_dx15 * theta,
               no_dx_inst85 = no_dx85 * theta)]
  
  
  ## Community - direct costs in community setting 
  #   1) merge in attributable fraction draws
  #   2) create attributable cost (AF*total)
  # 
  
  # names of AF draws were 'sim_1', etc. in the AF data.table, they also went from 1-1000
  #       instead of 0 - 999. Need to align with other draws
  af[, draw := str_replace(draw, 'sim', 'draw')]
  af[draw=="draw_1000", draw := "draw_0"]
  setnames(af, old = c("value"), new = c('af'))
  
  af_comm = af[moderator == "community"]
  
  setnames(cost_com, old = "data_var", new = "comm_total")
  # merge community AF with community costs: 
  cost_com_af = merge(cost_com, af_comm, by = "draw")
  # calculate community costs attributable to dementia
  cost_com_af[,comm_attr := comm_total*af]
  
  ## Sensitivity Analysis: Old Attributable Fraction
  af_old[, draw := str_replace(draw, 'sim', 'draw')]
  af_old[draw=="draw_1000", draw := "draw_0"]
  setnames(af_old, old = c("value"), new = c('af_old'))
  
  af_comm_old = af_old[moderator == "community"]
  
  # merge old community AF with community costs: 
  cost_com_af = merge(cost_com_af, af_comm_old, by = c("draw", "moderator"))
  
  # calculate old community costs attributable to dementia
  cost_com_af[,comm_attr_old := comm_total*af_old]
  
  # merge attributable community costs with rest of data: 
  comm_cols = c("year_id", "location_id", "draw", 
                "comm_total", "comm_attr", "comm_attr_old")
  data = merge(data, cost_com_af[, ..comm_cols], 
               by = c("year_id", "location_id", "draw"))
  
  # 
  ### Facility - diagnosis
  # 
  setnames(cost_fac, old = "data_var", new = "fac_total_dx")
  
  # get facility with diagnosis AF:
  af_fac = af[moderator == "facility"]
  
  # merge facility AF with facility costs: 
  cost_fac_af = merge(cost_fac, af_fac, by = "draw")
  # calculate facility costs attributable to dementia
  cost_fac_af[, fac_attr_dx := fac_total_dx*af]
  
  ## Sensitivity Analysis: Old Attributable Fraction
  af_fac_old = af_old[moderator == "facility"]
  
  # merge old facility AF with facility costs:
  cost_fac_af = merge(cost_fac_af, af_fac_old, by = c("draw", "moderator"))
  # calculate old facility costs attributuable to dementia
  cost_fac_af[, fac_attr_dx_old := fac_total_dx*af_old]
  
  # merge attributable diagnosed facility costs with rest of data: 
  fac_cols = c("year_id", "location_id", "draw", 
               "fac_total_dx", "fac_attr_dx", "fac_attr_dx_old")
  data = merge(data, cost_fac_af[, ..fac_cols], 
               by = c("year_id", "location_id", "draw"))
  
  # 
  ### Facility - no diagnosis
  # we make an assumption that PLwD in a nursing home who do not have a diagnosis have 
  # half the cost of someone in a nursing home who does have a diagnosis
  # to get the costs for this group (undiagnosed in nursing home), we use an attributable
  # fraction that is 1/2 of the value of the AF for diagnosed in nursing homes. This is created 
  # in the script 00_attributable_fraction_meta_analysis.R
  # 
  
  cost_fac_nodx = copy(cost_fac)
  setnames(cost_fac_nodx, old = "fac_total_dx", new = "fac_total_no_dx")
  
  # get facility with no diagnosis AF: 
  af_fac_nodx = af[moderator == "facility_no_dx"]
  
  # merge facility no dx AF with facility costs: 
  cost_fac_nodx_af_baseline = merge(cost_fac_nodx, af_fac_nodx, by = "draw")
  # calculate facility costs attributable to dementia
  cost_fac_nodx_af_baseline[, fac_attr_no_dx := fac_total_no_dx*af]
  
  ## Sensitivity Analysis: Old Attributable Fraction
  af_fac_nodx_old = af_old[moderator == "facility_no_dx"]
  
  # merge old facility no dx AF with facility costs:
  cost_fac_nodx_af_baseline = merge(cost_fac_nodx_af_baseline, af_fac_nodx_old, 
                                    by = c("draw", "moderator"))
  # calculate old facility costs attributuable to dementia
  cost_fac_nodx_af_baseline[, fac_attr_no_dx_old := fac_total_no_dx*af_old]
  
  # merge attributable un-diagnosed facility costs with rest of data: 
  fac_nodx_cols = c("year_id", "location_id", "draw", "fac_total_no_dx", 
                    "fac_attr_no_dx", "fac_attr_no_dx_old")
  data = merge(data, cost_fac_nodx_af_baseline[, ..fac_nodx_cols], 
               by = c("year_id", "location_id", "draw"))
  
  # 
  ### SENSITIVITY ANALYSIS - HIGH ATT. FRAC. : Facility - no diagnosis, use 75% of diagnosed attributable fraction
  #
  
  af_draws_high <- af[moderator=='facility_no_dx_75',]
  fac_total_nodx_75 <- merge(cost_fac_nodx,af_draws_high, by = "draw")
  
  fac_total_nodx_75[,fac_attr_no_dx_75 := fac_total_no_dx*af]
  
  fac_total_nodx_75[, af :=NULL]
  fac_total_nodx_75[, moderator :=NULL]
  
  # Merging with the rest of the data
  fac75_cols = c("year_id", "location_id", "location_name", 
                 "draw", "fac_attr_no_dx_75")
  data <- merge(data, fac_total_nodx_75[, ..fac75_cols], 
                by = c('location_id', "year_id", "draw"))
  
  # 
  ### SENSITIVITY ANALYSIS - LOW ATT. FRAC. : Facility - no diagnosis, use 25% of diagnosed attributable fraction
  #
  
  af_draws_low <- af[moderator=='facility_no_dx_25',]
  fac_total_nodx_25 <- merge(cost_fac_nodx,af_draws_low, by = "draw")
  
  fac_total_nodx_25[,fac_attr_no_dx_25 := fac_total_no_dx*af]
  
  fac_total_nodx_25[, af :=NULL]
  fac_total_nodx_25[, moderator :=NULL]
  
  # Merging with the rest of the data
  fac25_cols = c("year_id", "location_id", "draw", "fac_attr_no_dx_25")
  data <- merge(data, fac_total_nodx_25[, ..fac25_cols], 
                by = c('location_id', "year_id", "draw"))
  
  # 
  ### Caregiving hours
  # 
  setnames(care_hours, old = "data_var", new = "weekly_hours")
  
  # convert weekly to annual hours
  care_hours[, annual_hours := 52*weekly_hours]
  
  # create "male hours" and "female hours" variables:
  gender[, year_id := NULL] # using 2019 estimates, so don't want to merge by year
  setnames(gender, old = c("variable","value"), new = c("draw", "pct_female_cg"))
  care_hours_sex = merge(care_hours[,.(year_id, location_id, draw, annual_hours)], 
                         gender, by = c("location_id", "draw"))
  care_hours_sex[, ":=" (female_annual_hours = annual_hours*pct_female_cg,
                         male_annual_hours = annual_hours*(1-pct_female_cg))]
  
  # merge in care hours:
  data = merge(data, care_hours_sex, 
               by = c("year_id", "location_id", "draw"))
  
  #
  ### Wages
  #
  setnames(male_wage, old = "data_var", new = "male_annual_wage")
  setnames(female_wage, old = "data_var", new = "female_annual_wage")
  
  # convert to an hourly wage using 2080 hours per year
  male_wage[, male_hourly_wage := male_annual_wage/2080]
  female_wage[, female_hourly_wage := female_annual_wage/2080]
  
  # merge in wage data: 
  data = merge(data, male_wage[,.(year_id, location_id, draw, male_hourly_wage)], 
               by = c("year_id", "location_id", "draw"))
  all_data = merge(data, female_wage[,.(year_id, location_id, draw, female_hourly_wage)], 
                   by = c("year_id", "location_id", "draw"))
  
  # merge in LFP:
  # pivot wider to have one column for male lfp and female lfp each 
  lfp_wide = dcast(lfp, location_id + year_id + draw ~ sex_id, value.var = 'lfp')
  setnames(lfp_wide, c("1","2"), c("male_lfp","female_lfp"))
  all_data = merge(all_data, lfp_wide, 
                   by = c("year_id", "location_id", "draw"))
  
  ### SENSITIVITY ANALYSIS: Halve LFP
  # Assumption that those who are caregiving are more likely to have 
  # not been working in the first place
  lfp_half_wide = dcast(lfp_half, location_id + year_id + draw ~ sex_id, 
                        value.var = 'lfp_half')
  setnames(lfp_half_wide, c("1","2"), c("male_lfp_half","female_lfp_half"))
  all_data = merge(all_data, lfp_half_wide, 
                   by = c("year_id", "location_id", "draw"))
  
  # merge in caregiving hours attributable fraction:
  setnames(caregiving_af, old = "V1", new = "cg_af")
  # add draw labels to caregiving attributable fraction data
  caregiving_af[, draw := paste0("draw_",c(0:999))]
  all_data <- merge(all_data, caregiving_af, by = "draw")
  
  ## Sensitivity Analysis: Old caregiving attributable fraction
  setnames(caregiving_af_old, old = "V1", new = "cg_af_old")
  # add draw labels to caregiving attributable fraction data
  caregiving_af_old[, draw := paste0("draw_",c(0:999))]
  all_data <- merge(all_data, caregiving_af_old, by = "draw")
  
  
  ############################# Calculating Total Costs ##########################
  # calculate attributable direct costs
  all_data[, attributable_direct_all := comm_attr*dx_comm + fac_attr_dx*dx_inst + fac_attr_no_dx*no_dx_inst]
  
  ############## Sensitivity analysis: facility no diagnosis attributable fraction
  # create attributable cost estimates for undiagnosed in facility attributable 
  # fraction sensitivity analysis; 
  # total cost estimates don't change, since we are only changing the att. frac.
  ##############
  all_data[, attributable_direct_all_75 := comm_attr*dx_comm + fac_attr_dx*dx_inst + fac_attr_no_dx_75*no_dx_inst]
  all_data[, attributable_direct_all_25 := comm_attr*dx_comm + fac_attr_dx*dx_inst + fac_attr_no_dx_25*no_dx_inst]
  
  ############## Sensitivity analysis: old attributable fraction
  # create attributable cost estimates for old attributable fraction sensitivity analysis;
  ##############
  all_data[, attributable_direct_all_old := comm_attr_old*dx_comm + fac_attr_dx_old*dx_inst + fac_attr_no_dx_old*no_dx_inst]
  
  # also calculate the 'total' rather than attributable cost
  all_data[, total_direct_all := comm_total*dx_comm + fac_total_dx*dx_inst + fac_total_no_dx*no_dx_inst]
  
  ## calculate total caregiving costs for diagnosed and undiagnosed: 
  all_data[, ":=" (total_caregiving_dx = (female_annual_hours*female_hourly_wage*female_lfp + 
                                            male_annual_hours*male_hourly_wage*male_lfp)*dx_comm, 
                   total_caregiving_no_dx = (female_annual_hours*female_hourly_wage*female_lfp + 
                                               male_annual_hours*male_hourly_wage*male_lfp)*no_dx_comm)]
  # calculate total caregiving costs for all: 
  all_data[, total_caregiving_all := total_caregiving_dx + total_caregiving_no_dx]
  
  # calculate cost of informal care attributable to dementia
  all_data[, ":=" (attributable_caregiving_dx = total_caregiving_dx*cg_af,
                   attributable_caregiving_no_dx = total_caregiving_no_dx*cg_af,
                   attributable_caregiving_all = total_caregiving_all*cg_af)]
  
  ############## Sensitivity analysis: old caregiving attributable fraction
  # create attributable cost estimates for old caregiving attributable fraction sensitivity analysis
  ##############
  all_data[, ":=" (attributable_caregiving_dx_old = total_caregiving_dx*cg_af_old,
                   attributable_caregiving_no_dx_old = total_caregiving_no_dx*cg_af_old,
                   attributable_caregiving_all_old = total_caregiving_all*cg_af_old)]
  
  ############## Sensitivity analysis: Half Caregiver Labor Force Participation (LFP)
  # create attributable cost estimates for caregiver half LFP sensitivity analysis
  ##############
  # calculate total caregiving costs for diagnosed and undiagnosed: 
  all_data[, ":=" (total_caregiving_dx_half = (female_annual_hours*female_hourly_wage*female_lfp_half + 
                                                 male_annual_hours*male_hourly_wage*male_lfp_half)*dx_comm, 
                   total_caregiving_no_dx_half = (female_annual_hours*female_hourly_wage*female_lfp_half + 
                                                    male_annual_hours*male_hourly_wage*male_lfp_half)*no_dx_comm)]
  # calculate total caregiving costs for all: 
  all_data[, total_caregiving_all_half := total_caregiving_dx_half + total_caregiving_no_dx_half]
  
  # calculate cost of informal care attributable to dementia
  all_data[, ":=" (attributable_caregiving_dx_half = total_caregiving_dx_half*cg_af,
                   attributable_caregiving_no_dx_half = total_caregiving_no_dx_half*cg_af,
                   attributable_caregiving_all_half = total_caregiving_all_half*cg_af)]
  
  ############## Sensitivity analysis: Full Caregiver Labor Force Participation (LFP)
  # create attributable cost estimates for 100% caregiver LFP sensitivity analysis
  ##############
  # calculate total caregiving costs for diagnosed and undiagnosed: 
  all_data[, ":=" (total_caregiving_dx_full = (female_annual_hours*female_hourly_wage*1 + # setting LFP = 1 here
                                                 male_annual_hours*male_hourly_wage*1)*dx_comm, 
                   total_caregiving_no_dx_full = (female_annual_hours*female_hourly_wage*1 + 
                                                    male_annual_hours*male_hourly_wage*1)*no_dx_comm)]
  # calculate total caregiving costs for all: 
  all_data[, total_caregiving_all_full := total_caregiving_dx_full + total_caregiving_no_dx_full]
  
  # calculate cost of informal care attributable to dementia
  all_data[, ":=" (attributable_caregiving_dx_full = total_caregiving_dx_full*cg_af,
                   attributable_caregiving_no_dx_full = total_caregiving_no_dx_full*cg_af,
                   attributable_caregiving_all_full = total_caregiving_all_full*cg_af)]
  
  ############## Sensitivity analysis: High & Low prevalence: 
  # calculate attributable direct costs
  all_data[, ":=" (low_prev_attributable_direct_all = comm_attr*dx_comm15 + 
                     fac_attr_dx*dx_inst15 + fac_attr_no_dx*no_dx_inst15, 
                   high_prev_attributable_direct_all = comm_attr*dx_comm85 + 
                     fac_attr_dx*dx_inst85 + fac_attr_no_dx*no_dx_inst85)]
  
  # calculate total caregiving costs for diagnosed and undiagnosed: 
  all_data[, ":=" (low_prev_total_caregiving_dx = (female_annual_hours*female_hourly_wage*female_lfp + 
                                                     male_annual_hours*male_hourly_wage*male_lfp)*dx_comm15, 
                   high_prev_total_caregiving_dx = (female_annual_hours*female_hourly_wage*female_lfp + 
                                                      male_annual_hours*male_hourly_wage*male_lfp)*dx_comm85, 
                   low_prev_total_caregiving_no_dx = (female_annual_hours*female_hourly_wage*female_lfp + 
                                                        male_annual_hours*male_hourly_wage*male_lfp)*no_dx_comm15, 
                   high_prev_total_caregiving_no_dx = (female_annual_hours*female_hourly_wage*female_lfp + 
                                                         male_annual_hours*male_hourly_wage*male_lfp)*no_dx_comm85)]
  # calculate total caregiving costs for all: 
  all_data[, ":=" (low_prev_total_caregiving_all = low_prev_total_caregiving_dx + low_prev_total_caregiving_no_dx, 
                   high_prev_total_caregiving_all = high_prev_total_caregiving_dx + high_prev_total_caregiving_no_dx)]
  
  # calculate cost of informal care attributable to dementia
  all_data[, ":=" (low_prev_attributable_caregiving_dx = low_prev_total_caregiving_dx*cg_af,
                   low_prev_attributable_caregiving_no_dx = low_prev_total_caregiving_no_dx*cg_af,
                   low_prev_attributable_caregiving_all = low_prev_total_caregiving_all*cg_af,
                   high_prev_attributable_caregiving_dx = high_prev_total_caregiving_dx*cg_af,
                   high_prev_attributable_caregiving_no_dx = high_prev_total_caregiving_no_dx*cg_af,
                   high_prev_attributable_caregiving_all = high_prev_total_caregiving_all*cg_af)]
  ##############
  
  
  # get today's date to include in file names: 
  td = Sys.Date()
  
  # initialize folder name to save to based on scenario: 
  # scenario = 0 -> Reference Scenario (baseline)
  # scenario = 1 -> Accelerated Beta
  # scenario = 2 -> Accelerated Gamma
  # scenario = 3 -> Accelerated Cost (facility and community)
  # scenario = 4 -> Decelerated Gamma
  if (scenario == 1) {
    scenario_out = "accelerated_beta"
  } else if (scenario == 2) {
    scenario_out = "accelerated_gamma"
  } else if (scenario == 3) {
    scenario_out = "accelerated_cost"
  } else if (scenario == 4) {
    scenario_out = "decelerated_gamma"
  } else {
    scenario_out = "reference"
  }
  # sanity check:
  paste0("FILEPATH",
         scenario_out)
  # saving all draws: 
  fwrite(all_data,
         paste0("FILEPATH",
                scenario_out,"/forecast_costs_all_draws_RTR_",
                td,".csv"))
  
  # clear environment and unused memory before next loop
  save_vars = c("functions_dir","h_root","j_root","l_root","s","scenarios") # variables to keep
  rm(list=setdiff(ls(), save_vars)) # clear environment except for save_vars
  gc() # clear unused memory (this script is memory intensive)
}


# sum up all location_ids (at the draw level) to get a smaller file for global totals
global_data <- all_data[,.(attributable_direct_global = sum(attributable_direct_all),
                           total_direct_global = sum(total_direct_all),
                           attributable_indirect_global = sum(attributable_caregiving_all),
                           total_indirect_global = sum(total_caregiving_all)),
                        by = c("draw","year_id")]

# save out global estimates:
fwrite(global_data,
       paste0("FILEPATH",
              scenario_out,"/forecast_costs_global_draws_",
              td,".csv"))

