
##########################################################################
### Author: Amy Lastuka & Michael Breshock
### Date: 09/05/2023
### Project: Global dementia spending
### Purpose: aggregate cost components for 2000-2019 and saves out
### a file with final estimate draws by year-country 
##########################################################################

## Note: this script is very memory intensive 
## you will likely need to use >10GB of memory in your Rstudio cluster session

rm(list=ls()) 


today <- gsub('-','_',Sys.Date())

library(data.table)
library(stringr)

source(paste0(functions_dir,"get_location_metadata.R"))


# # # # # # # set up file paths & run numbers # # # # # # # 

# miscellaneous (non-ST-GPR) inputs needed
cov_path <- "FILEPATH"
theta_path <- "FILEPATH"
af_path <- "FILEPATH"

stgpr_path <- "FILEPATH"

# all ST-GPR models
wage_path <- paste0(stgpr_path,'wages/st_gpr_output/')
beta_path <- paste0(stgpr_path,'beta_all/st_gpr_output/')
gamma_path <- paste0(stgpr_path,'care_settings/st_gpr_output/')
community_path <- paste0(stgpr_path,'cost/st_gpr_output/community/')
facility_path <- paste0(stgpr_path,'cost/st_gpr_output/facility/')
care_hours_path <- paste0(stgpr_path,'caregiving/st_gpr_output/')
care_gender_path <- paste0(stgpr_path,'gender/st_gpr_output/')

wage_run <- 4
beta_run <- '06'
gamma_run <- 6
community_run <- 7
facility_run <- 8
care_hours_run <- '15'
care_gender_run <- '05'

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Loading data - prevalence is split into multiple files because it was so large, load them all in here
files <- paste0(cov_path,list.files(cov_path, 
                    pattern = glob2rx(paste0('all_age_sex_dementia_prevalence_draws_loc_list','**.csv'))))


prev = rbindlist(lapply(files, fread))
#prev <- fread(paste0(root, 'all_age_sex_dementia_prevalence_1990_2019_HK.csv'))

prev[, c("measure_id", "metric_id", "model_version_id", "version_id","modelable_entity_id")    := NULL]

# wide to long (melt)
id.vars <- c('location_id', 'year_id', 'age_group_id', 'sex_id')

# Melting - after melting, the column named "variable" will be the draw, e.g. 'draw_0', etc.

prev_long <- melt(prev, 
              id.vars = id.vars)

setnames(prev_long, "value", "prevalence_rate")

# We originally included Hong Kong in our location set, remove here since we decided not to use it
# also cut '1' which is global location
prev_long <- prev_long[(location_id != 354) & (location_id != 1),]

# loading population
# this does not need to be in draw space
pop <- fread("FILEPATH/population_1990_2019.csv")

# merging with prevalence
prev_merged <- merge(prev_long, pop)
prev_merged[, prev_counts := prevalence_rate * population]

# Loading betas
beta <- fread(paste0(beta_path,'dx_rate_all_draws_run_',beta_run,'.csv'))
setnames(beta, old = "value", new = "beta")
head(beta)

# beta is the output from ST-GPR so it has all subnats, we don't need this
# subset to countries only 
locs <- get_location_metadata(location_set_id=22, release_id = 6) 
# location_set_id=22: -> covariate computation
# release_id = 6 -> GBD 2019
loc_ids <- locs[level == 3]$location_id #filter to country level 
beta <- beta[location_id %in% loc_ids,]

# Creating dataset
data <- merge(prev_merged, beta,
              by = c('location_id', "year_id", "variable")) # "variable" is the draw number


# Loading probability of living in an institution/comunity given that you have a diagnois
gamma <- fread(paste0(gamma_path, 'gamma_model_',gamma_run,'_draws.csv')) # These are from ST-GPR
setnames(gamma, old = "value", new = 'gamma')


# Mergin with the rest of the data
data <- merge(data, gamma, by = c('location_id', "year_id", "variable"))
head(data)


# loading theta 
theta <- fread(paste0(theta_path, 'theta_draws.csv'))
head(theta)

theta <- theta[, c('year_id', 'location_id', 'variable', 'theta')]
head(theta)

# Adding it to the rest of the data
data <- merge(data, theta, by = c('year_id', 'location_id', 'variable'))
head(data)

# Diagnosed
data[, dx := prev_counts * beta]
# not diagnosed
data[, no_dx := prev_counts * (1-beta)]

# 3 of the 4 groups have non-zero costs:
#   1. diagnosed in community (dx_comm)
#   2. diagnosed in SNF (dx_inst)
#   3. undiagnosed in SNF (no_dx_inst)
#   

# living conditions of those with dx
data[, dx_comm := dx * (1-gamma)]
data[, dx_inst := dx * gamma]

# Not diagnosed and in nursing home
data[, no_dx_inst := no_dx * theta]
# Not diagnosed and in community
data[, no_dx_comm := no_dx * (1 - theta)]


#
## Community - direct costs in community setting 
#   1) merge in attributable fraction draws
#   2) create attributable cost (AF*total)
# 
com_total <- fread(paste0(community_path, 'community_draws',community_run,'.csv'))
head(com_total)

setnames(com_total, old = "value", new = 'comm_total')

# load attributable fraction draws
af_draws <- fread(paste0(af_path,'attributable_fraction_draws_RTR_2024_04_24_logit.csv'))

# names of draws were 'sim_1', etc. in the AF data.table, they also went from 1-1000
#       instead of 0 - 999. Need to align with other draws
af_draws[, draw := str_replace(draw, 'sim', 'draw')]
af_draws[draw=="draw_1000", draw := "draw_0"]
setnames(af_draws, old = c("value","draw"), new = c('af','variable'))

af_draws_comm <- af_draws[moderator=='community',]

com_total <- merge(com_total,af_draws_comm, by = "variable" )
head(com_total)

com_total[,comm_attr := comm_total*af]

### Sensitivity Analysis: Old Attributable Fraction
# load old attributable fraction draws
af_draws_old <- fread(paste0(af_path,'attributable_fraction_draws_2023_12_15.csv'))

# names of draws were 'sim_1', etc. in the AF data.table, they also went from 1-1000
#       instead of 0 - 999. Need to align with other draws
af_draws_old[, draw := str_replace(draw, 'sim', 'draw')]
af_draws_old[draw=="draw_1000", draw := "draw_0"]
setnames(af_draws_old, old = c("value","draw"), new = c('af_old','variable'))

af_draws_comm_old <- af_draws_old[moderator=='community',]

com_total <- merge(com_total,af_draws_comm_old, by = c("variable", "moderator"))
head(com_total)

com_total[,comm_attr_old := comm_total*af_old]

# cut global location & also subset to nationals only
com_total <- com_total[(location_id != 1),]
com_total <- com_total[location_id %in% loc_ids,]

# remove unnecessary columns
com_total[, af :=NULL]
com_total[, af_old :=NULL]
com_total[, moderator :=NULL]

# Merging with the rest of the data
data <- merge(data, com_total, by = c('location_id', "year_id", "variable"))

 # 
### Facility - diagnosis
 # 

fac_total_dx <- fread(paste0(facility_path, 'facility_draws',facility_run,'.csv'))
head(fac_total_dx)

setnames(fac_total_dx, old = "value", new = 'fac_total_dx')

af_draws_fac <- af_draws[moderator=='facility',]

fac_total_dx <- merge(fac_total_dx,af_draws_fac, by = "variable")

fac_total_dx[,fac_attr_dx := fac_total_dx*af]

### Sensitivity Analysis: Old Attributable Fraction
af_draws_fac_old <- af_draws_old[moderator=='facility',]

fac_total_dx <- merge(fac_total_dx,af_draws_fac_old, by = c("variable", "moderator"))
head(fac_total_dx)

fac_total_dx[,fac_attr_dx_old := fac_total_dx*af_old]

# cut global location & also subset to nationals only
fac_total_dx <- fac_total_dx[location_id %in% loc_ids]

# remove unnecessary columns
fac_total_dx[, af :=NULL]
fac_total_dx[, af_old :=NULL]
fac_total_dx[, moderator :=NULL]

# Merging with the rest of the data
data <- merge(data, fac_total_dx, by = c('location_id', "year_id", "variable"))

# 
### Facility - no diagnosis
# we make an assumption that PLwD in a nursing home who do not have a diagnosis have 
# half the cost of someone in a nursing home who does have a diagnosis
# to get the costs for this group (undiagnosed in nursing home), we use an attributable
# fraction that is 1/2 of the value of the AF for nursing homes. This is created 
# in the script 00_attributable_fraction_meta_analysis.R
# 

fac_total_nodx <- fread(paste0(facility_path, 'facility_draws',facility_run,'.csv'))
head(fac_total_nodx)

setnames(fac_total_nodx, old = "value", new = 'fac_total_no_dx')

af_draws_fac <- af_draws[moderator=='facility_no_dx',]

fac_total_nodx_baseline <- merge(fac_total_nodx,af_draws_fac, by = "variable")

fac_total_nodx_baseline[,fac_attr_no_dx := fac_total_no_dx*af]

### Sensitivity Analysis: Old Attributable Fraction
af_draws_fac_old <- af_draws_old[moderator=='facility_no_dx',]

fac_total_nodx_baseline <- merge(fac_total_nodx_baseline,af_draws_fac_old, 
                                 by = c("variable", "moderator"))
head(fac_total_nodx_baseline)

fac_total_nodx_baseline[,fac_attr_no_dx_old := fac_total_no_dx*af_old]

# subset to nationals only
fac_total_nodx_baseline <- fac_total_nodx_baseline[location_id %in% loc_ids,]

# remove unnecessary columns
fac_total_nodx_baseline[, af :=NULL]
fac_total_nodx_baseline[, af_old :=NULL]
fac_total_nodx_baseline[, moderator :=NULL]

# Merging with the rest of the data
data <- merge(data, fac_total_nodx_baseline, by = c('location_id', "year_id", "variable"))

# 
### SENSITIVITY ANALYSIS - HIGH ATT. FRAC. : Facility - no diagnosis, use 75% of diagnosed attributable fraction
#

af_draws_high <- af_draws[moderator=='facility_no_dx_75',]
fac_total_nodx_75 <- merge(fac_total_nodx,af_draws_high, by = "variable" )

fac_total_nodx_75[,fac_attr_no_dx_75 := fac_total_no_dx*af]

# cut global location & also subset to nationals only
fac_total_nodx_75 <- fac_total_nodx_75[(location_id != 1),]
fac_total_nodx_75 <- fac_total_nodx_75[location_id %in% loc_ids,]
head(fac_total_nodx_75)

# remove unnecessary columns
fac_total_nodx_75[, af :=NULL]
fac_total_nodx_75[, moderator :=NULL]
fac_total_nodx_75[, fac_total_no_dx :=NULL]

# Merging with the rest of the data
data <- merge(data, fac_total_nodx_75, by = c('location_id', "year_id", "variable"))

# 
### SENSITIVITY ANALYSIS - LOW ATT. FRAC. : Facility - no diagnosis, use 25% of diagnosed attributable fraction
#

af_draws_low <- af_draws[moderator=='facility_no_dx_25',]
fac_total_nodx_25 <- merge(fac_total_nodx,af_draws_low, by = "variable")

fac_total_nodx_25[,fac_attr_no_dx_25 := fac_total_no_dx*af]

# subset to nationals only
fac_total_nodx_25 <- fac_total_nodx_25[location_id %in% loc_ids,]
head(fac_total_nodx_25)

# remove unnecessary columns
fac_total_nodx_25[, af :=NULL]
fac_total_nodx_25[, moderator :=NULL]
fac_total_nodx_25[, fac_total_no_dx :=NULL]

# Merging with the rest of the data
data <- merge(data, fac_total_nodx_25, by = c('location_id', "year_id", "variable"))

 # 
### Caregiving hours
 # 

# load care hours
care_hours <- fread(paste0(care_hours_path, 'cg_hours_draws_run_',care_hours_run,'.csv'))
care_hours <- care_hours[(location_id != 1),]
care_hours <- care_hours[location_id %in% loc_ids,]
head(care_hours)

# convert weekly to annual hours
care_hours[, annual_hours := 52*care_hours]

# load wages 
wages <- fread(paste0(wage_path,'LIS_draws',wage_run,'.csv'))
wages <- wages[location_id %in% loc_ids,]
head(wages)

#reshape from long to wide by sex
wages_wide <- dcast(wages, location_id + year_id + variable ~ sex_id, fun.aggregate = mean, 
                    value.var = 'value')
setnames(wages_wide, c("1","2"), c("male","female"))

#convert to an hourly wage using 2080 hours per year
# using the LIS data that we have for now. Will update to newer estimates when available.
wages_wide[, male_hourly_wage := male/2080]
wages_wide[, female_hourly_wage := female/2080]

# load in LFP data: 
lfp = fread("FILEPATH/lfp_draws_by_sex_1990_2019.csv")
# remove population variable (not needed here)
lfp[,population := NULL]
# pivot wider to have one column for male lfp and female lfp each 
lfp_wide = dcast(lfp, location_id + year_id + draw ~ sex_id, value.var = 'lfp')
setnames(lfp_wide, c("1","2","draw"), c("male_lfp","female_lfp","variable"))

# merge lfp into wage data: 
wages_wide = merge(wages_wide, lfp_wide, 
                   by = c("year_id", "location_id", "variable"))

### SENSITIVITY ANALYSIS: Halve LFP
# Assumption that those who are caregiving are more likely to have 
# not been working in the first place 

# load in sensitivity LFP data: 
lfp_half = fread("FILEPATH/sensitivity_lfp_draws_by_sex_1990_2019.csv")
# remove population variable (not needed here)
lfp_half[,population := NULL]
# pivot wider to have one column for male lfp and female lfp each 
lfp_half_wide = dcast(lfp_half, location_id + year_id + draw ~ sex_id, 
                      value.var = 'lfp_half')
setnames(lfp_half_wide, c("1","2","draw"), c("male_lfp_half","female_lfp_half","variable"))

# merge lfp_half into wage data: 
wages_wide = merge(wages_wide, lfp_half_wide, 
                   by = c("year_id", "location_id", "variable"))

# load % female from caregiver gender model
gender_draws <- fread(paste0(care_gender_path,'gender_draws_run_',care_gender_run,'.csv'))
setnames(gender_draws,"value","pct_female_cg")

# create 'female hours' and 'male hours' variables
care_hours <- merge(care_hours, gender_draws, by = c('location_id', "year_id", "variable"))
care_hours[, female_annual_hours:= annual_hours*pct_female_cg]
care_hours[, male_annual_hours:= annual_hours*(1-pct_female_cg)]
# merge care hours, wages, and % female with the other data 
all_data <- merge(data, care_hours, by = c('location_id', "year_id", "variable"))
all_data <- merge(all_data, wages_wide, by = c('location_id', "year_id", "variable"))
# merge caregiving attributable fraction:
caregiving_af = fread(paste0(af_path, "caregiving_af_draws_2024_04_24.csv"))
head(caregiving_af)
setnames(caregiving_af, old = "V1", new = "cg_af")
# add draw labels to caregiving attributable fraction data
caregiving_af[, variable := paste0("draw_",c(0:999))]
all_data <- merge(all_data, caregiving_af, by = "variable")

## Sensitivity Analysis: Old caregiving attributable fraction: 
caregiving_af_old = fread(paste0(af_path, "caregiving_af_draws_2023_12_04.csv"))
setnames(caregiving_af_old, old = "V1", new = "cg_af_old")
caregiving_af_old[, variable := paste0("draw_",c(0:999))]
all_data <- merge(all_data, caregiving_af_old, by = "variable")

############# Calculating total costs

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

# save copy of totals by location
fwrite(all_data,paste0('/home/j/Project/IRH/Global_dementia_2023/output/final_cost_global_draws_by_loc_RTR_sens_',
                        today,'.csv'),row.names = F)

# sum up all location_ids (at the draw level) to get a smaller file for global totals
global_data <- all_data[,.(attributable_direct_global = sum(attributable_direct_all),
                           total_direct_global = sum(total_direct_all),
                           total_indirect_global = sum(total_caregiving)), by = c("variable","year_id") ]

fwrite(global_data,'FILEPATH/final_cost_global_draws.csv',
       row.names = F)


