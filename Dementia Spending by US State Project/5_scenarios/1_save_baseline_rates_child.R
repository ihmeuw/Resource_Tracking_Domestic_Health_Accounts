################################################################################
# The alternate forecast scenarios requires that the 2000-2019 rates, whether 
# from linear models or annualized rates of change (AROC) be saved for all 
# countries, such that percentiles can be calculated. This script calculates and 
# saves those rates for the beta, gamma and cost models.

# Plan: for each model, take median of all countries rates (the draws of the 1000 
# linear models for percentage models, the AROCs for the cost models) and then rank 
# those. Find the 85th percentile country, and then for countries whose medians 
# fall below that, replace all of their rates with the rates of that country
# Author: Elye Bliss
# Date: Oct 12, 2023
################################################################################

# Clean the environment 
rm(list=ls()) 

library(data.table)
library(MASS) 


# Helper function to calculate annualized rate of change (AROC)
source('FILEPATH/helper_functions.R')

locs <- fread(file = 'FILEPATH/location_set_22_metadata.csv')
locs <- locs[level==3] # Only need countries

# Load latest draws of everything from latest ST-GPR runs
stgpr_path <- 'FILEPATH'

# Save outputs in Project/IRH/Global_dementia_2023/data/scenarios
output_dir = 'FILEPATH'

variables <- c('beta_all','caregiving','gamma','cost_com','cost_fac',
               'theta','income')

# Since this is mean to parallelize over location_id, take location from command
# line input. This corresponds to the `lid` variable in the accompanying
# parallel_run_parent.R script
location_input <- commandArgs()[6]

########################## Required functions ##################################


logit <- function(x){
  # We use the logit on models that are for percentage models, to keep their bounds
  # between 0 and 1.
  return(log(x/(1-x)))
}

get_linear_rates <- function(path_to_data,variable,percent,loc){
  # Function to load draws data (large files), fit the linear models over sorted 
  # draws (same method used to forecast), take draws from those models and save 
  # results as the 'rates' for given country and model. These are needed for 
  # `beta_all`, and `gamma`. 
  # Parameters:
  # `path_to_data` = path to latest ST-GPR output at the draws level, including
  # model number if applicable
  # `variable` = i.e. 'gammma', 'beta', etc.
  # `percent` = is variable a percentage like gamma? Or number like caregiving
  # `loc` = location fed by parent script for parallelization.

  
  # Load draws data and standardize data variable name
  data <- fread(path_to_data) # These are from ST-GPR
  if (variable=='theta'){
    setnames(data, old = c("variable","theta"), new = c("draw","data_var"))
  }else{
    setnames(data, old = c("variable","value"), new = c("draw","data_var"))
  }

  # Selecting needed data years for fitting the forecasting model
  data_00_19 <- data[year_id >= 2000 & year_id <= 2019, ]
  data_00_19 <- merge(data_00_19, locs)
  
  data_00_19 <- data_00_19[location_id==loc]
  # logit transform any percent variables that should fall between 0 and 1
  # (percentage-based models)
  if(percent==T){
    data_00_19[data_var>=1.0,data_var:=0.99 ] # prevent Inf
    data_00_19[data_var<=0.0,data_var:=0.01 ] # prevent -Inf
    data_00_19[,data_var := logit(data_var)] 
  }
  # else{
  #   # The only other model that uses this function but is not percentage-based
  #   # is caregiving, which should be scaled between 0 and 168
  #   data_00_19[,data_var := scaled_logit(data_var,0,168)] 
  # }
  
  # Overview is to get a thousand, 'ranked' models after sorting the data for
  # each year (so not exactly corresponding to draws). Then take one random draw 
  # for the year_id coefficient for each, and use those to project future years.
  
  # Sort data by data_var for each year 
  setorder(data_00_19,'year_id',-'data_var')
  # create a rank_index variable
  data_00_19[,rank_index := rep(1:1000,20)]
  
  draw_from_retro <- function(rank,df){
    # function to get one random draw from the data
    set.seed(123)
    draw_n <- df[rank_index==rank] # should be 20 obs
    model <- lm(data_var ~ year_id,data=draw_n)
    vcov_mat <- vcov(model)
    model_means = coefficients(model)
    model_draws = setDT(data.frame(MASS::mvrnorm(1, model_means, vcov_mat)))
    names(model_draws) <- 'estimates'
    return(model_draws$estimates[2]) # The second is the coefficient estimate.
  }
  
  data_19 <- data_00_19[year_id==2019]


  # Get an array of 1000 parameter draws
  ranks <- c(1:1000)
  coef_draws_array <- lapply(ranks,draw_from_retro,data_00_19)
  coef_draws_array <- unlist(coef_draws_array)
  coef_draws_array <- sort(coef_draws_array,decreasing = T)
  data_19[,coef := coef_draws_array]
  
  # only save required variables
  data_19 <- data_19[,.(rank_index,coef)]
  
  # Save output 
  fwrite(data_19,paste0(output_dir,variable,'/rates_',loc,'_RTR.csv'))
  
  # free space
  # objects to remove
  rm_objs <- c('data','data_00_19','data_19')
  rm(list=rm_objs)
  
  
}

get_aroc_rates <- function(path_to_data,variable,percent,loc){
  # Function to load draws data (large files), calculate the 2000-2019 AROCs for 
  # given country and model, and save AROCs for future use. This is needed for
  # `cost_com` and `cost_fac`. 
  # Parameters:
  # `path_to_data` = path to latest ST-GPR output at the draws level, including
  # model number if applicable
  # `variable` = i.e. 'gammma', 'beta', etc.
  # `percent` = is variable a percentage like gamma? Or number like caregiving
  # `loc` = location fed by parent script for parallelization.

  # Load draws data and standardizes variables
  data <- fread(path_to_data) # These are from ST-GPR
  setnames(data, old = c("variable","value"), new = c("draw","data_var"))
  
  # Subset for parallelizing
  data <- data[location_id==loc]
  
  # Selecting needed data years and renaming variables as required by helper 
  # AROC function
  data_00_19 <- data[year_id == 2000 | year_id == 2019, ]
  data_00_19 <- merge(data_00_19, locs)
  
  # calculating aroc between 2000 and 2019 
  data_aroc <- create_metric(data_00_19,
                             id_var = c('year_id', 'location_id', 'location_name'), 
                             aroc_years = c(2000, 2019))
  setnames(data_aroc, old = 'aroc_output', new = 'aroc')
  
  # Only keep variables needed
  data_aroc <- data_aroc[, c('location_id', 'location_name', 'draw', 'aroc')]
  
  # Create data.table in long format with rbind
  # data_aroc only has aroc and not values, so need to merge data_00_19 and then 
  # filter to 2019 only.
  data_19 <- merge(data_00_19, data_aroc, by = c('draw','location_id','location_name'))
  data_19 <- data_19[year_id==2019,.(aroc)] # Do not need to keep 2000
  
  setorder(data_19,-'aroc')
  data_19[,rank_index := c(1:1000)]
  
  # Save output 
  fwrite(data_19,paste0(output_dir,variable,'/rates_',loc,'_RTR.csv'))
  
  # free space
  # objects to remove
  rm_objs <- c('data','data_00_19','data_19','data_aroc')
  rm(list=rm_objs)
  
  
}



############################ Project beta_all ##################################

# Loading beta_all draws
beta_all_path <- paste0(stgpr_path,'beta_all/st_gpr_output/')

# Get the draws file with the highest model number. This line of code works
# because the 'draws' files appear before the 'means' files in the directories.
# If it stops working for a given model, hard-code the file name to the correct
# one. Hard-code result and comment below lines once file is determined to be 
# correct.

# draw_files <- unlist(str_extract_all(list.files(beta_all_path),".*draws.*"))
# max_model_num_file <- draw_files[which.max(as.integer(str_extract_all(draw_files,"[0-9]{1,2}")))]
# print(paste0("Using draws from ",max_model_num_file))
max_model_num_file <- "dx_rate_all_draws_run_06.csv"
beta_all_path <- paste0(beta_all_path,max_model_num_file)
is_percent <- T

#get_linear_rates(beta_all_path,'beta_all',is_percent,location_input)


################# Project care_settings (gamma) ################################

# Get path to latest draws
gamma_path <- paste0(stgpr_path,'care_settings/st_gpr_output/')

# Get the draws file with the highest model number
# draw_files <- unlist(str_extract_all(list.files(gamma_path),".*draws.*"))
# max_model_num_file <- draw_files[which.max(as.integer(str_extract_all(draw_files,"[0-9]{1,2}")))]
# print(paste0("Using draws from ",max_model_num_file))
max_model_num_file <- "gamma_model_6_draws.csv"
gamma_path <- paste0(gamma_path,max_model_num_file)
is_percent <- T

# Call function to calculate projections, save draws and plot results
get_linear_rates(gamma_path,'gamma',is_percent,location_input)


######################## Project cost (community) ##############################

# Get path to latest draws
cost_path <- paste0(stgpr_path,'cost/st_gpr_output/community/')

# Get the draws file with the highest model number
# draw_files <- unlist(str_extract_all(list.files(cost_path),".*draws.*"))
# max_model_num_file <- draw_files[which.max(as.integer(str_extract_all(draw_files,"[0-9]{1,2}")))]
# print(paste0("Using draws from ",max_model_num_file))
max_model_num_file <- "community_draws7.csv"
cost_path <- paste0(cost_path,max_model_num_file)
is_percent <- F

# Call function to calculate projections, save draws and plot results
#get_aroc_rates(cost_path,'cost_com',is_percent,location_input)

################## Calculate cost (facility) ###################################

# Get path to latest draws
cost_path <- paste0(stgpr_path,'cost/st_gpr_output/facility/')

# Get the draws file with the highest model number
# max_model_num_file <- list.files(cost_path)[which.max(as.integer(str_extract_all(list.files(cost_path),"[0-9]{1,2}")))]
# print(paste0("Using draws from ",max_model_num_file))
max_model_num_file <- "facility_draws8.csv"
cost_path <- paste0(cost_path,max_model_num_file)
is_percent <- F

# Call function to calculate projections, save draws and plot results
#get_aroc_rates(cost_path,'cost_fac',is_percent,location_input)
