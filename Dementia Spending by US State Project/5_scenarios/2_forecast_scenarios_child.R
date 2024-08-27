################################################################################
# Load the baseline rates for different models, and create forecast draws under
# accelerated and decelerated scenarios.

# Plan: for each model, take median of all countries rates (the draws of the 1000 
# linear models for percentage models, the AROCs for the cost models) and then rank 
# those. Find the 85th percentile country, and then for countries whose medians 
# fall below that, replace all of their rates with the rates of that country. 
# For the decelerated scenario, take the 15th percentile of gamma, and replace 
# rates above that with the 15th-percentile country's gamma rates.
# Author: Elye Bliss
# Date: Oct 13, 2023
################################################################################

# Clean the environment 
rm(list=ls()) 

library(data.table)
library(ggplot2)
library(MASS) 


# Helper function to calculate annualized rate of change (AROC)
#source('/snfs1/Project/IRH/Global_dementia/functions/FGH_2019/04_functions/helper_functions.R')
source('FILEPATH/helper_functions.R')

locs <- fread(file = "FILEPATH/location_set_22_metadata.csv")
locs <- locs[level==3] # Only need countries

# Load latest draws of everything from latest ST-GPR runs
stgpr_path <- 'FILEPATH'

# baseline forecasts
baseline_outputs <- 'FILEPATH'

# rates directory has all baseline rates from which to compares country percentiles
rates_dir <- 'FILEPATH'

# Save outputs in Project/IRH/Global_dementia_2023/data/scenarios
output_dir <- 'FILEPATH'

# Years forecasted will cover 2020-2050
year_end <- (2020:2050)

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
inverse_logit <- function(x){
  # After forecasting the logit-transformed percentages, we can convert predicted
  # values back to their original units with inverse-logit.
  return(exp(x)/(1+exp(x)))
}


project_alt_linear <- function(path_to_data,variable,threshold_rates,loc,variable_dir){
  # Function analogous to project_alt_linear in 0_baselines_child.R, however, it 
  # takes replacement rates from alternate scenarios and then projects a country
  # forward using those. 
  # Parameters:
  # `path_to_data` = path to latest ST-GPR output at the draws level, including
  # model number if applicable
  # `variable` = i.e. 'gammma', 'beta', etc.
  # `threshold_rates` the rates of the replacement country (at the 15th or 85th
  # percentile)
  # `loc` = location fed by parent script for parallelization.
  # `variable_dir` = where updated forecasts should be saved.
  
  # Trouble-shoot/test function with individually-set parameters
  # Example:
  # path_to_data = gamma_path
  # threshold_rates <- replacement_rates
  # loc = location_input
  # variable_dir=variable_output_dir
  
  # Load draws data and standardize data variable name
  data <- fread(path_to_data) # These are from ST-GPR
  setnames(data, old = c("variable","value"), new = c("draw","data_var"))
  
  data <- data[location_id==loc]
  data_19 <- data[year_id==2019]
  
  # logit transform any percent variables that should fall between 0 and 1
  # (percentage-based models)
  data_19[data_var>=1.0,data_var:=0.99 ] # prevent Inf
  data_19[data_var<=0.0,data_var:=0.01 ] # prevent -Inf
  data_19[,data_var := logit(data_var)] 

  
  # Sort data by data_var 
  setorder(data_19,-'data_var') # descending order same as threshold rates
  
  # used same code block from 0_baselines_child.R only with threshold rates as 
  # replacements
  coef_draws_array <- threshold_rates$coef
  prev_year <- data_19$data_var 
  
  # Initialize empty data.table to hold projections for single year, as well as 
  # data.table to bind projections for 2020-2050
  
  # make empty data table to store projections
  data_empty <- copy(data_19)
  data_empty[,":=" (year_id=NA_integer_,
                    data_var=NA_real_)]
  
  data_future <- data.table(location_id=integer(),
                            year_id=integer(),
                            draw=character(),
                            data_var=numeric()) 
  
  # Project for each year in loop and rbind results to data_future data table of
  # forecasts. Result should be in long format.
  for (year in year_end){
    
    data_temp <- copy(data_empty)
    data_temp[,year_id:=year]
    
    # Advance each projection using the array of coefficient draws.
    data_temp[,data_var:= prev_year+coef_draws_array]
    data_future <- rbind(data_future,data_temp)
    
    # Increment all draws by estimated effect of year_id 
    prev_year <- data_future[year_id==year,]$data_var
    
  }
  
  # convert back to percentage units
  data_future[,data_var := inverse_logit(data_var)]
  
  # Create time-series plot that combines ST-GPR data with forecast data
  data_to_plot <- rbind(data[,c('location_id','year_id','draw','data_var')],
                        data_future[,c('location_id','year_id','draw','data_var')])
  
  data_to_plot <- data_to_plot[,.(median=quantile(data_var,prob=0.5),
                                  upper=quantile(data_var,prob=0.975),
                                  lower=quantile(data_var,prob=0.025)),by=c('year_id')]
  
  # Plot on y scale 0-1 if units are percentages  
  ylim_max <- 1
  # No need to save output since graphs will be plotted on single pdf in separate
  # script. The below can still be used to look at individual countries.
  p <- ggplot(data_to_plot,aes(x=year_id,y=median))+
    geom_line()+
    geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.1, fill = "red",
                color = "black", linetype = "dotted")+
    ylim(0,ylim_max)+
    theme_minimal()+
    labs(x = "Year", y = variable,
         title =paste0("Raw and projected ",variable," for ",locs[location_id==loc]$location_name))

  print(p)
  
  # Save output draws
  alt_output <- paste0(j_root,'Project/IRH/Global_dementia_2023/data/scenarios/output/alt_projection_draws/',
                       variable_dir)
  fwrite(data_future,paste0(alt_output,variable,'_projections_loc_',loc,'_RTR.csv'))
  
  # free space
  # objects to remove
  rm_objs <- grep("data.*",names(.GlobalEnv),value=TRUE)
  rm_objs <- rm_objs[!(rm_objs %in% c('path_to_data','get_he_data','get_location_metadata'))]
  rm(list=rm_objs)
  
  
}


project_alt_aroc <- function(path_to_data,variable,threshold_rates,loc,variable_dir){
  # Function analogous to project_alt_aroc in 0_baselines_child.R, however, it 
  # takes replacement AROCs from alternate scenarios and then projects a country
  # forward using those. 
  # Parameters:
  # `path_to_data` = path to latest ST-GPR output at the draws level, including
  # model number if applicable
  # `variable` = i.e. 'gammma', 'beta', etc.
  # `threshold_rates` the AROCs of the replacement country (at the 85th percentile)
  # `loc` = location fed by parent script for parallelization.
  # `variable_dir` = where updated forecasts should be saved.
  
  # Load draws data and standardizes variables
  data <- fread(path_to_data) # These are from ST-GPR
  setnames(data, old = c("variable","value"), new = c("draw","data_var"))
  
  # Subset for parallelizing
  data <- data[location_id==loc]
  
  # Selecting needed data years and renaming variables as required by helper 
  # AROC function
  data_19 <- data[year_id == 2019, ]
  data_19 <- merge(data_19, locs)
  
  # Use replacement rates as AROC variable
  data_19[,aroc := replacement_rates$aroc]
  
  # Only keep variables needed
  data_19 <- data_19[, c('location_id','year_id' ,'location_name', 'draw', 'data_var',
                             'aroc')]
  
  # Initialize empty data.table to hold projections
  data_future <- data_19[location_id==loc,c('location_id','year_id','location_name','draw','aroc','data_var')]
  data_future <- data_future[is.na(location_id)] # make empty data table
  
  # Get the array of replacement rates as AROC variable
  arocs_2019 <- replacement_rates$aroc
  
  # Note: unlike for the linear model projections, we do not sort the data by
  # AROC before projecting. When replacing data with the AROCs from the 85th
  # percentile, we therefore do not pre-sort the data either. 
  
  # Get the array of 2019 values for all draws for given location_id
  values_2019 <-data_19[location_id==loc]$data_var
  
  # Project for each year in loop and rbind results to data_future data table of
  # forecasts. Result should be in long format.
  for (year in year_end){
    
    # Create empty data.table for projected year 
    data_temp <- copy(data_19[,c('location_id','location_name','draw')])

    # set year_id to projected year
    data_temp[,year_id:=year]
    
    # Derive AROC between 2019 and projected year for all draws (arocs_2019 is
    # an array of length equal to number of draws)
    data_temp[,aroc :=(1 + arocs_2019)^(year-2019)]
    
    # Use derived AROCs to project values for projected year for all draws 
    # (values_2019 is also an array of length equal to number of draws)
    data_temp[,data_var := aroc*values_2019]
    
    data_future <- rbind(data_future,data_temp)
  }
  
  
  # Make plot of quantiles per year, both raw and projected values
  data_to_plot <- rbind(data[,c('location_id','year_id','draw','data_var')]
                        ,data_future[,c('location_id','year_id','draw','data_var')])
  
  data_to_plot <- data_to_plot[,.(median=quantile(data_var,prob=0.5),
                                  upper=quantile(data_var,prob=0.975),
                                  lower=quantile(data_var,prob=0.025)),by=c('year_id')]
  
  
  # Plot on y scale 0-1 only if units are percentages  
  ylim_max <- NA
  # No need to save output since graphs will be plotted on single pdf in separate
  # script. The below can still be used to look at individual countries.
  p <- ggplot(data_to_plot,aes(x=year_id,y=median))+
    geom_line()+
    geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.1, fill = "red", 
                color = "black", linetype = "dotted")+
    ylim(0,ylim_max)+
    theme_minimal()+
    labs(x = "Year", y = variable,
         title =paste0("Raw and projected ",variable," for ",locs[location_id==loc]$location_name))
  
  print(p)  

  # Save output draws
  alt_output <- paste0(j_root,'Project/IRH/Global_dementia_2023/data/scenarios/output/alt_projection_draws/',
                       variable_dir)
  fwrite(data_future,paste0(alt_output,variable,'_projections_loc_',loc,'_RTR.csv'))
  
  
  # free space
  # objects to remove
  rm_objs <- grep("data.*",names(.GlobalEnv),value=TRUE)
  rm_objs <- rm_objs[!(rm_objs %in% c('path_to_data','get_he_data','get_location_metadata'))]
  rm(list=rm_objs)
  
  
}



########################### Project beta_all ##################################

# Loading beta_all draws
variable <- 'beta_all'
beta_all_path <- paste0(stgpr_path,variable,'/st_gpr_output/')
# draw_files <- unlist(str_extract_all(list.files(beta_all_path),".*draws.*"))
# max_model_num_file <- draw_files[which.max(as.integer(str_extract_all(draw_files,"[0-9]{1,2}")))]
# print(paste0("Using draws from ",max_model_num_file))
max_model_num_file <- "dx_rate_all_draws_run_06.csv"
beta_all_path <- paste0(beta_all_path,max_model_num_file)

# get median of rates arrays for all countries
rates_path <- '/home/j/Project/IRH/Global_dementia_2023/data/scenarios/projection_rates/beta_all/'

# get median of all countries:
cntry_medians <- data.table(location_id = locs$location_id,median = NA_real_,filename = NA_character_)
for (loc in locs$location_id){
  # file is sorted in descending order (also shouldn't matter here, but will later)
  file <- paste0('rates_',loc,'.csv')
  rates <- fread(paste0(rates_path,file))
  m <- rates[rank_index==500]$coef # get median

  cntry_medians[location_id==loc,median := m]
  cntry_medians[location_id==loc,filename := file]
}

# get file of 85th percentile country
setorder(cntry_medians,'median') # sort ascending
threshold <- cntry_medians[(0.85*nrow(locs))%/%1] # this will be the 173rd row

variable_output_dir <-'accelerated_beta/'
# compare median of current country to threshold, and replace forecasts as needed.
if (cntry_medians[location_id==location_input]$median<threshold$median){
  replacement_rates <- fread(paste0(rates_path,threshold$filename))
  project_alt_linear(beta_all_path,variable,replacement_rates,location_input,variable_output_dir)
} else{
  # copy over unchanged forecast file to scenario directory
  baselines <- fread(paste0(baseline_outputs,variable,'_projections_loc_',location_input,'.csv'))
  fwrite(baselines,paste0(output_dir,variable_output_dir,variable,'_projections_loc_',location_input,'.csv'))
}


################# Project care_settings (gamma) ################################

# Get path to latest draws
gamma_path <- paste0(stgpr_path,'care_settings/st_gpr_output/')
variable <- 'gamma'
# Get the most up-to-date draws file 
# draw_files <- unlist(str_extract_all(list.files(gamma_path),".*draws.*"))
# max_model_num_file <- draw_files[which.max(as.integer(str_extract_all(draw_files,"[0-9]{1,2}")))]
# print(paste0("Using draws from ",max_model_num_file))
max_model_num_file <- "gamma_model_6_draws.csv"
gamma_path <- paste0(gamma_path,max_model_num_file)


# get median of rates arrays for all countries
rates_path <- '/home/j/Project/IRH/Global_dementia_2023/data/scenarios/projection_rates/gamma/'

# get median of all countries:
cntry_medians <- data.table(location_id = locs$location_id,median = NA_real_,filename = NA_character_)
for (loc in locs$location_id){
  # file is sorted in descending order (also shouldn't matter here, but will later)
  file <- paste0('rates_',loc,'.csv')
  rates <- fread(paste0(rates_path,file))
  m <- rates[rank_index==500]$coef # get median
  
  cntry_medians[location_id==loc,median := m]
  cntry_medians[location_id==loc,filename := file]
}


setorder(cntry_medians,'median') # sort ascending


# get 85th percentile country
threshold <- cntry_medians[(0.85*nrow(locs))%/%1] # this will be the 173rd row 

variable_output_dir <-'accelerated_gamma/'
# compare median of current country to threshold, and replace forecasts as needed.
if (cntry_medians[location_id==location_input]$median<threshold$median){
  replacement_rates <- fread(paste0(rates_path,threshold$filename))
  project_alt_linear(gamma_path,variable,replacement_rates,location_input,variable_output_dir)
} else{
  # copy over unchanged forecast file to scenario directory
  baselines <- fread(paste0(baseline_outputs,variable,'_projections_loc_',location_input,'.csv'))
  fwrite(baselines,paste0(output_dir,variable_output_dir,variable,'_projections_loc_',location_input,'_RTR.csv'))
}


# get 15th percentile country
threshold <- cntry_medians[(0.15*nrow(locs))%/%1] # this will be the 30th row 

variable_output_dir <-'decelerated_gamma/'
# compare median of current country to threshold, and replace forecasts as needed.
if (cntry_medians[location_id==location_input]$median>threshold$median){
  replacement_rates <- fread(paste0(rates_path,threshold$filename))
  project_alt_linear(gamma_path,variable,replacement_rates,location_input,variable_output_dir)
} else{
  # copy over unchanged forecast file to scenario directory
  baselines <- fread(paste0(baseline_outputs,variable,'_projections_loc_',location_input,'.csv'))
  fwrite(baselines,paste0(output_dir,variable_output_dir,variable,'_projections_loc_',location_input,'_RTR.csv'))
}

######################## Project cost (community) ##############################

# Get path to latest draws
cost_path <- paste0(stgpr_path,'cost/st_gpr_output/community/')
variable <- 'cost_com'

# Get the draws file with the highest model number
# draw_files <- unlist(str_extract_all(list.files(cost_path),".*draws.*"))
# max_model_num_file <- draw_files[which.max(as.integer(str_extract_all(draw_files,"[0-9]{1,2}")))]
# print(paste0("Using draws from ",max_model_num_file))
max_model_num_file <- "community_draws7.csv"
cost_path <- paste0(cost_path,max_model_num_file)


# get median of rates arrays for all countries
rates_path <- '/home/j/Project/IRH/Global_dementia_2023/data/scenarios/projection_rates/cost_com/'

# get median of all countries:
cntry_medians <- data.table(location_id = locs$location_id,median = NA_real_,filename = NA_character_)
for (loc in locs$location_id){
  # file is sorted in descending order (also shouldn't matter here, but will later)
  file <- paste0('rates_',loc,'.csv')
  rates <- fread(paste0(rates_path,file))
  m <- rates[rank_index==500]$aroc # get median

  cntry_medians[location_id==loc,median := m]
  cntry_medians[location_id==loc,filename := file]
}

# get file of 85th percentile country
setorder(cntry_medians,'median') # sort ascending
threshold <- cntry_medians[(0.85*nrow(locs))%/%1] # this will be the 173rd row

variable_output_dir <-'accelerated_cost_com/'
# compare median of current country to threshold, and replace forecasts as needed.
if (cntry_medians[location_id==location_input]$median<threshold$median){
  replacement_rates <- fread(paste0(rates_path,threshold$filename))
  project_alt_aroc(cost_path,variable,replacement_rates,location_input,variable_output_dir)
} else{
  # copy over unchanged forecast file to scenario directory
  baselines <- fread(paste0(baseline_outputs,variable,'_projections_loc_',location_input,'.csv'))
  fwrite(baselines,paste0(output_dir,variable_output_dir,variable,'_projections_loc_',location_input,'.csv'))
}

################## Calculate cost (facility) ###################################

# Get path to latest draws
cost_path <- paste0(stgpr_path,'cost/st_gpr_output/facility/')
variable <- 'cost_fac'

# Get the draws file with the highest model number
# max_model_num_file <- list.files(cost_path)[which.max(as.integer(str_extract_all(list.files(cost_path),"[0-9]{1,2}")))]
# print(paste0("Using draws from ",max_model_num_file))
max_model_num_file <- "facility_draws8.csv"
cost_path <- paste0(cost_path,max_model_num_file)

# get median of rates arrays for all countries
rates_path <- '/home/j/Project/IRH/Global_dementia_2023/data/scenarios/projection_rates/cost_fac/'

# get median of all countries:
cntry_medians <- data.table(location_id = locs$location_id,median = NA_real_,filename = NA_character_)
for (loc in locs$location_id){
  # file is sorted in descending order (also shouldn't matter here, but will later)
  file <- paste0('rates_',loc,'.csv')
  rates <- fread(paste0(rates_path,file))
  m <- rates[rank_index==500]$aroc # get median

  cntry_medians[location_id==loc,median := m]
  cntry_medians[location_id==loc,filename := file]
}

# compare median of current country to threshold, and replace forecasts as needed.
setorder(cntry_medians,'median') # sort ascending
threshold <- cntry_medians[(0.85*nrow(locs))%/%1] # this will be the 173rd row

variable_output_dir <-'accelerated_cost_fac/'
# compare median of current country to threshold
if (cntry_medians[location_id==location_input]$median<threshold$median){
  replacement_rates <- fread(paste0(rates_path,threshold$filename))
  project_alt_aroc(cost_path,variable,replacement_rates,location_input,variable_output_dir)
} else{
  # copy over unchanged forecast file to scenario directory
  baselines <- fread(paste0(baseline_outputs,variable,'_projections_loc_',location_input,'.csv'))
  fwrite(baselines,paste0(output_dir,variable_output_dir,variable,'_projections_loc_',location_input,'.csv'))
}
