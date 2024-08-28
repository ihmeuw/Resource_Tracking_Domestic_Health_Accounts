################################################################################
# This version of the forecast script is meant to be the child script to be run
# parallelized by location. This script was created for the RTR to only re-run
# the caregiving model.
# Author: Amy Lastuka
# Date: 4/25/24
################################################################################

# Clean the environment 
rm(list=ls()) 

library(data.table)
library(stringr)
library(ggplot2)
library(MASS) 

# Helper function to calculate annualized rate of change (AROC)
source('/snfs1/Project/IRH/Global_dementia/functions/FGH_2019/04_functions/helper_functions.R')
# Note: this also pulls a locs data frame which might be outdated. Override with 
# the below data frame which contains 9 countries not found in the helper 
# function, but are found in the draws data:
# [1] "Monaco"                "San Marino"            "Saint Kitts and Nevis" "Cook Islands"         
# [5] "Nauru"                 "Niue"                  "Palau"                 "Tokelau"              
# [9] "Tuvalu" 
locs <- fread(file = "FILEPATH/location_set_22_metadata.csv")
locs <- locs[level==3] # Only need countries

# Load latest draws of everything from latest ST-GPR runs
stgpr_path <- 'FILEPATH'

# Save outputs in Project/IRH/Global_dementia_2023/data/scenarios
output_dir = 'FILEPATH'

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

scaled_logit <- function(x,a,b){
  # Alternative to regular logit, found from https://otexts.com/fpp2/limits.html
  result <- log((x-a)/(b-x))
  return(result)
}
inverse_scaled_logit <- function(y,a,b){
  # Function to reverse scaled_logit transformation
  result <- ((b-a)*exp(y))/(1+exp(y))+a
  return(result) 
}



project_baseline <- function(path_to_data,variable,percent,loc){
  # Function to load draws data (large files), merge with location names
  # calculate the forecasted values and save results. This version of the function
  # Is used for the following models: beta_all, gamma, caregiving, and theta.
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
  }else if (variable=="caregiving"){
    setnames(data, old = c("variable","value"), new = c("draw","data_var"))
  }else{
    setnames(data, old = c("variable","value"), new = c("draw","data_var"))
  }
  
  # Subset for parallelizing
  data <- data[location_id==loc]
  
  # Selecting needed data years for fitting the forecasting model
  data_00_19 <- data[year_id >= 2000 & year_id <= 2019, ]
  data_00_19 <- merge(data_00_19, locs)
  
  # logit transform any percent variables that should fall between 0 and 1
  # (percentage-based models)
  if(percent==T){
    data_00_19[data_var>=1.0,data_var:=0.99 ] # prevent Inf
    data_00_19[data_var<=0.0,data_var:=0.01 ] # prevent -Inf
    data_00_19[,data_var := logit(data_var)] 
  }

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
    return(model_draws$estimates[2])
  }
  
  # Get an array of 1000 parameter draws
  ranks <- c(1:1000)
  coef_draws_array <- lapply(ranks,draw_from_retro,data_00_19)
  coef_draws_array <- unlist(coef_draws_array)
  coef_draws_array <- sort(coef_draws_array,decreasing = T)
  
  # The largest data_var value is now the first line of 2019 data, the smallest 
  # is the last row of 2019. This matches the order of the coef_draws_array.
  prev_year <- data_00_19[year_id==2019,]$data_var

  # Initialize empty data.table to hold projections, as well as data.table to 
  # bind projections for 2020-2050
  data_empty <- data_00_19[year_id==2019]
  data_empty[,":=" (year_id=NA_integer_,
                    data_var=NA_real_)]
  # make empty data table
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
    data_future <- rbind(data_future,data_temp[,.(location_id,
                                                  year_id,
                                                  draw,
                                                  data_var)])
    
    # Increment all draws by estimated effect of year_id 
    prev_year <- data_future[year_id==year,]$data_var
    
  }
  
  # inverse_logit data_var or exp, depending on model type
  if(percent==T){
    # Used for beta_all, gamma, theta
    data_future[,data_var := inverse_logit(data_var)]
  }else{
    # Used for caregiving
    data_future[,data_var := pmin(inverse_scaled_logit(data_var,0,169), 168)]
  }
  
  # Create time-series plot that combines ST-GPR data with forecast data
  data_to_plot <- rbind(data[,c('location_id','year_id','draw','data_var')],
                        data_future[,c('location_id','year_id','draw','data_var')])

  data_to_plot <- data_to_plot[,.(median=quantile(data_var,prob=0.5),
                                  upper=quantile(data_var,prob=0.975),
                                  lower=quantile(data_var,prob=0.025)),by=c('year_id')]
    
    
  # Save plot of projections. Trying to use png in case I can combine to make
  # one pdf in the future.
  png(paste0(output_dir,'plots/all/',variable,"_projections",'_projections_loc_',loc,'_RTR.png'))
  
  # Plot on y scale 0-1 if units are percentages  
  ylim_max <- ifelse(percent==T,1,NA)

  p <- ggplot(data_to_plot,aes(x=year_id,y=median))+
    geom_line()+
    geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.1, fill = "red", 
                color = "black", linetype = "dotted")+
    ylim(0,ylim_max)+
    theme_minimal()+
    labs(x = "Year", y = variable,
         title =paste0("Raw and projected ",variable," for ",locs[location_id==loc]$location_name))
  
  print(p)  
  dev.off()
  # Save output draws
  fwrite(data_future,paste0(output_dir,'projected_draws/',variable,'_projections_loc_',loc,'_RTR.csv'))
  
  # free space
  # objects to remove
  rm_objs <- grep("data.*",names(.GlobalEnv),value=TRUE)
  rm_objs <- rm_objs[!(rm_objs %in% c('path_to_data','get_he_data','get_location_metadata'))]
  rm(list=rm_objs)
  
  
}



project_baseline_aroc <- function(path_to_data,variable,percent,loc,sex=NULL){
  # Function to load draws data (large files), merge with location names
  # calculate the forecasted values using the AROC method and save results.
  # This function is currently applied to cost_com, cost_fac, and income models.
  # Parameters:
  # `path_to_data` = path to latest ST-GPR output at the draws level, including
  # model number if applicable
  # `variable` = i.e. 'gammma', 'beta', etc.
  # `percent` = is variable a percentage like gamma? Or number like income
  # `loc` = location fed by parent script for parallelization.
  # `sex` = currently only used for income model

  # Load draws data and standardizes variables
  data <- fread(path_to_data) # These are from ST-GPR
  setnames(data, old = c("variable","value"), new = c("draw","data_var"))
  
  # Subset for parallelizing
  data <- data[location_id==loc]
  
  # Subset by sex, if applicable to model (income-only)
  if(!is.null(sex)){
    data <- data[sex_id==sex]
  }
  
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
  data_ref <- merge(data_00_19, data_aroc, by = c('draw','location_id','location_name'))
  data_ref <- data_ref[year_id==2019] # Do not need to keep 2000
  
  # Initialize empty data.table to hold projections
  data_future <- data_ref[location_id==loc,c('location_id','year_id','location_name','draw','aroc','data_var')]
  data_future <- data_future[is.na(location_id)] # make empty data table
  
  # Get the array of 2000-to-2019 AROCs for all draws for given location_id
  arocs_2019 <- data_ref[location_id==loc]$aroc
  # Get the array of 2019 values for all draws for given location_id
  values_2019 <-data_ref[location_id==loc]$data_var
  
  # Project for each year in loop and rbind results to data_future data table of
  # forecasts. Result should be in long format.
  for (year in year_end){
    
    # Create empty data.table for projected year for given location
    data_temp <- copy(data_ref[,c('location_id','location_name','draw')])
    data_temp <- data_temp[location_id==loc]
    
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
  
  
  # Save plots of projections
  # Name graph depending on whether variable is separated by sex
  if(!is.null(sex)){
    pdf(paste0(output_dir,'plots/all/',variable,"_projections",'_projections_loc_',loc,'_',sex,'.pdf'))
  }else{
    pdf(paste0(output_dir,'plots/all/',variable,"_projections",'_projections_loc_',loc,'.pdf'))
  }
  
  # Plot on y scale 0-1 only if units are percentages  
  ylim_max <- NA
  # List sex_id in title if referring to income model
  if(!is.null(sex)){
    sex_in_title <- ifelse(sex==1,'Males ','Females ')
  }else{
    sex_in_title <- ''
  }
  
  p <- ggplot(data_to_plot,aes(x=year_id,y=median))+
    geom_line()+
    geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.1, fill = "red", 
                color = "black", linetype = "dotted")+
    ylim(0,ylim_max)+
    theme_minimal()+
    labs(x = "Year", y = variable,
         title =paste0("Raw and projected ",variable," for ",sex_in_title,locs[location_id==loc]$location_name))
  
  print(p)  
  dev.off()
  # Save output draws
  if(!is.null(sex)){
    fwrite(data_future,paste0(output_dir,'projected_draws/',variable,'_projections_loc_',loc,'_',sex,'_RTR.csv'))
  }else{
    fwrite(data_future,paste0(output_dir,'projected_draws/',variable,'_projections_loc_',loc,'_RTR.csv'))
  }
  
  
  # free space
  # objects to remove
  rm_objs <- grep("data.*",names(.GlobalEnv),value=TRUE)
  rm_objs <- rm_objs[!(rm_objs %in% c('path_to_data','get_he_data','get_location_metadata'))]
  rm(list=rm_objs)
  
  
}

# Variables available in stgpr_path and their directory names:
# beta_all - beta_all
# caregiving - caregiving
# care_settings - care_settings
# cost_com - cost
# cost_fac - cost
# wages - wages




########################### Project caregiving #################################

# Get path to latest draws
caregiving_path <- paste0(stgpr_path,'caregiving/st_gpr_output/')

# Get the draws file with the highest model number
# draw_files <- unlist(str_extract_all(list.files(caregiving_path),".*draws.*"))
# max_model_num_file <- draw_files[which.max(as.integer(str_extract_all(draw_files,"[0-9]{1,2}")))]
# print(paste0("Using draws from ",max_model_num_file))
max_model_num_file <- "cg_hours_draws_run_15.csv"
caregiving_path <- paste0(caregiving_path,max_model_num_file)
is_percent <- F

# Call function to calculate predictions, save draws and plot results
project_baseline(caregiving_path,'caregiving',is_percent,location_input)


