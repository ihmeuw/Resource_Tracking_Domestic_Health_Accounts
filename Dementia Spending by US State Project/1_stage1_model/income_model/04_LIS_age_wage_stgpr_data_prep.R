################################################################################
# Clean LIS wage data such that the output from the first round of ST-GPR, which
# was fit on ILO wage data, will then be used to predict LIS data. This script
# gets run after there are already ST-GPR output estimates base on ILO data.
#
# Scripts 04 to 06 in this series are for producing ST-GPR estimates 
# disaggregated by age group. Scripts 07 onward aggregate a mean across age groups
# and fill in any missing age groups with the ST-GPR output of
# this pass. 
# Author: Elye Bliss
# Date: Aug 21, 2023
################################################################################


rm(list = ls())

library(ggplot2)
library(readxl)
library(stringr)
library(data.table)
library(dplyr)

# Shared functions used in script:
source("FILEPATH/get_ids.R")
source("FILEPATH/currency_conversion.R")


# Load and clean LIS data
LIS_dt <- fread('FILEPATH/LIS_p_income_pilabour_consolidated_cleaned_outliered_curr_conv_2019USD_2023_10_27.csv')

age_group_ids <-get_ids('age_group')
LIS_dt <- merge(LIS_dt,age_group_ids,by='age_group_id',all_x=T)

# Save copy of data that preserves personal income by age group
LIS_age <- copy(LIS_dt)

# version with age
LIS_age[,index_col:=1:nrow(LIS_age)]

###################### Add ILO ST-GPR data as covariates #######################
model_num <- 1
ILO_stgpr <- fread(paste0('FILEPATH/income_mean', model_num, '.csv'))

setnames(ILO_stgpr,old='val',new='ILO_est')

LIS_age <- merge(LIS_age,ILO_stgpr[,.(location_id,sex_id,year_id,ILO_est)],by=c('location_id','sex_id','year_id'))

# Rename variables and save data
setnames(LIS_age,old=c('currency','wmean_p_pilabour_new'),new=c('currency_old','LIS_est'))

fwrite(LIS_age,file='FILEPATH/LIS_age_with_ILO_stgpr_updated_10_27.csv')

