# Load cleaned ILO data and merge with GDP per capita covariate
# Author: Elye Bliss
# Date: Aug 11, 2023

rm(list = ls())

library(data.table)

############################  Load data sets and merge #########################

# Load GDP per capita covariate:
# GDP per capita is given in 2019 USD
gdp_pc_dt <- fread(file = "FILEPATH/gdp_pc_1950_2027.csv")

# Load cleaned ILO data
ILO_dt <- fread('FILEPATH/ILO_clean.csv')

# Merge 

ILO_dt <- merge(ILO_dt,
              gdp_pc_dt[,.(ihme_loc_id,location_name,year_id,gdp_pc)],
              by=c('ihme_loc_id','location_name','year_id'),
              all.x = T)

# Save output to intermediate directory
fwrite(ILO_dt,file='FILEPATH/ILO_covars_merged.csv')
