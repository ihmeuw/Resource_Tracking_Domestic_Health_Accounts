################################################################################
# This script cleans the data similarly to 04_LIS_age_wage_stgpr_data_prep.R, 
# however, it reweights wages across income groups to derive an overall average.
# For any missing age groups, it imputes a value based on the output of the age-
# disaggregated LIS scripts. To reweight wages, it uses population size in the 
# age group as the weights instead of sample sizes. 
# Author: Elye Bliss
# Date: Oct 31, 2023
################################################################################


rm(list = ls())

library(ggplot2)
library(readxl)
library(stringr)
library(data.table)
library(dplyr)
library(tidyr)
library(ggpubr)

# Shared functions used in script:
source("FILEPATH/get_ids.R")
source("FILEPATH/currency_conversion.R")
source("FILEPATH/get_population.R")

# Load and clean LIS data
LIS_dt <- fread('FILEPATH/LIS_p_income_pilabour_consolidated_cleaned_outliered_curr_conv_2019USD_2023_10_27.csv')

# Get ages, locations and populations needed to merge
age_group_ids <-get_ids('age_group')
age_group_ids <- age_group_ids[age_group_id %in% unique(LIS_dt$age_group_id)]

locs <- fread(file = "FILEPATH/location_set_22_metadata.csv")
locs <- locs[level==3] #204 countries

# Get population sizes for each wage group/country
all_pop <- get_population(release_id = 6, 
                          location_id = unique(LIS_dt$location_id),
                          year_id = c(1990:2019),
                          age_group_id = unique(LIS_dt$age_group_id),
                          sex_id = c(1,2))
all_pop[, run_id := NULL] # remove unnecessary column

# Merge with LIS data
# First square LIS observations based off age group only
square_LIS <- unique(LIS_dt[,.(location_id,sex_id,year_id)])

# Performing a cross-join using tidyr function
square_ages <- crossing(square_LIS,age_group_ids)
setDT(square_ages)

# note that `age_group_name` will ensure missing age groups are filled in
LIS_dt <- merge(square_ages,LIS_dt,
              by=c('location_id','sex_id','year_id','age_group_id'),
              all=T) 

LIS_dt <- merge(LIS_dt,all_pop[,.(age_group_id,location_id,year_id,sex_id,
                                  population)],
                by=c('age_group_id','location_id','year_id','sex_id'),all.x = T)

# Get ST-GPR output from the age-disaggregated model
model_num = 6
LIS_age_output <- fread('FILEPATH/LIS_age_mean', model_num, '.csv')

LIS_age_output <- LIS_age_output[location_id %in% unique(LIS_dt$location_id)]
LIS_age_output <- LIS_age_output[,-c('lower','upper'),with=F] # drop unused columns
setnames(LIS_age_output,old='val',new='stgpr')

# merge with LIS data
LIS_dt <- merge(LIS_dt,LIS_age_output,
                by=c('age_group_id','location_id','year_id','sex_id'),
                all.x = T) 

# impute wmean_p_pilabour_new (the LIS income variable) with stgpr output when missing
LIS_dt <- LIS_dt[,imputed := ifelse(is.na(wmean_p_pilabour_new),T,F)]
LIS_dt <- LIS_dt[is.na(wmean_p_pilabour_new),wmean_p_pilabour_new:=stgpr]

# Add in full location info
LIS_dt[,":=" (ihme_loc_id=NULL,
          location_name=NULL)]
LIS_dt <- merge(LIS_dt,locs[,.(location_id,location_name,ihme_loc_id)],
                by='location_id')

# Plot a few examples to see how well the transition happens between imputed and
# raw 
set.seed(123)
rand_rows <- round(runif(4,min=1,max=nrow(LIS_dt[imputed==T,])))
missing_ages <- LIS_dt[imputed==T,][rand_rows]

# Make plots with first 4 missing location-sex-years
loc = missing_ages$location_id[1]
locname = missing_ages$location_name[1]
year = missing_ages$year_id[1]
sex = missing_ages$sex_id[1]

plot1 <- ggplot(LIS_dt[location_id==loc&year_id==year&sex_id==sex],
                aes(x=age_group_name,y=wmean_p_pilabour_new,fill = imputed))+
  geom_bar(position = "stack", stat = "identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title=paste0("Imputed vs Raw data for ",
                    locname,
                    " sex_id=",sex),
       x="",
       y="Wages")

loc = missing_ages$location_id[2]
locname = missing_ages$location_name[2]
year = missing_ages$year_id[2]
sex = missing_ages$sex_id[2]

plot2 <- ggplot(LIS_dt[location_id==loc&year_id==year&sex_id==sex],
                aes(x=age_group_name,y=wmean_p_pilabour_new,fill = imputed))+
  geom_bar(position = "stack", stat = "identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title=paste0("Imputed vs Raw data for ",
                    locname,
                    " sex_id=",sex),
       x="",
       y="Wages")

loc = missing_ages$location_id[3]
locname = missing_ages$location_name[3]
year = missing_ages$year_id[3]
sex = missing_ages$sex_id[3]

plot3 <- ggplot(LIS_dt[location_id==loc&year_id==year&sex_id==sex],
                aes(x=age_group_name,y=wmean_p_pilabour_new,fill = imputed))+
  geom_bar(position = "stack", stat = "identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title=paste0("Imputed vs Raw data for ",
                    locname,
                    " sex_id=",sex),
       x="",
       y="Wages")

loc = missing_ages$location_id[4]
locname = missing_ages$location_name[4]
year = missing_ages$year_id[4]
sex = missing_ages$sex_id[4]

plot4 <- ggplot(LIS_dt[location_id==loc&year_id==year&sex_id==sex],
                aes(x=age_group_name,y=wmean_p_pilabour_new,fill = imputed))+
  geom_bar(position = "stack", stat = "identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title=paste0("Imputed vs Raw data for ",
                      locname,
                      " sex_id=",sex),
         x="",
         y="Wages")

all_plots <- ggarrange(plot1,plot2,plot3,plot4,
                       ncol = 2, nrow = 2)
output_dir = "FILEPATH"

# Specify dpi to avoid text overlapping
ggsave(paste0(output_dir,"LIS_age_groups_imputation.pdf"),
       plot = all_plots, dpi = 300)


######## Re-weight using population as weights for each age group ##############
# The LIS data has been weighted by different age groups, which we will not use.

# Drop unused cols
LIS_dt <- LIS_dt[,-c('currency','currency_col','currency_col_new','year_id_new'),with=F] 

LIS_reweight <- LIS_dt[,.(wmean_p_pilabour_new = weighted.mean(wmean_p_pilabour_new, 
                                                   population)),
           by = .( location_id, 
                   location_name,
                   ihme_loc_id,
                   sex_id, 
                   year_id
                   )] #keep names that don't change with aggregation sample_size

# Add index
LIS_reweight[,index_col:=1:nrow(LIS_reweight)]

###################### Add ILO ST-GPR data as covariates #######################
model_num <- 1
ILO_stgpr <- fread(paste0('FILEPATH/income_mean', model_num, '.csv'))

setnames(ILO_stgpr,old='val',new='ILO_est')

LIS <- merge(LIS_reweight,ILO_stgpr[,.(location_id,sex_id,year_id,ILO_est)],by=c('location_id','sex_id','year_id'))

# Rename variables and save data
setnames(LIS,old=c('wmean_p_pilabour_new'),new=c('LIS_est'))

fwrite(LIS,file='FILEPATH/LIS_with_ILO_stgpr_updated_10_31.csv')
