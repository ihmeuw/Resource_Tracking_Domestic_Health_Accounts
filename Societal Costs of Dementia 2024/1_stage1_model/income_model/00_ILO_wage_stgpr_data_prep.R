# Clean data from ILOSTAT in order to prepare it for a ST-GPR stage 1 model.
# Author: Elye Bliss
# Date: Aug 11, 2023

rm(list = ls())

library(data.table)

########################## Load and clean ILO data #############################

ILO_dt <- fread('FILEPATH/EAR_4MTH_SEX_ECO_CUR_NB_A-full-2023-08-01.csv')
# data info: https://ilostat.ilo.org/resources/concepts-and-definitions/description-wages-and-working-time-statistics/

# Notes on the following filtering decisions:
# - there are more than one aggregate total to choose from:
#   'Economic activity (Aggregate): Total' (17442 obs)
#   'Economic activity (ISIC-Rev.3.1): Total' (4885)
#   'Economic activity (ISIC-Rev.4): Total' (8727)
#   -> Opting for the first option, since it has the most observations.
# - currencies are available in:
#   'Currency: 2017 PPP $' (4725 obs)
#   'Currency: Local currency' (6824)
#   'Currency: U.S. dollars' (5893)
#   -> during the exploration phase I tried all of the above. Despite having most
#     observations, the local currency units leads to wild outliers.
#     I will go with PPP as the most reliable.


table(ILO_dt[classif1.label=='Economic activity (Aggregate): Total' & classif2.label=='Currency: Local currency',]$sex.label)
# Sex: Female   Sex: Male  Sex: Other  Sex: Total 
# 1874        1886           2        3062 
# - The sum of Female, Male and Other is actually greater than Total, so it could 
#   be the case that some countries only report Total, or only by sex. Since we 
#   are interested in modeling the LIS data by sex, only Male/Female are included.
ILO_dt <- ILO_dt[classif1.label=='Economic activity (Aggregate): Total'&
                   classif2.label=='Currency: 2017 PPP $'&
                   sex.label %in% c("Sex: Male","Sex: Female"),] #3760 obs remain


setnames(ILO_dt,old=c('sex.label','ref_area.label'),new=c('sex','location_name'))
ILO_dt[sex=='Sex: Male',sex := 'male']
ILO_dt[sex=='Sex: Female',sex := 'female']


# Add columns needed for currency conversion
ILO_dt$index_col <- 1:nrow(ILO_dt)

# get location IDs
loc_ids <- fread('FILEPATH/location_set_22_metadata.csv')
# location_set_id=22: -> covariate computation
# release_id = 6 -> GBD 2019
loc_ids <- loc_ids[level == 3,] #filter to just country level

ILO_dt <- merge(ILO_dt,loc_ids[,.(location_name,ihme_loc_id)],by = 'location_name', all.x = T)

# Look at and correct as many name mis-matches as possible
names_mismatch <- ILO_dt[is.na(ihme_loc_id),]
unique(names_mismatch$location_name)

# Individually find IHME matches
loc_ids$location_name[loc_ids$location_name %like% "Palestine"] 

ILO_dt[location_name=='Bolivia',location_name := "Bolivia (Plurinational State of)"]
ILO_dt[location_name=="Congo, Democratic Republic of the",location_name := "Democratic Republic of the Congo"]
ILO_dt[location_name=="Hong Kong, China",location_name := "Hong Kong Special Administrative Region of China"]
ILO_dt[location_name=="Korea, Republic of",location_name := "Republic of Korea" ]
ILO_dt[location_name=="Moldova, Republic of" ,location_name := "Republic of Moldova"]
ILO_dt[location_name=="Tanzania, United Republic of" ,location_name := "United Republic of Tanzania"]
ILO_dt[location_name=="Türkiye" ,location_name := "Turkey"]
ILO_dt[location_name=="United States" ,location_name := "United States of America"]
ILO_dt[location_name=="Venezuela, Bolivarian Republic of" ,location_name := "Venezuela (Bolivarian Republic of)"]
ILO_dt[location_name=="Occupied Palestinian Territory" ,location_name := "Palestine"]
ILO_dt$ihme_loc_id <- NULL
ILO_dt <- merge(ILO_dt,loc_ids[,.(location_name,ihme_loc_id)],by = 'location_name', all.x = T) # remerge
ILO_dt <- ILO_dt[!is.na(ihme_loc_id),]
# Lost observations: "Aruba","Curaçao","Macau, China"

ILO_dt <- ILO_dt[,annual_pi :=12*obs_value] # convert to annual personal income



############### Perform currency conversion on ILO data ########################

source("FILEPATH/currency_conversion.R")

# GDP per capita data is expressed in 2019 US dollars. Convert the ILO data to 
# the same units.
ILO_convert_in <- ILO_dt[,.(index_col, ihme_loc_id, annual_pi)]
ILO_convert_out <- currency_conversion(ILO_convert_in,
                                       col.loc = "ihme_loc_id",
                                       col.value = "annual_pi",
                                       currency.year = 2017, #col.currency.year = "time",
                                       currency = "ppp",
                                       base.year = 2019,
                                       base.unit = "usd",
                                       converter.version = 6.2,
                                       simplify = F
)

ILO_dt <- merge(ILO_dt,ILO_convert_out[,.(index_col,
                                          annual_pi_new,
                                          currency_year_new,
                                          currency_new,
                                          deflator,
                                          lcu_usd)],by='index_col')

setnames(ILO_dt,old = c('time','annual_pi_new'),new=c('year_id','income'))

# Save output to intermediate directory
fwrite(ILO_dt,file='FILEPATH/ILO_clean.csv')
