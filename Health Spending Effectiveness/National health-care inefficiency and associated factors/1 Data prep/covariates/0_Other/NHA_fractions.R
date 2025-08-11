##########################################################################
### Author: USERNAME
### Date: 11/26/24
### Project: Health Spending Effectiveness 
### Purpose: Processing health expenditure distribution generated from National Health Accounts (NHA) data
##########################################################################

# clear environment
rm(list=ls())

# load libraries and functions
library(data.table)

# load data: 
data_path = 'FILEPATH/final_estimates_allhchp.csv'
raw_df <- fread(data_path)

# explore dataset
head(raw_df)

# see what HP values are provided
unique(raw_df$sha_hp_name)
# [1] "1. Hospitals"                                                     
# [2] "1.1 Hospitals - General"                                          
# [3] "1.2 Hospitals - Mental health"                                    
# [4] "1.3 Hospitals - Specialized"                                      
# [5] "1.nec Hospitals - Other"                                          
# [6] "2. Resid. long-term care fac."                                    
# [7] "3. Providers of ambulatory care"                                  
# [8] "3.1 Providers of ambulatory care - Medical practices"             
# [9] "3.2 Providers of ambulatory care - Dental practices"              
# [10] "3.3 Providers of ambulatory care - Other practices"               
# [11] "3.4 Providers of ambulatory care - Ambulatory health care centers"
# [12] "3.5 Providers of home health care services"                       
# [13] "3.nec Providers of ambulatory care - NEC"                         
# [14] "4. Providers of ancillary services"                               
# [15] "5. Retailers and other providers of medical goods"                
# [16] "6. Providers of preventative care"                                
# [17] "7. Health care system admin."                                     
# [18] "8. Rest of economy"                                               
# [19] "9. Rest of world"                                                 
# [20] "HP Total"                                                         
# [21] "N.E.C."     

# use total HP (health providers)
# keep disaggregated by HC (health-care function)
total_df = raw_df[hp == 'Total']

# see what HC values are provided
unique(raw_df$sha_hc_name)
# [1] "1. Curative care"                                                            
# [2] "1.1 Curative care - Inpatient"                                               
# [3] "1.2 Curative care - Day"                                                     
# [4] "1.3 Curative care - Outpatient"                                              
# [5] "1.3.1 Curative care - Outpatient - General"                                  
# [6] "1.3.2 Curative care - Outpatient - Dental"                                   
# [7] "1.3.3 Curative care - Outpatient - Specialized"                              
# [8] "1.3.nec Curative care - Outpatient - NEC"                                    
# [9] "1.4 Curative care - Home-based"                                              
# [10] "1. nec Curative care - NEC"                                                  
# [11] "2. Rehabilitative care"                                                      
# [12] "2.1 Rehabilitative care - Inpatient"                                         
# [13] "2.2 Rehabilitative care - Day"                                               
# [14] "2.3 Rehabilitative care - Outpatient"                                        
# [15] "2.4 Rehabilitative care - Home-based"                                        
# [16] "2.nec Rehabilitative care - NEC"                                             
# [17] "3. Long-term care"                                                           
# [18] "3.1 Long-term care - Inpatient"                                              
# [19] "3.2 Long-term care - Day"                                                    
# [20] "3.3 Long-term care - Outpatient"                                             
# [21] "3.4 Long-term care - Home-based"                                             
# [22] "3.nec Long-term care - NEC"                                                  
# [23] "4. Ancillary care"                                                           
# [24] "5. Medical goods"                                                            
# [25] "5.1 Medical goods - Pharms & other medical non-durable goods"                
# [26] "5.1.1 Medical goods - Prescribed meds"                                       
# [27] "5.1.2 Medical goods - Over the counter meds"                                 
# [28] "5.1.3 Medical goods - Other medical non-durables"                            
# [29] "5.2 Medical goods - Therapeutic appliances & prosthetics"                    
# [30] "5.nec Medical goods - NEC"                                                   
# [31] "6. Preventative care"                                                        
# [32] "6.1 Prevention care - IEC programs"                                          
# [33] "6.2 Prevention care - Immunization programs"                                 
# [34] "6.3 Prevention care - Early disease detection"                               
# [35] "6.4 Prevention care - Healthy condition monitoring programs"                 
# [36] "6.5 Prevention care - Epi surveillance and risk and disease control programs"
# [37] "6.6 Prevention care - Preparing for disaster and emergency response programs"
# [38] "6.nec Prevention care - NEC"                                                 
# [39] "7. Governance & admin"                                                       
# [40] "9. Other health care services"                                               
# [41] "HC Total"     

### Aggregation Plan
## Inpatient: 
# "1.1 Curative care - Inpatient"
# "2.1 Rehabilitative care - Inpatient" 
# Remove "3.1 Long-term care - Inpatient"

## Outpatient: 
# "1.3 Curative care - Outpatient" 
# "2.3 Rehabilitative care - Outpatient"
# "3.3 Long-term care - Outpatient" 

## Pharmaceuticals:
# "5.1 Medical goods - Pharms & other medical non-durable goods" 

# create new variable for the categories of interest (inpatient, outpatient, pharmaceuticals)
total_df[, expend_type := dplyr::case_when(
  hc %in% c("1.1", "2.1") ~ "inpatient", 
  hc %in% c("1.3", "2.3", "3.3") ~ "outpatient", 
  hc == "5.1" ~ "pharma", 
  TRUE ~ "other")]

# remove the other categories
total_df = total_df[expend_type != "other"]

# sum up the fraction of total expenditure for each category
frac_df = total_df[,.(frac_of_the = sum(raked_pred_share_value_the), 
                      frac_of_the_lower = sum(raked_pred_lci_share_value_the), 
                      frac_of_the_upper = sum(raked_pred_uci_share_value_the)),
                   by = c("year", "iso3", "country", "expend_type")]

## check means/confidence interval for each category: 
# inpatient:
mean(frac_df[expend_type == "inpatient"]$frac_of_the) # 0.1984053
mean(frac_df[expend_type == "inpatient"]$frac_of_the_lower) # 0.0379005
mean(frac_df[expend_type == "inpatient"]$frac_of_the_upper) # 0.4718197

# outpatient:
mean(frac_df[expend_type == "outpatient"]$frac_of_the) # 0.1960589
mean(frac_df[expend_type == "outpatient"]$frac_of_the_lower) # 0.0361522
mean(frac_df[expend_type == "outpatient"]$frac_of_the_upper) # 0.4839333

# Pharma:
mean(frac_df[expend_type == "pharma"]$frac_of_the) # 0.1388033
mean(frac_df[expend_type == "pharma"]$frac_of_the_lower) # 0.03472401
mean(frac_df[expend_type == "pharma"]$frac_of_the_upper) # 0.3093913

# save max value by category for fixing >1 draws later
max_inpatient = max(frac_df[expend_type == "inpatient"]$frac_of_the_upper) # 0.7518663
max_outpatient = max(frac_df[expend_type == "outpatient"]$frac_of_the_upper) # 0.809549
max_pharma = max(frac_df[expend_type == "pharma"]$frac_of_the_upper) # 0.6567472

# create draws in log-space to force draws >= 0
frac_df[, ":=" (log_frac = log(frac_of_the), 
                log_frac_lower = log(frac_of_the_lower), 
                log_frac_upper = log(frac_of_the_upper))]

# calculate standard error 
frac_df[, log_SE := (log_frac_upper - log_frac_lower) / 3.92]
# divide by 3.92 for 95% confidence interval

# create 500 draws of scores for each year-location
set.seed(123)
drawDT <- data.table(t(apply(frac_df[,.(log_frac, log_SE)], 1,
                             FUN = function(x) rnorm(500, mean = x[1], sd = x[2]))))
frac_df = cbind(frac_df, drawDT) # add draws to original data

# remove unnecessary columns: 
frac_df[, c("frac_of_the", "frac_of_the_lower", "frac_of_the_upper", 
            "log_frac", "log_frac_lower", "log_frac_upper", "log_SE") := NULL]

# pivot draws to long format
frac_draws = melt(frac_df, id.vars = c("year", "country", "iso3", "expend_type"), 
                  variable.name = "draw")

# remove "V" from the draw column
frac_draws[, draw := as.numeric(gsub("V", "", draw))]

# exponentiate values to level space
frac_draws[, value := exp(value)]
# see range of draw values
range(frac_draws$value) # 3.010772e-06 - 540.1823

# see what percent of draws are outside the possible range now
nrow(frac_draws[value >= 1]) / nrow(frac_draws) # 4.25% of draws are >= 1

# reassign these values using the maximum upper value for each category
frac_draws[expend_type == "inpatient" & value >= 1, value := max_inpatient]
frac_draws[expend_type == "outpatient" & value >= 1, value := max_outpatient]
frac_draws[expend_type == "pharma" & value >= 1, value := max_pharma]

# pivot wider so each category has its own column
frac_wide = dcast(frac_draws, year + iso3 + draw ~ expend_type, 
                  value.var = "value")

# see range of draw estimates
range(frac_wide$inpatient) # 5.702423e-05 0.9999922
range(frac_wide$outpatient) # 9.025315e-05 - 0.9999903
range(frac_wide$pharma) # 3.010772e-06 - 0.9999997

# change column names to match GBD database
setnames(frac_wide, old = c("year", "iso3"), 
         new = c("year_id", "ihme_loc_id"))

# get location metadata to merge in location_id
source("FILEPATH/get_location_metadata.R")
locs <- get_location_metadata(location_set_id = 35, 
                              release_id = 9)  # GBD 2021
frac_wide = merge(frac_wide, locs[,.(ihme_loc_id, location_id, location_name)],
                  by = "ihme_loc_id")

# save out data
output_dir = "FILEPATH"
fwrite(frac_wide, file.path(output_dir, "NHA_expenditure_fraction_draws.csv"))

# save out number of years of data available for each country
df_N = frac_wide[year_id < 2022 & draw == 1,
                 .(N_years = .N), by = .(location_name, location_id)]
fwrite(df_N, file.path(output_dir, "NHA_expenditure_fractions_N.csv"))

