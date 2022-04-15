################################################################################
## These models have very little data. We are trying a different modeling metho-
## dology.
##
## 1. Take the ratio of cost_tot_facility(severity)/cost_tot_facility(all)
## 2. run stgpr for all severity 
## 3. Multiply 1 by 2
##
## Theis script calculates the ratio in the first step
################################################################################




# Clean environment
rm(list=ls())

# Libraries
library(data.table)


# Loads data
# Predictions
predictions <- fread("FILEPATH")
location <- fread("FILEPATH")
head(predictions)

# Location
locations <- location[level == 3, .(ihme_loc_id, location_id, location_name)]


# Data manipulation
pred1 <- predictions[, c('location_id', 'year_id', 'severity1', 'ihme_loc_id', 'location_name', 'model', 'pred_tot')]

# merging with location to drop extra locations

pred2 <- merge(pred1, locations, by = c('location_id', 'ihme_loc_id', 'location_name'))


#Chaning model names
pred2[severity1 == "0. all", severity1 := "all"]
pred2[severity1 == "1. low", severity1 := "low"]
pred2[severity1 == "2. moderate", severity1 := "moderate"]
pred2[severity1 == "3. severe", severity1 := "severe"]


# transforming from long to wide so it's easier to calculate the ratios 

pred_wide <- dcast(pred2, 
             location_id + ihme_loc_id + location_name + year_id + model ~ severity1, 
             value.var = "pred_tot")

# calculating the ratios
pred_wide <- as.data.table(pred_wide)
head(pred_wide)

pred_wide[, low_r := low/all]
pred_wide[, mod_r := moderate/all]
pred_wide[, sev_r := severe/all]


# subsetting and saving

MODEL1 <- "com_tot"
com <- pred_wide[model == MODEL1, ]

write.csv(com, "FILEPATH", 
          row.names = F)


MODEL2 <- "fac_tot"
fac <- pred_wide[model == MODEL2, ]

write.csv(fac, "FILEPATH", 
          row.names = F)


