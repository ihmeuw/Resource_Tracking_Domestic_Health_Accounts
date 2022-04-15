# Clean environment
rm(list=ls())

# Libraries
library(data.table)

# Loads data
predictions <- fread("FILEPATH")

# Making custom_covariates_file
# medical_total_cost
pred1 <- predictions[living_conditions == "" , c('year_id', 'location_id', "exp_pred")]
setnames(pred1, old = "exp_pred", new = "cv_custom_stage_1")

# Adding missing variables
pred1[, age_group_id := 22]
pred1[, sex_id := 3]


write.csv(pred1,
          "FILEPATH",
          row.names = FALSE)

# Making raw data file
cmc_raw <- fread("FILEPATH")

cmc_raw2 <- cmc_raw[ihme_loc_id != "SGP", ] 
head(cmc_raw2)

cmc_raw1 <- cmc_raw2[clean_units == "dx_over_dem_cases" & year_id > 1994 & living_conditions == "", 
                    c('year_id', 'location_id', 'clean_value', 'sample_size')]

cmc_raw1[, .N, by = location_id]


setnames(cmc_raw1, old = 'clean_value',
         new = 'val')

#Adding the missing covariates
cmc_covs <- predictions[, c('year_id', 'location_id', "log_the", "sdi")]
setnames(cmc_covs, old = 'log_the', new = 'cv_log_the')
#setnames(cmc_covs, old = 'sdi', new = 'cv_sdi')

# Bringing it all together
cmc_data <- merge(cmc_raw1, cmc_covs, by = c('year_id', 'location_id'), all.y = TRUE)
cmc_data[, age_group_id := 22]
cmc_data[, sex_id := 3]
cmc_data[, measure := "continuous"]
cmc_data[, measure_id := 19]
cmc_data[, nid := 999999]
cmc_data[, is_outlier := 0]

cmc_data[is.na(sample_size), sample_size := 1000]

# calculating variance
cmc_data[!is.na(val), variance := var(val)]
com_variance <- unique(cmc_data$variance)[2]
print(com_variance)

# Setting variance
cmc_data[, variance := com_variance]

head(cmc_data)
cmc_data1 <-unique(cmc_data[!is.na(val), ])

head(cmc_data1)

write.csv(cmc_data1,
          "FILEPATH",
          row.names = FALSE)



