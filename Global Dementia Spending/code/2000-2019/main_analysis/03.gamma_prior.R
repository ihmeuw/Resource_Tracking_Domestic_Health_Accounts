###########################################################
# This files runs the regression and makes predictions to 
# run ST-GPR
##########################################################
# Clean environment
rm(list=ls())

library(data.table)

# Load raw data

root <- "FILEPATH"
cs <- fread(paste0(root, "FILEPATH"))

cs_d <- cs[!is.na(data), c('location_id', 'year_id', 'data', 
                           'sex_id', 'age_group_id', 'measure', 'nid', 
                           'me_name', 'is_outlier', 'sample_size', 'variance')]

# Loading covariates
cov <- fread("FILEPATH")

cov_d <- cov[, c('year_id', 'location_id', 'sdi')]

# Bringing the two datasets together
reg_data <- merge(cs_d, cov_d)
reg_data[, log_data := log(data)]

# Runs regression
fit <- lm(log_data ~ sdi, data = reg_data)
summary(fit)

# Removing outliers
cd1 <- cooks.distance(fit)
plot(cd1)

influential <- as.numeric(names(cd1)[(cd1 > (4/dim(reg_data)[1]))])
reg_data[influential, data  := NA]
reg_data1 <- reg_data[!is.na(data),]

# Runs regression wihtout outliers
fit1 <- lm(log_data ~ sdi, data = reg_data1)
summary(fit1)

# Making predictions
cov_d[, log_predictions := predict(object = fit1, cov_d)]
cov_d[, cv_custom_stage_1 := exp(log_predictions)]
cov_d[, age_group_id := 22]
cov_d[, sex_id := 3]

cov_d[, log_predictions := NULL]
cov_d[, sdi := NULL]

# saving custom stage 1 data
write.csv(cov_d, 
          paste0(root, "FILEPATH"), 
          row.names = F)

reg_data1[, variance := NULL]
reg_data1[, log_data := NULL]



# Making raw data file
cs_data <- merge(cov_d, 
          reg_data1, 
          by = c('year_id', 'location_id', 'age_group_id', 'sex_id'), 
          all = T)

cs_data[, measure := "proportion"]
cs_data[, nid := 999999]
cs_data[, me_name := "AD_care_settings"]
cs_data[, is_outlier := 0]
cs_data[, sample_size := 1000]
#cs_data[, sex_id := 3]

variance <- cs_data[!is.na(data), var(data)]
variance

cs_data[, variance := variance]
cs_data[, cv_custom_stage_1 := NULL]

setnames(cs_data, old = 'sdi', new = 'cv_sdi')
setnames(cs_data, old = 'data', new = 'val')

head(cs_data)

cs_data1 <- cs_data[!is.na(val), ]

write.csv(cs_data1, 
          paste0(root, "FILEPATH"), 
          row.names = F)