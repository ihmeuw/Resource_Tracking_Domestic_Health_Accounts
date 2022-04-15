#
# Looking at a mixed effects modeling approach
# fixed effects on cost type, care settings, cost components
#


rm(list = ls())

library(data.table)
library(ggpubr)
library(ggplot2)
library(lme4)
library(predictmeans)
library(influence.ME)

data <- fread("FILEPATH")
head(data)
loc <- fread("FILEPATH")
locs <- loc[, c('ihme_loc_id', 'location_id', 'super_region_id', 'super_region_name','region_id', 'region_name')]

setnames(data, old = 'care_type', new = 'care_settings')
data[, care_settings := as.factor(care_settings)]
data[, cost_components := as.factor(cost_components)]
data[, cost_type := as.factor(cost_type)]


# loging cost variables
data[, lg_spending_clean := log(spending_clean)]
data[, lg_the_pc := log(the_pc)]

data[payer_type == "" | payer_type == 'all', payer_type := 'all' ]


# data cleaning
# Labeling outliers
dat <- data[spending_clean < 500000 & payer_type == 'all' & cost_type != "" & cost_type == 'direct', ]
# labeling outliers that don't meet the comm < fac inequality
dat[year_estimate == 2004 & ihme_loc_id == 'KOR' & cost_components == 'total' & care_settings == 'community', outlier := 1]
dat[year_estimate == 2014 & ihme_loc_id == 'JPN' & cost_components == 'total', outlier := 1 ]
dat[year_estimate == 2009 & ihme_loc_id == 'FIN' & cost_components == 'total', outlier := 1 ]
dat[year_estimate == 2006 & ihme_loc_id == 'GBR' & cost_components == 'total', outlier := 1 ]
dat[year_estimate == 2010 & ihme_loc_id == 'TWN' & cost_components == 'total', outlier := 1 ]
dat[year_estimate == 2000 & ihme_loc_id == 'CHN' & cost_components == 'total' & care_settings == 'institutional' & spending_clean < 1000, outlier := 1]
dat[year_estimate == 2008 & ihme_loc_id == 'DEU' & cost_components == 'total' & care_settings == 'community' & spending_clean > 50000, outlier := 1]
dat[year_estimate == 2001 & ihme_loc_id == 'ESP' & cost_components == 'total' & care_settings == 'community' & spending_clean > 40000,  outlier := 1 ]
dat[year_estimate == 2012 & ihme_loc_id == 'NLD' & cost_components == 'total' & care_settings == 'institutional' & spending_clean > 100000, outlier := 1 ]
dat[year_estimate == 2005 & ihme_loc_id == 'DEU' & cost_components == 'total' & care_settings == 'institutional', outlier := 1]

# making small df for outliers
outlier <- dat[outlier == 1, ]

## 
# Continue with analysis
dat <- dat[is.na(outlier), ]

dat <- merge(locs, dat, by = c('location_id', 'ihme_loc_id'))
dat[, ihme_loc_id := as.factor(ihme_loc_id)]
dat[, super_region_name := as.factor(super_region_name)]

summary(dat$year_estimate)
dat[, .N, by = ihme_loc_id]
dat[, .N, by = care_settings]
dat[, .N, by = cost_components]
dat[, .N, by = cost_type]
dat[, .N, by = payer_type]

# cleaning data
dat[cost_components == 'care at nursing home' & (care_settings == 'institutional') & location_id == 95, cost_components := 'total']

###
#
# THE as covariate
#
###

# Fixed effects model for THE
model1 <- lm(lg_spending_clean ~ lg_the_pc + care_settings  + 
                 cost_components, 
             data = dat)

# print the model results
summary(model1)

### Running Cook's ditance to label outliers

dat[, cd := cooks.distance(model1)] ## run the cook's distance function

## Plotting cook's distance
plot(cooks.distance(model1))

summary(dat$cd)

cutoff <- 4/dim(dat)[1]
cutoff

# removing outliers

test <- dat[cd < cutoff, ]

# Fixed effects model 
# Fixed effects model for THE

t <- lm(lg_spending_clean ~ lg_the_pc + care_settings +
            cost_components, data = test)
summary(t)


##### Using the models to make predictions
# loading dataset to make predictions
covs <- fread("FILEPATH")
cov <- covs[, c('location_id', 'year','the_pc')]
cov <- merge(cov, locs, by = c('location_id'))
# creating variables needed for predictions
cov[, lg_the_pc := log(the_pc)]

cov[, cost_components := 'total']
cov[, cost_type := 'direct']
cov[, payer_type := 'all']

cs1 <- copy(cov)
cs2 <- copy(cov)
cs3 <- copy(cov)
cs4 <- copy(cov)
cs5 <- copy(cov)
cs6 <- copy(cov)
cs7 <- copy(cov)
cs8 <- copy(cov)

cs1[, cost_components := "medical (all)"]
cs2[, cost_components := "outpatient"]
cs3[, cost_components := "care at home"]
cs4[, cost_components := "drugs"]
cs5[, cost_components := "inpatient"]
cs6[, cost_components := "care at nursing home"]
cs7[, cost_components := "others"]
cs8[, cost_components := "caregiver wages"]

cc <- rbind(cov, cs1, cs2, cs3, cs4, cs5, cs6, cs7, cs8)

# creating copies of the df for care settings
com <- copy(cc)
com[, care_settings := 'community']
fac <- copy(cc)
fac[, care_settings := 'institutional']
nr <- copy(cc)
nr[, care_settings := 'not reported']

cs <- rbind(com, fac, nr)

# Making predictions using the as covariate
cs[, lg_cost_THE := predict(object = t, cs, allow.new.levels = T)]
cs[, pred_cost_THE := exp(lg_cost_THE)]

# adding raw data to predictions
tp <- test[, c('ihme_loc_id', 'location_id', 'year_estimate', 'spending_clean', 'cost_type', 'care_settings', 'cost_components', 'sample_size')]
eve <- merge(cs, tp, 
             by.x = c('location_id', 'ihme_loc_id', 'year', 'cost_components', 'cost_type', 'care_settings'), 
             by.y = c('location_id', 'ihme_loc_id','year_estimate', 'cost_components', 'cost_type', 'care_settings'), all.x = T )

total <- eve[cost_components == 'total' & care_settings != 'not reported', ]
# saving file with preditions
fwrite(total, 
       "FILEPATH", 
       row.names = F)
