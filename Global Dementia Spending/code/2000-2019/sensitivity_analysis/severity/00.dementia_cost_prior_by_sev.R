# Clean working directory
rm(list = ls())
library(data.table)
# Comparing the two dfs to see what are the differences

cd <- fread("FILEPATH")
cd <- cd[clean_units == "dx_over_dem_cases", c('location_id', 'year_id', 'ihme_loc_id', 'severity', 'living_conditions','clean_value', 'clean_units') ]

# Removing severity 
cd_aa <- cd[year_id > 1994,]
cd_a <- cd_aa[ihme_loc_id != "SGP", ]
cd_a[, .N, by = ihme_loc_id]

the <- fread("FILEPATH")
the[, log_the := log(THE_pc)] # log transforming THE


# Bringing everything together
cd_a <- merge(cd_a, the, by = c('location_id', 'year_id'))
# log transforming the dependent variable
cd_a[, lncl := log(clean_value)]



set.seed(212)
ind <- sample(2, nrow(cd_a), replace = TRUE, prob = c(0.8, 0.2))


tdata <- cd_a[ind ==1, ] # training dataset
vdata <- cd_a[ind ==2, ] # valdiation dataset

# Running the model on training data
tfit <-  lm(lncl ~  log_the + as.factor(living_conditions) + as.factor(severity),
            data = cd_a)

summary(tfit)

#making predictions

vdata[, predictions := predict(tfit, vdata)]

vdata[, pred_nu := exp(predictions)]


###
## Out of sample validation
###

library(caret)
# values in natural units
RMSE(obs = vdata$clean_value,
     pred = vdata$pred_nu)

#MAE
MAE(vdata$pred_nu, vdata$clean_value)

#R2
R2(vdata$pred_nu, vdata$clean_value)

# Predictor Error lncl
RMSE(vdata$pred_nu, vdata$clean_value)/mean(vdata$clean_value)


#########
#
#  Looking for outliers
# 
##############

cd_a[, cooksd := cooks.distance(tfit)]
cd_a[, cut_off := mean(cooksd) * 4]
cd_a[cooksd > 0.1, ]
plot(cooks.distance(lm(lncl ~  log_the + as.factor(living_conditions) + as.factor(severity),
              data = cd_a)))


#### removing the outliers, updating fit, and 
#### re-running the regression and making predictions
test <- cd_a[cooksd < 0.5, ]

# to updated the raw data to run st-gpr
#fwrite(test, "FILEPATH")

tfit <-  lm(lncl ~  log_the + as.factor(living_conditions) + as.factor(severity),
            data = test)

summary(tfit)


# I need to duplicate the data set to predict for nstitution by severity

the_i <- copy(the)
the_i[, living_conditions := 'institution']

the_i_mild <- copy(the_i)
the_i_mild[, severity := 'mild']

the_i_mode <- copy(the_i)
the_i_mode[, severity := 'moderate']

the_i_seve <- copy(the_i)
the_i_seve[, severity := 'severe']

the_i_null <- copy(the_i)
the_i_null[, severity := '']


the.1 <- rbind(the_i_null, the_i_mild, the_i_mode, the_i_seve)

### Making predictions for 195 country from 1990 to 2016

the.1[, log_pred := predict(tfit, the.1)]
the.1[, exp_pred := exp(log_pred)]


# write.csv(the.1,
#           "FILEPATH",
#           row.names = F)
