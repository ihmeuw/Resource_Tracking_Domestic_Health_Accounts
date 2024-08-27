# Test different model specifications for cost model
# Author: Elye Bliss
# Date: 07/19/2023

# Clear environment
rm(list = ls())

# Loading libraries
library(data.table)
library(ggplot2)
library(Hmisc)
library(corrplot)
library(caret)
library(tidyr)
library(dplyr)
library(RColorBrewer)
library(stargazer)
library(BAS, lib.loc = "FILEPATH")

options(scipen = 1000)

data <- fread("FILEPATH/04_merged_with_covariates.csv")

# Notes on data:
# - Dependent variable is spending_clean (log)
# - Potential covariates: ldi_pc, the_pc, gdp_pc (all logged), sdi (not logged):
# SDI: a summary measure that identifies where countries or other geographic areas
# sit on the spectrum of development. It is expressed on a scale of 0 to 100, with
# 0 being the lowest SDI value and 100 being the highest.

# logging cost variables
data[, lg_spending_clean := log(spending_clean)]
data[, lg_the_pc := log(the_pc)]
data[, lg_ldi_pc := log(ldi_pc)]
data[, lg_gdp_pc := log(gdp_pc)]

# - Controls: 
# care_type -what type of care care was costed (formal for institutional settings 
#       and informal for community and home based settings)? Possible values are 
#       "community", "institutional", "not reported", and "total"
# cost_type -what type of costs are reported? Possible values are "direct", and 
#       "total=d+i"
# cost_components -What sub categories of costs are available? Possible values 
#       are "medicall (all)", and "total
# - Potential random effect: ihme_loc_id (country)


######################## Correlation matrix ####################################

var_list <- c("spending_clean", "ldi_pc", "lg_ldi_pc","the", "the_pc","lg_the_pc", "sdi", "gdp_pc","lg_gdp_pc")

corrplot(cor(data[, var_list, with = F], method = "pearson", use = "complete.obs"))

rcorr(as.matrix(data[, var_list, with = F]))$P

# Notes: weak correlations between spending_clean and explanatory variables
# but seemingly strong p-values

correlationMatrix <- cor(data[, var_list, with = F])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff = 0.75)
# print indexes of highly correlated attributes
colnames(correlationMatrix)[highlyCorrelated] # "the_pc" "ldi_pc" "gdp_pc"

# To avoid multicollinearity, only one from the following should be chosen:
# "the_pc" "ldi_pc" "gdp_pc" (due to high correlation)
# test with and without sdi, since correlation is low


############ Replicate final specification from previous pipeline ##############

# Use the results to identify outliers with cook's distance, and remove before
# further model testing


# check correlations after log-transformation
cor(data[, .(lg_the_pc, lg_ldi_pc, lg_gdp_pc)]) # very highly correlated

# Note that previous script performs outlier removal at this point before moving on
max(data$spending_clean) # 4542072306, clearly either not USD or not method 2b

# log of 0 is negative infinity
data <- data[spending_clean > 0, ]

dim(data)
# dims of data before outlier removal: 1385 x 27
# compare to previous: 1171 x 11

## Filter data to only include cost types and components of previous pipeline ##

setnames(data,old='location_id.x',new='location_id')
setnames(data, old = "care_settings", new = "care_type")

dat <- data[spending_clean < 500000 & payer_type == 'all' & cost_type != "" & cost_type == 'direct', ]

dat[cost_components == 'care at nursing home' & (care_type == 'institutional') & location_id == 95, cost_components := 'total']

# Create side-by-side comparisons for the cost components 'community' and 
# 'institutional' to check for comm < fac inequality

# Select relevant rows and columns
compare_settings <- dat[cost_components=='total'&
                          care_type %in% c('community','institutional')& 
                          cost_type == 'direct',
                        .(year_estimate,ihme_loc_id,care_type,spending_clean)]

compare_settings[,id := .I]

# Convert from long to wide format to get side-by-side comparison:
compare_settings <- data.table::dcast(compare_settings,id+year_estimate+ihme_loc_id~care_type,value.var ='spending_clean')

fwrite(compare_settings, "FILEPATH/compare_settings_before_manual_removal.csv")

# labeling outliers that don't meet the comm < fac inequality
dat[year_estimate == 2004 & ihme_loc_id == 'KOR' & cost_components == 'total' & care_type == 'community', outlier := 1]
dat[year_estimate == 2014 & ihme_loc_id == 'JPN' & cost_components == 'total', outlier := 1 ]
dat[year_estimate == 2009 & ihme_loc_id == 'FIN' & cost_components == 'total', outlier := 1 ]
dat[year_estimate == 2006 & ihme_loc_id == 'GBR' & cost_components == 'total', outlier := 1 ]
dat[year_estimate == 2010 & ihme_loc_id == 'TWN' & cost_components == 'total', outlier := 1 ]
dat[year_estimate == 2000 & ihme_loc_id == 'CHN' & cost_components == 'total' & care_type == 'institutional' & spending_clean < 1000, outlier := 1]
dat[year_estimate == 2008 & ihme_loc_id == 'DEU' & cost_components == 'total' & care_type == 'community' & spending_clean > 50000, outlier := 1]
dat[year_estimate == 2001 & ihme_loc_id == 'ESP' & cost_components == 'total' & care_type == 'community' & spending_clean > 40000,  outlier := 1 ]
dat[year_estimate == 2012 & ihme_loc_id == 'NLD' & cost_components == 'total' & care_type == 'institutional' & spending_clean > 100000, outlier := 1 ]
dat[year_estimate == 2005 & ihme_loc_id == 'DEU' & cost_components == 'total' & care_type == 'institutional', outlier := 1]

# making small df for outliers
# Creating outlier data.table to save for reference
outliers_dt <- data.table(matrix(nrow = 0, ncol = length(colnames(dat))))
colnames(outliers_dt) <- colnames(dat)
outliers_dt <- rbind(outliers_dt,dat[outlier == 1, ])
outliers_dt$outlier <- NULL

# Continue with analysis
dat <- dat[is.na(outlier), ]

# Check which observations get removed after manual removal process
compare_settings <- dat[cost_components=='total'&
                          care_type %in% c('community','institutional')& 
                          cost_type == 'direct',
                        .(year_estimate,ihme_loc_id,care_type,spending_clean)]
compare_settings[,id := .I]
compare_settings <- data.table::dcast(compare_settings,id+year_estimate+ihme_loc_id~care_type,value.var ='spending_clean')
fwrite(compare_settings, "FILEPATH/compare_settings_after_manual_removal.csv")
# Note: there are still some observations here where community costs are 
# greater than facility

############# Run prelim model to identify further outliers ####################

dat$outlier <- NULL

# Fixed effects model for THE - This is previous pipeline's specification
model0 <- lm(
  lg_spending_clean ~ lg_the_pc + as.factor(care_type) +
    as.factor(cost_components) ,
  data = dat
) 

# Cook's distance
plot(model0, 4)
# Residuals vs Leverage
plot(model0, 5)

dat$cooksd <- cooks.distance(model0)

is_outlier <- function(x, y) {
  if (x > y) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# use 4/n as cook's distance cutoff
dat$is_outlier <- sapply(dat$cooksd, FUN = is_outlier, y = 4 / nrow(dat))

# Add to outlier_dt
add_outliers <- dat[is_outlier == T, ]
add_outliers <- add_outliers[,is_outlier:= NULL]
add_outliers <- add_outliers[,cooksd:= NULL]
outliers_dt <- rbind(outliers_dt,add_outliers)
# No outliers are from newly-extracted (11_02) data

outlier_formatted <- outliers_dt[,.(location_name,year_estimate,
                                    care_type,spending_clean)]
setnames(outlier_formatted,old=c('location_name','year_estimate',
                                 'care_type','spending_clean'),
         new = c('Country','Year','Care setting','Value'))

outlier_formatted[,Currency := '2019 USD']

# Save outlier_dt for future reference
fwrite(outliers_dt, "FILEPATH/removed_outliers.csv")
fwrite(outlier_formatted, "FILEPATH/outlier_formatted.csv")


dat <- dat[is_outlier == F, ]

# Check again if any observations that violate the community < facility
# inequality
compare_settings <- dat[cost_components=='total'&
                          care_type %in% c('community','institutional')& 
                          cost_type == 'direct',
                        .(year_estimate,ihme_loc_id,care_type,spending_clean)]
compare_settings[,id := .I]
compare_settings <- data.table::dcast(compare_settings,id+year_estimate+ihme_loc_id~care_type,value.var ='spending_clean')
fwrite(compare_settings, "FILEPATH/compare_settings_after_cooks_removal.csv")

# fixed effects on cost type, care settings, cost components
dat[, ihme_loc_id := as.factor(ihme_loc_id)]
dat[, payer_type := as.factor(payer_type)]

# Factor control variables
dat[, care_type := as.factor(care_type)]

dat[, cost_components := as.factor(cost_components)]
dat[, cost_type := as.factor(cost_type)]

# rerun model and check effects:
model1 <- lm(
  lg_spending_clean ~ lg_the_pc + care_type +
    cost_components ,
  data = dat
)

# Cook's distance
plot(model1, 4) # looks better
# Residuals vs Leverage
plot(model1, 5)


########################### test other specifications ##########################

# Perform manual k-fold CV (k=10), on non-stratified splits
all_results <- data.table(
  model = character(),
  MSE = double(),
  RMSE = double(),
  MAE = double(),
  fold = integer()
)

# randomize n
dat[,n:=1:nrow(dat)]
n_rand <- sample(dat$n, length(dat$n))
flds <- createFolds(n_rand, k = 10, list = TRUE, returnTrain = FALSE)

# Loop over k-folds (not sure if there's an easy way to avoid a loop here)
for (i in 1:length(flds)) {
  # split into train and test
  test <- dat[flds[[i]], ]
  train <- anti_join(dat, test)

  # Using THE as main covar
  model1 <- lm(
    lg_spending_clean ~ lg_the_pc + cost_components +
      care_type,
    data = train
  )
  model2 <- lm(
    lg_spending_clean ~ lg_the_pc + sdi + cost_components +
      care_type,
    data = train
  )
  model3 <- lme4::lmer(lg_spending_clean ~ lg_the_pc + cost_components +
    care_type + (1 | ihme_loc_id), data = train)
  model4 <- lme4::lmer(lg_spending_clean ~ lg_the_pc + sdi + cost_components +
    care_type + (1 | ihme_loc_id), data = train)

  # Using LDI as main covar
  model5 <- lm(
    lg_spending_clean ~ lg_ldi_pc + cost_components +
      care_type,
    data = train
  )
  model6 <- lm(
    lg_spending_clean ~ lg_ldi_pc + sdi + cost_components +
      care_type,
    data = train
  )
  model7 <- lme4::lmer(lg_spending_clean ~ lg_ldi_pc + cost_components +
    care_type + (1 | ihme_loc_id), data = train)
  model8 <- lme4::lmer(lg_spending_clean ~ lg_ldi_pc + sdi + cost_components +
    care_type + (1 | ihme_loc_id), data = train)

  # Using GDP as main covar
  model9 <- lm(
    lg_spending_clean ~ lg_gdp_pc + cost_components +
      care_type,
    data = train
  )
  model10 <- lm(
    lg_spending_clean ~ lg_gdp_pc + sdi + cost_components +
      care_type,
    data = train
  )
  model11 <- lme4::lmer(lg_spending_clean ~ lg_gdp_pc + cost_components +
    care_type + (1 | ihme_loc_id), data = train)
  model12 <- lme4::lmer(lg_spending_clean ~ lg_gdp_pc + sdi + cost_components +
    care_type + (1 | ihme_loc_id), data = train)

  # Examine with SDI only
  model13 <- lme4::lmer(lg_spending_clean ~ sdi + cost_components +
                          care_type + (1 | ihme_loc_id), data = train)
  
  test$model1_pred <- predict(model1, test)
  test$model2_pred <- predict(model2, test)
  # Specify allow.new.levels=Tto predict random effects variable on any unseen categories
  test$model3_pred <- predict(model3, test, allow.new.levels=T) 
  test$model4_pred <- predict(model4, test, allow.new.levels=T)
  test$model5_pred <- predict(model5, test)
  test$model6_pred <- predict(model6, test)
  test$model7_pred <- predict(model7, test, allow.new.levels=T)
  test$model8_pred <- predict(model8, test, allow.new.levels=T)
  test$model9_pred <- predict(model9, test)
  test$model10_pred <- predict(model10, test)
  test$model11_pred <- predict(model11, test, allow.new.levels=T)
  test$model12_pred <- predict(model12, test, allow.new.levels=T)
  test$model13_pred <- predict(model13, test, allow.new.levels=T)

  # Summarize results 
  results <- data.table(test)[,.(lg_spending_clean, model1_pred, model2_pred, 
                                 model3_pred, model4_pred,model5_pred, model6_pred, 
                                 model7_pred, model8_pred, model9_pred, model10_pred,
                                 model11_pred, model12_pred,model13_pred)]
  
  results <- melt(results,id.vars='lg_spending_clean')
  setDT(results)
  setnames(results,old=c('variable','value'),new=c('model','predictions'))
  results <- results[,.(MSE = mean((predictions - lg_spending_clean)^2),
                        RMSE = sqrt(mean((predictions - lg_spending_clean)^2)),
                        MAE = mean(abs(predictions - lg_spending_clean))),
                     by=model]

  results$fold <- i
  all_results <- rbind(all_results, results)
}

# Take average of k results:
manual_CV_results <- all_results[, .(
  avg_MSE = mean(MSE),
  avg_RMSE = mean(RMSE),
  avg_MAE = mean(MAE)
), by = model]



manual_CV_results[model=='model1_pred',specification := 'lg_spending_clean ~ lg_the_pc + cost_components + care_type']
manual_CV_results[model=='model2_pred',specification := 'lg_spending_clean ~ lg_the_pc + sdi + cost_components + care_type']
manual_CV_results[model=='model3_pred',specification := 'lg_spending_clean ~ lg_the_pc + cost_components + care_type + (1 | ihme_loc_id)']
manual_CV_results[model=='model4_pred',specification := 'lg_spending_clean ~ lg_the_pc + sdi + cost_components + care_type + (1 | ihme_loc_id)']
manual_CV_results[model=='model5_pred',specification := 'lg_spending_clean ~ lg_ldi_pc + cost_components + care_type']
manual_CV_results[model=='model6_pred',specification := 'lg_spending_clean ~ lg_ldi_pc + sdi + cost_components + care_type']
manual_CV_results[model=='model7_pred',specification := 'lg_spending_clean ~ lg_ldi_pc + cost_components + care_type + (1 | ihme_loc_id)']
manual_CV_results[model=='model8_pred',specification := 'lg_spending_clean ~ lg_ldi_pc + sdi + cost_components + care_type + (1 | ihme_loc_id)']
manual_CV_results[model=='model9_pred',specification := 'lg_spending_clean ~ lg_gdp_pc + cost_components + care_type']
manual_CV_results[model=='model10_pred',specification := 'lg_spending_clean ~ lg_gdp_pc + sdi + cost_components + care_type']
manual_CV_results[model=='model11_pred',specification := 'lg_spending_clean ~ lg_gdp_pc + cost_components + care_type + (1 | ihme_loc_id)']
manual_CV_results[model=='model12_pred',specification := 'lg_spending_clean ~ lg_gdp_pc + sdi + cost_components + care_type + (1 | ihme_loc_id)']
manual_CV_results[model=='model13_pred',specification := 'lg_spending_clean ~ sdi + cost_components + care_type + (1 | ihme_loc_id)']

manual_CV_results <- manual_CV_results[order(avg_RMSE, decreasing = FALSE), ]

# Answer -> model 12 performs the best in terms of average RMSE
fwrite(manual_CV_results, "FILEPATH/13-way_manual_CV.csv")

#####################  Interpret the models and coefficients ###################

# After outliers removed:
sg <- stargazer(model1,
  model2,
  model3,
  model4,
  model5,
  model6,
  model7,
  model8,
  model9,
  model10,
  model11,
  model12,
  title = "Regression Results", type = "text"
) #

# Key takeaways:
# - the care settings and cost components are pretty consistent in significance
#   and direction across model specifications
# - whichever main covar is used, the result is positive and significant
#   (roughly 1% increase in the/ldi/gdp leads to 1% increase in spending)
# - sdi is only significant in model 8 (along with lg_ldi_pc and location random
#   effects), where it is positive


# Question: are the results similar on the full data?
# Using THE as main covar
model1_f <- lm(lg_spending_clean ~ lg_the_pc +  cost_components + care_type,
  data = dat
)
model2_f <- lm(lg_spending_clean ~ lg_the_pc + sdi + cost_components + care_type, data = dat)
model3_f <- lme4::lmer(lg_spending_clean ~ lg_the_pc +  cost_components + care_type + (1 | ihme_loc_id), data = dat)
model4_f <- lme4::lmer(lg_spending_clean ~ lg_the_pc + sdi +  cost_components + care_type + (1 | ihme_loc_id), data = dat)

# Using LDI as main covar
model5_f <- lm(lg_spending_clean ~ lg_ldi_pc + cost_components + care_type, data = dat)
model6_f <- lm(lg_spending_clean ~ lg_ldi_pc + sdi + cost_components + care_type, data = dat)
model7_f <- lme4::lmer(lg_spending_clean ~ lg_ldi_pc + cost_components + care_type + (1 | ihme_loc_id), data = dat)
model8_f <- lme4::lmer(lg_spending_clean ~ lg_ldi_pc + sdi + cost_components + care_type + (1 | ihme_loc_id), data = dat)

# Using GDP as main covar
model9_f <- lm(lg_spending_clean ~ lg_gdp_pc + cost_components + care_type,
  data = dat
)
model10_f <- lm(lg_spending_clean ~ lg_gdp_pc + sdi + cost_components + care_type, data = dat)
model11_f <- lme4::lmer(lg_spending_clean ~ lg_gdp_pc + cost_components + care_type + (1 | ihme_loc_id), data = dat)
model12_f <- lme4::lmer(lg_spending_clean ~ lg_gdp_pc + sdi+ cost_components + care_type + (1 | ihme_loc_id), data = dat)
model13_f <- lme4::lmer(lg_spending_clean ~ sdi+ cost_components + care_type + (1 | ihme_loc_id), data = dat)

sg_f <- stargazer(model1_f,
  model2_f,
  model3_f,
  model4_f,
  model5_f,
  model6_f,
  model7_f,
  model8_f,
  model9_f,
  model10_f,
  model11_f,
  model12_f,
  model13_f,
  title = "Regression Results", type = "text"
)

stargazer(model12_f,
          title = "Regression Results", type = "text")

# Note: no substantial differences between the two sets of results.

######################## Checking for heteroscedasticity #######################


dev.off()
pdf("FILEPATH/model1_heteroscedasticity.pdf")
par(mfrow = c(3, 2))
plot(model1_f)
# First visualize the residuals
res <- resid(model1_f)
# Create density plot of residuals
plot(density(res)) # mean slightly skewed to right
# look at residuals against actually observations
plot(dat$lg_spending_clean, res) # Note: Look for "cone"-looking shape
dev.off()
# Note: residuals stray (slightly) at the tails

############################ Visualization #####################################

pdf("FILEPATH/plot_by_care_type.pdf")
ggplot(dat, aes(x = lg_the_pc, y = lg_spending_clean)) +
  geom_point() +
  geom_smooth(method = "lm", fullrange = T) +
  facet_wrap(~care_type) +
  labs(
    title = "Spending on THE_pc by care type",
    x = "Log THE per capita (2019 USD)",
    y = "Log Dementia spending per treated case (2019 USD)"
  )
dev.off()

pdf("FILEPATH/plot_by_cost_type.pdf")
ggplot(dat, aes(x = lg_the_pc, y = lg_spending_clean)) +
  geom_point() +
  geom_smooth(method = "lm", fullrange = T) +
  facet_wrap(~cost_type) +
  labs(
    title = "Spending on THE_pc by cost type",
    x = "Log THE per capita (2019 USD)",
    y = "Log Dementia spending per treated case (2019 USD)"
  )
dev.off()

pdf("FILEPATH/plot_by_cost_component.pdf")
ggplot(dat, aes(x = lg_the_pc, y = lg_spending_clean)) +
  geom_point() +
  geom_smooth(method = "lm", fullrange = T) +
  facet_wrap(~cost_components) +
  labs(
    title = "Spending on THE_pc by cost component",
    x = "Log THE per capita (2019 USD)",
    y = "Log Dementia spending per treated case (2019 USD)"
  )
dev.off()

############# Save best-performing model, fit on full data set #################

# Use location_id instead of ihme_loc_id as ST-GPR input format
model11_f <- lme4::lmer(lg_spending_clean ~ lg_gdp_pc + cost_components + care_type + (1 | location_id), data = dat)


# Save final filtered data used for model 
fwrite(dat, "FILEPATH/final_data_for_model_training.csv")

# saving the model
saveRDS(model11_f, "FILEPATH/stage1_cost_model.rda")
