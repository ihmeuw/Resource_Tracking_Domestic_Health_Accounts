################################################################################
# Plots another scatter plot of LIS data fit vs ILO ST-GPR estimates as a sanity
# check, and then fits a linear model in preparation for ST-GPR send-off round 2.
# Author: Elye Bliss
# Date: Aug 28, 2023
################################################################################

rm(list = ls())

library(data.table)
library(ggplot2)
library(stargazer)
library(lme4)
library(influence.ME,lib.loc = "FILEPATH")
library(caret)

LIS <- fread('FILEPATH/LIS_age_with_ILO_stgpr_updated_10_27.csv')

############################# Explore data #####################################

# Check correlation
cor(LIS$LIS_est,LIS$ILO_est)
# [1] 0.7428606 Was 0.8332996 with previous measure of income.

# Which age groups are most/least correlated?
correlations <- LIS[,correlation := cor(LIS_est,ILO_est),by='age_group_name']
correlations <- unique(correlations[,.(age_group_name,correlation)])

output_dir = "FILEPATH"
pdf(paste0(output_dir,"LIS_vs_ILO_corr_by_age_updated_10_27.pdf")) 

ggplot(correlations,aes(x=age_group_name,y=correlation)) +
  geom_point()+
  theme_minimal()+
  labs(title="Correlation of LIS personal income with ILO wage data by age group",
       x="Age group",
       y="Correlation with ILO wages",
       color=NULL)+
  theme(axis.text.x = element_text(angle = 90))

dev.off()
# Answer -> graph shows high correlation for adult, working-age age groups

############################# Fit linear model #################################

# log-transform data
LIS[,lg_LIS := log(LIS_est)]
LIS[,lg_ILO := log(ILO_est)]

# Distribution of observations by age group. There seem to be many more obs
# in the updated data.
table(LIS$age_group_name) 
# 15 to 19 20 to 24 25 to 29 30 to 34 35 to 39 40 to 44 45 to 49 50 to 54 55 to 59 
# 1286     1250     1292     1292     1292     1292     1292     1292     1292  
# 60 to 64 65 to 69 70 to 74 75 to 79 80 plus 
# 1292     1292     1280     1213     1136 

# First explore fixed effects model
model1 <- lm(
  lg_LIS ~ lg_ILO + as.factor(sex_id) + as.factor(age_group_name),
  data=LIS
)

summary(model1)


model2 <- lme4::lmer(lg_LIS ~ lg_ILO + as.factor(sex_id) + (1|ihme_loc_id/age_group_name), 
                     data = LIS)

model3 <- lme4::lmer(lg_LIS ~ lg_ILO + as.factor(sex_id) + (1|age_group_name/ihme_loc_id), 
                     data = LIS)

# Models 4 and 5 are the revised versions of 2 and 3, but with random 
# coefficients on lg_ILO.
model4 <- lme4::lmer(lg_LIS ~ lg_ILO + as.factor(sex_id) + (1+lg_ILO|ihme_loc_id/age_group_name), 
                     data = LIS,REML = FALSE)

model5 <- lme4::lmer(lg_LIS ~ lg_ILO + as.factor(sex_id) + (1+lg_ILO|age_group_name/ihme_loc_id), 
                     data = LIS,REML = FALSE)

# Test all 5 specifications with self-constructed train-test k=10 cross-validation
all_results <- data.table(
  model = character(),
  MSE = double(),
  RMSE = double(),
  MAE = double(),
  fold = integer()
)
# randomize index_col
n_rand <- sample(LIS$index_col, length(LIS$index_col))
flds <- createFolds(n_rand, k = 10, list = TRUE, returnTrain = FALSE)

# Loop over k-folds (not sure if there's an easy way to avoid a loop here)
for (i in 1:length(flds)) {
  # split into train and test
  test <- LIS[flds[[i]], ]
  train <- anti_join(LIS, test)
  
  model1 <- lm(
    lg_LIS ~ lg_ILO + as.factor(sex_id) + as.factor(age_group_name),
    data=train
  )
  
  model2 <- lme4::lmer(lg_LIS ~ lg_ILO + as.factor(sex_id) + (1|ihme_loc_id/age_group_name), 
                       data = train)

  model3 <- lme4::lmer(lg_LIS ~ lg_ILO + as.factor(sex_id) + (1|age_group_name/ihme_loc_id), 
                       data = train)
  
  model4 <- lme4::lmer(lg_LIS ~ lg_ILO + as.factor(sex_id) + (1+lg_ILO|ihme_loc_id/age_group_name), 
                       data = train,REML = FALSE)
  
  model5 <- lme4::lmer(lg_LIS ~ lg_ILO + as.factor(sex_id) + (1+lg_ILO|age_group_name/ihme_loc_id), 
                       data = train,REML = FALSE)
  
  # Changing paramater from re.form = ~0 to allow.new.levels = TRUE
  test$model1_pred <- predict(model1, test) 
  test$model2_pred <- predict(model2, test,allow.new.levels = TRUE) 
  test$model3_pred <- predict(model3, test,allow.new.levels = TRUE) 
  test$model4_pred <- predict(model4, test,allow.new.levels = TRUE) 
  test$model5_pred <- predict(model5, test,allow.new.levels = TRUE) 
  
  # Summarize results 
  results <- data.table(test)[,.(lg_LIS, model1_pred, model2_pred,model3_pred,
                                 model4_pred, model5_pred)]
  
  results <- melt(results,id.vars='lg_LIS')
  setnames(results,old=c('variable','value'),new=c('model','predictions'))
  setDT(results)
  results <- results[,.(MSE = mean((predictions - lg_LIS)^2),
                        RMSE = sqrt(mean((predictions - lg_LIS)^2)),
                        MAE = mean(abs(predictions - lg_LIS))),
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

manual_CV_results <- manual_CV_results[order(avg_RMSE, decreasing = FALSE), ]

setDT(manual_CV_results)
manual_CV_results[model=='model1_pred',spec:= paste0('lg_LIS ~ lg_ILO + as.factor(sex_id) + as.factor(age_group_name)')]
manual_CV_results[model=='model2_pred',spec:= paste0('lg_LIS ~ lg_ILO + as.factor(sex_id) + (1|ihme_loc_id/age_group_name)')]
manual_CV_results[model=='model3_pred',spec:= paste0('lg_LIS ~ lg_ILO + as.factor(sex_id) + (1|age_group_name/ihme_loc_id)')]
manual_CV_results[model=='model4_pred',spec:= paste0('lg_LIS ~ lg_ILO + as.factor(sex_id) + (1+lg_ILO|ihme_loc_id/age_group_name)')]
manual_CV_results[model=='model5_pred',spec:= paste0('lg_LIS ~ lg_ILO + as.factor(sex_id) + (1+lg_ILO|age_group_name/ihme_loc_id)')]

# Answer -> Still mixed effects model4 after data update, i.e. 
# lg_LIS ~ lg_ILO + as.factor(sex_id) + (1+lg_ILO|ihme_loc_id/age_group_name)
fwrite(manual_CV_results, "FILEPATH/5-way_LIS_age_CV_updated_10_27.csv")

########################### Check for outliers #################################

# Switch ihme_loc_id and age_group_name to location_id and age_group_id for ST-GPR input
model4 <- lme4::lmer(lg_LIS ~ lg_ILO + as.factor(sex_id) + (1+lg_ILO|location_id/age_group_id),
                     data = LIS,REML = TRUE) # does not throw error only when REML = TRUE

# saving the model
saveRDS(model4, "FILEPATH/LIS_age_stage1_model_updated_10_27.rda")

