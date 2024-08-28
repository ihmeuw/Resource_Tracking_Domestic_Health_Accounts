################################################################################
# Plots another scatter plot of LIS data fit vs ILO ST-GPR estimates as a sanity
# check, and then fits a linear model in preparation for ST-GPR send-off round 2.
# This is for LIS income data reweighted across age groups.
# Author: Elye Bliss
# Date: Aug 21, 2023
################################################################################

rm(list = ls())

library(data.table)
library(ggplot2)
library(stargazer)
library(lme4)

LIS <- fread('FILEPATH/LIS_with_ILO_stgpr_updated_10_31.csv')

############################# Explore data #####################################

# Check correlation
cor(LIS$LIS_est,LIS$ILO_est)
#[1] 0.9518615, was previously 0.9754228

# Use chunk to label a few data points farthest from 45-degree line
LIS[,LIS_ILO_ratio := (abs(LIS_est-ILO_est))]

LIS_ILO_list <- LIS[,max(LIS_ILO_ratio),by=ihme_loc_id]$V1 # limit max 1 per country 

LIS[!(LIS_ILO_ratio %in% sort(LIS_ILO_list,decreasing =T)[0:2]),LIS_ILO_ratio := NA]

output_dir = "FILEPATH"
pdf(paste0(output_dir,"LIS_vs_ILO_stgpr_scatter_raw_updated_10_31.pdf")) 

Income_scatter_plot <- ggplot(LIS,aes(x=ILO_est,y=LIS_est))+
  geom_point(aes(color=ihme_loc_id),alpha=0.6,show.legend = FALSE) + 
  geom_smooth(method = "lm", se = FALSE)+
  labs(title="Scatter plot of LIS raw data vs ILO-fit ST-GPR estimates",
       subtitle="With OLS best fit",
       x="ILO-fit ST-GPR estimates",
       y="LIS income",
       color=NULL)+
  geom_text(aes(label=ifelse(!is.na(LIS_ILO_ratio),paste0(location_name,', ',year_id),''),hjust="inward",vjust=0))+
  theme_minimal()

plot(Income_scatter_plot)
dev.off()

############################# Fit linear model #################################

# log-transform data
LIS[,lg_LIS := log(LIS_est)]
LIS[,lg_ILO := log(ILO_est)]


# Compare default fixed effects model against mixed effects specification with
# random effect on country. 

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
    lg_LIS ~ lg_ILO + as.factor(sex_id),
    data = train
  ) 
  
  model2 <- lme4::lmer(lg_LIS ~ lg_ILO + as.factor(sex_id)
                       + (1 | location_id), data = train)
  
  # Specify allow.new.levels = TRUE to predict random effects variable on any unseen countries
  test$model1_pred <- predict(model1, test) 
  test$model2_pred <- predict(model2, test, allow.new.levels = TRUE)
  
  # Summarize results 
  results <- data.table(test)[,.(lg_LIS, model1_pred, model2_pred)]
  
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
# Conclusion: mixed effects model performs better across measures.
#         model    avg_MSE  avg_RMSE    avg_MAE
# 1: model2_pred 0.01426692 0.1188965 0.08466985
# 2: model1_pred 0.06215369 0.2468657 0.17965059


# Print model estimates 
model2 <- lme4::lmer(lg_LIS ~ lg_ILO + as.factor(sex_id)
                     + (1 | location_id), data = LIS)

# saving the model
saveRDS(model2, "FILEPATH/LIS_stage1_model_updated_10_31.rda")
