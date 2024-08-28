# Stage1 Model Selection for Caregiver Gender
# Author: Michael Breshock
# Date: 09/19/2023

# Clear environment
rm(list = ls())

# Loading libraries
library(data.table)
library(dplyr)
library(ggplot2)
library(stargazer)
library(lme4)
library(cowplot)
library(corrplot)
library(ggpubr)
library(BAS, lib.loc = "FILEPATH")

# load clean data with covariates: 
data = fread("FILEPATH/01_clean_data_covars.csv")

# log transform the variables of interest: 
data[, ":=" (lg_pct_fm = log(percent_female_cg),
             lg_sdi = log(sdi),
             lg_the_pc = log(the_pc), 
             lg_ldi_pc = log(ldi_pc), 
             lg_gdp_pc = log(gdp_pc), 
             lg_dementia = log(dementia_prevalence), 
             lg_frac_over65 = log(frac_over65), 
             lg_diabetes = log(DM_prevalence),
             lg_edu = log(edu_years_pc), 
             lg_edu_F = log(female_edu_years_pc), 
             lg_tfr = log(tfr), 
             lg_pct_fem_pop = log(pct_fem_pop),
             lg_fem_edu_ratio = log(fem_edu_ratio))]

############################# Explore data #####################################
# Correlation matrix
# With dep var
var_list <- c("percent_female_cg", "sdi", "ldi_pc", "the_pc", "gdp_pc", 
              "dementia_prevalence", "frac_over65", "DM_prevalence", "edu_years_pc", 
              "female_edu_years_pc", "tfr", "pct_fem_pop", "fem_edu_ratio")
corrplot(cor(data[, var_list, with = F], method = "pearson", use = "complete.obs"))
# Note: Gender is least correlated with THE_pc

# Just independent vars
var_list <- c("sdi", "ldi_pc","the_pc", "gdp_pc", 
              "dementia_prevalence", "frac_over65", "DM_prevalence", "edu_years_pc", 
              "female_edu_years_pc", "tfr", "pct_fem_pop", "fem_edu_ratio")
corrplot(cor(data[, var_list, with = F], method = "pearson", use = "complete.obs"))

# scatter plot by year of percent female values
ggplot(data,aes(x=year_id,y=percent_female_cg,group=location_name))+
  geom_point(aes(color=location_name))+
  theme_minimal() + ylim(0,1)


# Scatter plots for gender against each possible covariate
dev.off()
pdf("FILEPATH/gender_covars_scatter_plots.pdf")

# SDI
gg_sdi <- ggplot(data,aes(x=lg_sdi,y=lg_pct_fm))+
  geom_point(show.legend = FALSE)+
  geom_smooth(method ='lm')+
  theme_minimal() + 
  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "bottom")
  

# ldi_pc
gg_ldi_pc <- ggplot(data,aes(x=lg_ldi_pc,y=lg_pct_fm))+
  geom_point(show.legend = FALSE)+
  geom_smooth(method ='lm')+
  theme_minimal()+ 
  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "bottom")

# the_pc
gg_the_pc <- ggplot(data,aes(x=lg_the_pc,y=lg_pct_fm))+
  geom_point(show.legend = FALSE)+
  geom_smooth(method ='lm')+
  theme_minimal()+ 
  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "bottom")

# gdp_pc
gg_gdp_pc <- ggplot(data,aes(x=lg_gdp_pc,y=lg_pct_fm))+
  geom_point(show.legend = FALSE)+
  geom_smooth(method ='lm')+
  theme_minimal()+ 
  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "bottom")

# dementia prevalence
gg_dementia <- ggplot(data,aes(x=lg_dementia,y=lg_pct_fm))+
  geom_point(show.legend = FALSE)+
  geom_smooth(method ='lm')+
  theme_minimal()+ 
  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "bottom")

# diabetes prevalence
gg_diabetes <- ggplot(data,aes(x=lg_diabetes,y=lg_pct_fm))+
  geom_point(show.legend = FALSE)+
  geom_smooth(method ='lm')+
  theme_minimal()+ 
  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "bottom")

# fraction over 65
gg_frac65 <- ggplot(data,aes(x=lg_frac_over65,y=lg_pct_fm))+
  geom_point(show.legend = FALSE)+
  geom_smooth(method ='lm')+
  theme_minimal()+ 
  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "bottom")

# avg education years per capita
gg_edu <- ggplot(data,aes(x=lg_edu,y=lg_pct_fm))+
  geom_point(show.legend = FALSE)+
  geom_smooth(method ='lm')+
  theme_minimal()+ 
  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "bottom")

# female education years per capita 
gg_edu_F <- ggplot(data,aes(x=lg_edu_F,y=lg_pct_fm))+
  geom_point(show.legend = FALSE)+
  geom_smooth(method ='lm')+
  theme_minimal()+ 
  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "bottom")

# total fertility rate
gg_tfr <- ggplot(data,aes(x=lg_tfr,y=lg_pct_fm))+
  geom_point(show.legend = FALSE)+
  geom_smooth(method ='lm')+
  theme_minimal()+ 
  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "bottom")

# percent female population 
gg_pct_fem <- ggplot(data,aes(x=lg_pct_fem_pop,y=lg_pct_fm))+
  geom_point(show.legend = FALSE)+
  geom_smooth(method ='lm')+
  theme_minimal()+ 
  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "bottom")

# female - male education ratio
gg_edu_F_ratio <- ggplot(data,aes(x=lg_fem_edu_ratio,y=lg_pct_fm))+
  geom_point(show.legend = FALSE)+
  geom_smooth(method ='lm')+
  theme_minimal()+ 
  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "bottom")

print(gg_sdi)
print(gg_ldi_pc)
print(gg_the_pc)
print(gg_gdp_pc)
print(gg_dementia)
print(gg_diabetes)
print(gg_frac65)
print(gg_edu)
print(gg_edu_F)
print(gg_tfr)
print(gg_pct_fem)
print(gg_edu_F_ratio)

dev.off()

# fraction over 65 is highest correlated, with R = -0.19
# second highest is tie between SDI & education, with R = -0.16

########################### Bayesian Model Selection ########################### 
dependent_var = "lg_pct_fm"
control_list = c("lg_the_pc", "lg_gdp_pc", "lg_ldi_pc", "lg_dementia", 
                 "lg_diabetes", "lg_frac_over65", "lg_sdi", "lg_edu", 
                 "lg_edu_F", "lg_tfr", "lg_pct_fem_pop", "lg_fem_edu_ratio")
bms_list = c(dependent_var, control_list)

# select just the columns we need
bms_data = data[, ..bms_list]

# create RHS formula
controls <- paste(control_list, collapse = " + ")

# create model formula
model_string <- as.formula(paste(dependent_var, "~", controls))

# run BMS on raw data
model_bms <- bas.lm(model_string,
                    data = bms_data,
                    prior = "JZS",
                    modelprior = uniform())

# view BMS plots
mip_plot <- plot(model_bms, which = 4)
lpo_plot <- image(model_bms)

pdf("FILEPATH/gender_BMS_plots_RTR.pdf")
plot(model_bms, which = 4)
image(model_bms)
dev.off()

# try best model picked by BMS: 
# bms_model = lm(lg_pct_fm ~ lg_frac_over65 + lg_tfr + lg_pct_fem_pop, data = data)
bms_model = lm(lg_pct_fm ~ lg_frac_over65 + lg_pct_fem_pop, data = data)
stargazer(bms_model, type = "text") 

########################## Cross Validation ####################################
# add index column to data
data[, n := c(1:nrow(data))]

# run cross validation ten times to see which model is most frequently the best: 
# initialize table to store best models in: 
all_best_model = data.table(model = character(), 
                            RMSE = double(),
                            run = integer(), 
                            spec = character())
for (run in c(1:10)){
  # cross validate top 8 models picked by BMS and try random effects on location
  all_results <- data.table(
    model = character(),
    MSE = double(),
    RMSE = double(),
    MAE = double(),
    fold = integer()
  )
  
  # randomize n
  n_rand <- sample(data$n, length(data$n))
  flds <- caret::createFolds(n_rand, k = 10, list = TRUE, returnTrain = FALSE)
  
  model_specs <- list('model1' = 'lg_pct_fm ~ lg_frac_over65 + lg_tfr + lg_pct_fem_pop',
                      'model2' = 'lg_pct_fm ~ lg_frac_over65 + lg_tfr + lg_pct_fem_pop + (1 | location_name)',
                      'model3' = 'lg_pct_fm ~ lg_frac_over65 + lg_tfr + lg_pct_fem_pop + lg_dementia',
                      'model4' = 'lg_pct_fm ~ lg_frac_over65 + lg_tfr + lg_pct_fem_pop + lg_dementia + (1 | location_name)',
                      'model5' = 'lg_pct_fm ~ lg_frac_over65 + lg_pct_fem_pop',
                      'model6' = 'lg_pct_fm ~ lg_frac_over65 + lg_pct_fem_pop + (1 | location_name)',
                      'model7' = 'lg_pct_fm ~ lg_frac_over65 + lg_tfr + lg_pct_fem_pop + lg_ldi_pc',
                      'model8' = 'lg_pct_fm ~ lg_frac_over65 + lg_tfr + lg_pct_fem_pop + lg_ldi_pc + (1 | location_name)',
                      'model9' = 'lg_pct_fm ~ lg_frac_over65 + lg_tfr + lg_pct_fem_pop + lg_the_pc',
                      'model10' = 'lg_pct_fm ~ lg_frac_over65 + lg_tfr + lg_pct_fem_pop + lg_the_pc + (1 | location_name)',
                      'model11' = 'lg_pct_fm ~ lg_frac_over65 + lg_tfr + lg_pct_fem_pop + lg_gdp_pc',
                      'model12' = 'lg_pct_fm ~ lg_frac_over65 + lg_tfr + lg_pct_fem_pop + lg_gdp_pc + (1 | location_name)',
                      'model13' = 'lg_pct_fm ~ lg_frac_over65 + lg_tfr + lg_pct_fem_pop + lg_fem_edu_ratio',
                      'model14' = 'lg_pct_fm ~ lg_frac_over65 + lg_tfr + lg_pct_fem_pop + lg_fem_edu_ratio + (1 | location_name)',
                      'model15' = 'lg_pct_fm ~ lg_frac_over65 + lg_tfr + lg_pct_fem_pop + lg_diabetes', 
                      'model16' = 'lg_pct_fm ~ lg_frac_over65 + lg_tfr + lg_pct_fem_pop + lg_diabetes + (1 | location_name)')
  
  # Initialize table for the mixed effects models, assume no model over-fitting.
  # If at any point during the CV loop, the singularity warning occurs for a me
  # model, change the overfit value for that model to TRUE, and disallow that
  # model from consideration after the loop.
  me_models <- data.table(model=c('model2',
                                  'model4',
                                  'model6',
                                  'model8', 
                                  'model10',
                                  'model12',
                                  'model14',
                                  'model16'),overfit=FALSE)
  
  
  # Loop over k-folds
  for (i in 1:length(flds)) {
    # split into train and test
    test <- data[flds[[i]], ]
    train <- anti_join(data, test)
    
    # BMS 1
    model1 <- lm(lg_pct_fm ~ lg_frac_over65 + lg_tfr + lg_pct_fem_pop, 
                 data = train)
    model2 <- lmer(lg_pct_fm ~ lg_frac_over65 + lg_tfr + lg_pct_fem_pop +
                     (1 | location_name), data = train)
    
    # BMS 2
    model3 <- lm(lg_pct_fm ~ lg_frac_over65 + lg_tfr + lg_pct_fem_pop + lg_dementia,
                 data = train)
    model4 <- lmer(lg_pct_fm ~ lg_frac_over65 + lg_tfr + lg_pct_fem_pop + lg_dementia + 
                     (1 | location_name), data = train)
    
    # BMS 3
    model5 <- lm(lg_pct_fm ~ lg_frac_over65 + lg_pct_fem_pop, 
                 data = train)
    model6 <- lmer(lg_pct_fm ~ lg_frac_over65 + lg_pct_fem_pop + 
                     (1 | location_name), data = train)
    
    # BMS 4
    model7 <- lm(lg_pct_fm ~ lg_frac_over65 + lg_tfr + lg_pct_fem_pop + lg_ldi_pc, 
                 data = train)
    model8 <- lmer(lg_pct_fm ~ lg_frac_over65 + lg_tfr + lg_pct_fem_pop + lg_ldi_pc +
                     (1 | location_name), data = train)
    
    # BMS 5
    model9 <- lm(lg_pct_fm ~ lg_frac_over65 + lg_tfr + lg_pct_fem_pop + lg_the_pc,
                 data = train)
    model10 <- lmer(lg_pct_fm ~ lg_frac_over65 + lg_tfr + lg_pct_fem_pop + lg_the_pc +
                      (1 | location_name), data = train)
    
    # BMS 6
    model11 <- lm(lg_pct_fm ~ lg_frac_over65 + lg_tfr + lg_pct_fem_pop + lg_gdp_pc,
                  data = train)
    model12 <- lmer(lg_pct_fm ~ lg_frac_over65 + lg_tfr + lg_pct_fem_pop + lg_gdp_pc + 
                      (1 | location_name), data = train)
    
    # BMS 7
    model13 <- lm(lg_pct_fm ~ lg_frac_over65 + lg_tfr + lg_pct_fem_pop + lg_fem_edu_ratio,
                  data = train)
    model14 <- lmer(lg_pct_fm ~ lg_frac_over65 + lg_tfr + lg_pct_fem_pop + lg_fem_edu_ratio + 
                      (1 | location_name), data = train)
    
    # BMS 8
    model15 <- lm(lg_pct_fm ~ lg_frac_over65 + lg_tfr + lg_pct_fem_pop + lg_diabetes, 
                  data = train)
    model16 <- lmer(lg_pct_fm ~ lg_frac_over65 + lg_tfr + lg_pct_fem_pop + lg_diabetes + 
                      (1 | location_name), data = train)
    
    if(isSingular(model2)){
      me_models[model=='model2',overfit:=T]
    }
    if(isSingular(model4)){
      me_models[model=='model4',overfit:=T]
    }
    if(isSingular(model6)){
      me_models[model=='model6',overfit:=T]
    }
    if(isSingular(model8)){
      me_models[model=='model8',overfit:=T]
    }
    if(isSingular(model10)){
      me_models[model=='model10',overfit:=T]
    }
    if(isSingular(model12)){
      me_models[model=='model12',overfit:=T]
    }
    if(isSingular(model14)){
      me_models[model=='model14',overfit:=T]
    }
    if(isSingular(model16)){
      me_models[model=='model16',overfit:=T]
    }
    
    test$model1 <- exp(predict(model1, test))
    test$model2 <- exp(predict(model2, test, allow.new.levels=T))
    test$model3 <- exp(predict(model3, test))
    test$model4 <- exp(predict(model4, test, allow.new.levels=T))
    test$model5 <- exp(predict(model5, test))
    test$model6 <- exp(predict(model6, test, allow.new.levels=T))
    test$model7 <- exp(predict(model7, test))
    test$model8 <- exp(predict(model8, test, allow.new.levels=T))
    test$model9 <- exp(predict(model9, test))
    test$model10 <- exp(predict(model10, test, allow.new.levels=T))
    test$model11 <- exp(predict(model11, test))
    test$model12 <- exp(predict(model12, test, allow.new.levels=T))
    test$model13 <- exp(predict(model13, test))
    test$model14 <- exp(predict(model14, test, allow.new.levels=T))
    test$model15 <- exp(predict(model15, test))
    test$model16 <- exp(predict(model16, test, allow.new.levels=T))
    
    
    # Summarize results 
    results <- data.table(test)[,.(percent_female_cg,
                                   model1, model2, model3, model4,model5, model6, 
                                   model7, model8, model9, model10, model11, 
                                   model12, model13, model14, model15, model16)]
    
    results <- melt(results,id.vars='percent_female_cg')
    setnames(results,old=c('variable','value'),new=c('model','predictions'))
    results <- results[,.(MSE = mean((predictions - percent_female_cg)^2),
                          RMSE = sqrt(mean((predictions - percent_female_cg)^2)),
                          MAE = mean(abs(predictions - percent_female_cg))),
                       by=model]
    
    results$fold <- i
    all_results <- rbind(all_results, results)
  }
  # Disallow models that caused singularity warning (implying over-fitting) during CV
  all_results <- all_results[!(model %in% me_models[overfit==T]$model)]
  # Take average of k results:
  manual_CV_results <- all_results[, .(
    avg_MSE = mean(MSE),
    avg_RMSE = mean(RMSE),
    avg_MAE = mean(MAE)
  ), by = model]
  
  # Add model specification using list names as keys
  manual_CV_results$spec <- model_specs[manual_CV_results$model]
  
  # Order by avg_RMSE
  manual_CV_results <- manual_CV_results[order(avg_RMSE, decreasing = FALSE), ]
  # best model by RMSE:
  bm <- manual_CV_results[1]
  
  best_model = data.table(model = bm$model, 
                          RMSE = bm$avg_RMSE,
                          run = run, 
                          spec = bm$spec)
  
  # save best model to table: 
  all_best_model = rbind(all_best_model, best_model)
}

all_best_model
# see most frequent best model: 
table(all_best_model$model)
# see which model resulted in the lowest RMSE in any run: 
all_best_model[RMSE == min(all_best_model$RMSE)]
# overall best: model5 -> lg_pct_fm ~ lg_frac_over65 + lg_pct_fem_pop

fwrite(manual_CV_results, "FILEPATH/16_way_manual_CV_RTR.csv")

# get model summaries for all fixed effect models tested: 
fit1 = lm(lg_pct_fm ~ lg_frac_over65 + lg_tfr + lg_pct_fem_pop, data = data)
fit2 = lm(lg_pct_fm ~ lg_frac_over65 + lg_tfr + lg_pct_fem_pop + lg_dementia, data = data) 
fit3 = lm(lg_pct_fm ~ lg_frac_over65 + lg_pct_fem_pop, data = data) # <- best model according to XVal & BMS
fit4 = lm(lg_pct_fm ~ lg_frac_over65 + lg_tfr + lg_pct_fem_pop + lg_ldi_pc, data = data)
fit5 = lm(lg_pct_fm ~ lg_frac_over65 + lg_tfr + lg_pct_fem_pop + lg_the_pc, data = data)
fit6 = lm(lg_pct_fm ~ lg_frac_over65 + lg_tfr + lg_pct_fem_pop + lg_gdp_pc, data = data)
fit7 = lm(lg_pct_fm ~ lg_frac_over65 + lg_tfr + lg_pct_fem_pop + lg_fem_edu_ratio, data = data)
fit8 = lm(lg_pct_fm ~ lg_frac_over65 + lg_tfr + lg_pct_fem_pop + lg_diabetes, data = data)

stargazer(fit1,fit2,fit3,fit4,fit5,fit6,fit7,fit8, type = "html", 
          out = "FILEPATH/regression_results_BMS_top8_RTR.html")

# Plot predictions vs actual values from best model selected by XVal
data$pred <- exp(predict(fit2, data))

output_dir = "FILEPATH"
pdf(paste0(output_dir,"stage1_pred_vs_actual_RTR_4.18.24.pdf")) 
ggplot(data,aes(x=percent_female_cg,y=pred))+
  geom_point(aes(color=location_name),alpha=0.6,show.legend = FALSE)+
  geom_abline(color = "red")+
  theme_minimal() +
  labs(title="Percent Female Caregivers Predictions vs Actual",
       subtitle="Stage 1 model",
       x="Actual",
       y="Prediction",
       color=NULL)
dev.off()

