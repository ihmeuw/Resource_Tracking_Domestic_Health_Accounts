################################################################################
# Fit stage 1 linear model on care settings data
# Author: Elye Bliss
# Date: Sep 1, 2023
################################################################################

# Clean the environment 
rm(list=ls()) 

library(influence.ME,lib.loc = "FILEPATH")
library(data.table)
library(lme4)
library(cowplot)
library(ggplot2)

data <- fread("FILEPATH/00_combined_with_prev_11_08.csv")

# Get logged covariates and dependent variable
data[,":=" (lg_sdi = log(sdi),
            lg_ldi_pc = log(ldi_pc),
            lg_the_pc = log(the_pc),
            lg_gdp_pc = log(gdp_pc),
            lg_gamma = log(gamma))]

############################# Explore data #####################################

# Correlation matrix
# With dep var
var_list <- c("gamma","sdi", "ldi_pc", "the", "the_pc", "gdp_pc")
corrplot(cor(data[, var_list, with = F], method = "pearson", use = "complete.obs"))
# Note: Gamma is least correlated with THE.

# Just independent vars
var_list <- c("sdi", "ldi_pc","the_pc", "gdp_pc")
corrplot(cor(data[, var_list, with = F], method = "pearson", use = "complete.obs"))
# Note: These are actually all fairly correlated when ldi, the and gdp are in pc
# Include max 1.

# Box plots by year of gamma values
ggplot(data,aes(x=year_id,y=gamma,group=ihme_loc_id))+
  geom_point(aes(color=ihme_loc_id))+
  theme_minimal()

# Scatter plots for gamma against each of: sdi, ldi_pc, the_pc, gdp_pc
dev.off()
pdf("FILEPATH/gamma_covars_scatter_plots.pdf")

# SDI
gg_sdi <- ggplot(data,aes(x=sdi,y=gamma))+
  geom_point(show.legend = FALSE)+
  geom_smooth(method ='lm')+
  theme_minimal()

# ldi_pc
gg_ldi_pc <- ggplot(data,aes(x=lg_ldi_pc,y=gamma))+
  geom_point(show.legend = FALSE)+
  geom_smooth(method ='lm')+
  theme_minimal()

# the_pc
gg_the_pc <- ggplot(data,aes(x=lg_the_pc,y=gamma))+
  geom_point(show.legend = FALSE)+
  geom_smooth(method ='lm')+
  theme_minimal()

# gdp_pc
gg_gdp_pc <- ggplot(data,aes(x=lg_gdp_pc,y=gamma))+
  geom_point(show.legend = FALSE)+
  geom_smooth(method ='lm')+
  theme_minimal()

plot_grid(gg_sdi,gg_ldi_pc,gg_the_pc,gg_gdp_pc,nrow=2,ncol=2)
dev.off()


########################## Model selection #####################################



all_results <- data.table(
  model = character(),
  MSE = double(),
  RMSE = double(),
  MAE = double(),
  fold = integer()
)

# randomize n
n_rand <- sample(data$n, length(data$n))
flds <- createFolds(n_rand, k = 10, list = TRUE, returnTrain = FALSE) #k= nrow(data) for LOO

model_specs <- list('model1'='lg_gamma ~ lg_sdi',
                    'model2' = 'lg_gamma ~ lg_sdi + (1 | ihme_loc_id)',
                    'model3' = 'lg_gamma ~ lg_ldi_pc',
                    'model4' = 'lg_gamma ~ lg_ldi_pc + (1 | ihme_loc_id)',
                    'model5' = 'lg_gamma ~ lg_the_pc',
                    'model6' = 'lg_gamma ~ lg_the_pc + (1 | ihme_loc_id)',
                    'model7' = 'lg_gamma ~ lg_gdp_pc',
                    'model8' = 'lg_gamma ~ lg_gdp_pc + (1 | ihme_loc_id)')

# Initialize table for the mixed effects models, assume no model over-fitting.
# If at any point during the CV loop, the singularity warning occurs for a me
# model, change the overfit value for that model to TRUE, and disallow that
# model from consideration after the loop.
me_models <- data.table(model=c('model2',
                                'model4',
                                'model6',
                                'model8'),overfit=FALSE)

# Loop over k-folds (not sure if there's an easy way to avoid a loop here)
for (i in 1:length(flds)) {
  # split into train and test
  test <- data[flds[[i]], ]
  train <- anti_join(data, test)
  
  # Using SDI as main covariate
  model1 <- lm(
    lg_gamma ~ sdi,
    data = train
  )
  
  model2 <- lme4::lmer(
    lg_gamma ~ sdi + (1 | ihme_loc_id), 
    data = train)


  # Using LDI as main covar
  model3 <- lm(
    lg_gamma ~ lg_ldi_pc,
    data = train
  )

  model4 <- lme4::lmer(
    lg_gamma ~ lg_ldi_pc + (1 | ihme_loc_id), 
    data = train)
  
  # Using THE as main covar
  model5 <- lm(
    lg_gamma ~ lg_the_pc,
    data = train
  )

  model6 <- lme4::lmer(
    lg_gamma ~ lg_the_pc + (1 | ihme_loc_id), 
    data = train)
  
  # Using GDP as main covar
  model7 <- lm(
    lg_gamma ~ lg_gdp_pc,
    data = train
  )
  
  model8 <- lme4::lmer(
    lg_gamma ~ lg_gdp_pc + (1 | ihme_loc_id), 
    data = train)

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
  
  test$model1 <- predict(model1, test)
  test$model2 <- predict(model2, test, allow.new.levels=T)
  test$model3 <- predict(model3, test) 
  test$model4 <- predict(model4, test, allow.new.levels=T)
  test$model5 <- predict(model5, test)
  test$model6 <- predict(model6, test, allow.new.levels=T)
  test$model7 <- predict(model7, test)
  test$model8 <- predict(model8, test, allow.new.levels=T)

  
  # Summarize results 
  results <- data.table(test)[,.(lg_gamma, model1, model2, 
                                 model3, model4,model5, model6, 
                                 model7, model8)]
  
  results <- melt(results,id.vars='lg_gamma')
  setDT(results)
  setnames(results,old=c('variable','value'),new=c('model','predictions'))
  results <- results[!is.na(predictions)] # remove predictions from overfit models
  results <- results[,.(MSE = mean((predictions - lg_gamma)^2),
                        RMSE = sqrt(mean((predictions - lg_gamma)^2)),
                        MAE = mean(abs(predictions - lg_gamma))),
                     by=model]
  
  results$fold <- i
  all_results <- rbind(all_results, results)
}

# Disallow models that caused singularity warning (implying over-fitting) during
# CV
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

# Best performing is model7, i.e. lg_gamma ~ lg_gdp_pc. This is still the top-
# performing model after the inclusion of new data on 11-08

fwrite(manual_CV_results, "FILEPATH/8-way_manual_CV.csv")

# Get model summary on full data, switch to location_id to conform with ST-GPR
# input format. 
model7 <- lm(lg_gamma ~ lg_gdp_pc, data = data)
summary(model7)
sg <- stargazer(model7,
                title = "Regression Results", type = "text"
) 

# Plot model predictions with SDI on x-axis
data$pred_lg <- predict(model7, data)
data[,pred := exp(pred_lg)]

output_dir = "FILEPATH"
pdf(paste0(output_dir,"gamma_by_gdp_pc_stage1_model.pdf")) 
gg_gdp_pc <- ggplot(data,aes(x=gdp_pc,y=gamma))+
  geom_point(aes(color=ihme_loc_id),alpha=0.6,show.legend = FALSE)+
  geom_line(aes(y=pred))+
  theme_minimal()+
  labs(title="Scatter plot of institution care and GDP_pc",
       subtitle="Stage 1 model",
       x="GDP_pc",
       y="Gamma",
       color=NULL)
dev.off()

data$pred_lg <- NULL
data$pred <- NULL

########################## Outlier removal #####################################

# Using the fit of the final model specification for outlier 
# removal


data[,cd := cooks.distance(model7)]


is_outlier <- function(x, y) {
  if (x > y) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# use 4/n as cook's distance cutoff
data$is_outlier <- sapply(data$cd, FUN = is_outlier, y = 4 / nrow(data))


# Save outliers for manual inspection
outliers <- data[is_outlier==T]
outliers <- outliers[order(cd,decreasing = T)]
# Currently, this cuts 7 data points. 

outliers_formatted <- outliers[,.(location_name,year_id,gamma)]
fwrite(outliers,"FILEPATH/gamma_outliers_11_08.csv")
fwrite(outliers_formatted,"FILEPATH/gamma_outliers_formatted_11_08.csv")

data <- data[is_outlier==F]
model7 <- lm(lg_gamma ~ lg_gdp_pc, data = data)
################################# Save model ###################################

# saving the model
saveRDS(model7,"FILEPATH/stage1_care_settings_model.rda")

# save outliered data 
fwrite(data, "FILEPATH/01_outliered_11_08.csv")
