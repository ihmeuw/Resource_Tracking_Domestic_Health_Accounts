# Stage1 Model Selection for Beta (Diagnosis Rate)
# Author: Michael Breshock
# Date: 08/02/2023

# Clear environment
rm(list = ls())

# Loading libraries
library(data.table)
library(BAS, lib.loc = "FILEPATH")
library(ggplot2)
library(RColorBrewer)
library(stargazer)

# load clean data with outliers cut: 
data = fread("FILEPATH/01_clean_data_covars.csv")

# filter to just diagnosis rate data and remove severity splits: 
# this already gets done in 00_clean_and_rbind_data.R, but filtering here as a fail safe
data = data[clean_units == "dx_over_dem_cases" & is.na(severity)]

# see mean diagnosis rate by living condition: 
data[, .(mean_dx_rate = mean(clean_value)), by = "living_conditions"]

# log transform the variables of interest: 
data[, ":=" (lg_dx_rate = log(clean_value),
             lg_the_pc = log(the_pc), 
             lg_ldi_pc = log(ldi_pc), 
             lg_gdp_pc = log(gdp_pc), 
             lg_prev = log(prevalence), 
             lg_frac_over65 = log(frac_over65))]

# convert living_conditions to factor: 
data[, living_conditions := as.factor(living_conditions)]

############################ Bayesian Model Selection ##########################

# filter dataset to just the dependent and independent variables: 
dependent_var = "lg_dx_rate"
control_list = c("lg_the_pc", "lg_ldi_pc", "lg_gdp_pc", "lg_prev", 
                 "lg_frac_over65", "sdi", "living_conditions")
bms_list = c(dependent_var, control_list)

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

# save coefficients
temp_coef <- coef(model_bms)

bms_coefs <- data.table(factor = temp_coef$namesx, beta = temp_coef$postmean, prob = temp_coef$probne0, se = 0)

bms_coefs[, sig := ifelse(beta<0 & prob > 0.5, '-1',
                          ifelse(beta>0 & prob > 0.5, '1', '0'))]

bms_coefs <- bms_coefs[! factor=="Intercept",]

fwrite(bms_coefs,"FILEPATH/beta_BMS_results.csv")

# save plots

# define colors
red <- brewer.pal(3, 'Set1')[1]
green <- brewer.pal(3, 'Set1')[3]
black <- '#000000'

# define plotting function
plot_effects_and_se <- function(title_name,coefs){
  p <- ggplot(coefs, aes(x=factor, y=beta, color=sig)) +
    geom_hline(yintercept = 0) +
    geom_point() +
    geom_errorbar(aes(ymin=beta-1.96*se, ymax=beta+1.96*se), width=0) + 
    scale_color_manual("", breaks=c("-1","0","1"), values=c(red, black, green)) +
    coord_flip() +
    #scale_x_discrete(factor="", limits = rev(covariate_name_set)) +
    #scale_y_continuous(factor="", limits = c(ymin, ymax)) +
    ggtitle(title_name) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor.x = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_blank(),
          legend.position = "none")
  return(p)
}

plot_title = paste0("Diagnosis Rates: bayesian model coefficients")
treemap_bms <- plot_effects_and_se(plot_title,bms_coefs)

pdf("FILEPATH/beta_BMS_plots.pdf")
plot(model_bms, which = 4)
image(model_bms)
treemap_bms
dev.off()

# try the best model: 
# run model
best_model = lm(lg_dx_rate ~ lg_the_pc + lg_frac_over65 + living_conditions, # <- using this one! 
                data = bms_data)
stargazer(best_model, type = "text")

# run predictions
bms_data[, predicted_beta := exp(predict(best_model, bms_data))]

# plot actual vs predicted: 
p1 = ggplot(bms_data, aes(x = exp(lg_dx_rate), y = predicted_beta)) +
  geom_point() + geom_abline(color = "red") + 
  labs(title = "Fixed Effects Predictions") + ylim(0,1) + xlim(0,1) + 
  ggpubr::stat_cor(method = "pearson", label.x = 0, label.y = 1)

MSE = mean((bms_data$predicted_beta - data$clean_value)^2)
RMSE = sqrt(mean((bms_data$predicted_beta - data$clean_value)^2))
MAE = mean(abs(bms_data$predicted_beta - data$clean_value))

# try a few different models:
# create models and try a few different specifications:
fit = lm(lg_dx_rate ~ lg_the_pc + lg_ldi_pc + lg_frac_over65 + living_conditions, 
         data = data)
fit1 = lm(lg_dx_rate ~ lg_the_pc + lg_ldi_pc + living_conditions, data = data)
fit2 = lm(lg_dx_rate ~ lg_the_pc + living_conditions, data = data)
fit3 =  lm(lg_dx_rate ~ lg_prev + living_conditions, data = data)
fit4 = lm(lg_dx_rate ~ lg_prev + lg_the_pc + living_conditions,
          data = data) 
fit5 = lm(lg_dx_rate ~ lg_prev + lg_frac_over65 + living_conditions,
          data = data) 
fit6 = lm(lg_dx_rate ~ lg_prev + lg_the_pc + lg_frac_over65 + living_conditions,
          data = data) 
stargazer(best_model, fit, fit1, fit2, fit3, fit4, fit5, fit6, type = "html", 
          out = "FILEPATH/all_data_model_selection.html")

# Using the model specification from "best_model" as 
# THE and LDI are highly correlated so we only need to use one of them.
# THE had higher inclusion probability so we are picking that one instead of LDI
# so we are using the best model picked by BMS but without LDI

# try random effects on location: 
RE_model = lme4::lmer(lg_dx_rate ~ lg_the_pc + lg_frac_over65 + 
                        living_conditions + (1 | location_name), data = data)
summary(RE_model)

data[, RE_dx_pred := exp(predict(RE_model, data, allow.new.levels = TRUE))]

# see prediction accuracy metrics 
RE_MSE = mean((data$RE_dx_pred - data$clean_value)^2)
RE_RMSE = sqrt(mean((data$RE_dx_pred - data$clean_value)^2))
RE_MAE = mean(abs(data$RE_dx_pred - data$clean_value))

# plot predictions vs actual: 
p2 = ggplot(data, aes(x = clean_value, y = RE_dx_pred)) +
  geom_point() + geom_abline(color = "red") + 
  labs(title = "Mixed Effects Predictions") + ylim(0,1) + xlim(0,1) + 
  ggpubr::stat_cor(method = "pearson", label.x = 0, label.y = 1)

cowplot::plot_grid(p1, p2, nrow = 1, ncol = 2)

# print model accuracy metrics comparison: 
cat("Fixed Effects MSE:", FE_MSE, "\nMixed Effects MSE:", RE_MSE)
cat("Fixed Effects RMSE:", FE_RMSE, "\nMixed Effects RMSE:", RE_RMSE)
cat("Fixed Effects MAE:", FE_MAE, "\nMixed Effects MAE:", RE_MAE)

# mixed effects model outperforms fixed effects

## compare accuracy using train/test split: 
# split data into train and test:
set.seed(123)
splits <- sample(2, nrow(data), replace = TRUE, prob = c(0.8, 0.2))

train <- data[(splits == 1)] # training dataset
test <- data[(splits == 2)] # valdiation dataset

# fit models on train data
fixed_train = lm(lg_dx_rate ~ lg_the_pc + lg_frac_over65 + living_conditions,
                 data = train)
mixed_train = lme4::lmer(lg_dx_rate ~ lg_the_pc + lg_frac_over65 + 
                           living_conditions + (1 | location_name), data = train)

# run predictions on test data: 
test[, fixed_pred := exp(predict(fixed_train, test))]
test[, mixed_pred := exp(predict(mixed_train, test, allow.new.levels = T))]

# calculate prediction accuracy metrics 
FE_MSE = mean((test$fixed_pred - test$clean_value)^2)
FE_RMSE = sqrt(mean((test$fixed_pred - test$clean_value)^2))
FE_MAE = mean(abs(test$fixed_pred - test$clean_value))

ME_MSE = mean((test$mixed_pred - test$clean_value)^2)
ME_RMSE = sqrt(mean((test$mixed_pred - test$clean_value)^2))
ME_MAE = mean(abs(test$mixed_pred - test$clean_value))

# print model accuracy metrics comparison: 
cat("Fixed Effects MSE:", FE_MSE, "\nMixed Effects MSE:", ME_MSE)
cat("Fixed Effects RMSE:", FE_RMSE, "\nMixed Effects RMSE:", ME_RMSE)
cat("Fixed Effects MAE:", FE_MAE, "\nMixed Effects MAE:", ME_MAE)

# fixed effects outperforms mixed effects on test data


