# Stage1 Model Selection for Caregiving Hours
# Author: Michael Breshock
# Date: 08/31/2023

# Clear environment
rm(list = ls())

# Loading libraries
library(data.table)
library(BAS, lib.loc = "FILEPATH")
library(ggplot2)
library(RColorBrewer)
library(stargazer)
library(lme4)
library(dplyr)

################# Model Selection for Overall Caregiving Data ##################
data = fread("FILEPATH/01_clean_data_covars_overall.csv")

# log transform the variables of interest:
data[, ":=" (lg_hours = log(weekly_hours),
             lg_sdi = log(sdi),
             lg_the_pc = log(the_pc),
             the_pc2 = the_pc^2,
             lg_ldi_pc = log(ldi_pc),
             lg_gdp_pc = log(gdp_pc),
             lg_dementia = log(dementia_prevalence),
             lg_frac_over65 = log(frac_over65),
             lg_diabetes = log(DM_prevalence))]

# add squared THE option:
data[, lg_the_pc2 := log(the_pc2)]

# scaled logit transform dependent variable: 
scaled_logit <- function(x,a,b){
  # Alternative to regular logit, found from https://otexts.com/fpp2/limits.html
  result <- log((x-a)/(b-x))
  return(result)
}

data[, logit_hours := scaled_logit(weekly_hours, 0, 169)] 
# setting upper limit of 169 here to avoid infinite values when weekly_hours == 168

############################ Bayesian Model Selection ##########################

# filter data set to just the dependent and independent variables:
dependent_var = "logit_hours"
control_list = c("lg_the_pc", "lg_the_pc2", "lg_gdp_pc", "lg_ldi_pc", 
                 "lg_dementia", "lg_diabetes", "lg_frac_over65", "sdi", 
                 "edu_years_pc", "lg_sdi", "region_SDI", "severity", "care_type")
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

# save coefficients
temp_coef <- coef(model_bms)

bms_coefs <- data.table(factor = temp_coef$namesx, beta = temp_coef$postmean, prob = temp_coef$probne0, se = 0)

bms_coefs[, sig := ifelse(beta<0 & prob > 0.5, '-1',
                          ifelse(beta>0 & prob > 0.5, '1', '0'))]

bms_coefs <- bms_coefs[! factor=="Intercept",]

fwrite(bms_coefs, "FILEPATH/RTR_logit_cg_overall_and_splits_BMS_results.csv")

## save plots

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

plot_title = paste0("Caregiving Hours: bayesian model coefficients")
treemap_bms <- plot_effects_and_se(plot_title,bms_coefs)

pdf("FILEPATH/RTR_logit_cg_overall_and_splits_BMS_plots.pdf")
plot(model_bms, which = 4)
image(model_bms)
treemap_bms
dev.off()

# try the top 8 models picked by BMS: 
fit1 = lm(logit_hours ~ sdi + severity + care_type, data = data)
fit2 = lm(logit_hours ~ lg_sdi + severity + care_type, data = data)
fit3 = lm(logit_hours ~ sdi + lg_the_pc2 + severity + care_type, data = data)
fit4 = lm(logit_hours ~ sdi + lg_the_pc + severity + care_type, data = data)
fit5 = lm(logit_hours ~ sdi + lg_the_pc + lg_the_pc2 + severity + care_type, data = data)
fit6 = lm(logit_hours ~ lg_sdi + lg_the_pc + lg_the_pc2 + severity + care_type, data = data)
fit7 = lm(logit_hours ~ lg_sdi + lg_the_pc + severity + care_type, data = data)
fit8 = lm(logit_hours ~ lg_sdi + lg_the_pc2 + severity + care_type, data = data)
# print model summaries:
stargazer(fit1, fit2, fit3, fit4, fit5, fit6, fit7, fit8, type = "html",
          out = "FILEPATH/RTR_logit_overall_and_splits_model_selection.html")

# going with the best model picked by BMS:
# logit_hours ~ sdi + severity + care_type
