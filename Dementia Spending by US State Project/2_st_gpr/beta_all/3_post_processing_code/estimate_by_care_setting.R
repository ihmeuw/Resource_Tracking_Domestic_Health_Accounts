# Create diagnosis rate estimates by care setting from overall estimate
# Author: Michael Breshock
# Date: 08/11/2023

# clear environment
rm(list = ls())

# load libraries
library(ggplot2)
library(dplyr)
library(data.table)
library(tidyr)
library(MASS)


# load clean data with outliers cut: 
data = fread("FILEPATH/02_clean_data_no_outliers.csv")
# load overall beta stgpr results: 
draws = fread('FILEPATH/dx_rate_all_draws_run_06.csv')

data[, ":=" (lg_dx_rate = log(clean_value),
             lg_the_pc = log(the_pc), 
             lg_frac_over65 = log(frac_over65))]

# create dummy variables for each living condition (care setting)
data[, ":=" (institution = 1*(living_conditions == "Institution"),
             community = 1*(living_conditions == "Community"), 
             not_reported = 1*(living_conditions == ""), 
             overall = 0
             )]

# create a copy of the data where all observations are flagged as overall
data_all = copy(data)
data_all$institution = 0
data_all$community = 0
data_all$not_reported = 0
data_all$overall = 1

# combine the two copies: 
data_c = rbind(data, data_all)

# run regression to get coefficients for each care setting: 
cs_model = lm(lg_dx_rate ~ lg_the_pc + lg_frac_over65 + institution +
                community + not_reported, data = data_c)
  # old model: 
  # lme4::lmer(lg_dx_rate ~ lg_the_pc + lg_frac_over65 + institution +
  #              community + not_reported + (1 | location_name), data = data_c)
summary(cs_model)
html_outpath = 'FILEPATH'
covar_labels = c("Total Health Expenditure Per-Capita", 
                 "Fraction of Population Over Age 65", 
                 "Care Setting: Institution",
                 "Care Setting: Community", 
                 "Care Setting: Not Reported")
dep_label = "Diagnosis Rate"
stargazer(cs_model, type = "html", 
          covariate.labels = covar_labels, 
          dep.var.labels = dep_label, 
          out = paste0(html_outpath, "beta_care_setting_model.html"))

# multiply by exp(coefficient)

# pulling draws of beta coefficients from variance-covariance matrix: 
vcov_mat = vcov(cs_model)
beta_means = coefficients(cs_model)
set.seed(123)
beta_draws = MASS::mvrnorm(1000, beta_means, vcov_mat)
colnames(beta_draws)
# grab just the institution and community coefficients: 
inst_draws = beta_draws[, 4] # institution is the 4th column 
comm_draws = beta_draws[, 5] # community is the 5th column 

# create scale factor draws data table from coefficient draws: 
beta_scales = data.table(variable = paste0("draw_", 0:999), 
                         inst_scale = exp(inst_draws), 
                         comm_scale = exp(comm_draws))
# save out institutional dx rate scale factors to use for beta_inst forecasts 
fwrite(beta_scales, paste0(j_root, "Project/IRH/Global_dementia_2023/output/beta/care_setting_scale_factor.csv"))
# merge scale factor in with overall diagnosis rate draws
dx_draws = merge(draws, beta_scales, by = "variable")

# calculate institutional and community rates by scaling from overall rate 
dx_draws[, ":=" (inst_rate = pmin(value*inst_scale,1),  # setting a ceiling of 1
                 comm_rate = value*comm_scale)]

# save out overall and institutional rates: 
overall_draws = dx_draws[,.(year_id, location_id, variable, value)]
setnames(overall_draws, old = c("variable", "value"),
         new = c("draw", "dx_rate"))
fwrite(overall_draws, file = "FILEPATH/overall_dx_rate_draws.csv")

inst_draws = dx_draws[,.(year_id, location_id, variable, inst_rate)]
setnames(inst_draws, old = c("variable", "inst_rate"), 
         new = c("draw", "dx_rate"))
fwrite(inst_draws, file = "FILEPATH/institution_dx_rate_draws_run_06.csv")

# get mean, lower and upper diagnosis rates by care setting from draws: 
dx_means = dx_draws[, .(overall_mean = mean(value), 
                        overall_lower = quantile(value, 0.025), 
                        overall_upper = quantile(value, 0.975),
                        inst_mean = mean(inst_rate), 
                        inst_lower = quantile(inst_rate, 0.025), 
                        inst_upper = quantile(inst_rate, 0.975),
                        comm_mean = mean(comm_rate), 
                        comm_lower = quantile(comm_rate, 0.025), 
                        comm_upper = quantile(comm_rate, 0.975)), 
                    by = c("year_id", "location_id")]

# sanity check one location / year: US 2019
dx_means[year_id == 2019 & location_id == 102]

# check that data follows institution > overall > community rates assumption
nrow(dx_means[inst_mean < overall_mean | overall_mean < comm_mean]) # some, but all sub-nats


# create long data frame for plotting: 
# first create data frames for each care setting
dx_all = dx_means[,.(year_id, location_id, 
                     overall_mean, overall_lower, overall_upper)]
dx_all[, care_setting := "overall"]
setnames(dx_all, old = c("overall_mean", "overall_lower", "overall_upper"),
         new = c("mean", "lower", "upper"))

dx_inst = dx_means[,.(year_id, location_id, 
                      inst_mean, inst_lower, inst_upper)]
dx_inst[, care_setting := "institution"]
setnames(dx_inst, old = c("inst_mean", "inst_lower", "inst_upper"),
         new = c("mean", "lower", "upper"))

dx_comm = dx_means[,.(year_id, location_id, 
                      comm_mean, comm_lower, comm_upper)]
dx_comm[, care_setting := "community"]
setnames(dx_comm, old = c("comm_mean", "comm_lower", "comm_upper"),
         new = c("mean", "lower", "upper"))

# bind all together: 
dx = rbind(dx_all, dx_inst, dx_comm)

# get location names;
locs = fread("FILEPATH/location_set_22_metadata.csv")
country_ids = locs[level == 3] # level 3 -> countries

# merge in location names to diagnosis data: 
dx_named = merge(dx, locs[,.(location_id, location_name)], by = "location_id")

# plot all countries with raw input data and save to single pdf (remember to update date): 
pdf("FILEPATH/care_setting_dx_rates_scaled_from_overall_stgpr_9.19.23.pdf") 
for(country in unique(data$location_id)){
  country_df = dx_named[location_id == country]
  p = ggplot(country_df, aes(x = year_id, y = mean, ymin = lower, ymax = upper, 
                             color = care_setting)) +
    geom_line() + geom_point() + geom_ribbon(alpha = 0.05) +
    ylim(0,1) + 
    labs(x = "Year", y = "Diagnosis Rate",
         title = paste(unique(country_df$location_name), 
                       "Diagnosis Rates by Care Setting 1990-2019")) 
  print(p)
}
dev.off()  
