##########################################################################
# Title:    06_model_analysis_caregiver_all_ages.R
# Author: USERNAME
# Date: 2025 03 13
# Project:  BHI - Brain Health Initiative
# Purpose : To create predicted values from mixed-effects model for 
#           caregiver manuscript
##########################################################################

# Setting up environment
# clear environment
rm(list = ls())

# packages
library(data.table)
library(ggplot2)
library(lme4)
library(splines)
library(lmerTest)
library(arrow)
library(stringr)
library(dplyr)
library(stargazer)

# functions
source("FILEPATH/get_location_metadata.R")
source('FILEPATH/currency_conversion.R')
source('FILEPATH/get_covariate_estimates.R')
source("FILEPATH/get_outputs.R")

# file paths
model_data_path <- "FILEPATH/caregiving_all_ages_covariates_model_dt.csv"
pred_data_path <- "FILEPATH/caregiver_global_all_ages_pred_dt.csv"

# GBD release ID
gbd_release_id <- 16

# add location id to
hierarchy <- get_location_metadata(
  location_set_id = 35,
  release_id = gbd_release_id
)

# Loading in Data
model_dt <- fread(model_data_path)
pred_dt <- fread(pred_data_path)[cause_name != "Other neurological disorders"]
scalar_dt <- fread(scalar_path)

# DEMOGRAPHIC INFO
lvl_3_locs <- sort(hierarchy[level == 3]$location_id)
years <- c(2000:2021)
ages <- c(8:20)
sexes <- c(1:2)
causes <- sort(unique(pred_dt$cause_name))

################################################################################
# MODEL ANALYSIS
################################################################################
# Storing universal model info
set.seed(123)

# set control to increase max iterations to 200000 and increase tolerance for singularity
control_iter <- lmerControl(optimizer = "bobyqa", 
                            optCtrl = list(maxfun = 500000),
                            check.conv.singular = .makeCC(action = "warning", tol = 1e-8))


################################################################################
# MIXED-EFFECTS MODEL
################################################################################
# OUTLIERING MODEL DT
model_dt_out <- model_dt[!care_type %in% c("Supervision", "Supervisory")]

# 2.5th quantile of data by cause
model_dt_out[, q2.5 := quantile(lg_hours_informal_care_wk, 0.025), by = cause_name]

# 97.5th quantile of data by cause
model_dt_out[, q97.5 := quantile(lg_hours_informal_care_wk, 0.975), by = cause_name]

# outliering if outside of these values
model_dt_out[, outlier_id := ifelse(lg_hours_informal_care_wk < q2.5 | lg_hours_informal_care_wk > q97.5, 1, 0), by = cause_name]

# adding column of number of rows by cause
model_dt_out[, cause_n_rows := .N, by = cause_name]

# removing outliers if there are less than 5 rows
model_dt_out[cause_n_rows <= 5, outlier_id := 0]

################################################################################
# LINEAR MODEL
################################################################################
lm1 <- lm(lg_hours_informal_care_wk ~ 0 + disability_weight + prop_pre_mortality + sdi_val,
          data = model_dt_out[outlier_id == 0])

summary(lm1)
performance::model_performance(lm1)
# AIC     |    AICc |     BIC |    R2 | R2 (adj.) |  RMSE | Sigma
# ---------------------------------------------------------------
# 273.621 | 273.966 | 284.804 | 0.957 |     0.956 | 0.725 | 0.734

# creating predicted values
pred_dt[, lm_lg_pred := predict(lm1, newdata = pred_dt, allow.new.levels = TRUE, re.form = NULL)]

# exponentiating predicted values
pred_dt[, lm_exp_pred := exp(lm_lg_pred)]

################################################################################
# MIXED-EFFECTS MODEL
################################################################################
ME_model1 <- lmerTest::lmer(lg_hours_informal_care_wk ~ 0 + disability_weight + prop_pre_mortality + sdi_val +
                              (1 | region_name/location_name),
                            REML = FALSE,
                            control = control_iter,
                            data = model_dt_out[outlier_id == 0])

summary(ME_model1)
performance::model_performance(ME_model1)
# AIC     |    AICc |     BIC | R2 (cond.) | R2 (marg.) |  RMSE | Sigma
# ---------------------------------------------------------------------
# 276.633 | 277.370 | 293.408 |            |      0.261 | 0.678 | 0.692


# Stargazer table of M2 linear model covariates
stargazer(lm1, type = "html",
          covariate.labels = c("Disability Weight", 
                               "Proportion Pre-Mortality (M)", 
                               "Sociodemographic Index"), 
          dep.var.labels = "Log Weekly Hours of Informal Care per Case", 
          out = paste0(fig_output_dir,"M2_covariates_table.html"))

# SAVING OUT MODEL DT
fwrite(model_dt_out, file = "FILEPATH/M2_model_dt_outliered.csv")

#####################################################################################################
# GT MODEL SUMMARY TABLE
#####################################################################################################

gtsummary::tbl_regression(ME_model1, tidy_fun = broom.mixed::tidy)

#####################################################################################################
# CREATE PRED DRAW VALUES
#####################################################################################################
# CREATE PRED DRAWS DT
pred_dt_draws <- copy(pred_dt)

# predict function
pred_re_function <- function(fit_mod){
  predict(fit_mod, 
          newdata = pred_dt_draws, 
          allow.new.levels = TRUE, 
          re.form = NULL) } # re.form = NULL to include random effects

# Number of draws
sims <- 500
seed_val <- 123

# bootMer
boot_mod <- bootMer(ME_model1,
                    FUN = pred_re_function,
                    type = "semiparametric",
                    nsim = sims,
                    seed = seed_val,
                    use.u = T)

# transpose bootMer outputs
boot_mod_matrix <- t(boot_mod[["t"]])

# convert to data.table
boot_mod_draws_dt <- data.table(boot_mod_matrix)

# Draw columns
draw_col_names <- paste0("Draw_", 1:sims)
colnames(boot_mod_draws_dt) <- draw_col_names

# exponentiating and reverse offsetting draw values
boot_mod_draws_dt[, (draw_col_names) := lapply(.SD, function(x) exp(x)),.SDcols = draw_col_names]

# boot_mod_draws_dt[, (draw_col_names) := lapply(.SD, function(x) x - bhi_offset_val)]
boot_mod_draws_dt[, (draw_col_names) := lapply(.SD, function(x) ifelse(x < 0, 0, x))]

# implementing 112 hr/wk ceiling on draw values
unadjusted_scaled_final_draws_dt <- copy(boot_mod_draws_dt)
boot_mod_draws_dt[, (draw_col_names) := lapply(.SD, function(x) ifelse(x > 112, 112, x))]

# adding ID row for merging
boot_mod_draws_dt[, ID := 1:nrow(boot_mod_draws_dt)]

# Creating dt shell for draws dt
final_draws_dt <- pred_dt_draws[, .(location_id, 
                                    location_name,
                                    ihme_loc_id,
                                    super_region_name,
                                    region_name,
                                    year_id, 
                                    age_group_id = 22,
                                    sex_id = 3,
                                    cause_name, 
                                    sdi_val,
                                    prop_pre_mortality,
                                    disorder_classification,
                                    disability_weight,
                                    ID = 1:nrow(boot_mod_draws_dt))]

# merging draws dt
final_draws_dt <- merge(final_draws_dt, boot_mod_draws_dt, by = "ID", all.x = T)

# Calculating upper and lower CI intervals
final_draws_dt[, `:=` (draw_mean = rowMeans(.SD, na.rm = TRUE),
                       lwr = apply(.SD, 1, function(x) quantile(x, probs = 0.025, na.rm = TRUE)),
                       uppr = apply(.SD, 1, function(x) quantile(x, probs = 0.975, na.rm = TRUE))), .SDcols = draw_col_names]

# SAVING OUT FINAL DRAWS DT
fwrite(final_draws_dt, file = "FILEPATH/M2_all_age_final_draws_dt.csv")
