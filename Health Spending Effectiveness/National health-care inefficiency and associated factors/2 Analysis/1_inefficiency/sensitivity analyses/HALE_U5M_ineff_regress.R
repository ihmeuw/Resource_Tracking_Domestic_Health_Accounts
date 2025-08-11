##########################################################################
### Author: USERNAME
### Date: 10/08/2024
### Project: Health Spending Effectiveness 
### Purpose: Investigate relationship between DALY inefficiency scores
###          and various covariates
##########################################################################

rm(list=ls())

# set seed for reproducible results for the beta draws
set.seed(123)

# load libraries
library(data.table)
library(ggplot2)
library(MASS)
library(forcats) # for making plot labels ordered
library(RColorBrewer)

# set # of threads for data.table
setDTthreads(2)

# load regression helper functions
source('FILEPATH/regression_helpers.R')

# specify model version name and outcome (each sensitivity analysis corresponds to one outcome)
version = "O"
outcome = "U5M"  

# initalize directory paths
output_dir = "FILEPATH"

# read in HALE or U5M frontier inefficiency scores
if(outcome=="HALE"){
  HALE_path = file.path(output_dir, paste0("FILEPATH/", version, "/pooled_frontier_draws.csv"))
  HALE = fread(HALE_path)
} else{
  U5M_path = file.path(output_dir, paste0("FILEPATH/", version, "/pooled_frontier_draws.csv"))
  U5M = fread(U5M_path)
}

# read in covariates
all_covs <- fread("FILEPATH/all_covs_draw_space.csv")


# reformat all_covs draw labels to match frontier results data
all_covs[, draw := paste0("draw_", draw - 1)]
unique(all_covs$draw) # draw_0 - draw_499

# normalize all variables to see comparable size of coefficients
indep_vars <- c("effectiveness","stability","reg_quality","rule_of_law","voice",
                "HWD","oop_per_the","ghe_per_the","prepaid_per_the",
                "dah_per_the","dah_pc", "inpatient", "outpatient", "pharma", "CPI_score", 
                "Hib3", "MCV", "SBA", "ANC1", "ANC4")

all_covs[, (indep_vars) := lapply(.SD,
                                  function(x){
                                    (x - mean(x, na.rm = T))/sd(x, na.rm = T)
                                  }), # NAs in several of these variables from missing locations/years
         .SDcols = indep_vars]

# merge covariates with inefficiency scores
keep_cols = c("year_id", "location_id", "draw", "ineff")
if(outcome=="HALE"){
  HALE <- merge(HALE[, ..keep_cols], all_covs, 
                by = c("year_id", "location_id", "draw"))
} else{
  U5M <- merge(U5M[, ..keep_cols], all_covs, 
               by = c("year_id", "location_id", "draw"))
}


# define function to run regressions on frontier inefficiencies with each independent variable 
run_regressions <- function(ineff_df, ineff_var, indep_var_list, covars){
  # initialize empty data.table to store results
  all_results <- data.table()
  for (ind_var in indep_var_list){
    print(ind_var) # status update
    if (ind_var == "CPI_score"){
      input_df = ineff_df[!is.na(CPI_score)] # corruption data was missing some location-years
    } else {
      input_df = ineff_df
    }
    temp_result = draw_space_results(model_type = "year", 
                                     # num_draws = 500, 
                                     dep_var = ineff_var,
                                     indep_var = ind_var, 
                                     global_data = input_df, 
                                     covs = covars)
    all_results = rbind(all_results, temp_result)
  }
  
  # add results for variables that are not in draw space
  all_results = rbind(all_results, 
                      lm_policy("hhi", ineff_df, ineff_var), # DAH fragmentation
                      lm_policy("Democracy", ineff_df, ineff_var),
                      lm_policy("pct_women_in_govt", ineff_df, ineff_var), 
                      lm_policy("PHC_per_CHE", ineff_df, ineff_var), # WHO Primary Care
                      lm_policy("log_density", ineff_df, ineff_var), # Road density
                      lm_policy("social_protection_expend", ineff_df, ineff_var),
                      lm_policy("educ_ineq", ineff_df, ineff_var), # Female Education Inequality (Gini)
                      lm_policy("pop_dens", ineff_df, ineff_var), # Population Density
                      lm_policy("electricity_access", ineff_df, ineff_var))
  
  return(all_results)
}

## run regressions
if(outcome=="HALE"){
  betas = run_regressions(HALE, "ineff", indep_vars, covars = "")
} else{
  betas = run_regressions(U5M, "ineff", indep_vars, covars = "")
}

# set plot labels
plot_labs = c("Gov. effectiveness","Gov. stability",
              "Regulatory quality","Rule of law",
              "Voice and accountability",
              "Health worker density","OOP per THE", "GHE per THE", 
              "PPP per THE", "DAH per THE","DAH per capita",
              "% Inpatient", "% Outpatient", "% Pharmaceutical",
              "Corruption", "Hib3 Vaccination", "Measles Vaccination",
              "Skilled Birth Attendance", 
              "ANC Coverage (1 visit)", "ANC Coverage (4 visits)",
              "DAH Fragmentation", "Democracy",
              "% Women in Parliament", 
              "WHO Primary Care",
              "Road density (km/km sq.)", 
              "IMF Social Protection", 
              "Female Education Inequality (Gini)", 
              "Population Density", "Electricity Access")


# create plots
reg_plot = plot_effects_and_se(betas, 
                                fig_title = paste0(outcome," Inefficiency"), 
                                ymin = min(betas$beta_lower), 
                                ymax = max(betas$beta_upper), 
                                var_labels = plot_labs)
reg_plot


# save plot
figure_path = "FILEPATH"

if(outcome=="HALE"){
  ggsave(paste0(figure_path, version,"_HALE_ineff_effects.png"),
         reg_plot, width = 12, height = 8)
} else {
  ggsave(paste0(figure_path, version, "_U5M_ineff_effects.png"),
         reg_plot, width = 12, height = 8)
}

