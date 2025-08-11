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

# specify outcome & model version name to pull results from
outcome = "HALE"
# outcome = "U5M"
version = "E" 

# initalize directory paths
output_dir = "FILEPATH"

# read in HALE & U5M frontier inefficiency scores
outcome_path = file.path(output_dir, outcome, version, "pooled_frontier_draws_RTR.csv")
df = fread(outcome_path)

# flag for splitting results by income group
filter_income <- 0

if (filter_income == 1){
  df = df[income_group %in% c("H")] # High income
} else if (filter_income == 2){
  df = df[income_group %in% c("UM", "LM", "L")] # Upper middle, Lower middle & Low income
}


# read in covariates
all_covs <- fread("FILEPATH/all_covs_draw_space_RTR_FGH24.csv")

# check structure of draw variable in covariates and HALE/U5M
unique(df$draw) # draw_0 - draw_249
unique(all_covs$draw) # 1 - 500

# reformat all_covs draw labels to match frontier results
all_covs[, draw := paste0("draw_", draw - 1)]
unique(all_covs$draw) # draw_0 - draw_499

# normalize all variables to see comparable size of coefficients
indep_vars <- c("effectiveness","stability","reg_quality","rule_of_law","voice",
                "HWD","oop_per_the","ghe_per_the","ppp_per_the",
                "dah_per_the", "inpatient", "outpatient", "pharma", "CPI_score", 
                "Hib3", "MCV", "SBA", "ANC4")

all_covs[, (indep_vars) := lapply(.SD,
                                  function(x){
                                    (x - mean(x, na.rm = T))/sd(x, na.rm = T) 
                                  }), # NAs in several of these variables from missing locations/years
         .SDcols = indep_vars]

# merge covariates with inefficiency scores
keep_cols = c("year_id", "location_id", "draw", "ineff")
df <- merge(df[, ..keep_cols], all_covs, 
            by = c("year_id", "location_id", "draw"))


## run regressions
betas = run_regressions(df, "ineff", indep_vars, covars = "")

# save out coefficients
fwrite(betas,paste0("FILEPATH",
                    outcome,"_",version,"_inefficiency_regressions.csv"))

# create plot label dictionary
# Define our cross-sections
plot_label_dict <- data.table(label = c("Health worker density","Road density (km/km sq.)","Population density","Electricity access",
                                         "Skilled birth attendance","ANC coverage (4 visits)","Measles vaccination","Hib3 vaccination",
                                         "Education inequality","Government effectiveness","Government stability","Regulatory quality",
                                         "Rule of law","Voice and accountability","Government corruption","Democracy",
                                         "Social protection spending (% of total)","Pharmaceutical spending (% of total)",
                                         "Outpatient spending (% of total)","Inpatient spending (% of total)",
                                         "DAH Fragmentation","DAH spending (% of total)","OOP spending (% of total)",
                                         "Prepaid private spending (% of total)","Govt. spending (% of total)"),
                              indep_var  =  c("HWD","log_density","pop_dens","electricity_access",
                                             "SBA","ANC4","MCV","Hib3",
                                             "educ_ineq","effectiveness","stability","reg_quality",
                                             "rule_of_law","voice","CPI_score","Democracy",
                                             "social_protection_expend","pharma",
                                             "outpatient","inpatient",
                                             "hhi","dah_per_the","oop_per_the",
                                             "ppp_per_the","ghe_per_the"))

# save plot_label_dict to intermediate folder
fwrite(plot_label_dict, "FILEPATH/plot_label_dictionary.csv")

plot_label_dict <- merge(plot_label_dict,betas[,.(indep_var, nsamples)], by = "indep_var", all.x = T)
 

# set plot labels
plot_labs = c(
  paste0("Health worker density (N = ", plot_label_dict[label == "Health worker density", nsamples],")"),
  paste0("Road density (km/km sq.) (N = ", plot_label_dict[label == "Road density (km/km sq.)", nsamples],")"),
  paste0("Population density (N = ", plot_label_dict[label == "Population density", nsamples],")"),
  paste0("Electricity access (N = ", plot_label_dict[label == "Electricity access", nsamples],")"),
  "***Infrastructure***", " ",
  paste0("Skilled birth attendance (N = ", plot_label_dict[label == "Skilled birth attendance", nsamples],")"), 
  paste0("ANC coverage (4 visits) (N = ", plot_label_dict[label == "ANC coverage (4 visits)", nsamples],")"),
  paste0("Measles vaccination (N = ", plot_label_dict[label == "Measles vaccination", nsamples],")"),
  paste0("Hib3 vaccination (N = ", plot_label_dict[label == "Hib3 vaccination", nsamples],")"), 
  "***Preventive care***", " ",
  paste0("Education inequality (N = ", plot_label_dict[label == "Education inequality", nsamples],")"),
  paste0("Government effectiveness (N = ", plot_label_dict[label == "Government effectiveness", nsamples],")"),
  paste0("Government stability (N = ", plot_label_dict[label == "Government stability", nsamples],")"),
  paste0("Regulatory quality (N = ", plot_label_dict[label == "Regulatory quality", nsamples],")"),
  paste0("Rule of law (N = ", plot_label_dict[label == "Rule of law", nsamples],")"),
  paste0("Voice and accountability (N = ", plot_label_dict[label == "Voice and accountability", nsamples],")"),
  paste0("Government corruption (N = ", plot_label_dict[label == "Government corruption", nsamples],")"),
  paste0("Democracy (N = ", plot_label_dict[label == "Democracy", nsamples],")"),
  "***Governance***", " ",
  paste0("Social protection spending (% of total) (N = ", plot_label_dict[label == "Social protection spending (% of total)", nsamples],")"),
  paste0("Pharmaceutical spending (% of total) (N = ", plot_label_dict[label == "Pharmaceutical spending (% of total)", nsamples],")"),
  paste0("Outpatient spending (% of total) (N = ", plot_label_dict[label == "Outpatient spending (% of total)", nsamples],")"),
  paste0("Inpatient spending (% of total) (N = ", plot_label_dict[label == "Inpatient spending (% of total)", nsamples],")"),
  paste0("DAH Fragmentation (N = ", plot_label_dict[label == "DAH Fragmentation", nsamples],")"),
  paste0("DAH spending (% of total) (N = ", plot_label_dict[label == "DAH spending (% of total)", nsamples],")"),
  paste0("OOP spending (% of total) (N = ", plot_label_dict[label == "OOP spending (% of total)", nsamples],")"),
  paste0("Prepaid private spending (% of total) (N = ", plot_label_dict[label == "Prepaid private spending (% of total)", nsamples],")"),
  paste0("Govt. spending (% of total) (N = ", plot_label_dict[label == "Govt. spending (% of total)", nsamples],")"),
  "***Financing***")

# create plots
beta_plot = plot_effects_and_se(betas, 
                                fig_title = paste0(outcome," Inefficiency"), 
                                ymin = min(betas$beta_lower), 
                                ymax = max(betas$beta_upper), 
                                var_labels = plot_labs)
# beta_plot <- beta_plot + ggtitle("Figure 5") 
beta_plot

# save plots
figure_path = "FILEPATH"

# save out
if (filter_income == 1){
  plot_name =  paste0(version, "_high_only_HALE_policy_regressions.pdf") # High income
} else if (filter_income == 2){
  plot_name =  paste0(version, "_UM_LI_HALE_policy_regressions.pdf") # Upper middle, Lower middle & Low income
} else {
  plot_name =  paste0(version,"_",outcome, "_policy_regressions.pdf") # All income groups
}
plot_name

ggsave(file.path(figure_path, plot_name),
       beta_plot, width = 12, height = 9)

