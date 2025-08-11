##########################################################################
### Author: USERNAME
### Date: 03/18/25
### Project: Health Spending Effectiveness 
### Purpose: Run regressions for each income group
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
source(paste0(functions_dir,"get_location_metadata.R"))


# specify outcome & model version name to pull results from
outcome = "HALE"
version = "T"

# initalize directory paths
output_dir = "FILEPATH"

# read in HALE & U5M frontier inefficiency scores
outcome_path = file.path(output_dir, outcome, version, "pooled_frontier_draws_RTR.csv")
df = fread(outcome_path)

# read in covariates
all_covs <- fread("FILEPATH/all_covs_draw_space_RTR.csv")

# reformat all_covs draw labels to match frontier results data
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

ctryFE = 1 # set to 1 to run models with country fixed-effects


##################################
####  WB INCOME GROUPS 
#################################

ctryFE = 0
for(loc in unique(df$income_group)){
  
  #loc <- "South Asia"
  #loc <- "Sub-Saharan Africa"
  #loc <- "High-income"
  print(paste0("Running regressions for ", loc))
  df_wb_region <- copy(df)
  df_wb_region <- df_wb_region[income_group==loc,]
  
  
  
  ## run regressions
  betas = run_regressions(df_wb_region, "ineff", indep_vars, covars = "", countryFE=ctryFE)
  
  # save out coefficients
  if(ctryFE==1){
    fwrite(betas,paste0("FILEPATH",
                        outcome,"_",version,"_inefficiency_regressions_",loc,"_countryFE.csv"))
  }else{
    fwrite(betas,paste0("FILEPATH",
                        outcome,"_",version,"_inefficiency_regressions_",loc,".csv"))
  }
  
  
  plot_label_dict <- fread("FILEPATH/plot_label_dictionary.csv")
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
  
  
  if(loc=="H"){
    plot_labs <- plot_labs[!grepl("DAH", plot_labs)]
    betas <- betas[indep_var != "dah_per_the",]
    betas <- betas[indep_var != "hhi",]
  }
  
  # create plots
  beta_plot = plot_effects_and_se(betas, 
                                  fig_title = paste0("HALE Inefficiency - ", loc), 
                                  ymin = min(betas$beta_lower), 
                                  ymax = max(betas$beta_upper), 
                                  var_labels = plot_labs)
  # save plots
  figure_path = "FILEPATH"
  if(ctryFE==1){
    plot_name =  paste0(version, "_HALE_policy_regressions_",loc,"_countryFE.pdf") # All income groups
  }else{
    plot_name =  paste0(version, "_HALE_policy_regressions_",loc,".pdf") # All income groups
    
  }
  
  ggsave(file.path(figure_path, plot_name),
         beta_plot, width = 12, height = 9)
  
}



##################################
####  time-based subgroups
#################################

# first, 1995-2009
ctryFE = 0

df_subset <- copy(df)
df_subset <- df_subset[year_id < 2010,]
  
## run regressions
betas = run_regressions(df_subset, "ineff", indep_vars, covars = "", countryFE=ctryFE)
  
# save out coefficients
if(ctryFE==1){
    fwrite(betas,paste0("FILEPATH",
                        outcome,"_",version,"_inefficiency_regressions_through2009_countryFE.csv"))
}else{
    fwrite(betas,paste0("FILEPATH",
                        outcome,"_",version,"_inefficiency_regressions_through2009.csv"))
}
  
  
  plot_label_dict <- fread("FILEPATH/plot_label_dictionary.csv")
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
                                  fig_title = "HALE Inefficiency 1995-2009", 
                                  ymin = min(betas$beta_lower), 
                                  ymax = max(betas$beta_upper), 
                                  var_labels = plot_labs)
  # save plots
  figure_path = "FILEPATH"
  if(ctryFE==1){
    plot_name =  paste0(version, "_HALE_policy_regressions_through2009_countryFE.pdf") # All income groups
  }else{
    plot_name =  paste0(version, "_HALE_policy_regressions_through2009.pdf") # All income groups
    
  }
  
  ggsave(file.path(figure_path, plot_name),
         beta_plot, width = 12, height = 9)
  

  
  ######  second, 2010-2022
  
  df_subset <- copy(df)
  df_subset <- df_subset[(year_id >= 2010) & year_id < 2020,]
  
  ## run regressions
  betas = run_regressions(df_subset, "ineff", indep_vars, covars = "", countryFE=ctryFE)
  
  # save out coefficients
  if(ctryFE==1){
    fwrite(betas,paste0("FILEPATH",
                        outcome,"_",version,"_inefficiency_regressions_2010_2019_countryFE.csv"))
  }else{
    fwrite(betas,paste0("FILEPATH",
                        outcome,"_",version,"_inefficiency_regressions_2010_2019.csv"))
  }


  plot_label_dict <- fread("FILEPATH/plot_label_dictionary.csv")
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
                                  fig_title = "HALE Inefficiency 2010-2022", 
                                  ymin = min(betas$beta_lower), 
                                  ymax = max(betas$beta_upper), 
                                  var_labels = plot_labs)
  # save plots
  figure_path = "FILEPATH"
  if(ctryFE==1){
    plot_name =  paste0(version, "_HALE_policy_regressions_2010_2019_countryFE.pdf") # All income groups
  }else{
    plot_name =  paste0(version, "_HALE_policy_regressions_2010_2022.pdf") # All income groups
    
  }
  
  ggsave(file.path(figure_path, plot_name),
         beta_plot, width = 12, height = 9)
  

