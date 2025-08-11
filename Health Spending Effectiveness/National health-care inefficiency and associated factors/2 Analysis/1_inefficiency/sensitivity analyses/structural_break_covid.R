##########################################################################
### Author: USERNAME
### Date: 03/21/25
### Project: Health Spending Effectiveness 
### Purpose: Run regressions and test for a structural break at Covid years
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

# collapse to means
df <- df[, .(location_id, year_id, super_region_name, ihme_loc_id, the_pc, income_group, ineff,
             dah_pc, oop_per_the, ghe_per_the,ppp_per_the, dah_per_the,HWD, PCS_per_THE,
             effectiveness, stability, reg_quality, rule_of_law, voice, CPI_score, inpatient,
             outpatient, pharma, Hib3, MCV, SBA, ANC1, ANC4, draw)]
df_means <- df[, .(the_pc = mean(the_pc), ineff = mean(ineff),
                   dah_pc = mean(dah_pc), oop_per_the = mean(oop_per_the), 
                   ghe_per_the = mean(ghe_per_the),ppp_per_the = mean(ppp_per_the), dah_per_the = mean(dah_per_the), 
                   HWD = mean(HWD), PCS_per_THE = mean(PCS_per_THE), effectiveness = mean(effectiveness),
                   stability = mean(stability), reg_quality = mean(reg_quality), rule_of_law = mean(rule_of_law),
                   voice = mean(voice), CPI_score = mean(CPI_score), inpatient = mean(inpatient),
                   outpatient = mean(outpatient), pharma = mean(pharma), Hib3 = mean(Hib3), MCV = mean(MCV),
                   SBA = mean(SBA), ANC1 = mean(ANC1), ANC4 = mean(ANC4)), 
               by = c("location_id", "year_id", "super_region_name", "ihme_loc_id")]


# read in additional variables that were already in mean space
covar_dir = "FILEPATH"
democracy = fread(file.path(covar_dir, "democracy_index.csv"))
roads = fread(file.path(covar_dir, "road_network_data.csv"))
hhi = fread(file.path(covar_dir, "FILEPATH/dah_HHI_all_8.28.24.csv"))
social_expenditure = fread(file.path(covar_dir, "FILEPATH/social_protections_expenditure.csv"))
education_ineq = fread(file.path(covar_dir, "FILEPATH/aggregate_education_inequality_GINI_age_std.csv"))
pop_density = fread(file.path(covar_dir, "FILEPATH/population_density.csv"))
electricity_access = fread(file.path(covar_dir, "electricity_access.csv"))

hhi <- hhi[, .(location_id, year_id, hhi_all)]
education_ineq <- education_ineq[, .(location_id, year_id, educ_ineq)]
pop_density <- pop_density[, .(location_id, year_id, pop_dens)]
social_expenditure <- social_expenditure[, .(location_id, year_id, social_protection_expend)]
electricity_access <- electricity_access[, .(location_id, year_id, electricity_access)]

df_means <- merge(df_means, hhi, by = c("location_id", "year_id"), all.x = T)
df_means <- merge(df_means, social_expenditure, by = c("location_id", "year_id"), all.x = T)
df_means <- merge(df_means, education_ineq, by = c("location_id", "year_id"), all.x = T)
df_means <- merge(df_means, pop_density, by = c("location_id", "year_id"), all.x = T)
df_means <- merge(df_means, electricity_access, by = c("location_id", "year_id"), all.x = T)


# create covid indicator variable
df_means[, covid := 0]
df_means[year_id >= 2020, covid := 1]
# loop over all independent variables
# skip Democracy b/c there is only 1 covid-era data point
# skip log_density (roads) b/c there is no data after 2016
indep_vars <- c("effectiveness","stability","reg_quality","rule_of_law","voice",
                "HWD","oop_per_the","ghe_per_the","ppp_per_the",
                "dah_per_the", "inpatient", "outpatient", "pharma", "CPI_score", 
                "Hib3", "MCV", "SBA", "ANC4","hhi_all","social_protection_expend",
                "educ_ineq","pop_dens","electricity_access")

# normalize all indep_vars
df_means[, (indep_vars) := lapply(.SD,
                                  function(x){
                                    (x - mean(x, na.rm = T))/sd(x, na.rm = T) 
                                  }), # NAs in several of these variables from missing locations/years
         .SDcols = indep_vars]


all_results <- data.table(var_name = NA, intercept = NA, indep_var = NA, covid = NA, interaction = NA)

for (var in indep_vars){
  covid_model <- lm(as.formula(paste('ineff ~ ', var, ' + ',var,' * covid ')), data = df_means)
  beta_values <- transpose(as.data.frame(c(eval(var),coefficients(summary(covid_model))[,1])))
  #for beta_values, round columns V2-V5 to have 3 decimal places
  beta_values[,2:5] <- round(as.numeric(beta_values[,2:5]), digits = 3)
  p_name <- paste0("p_", var)
  p_values <- transpose(as.data.frame(c(p_name,coefficients(summary(covid_model))[,4])))
  p_values[,2:5] <- round(as.numeric(p_values[,2:5]), digits = 3)
  
  #format p_values columns 2-5 to have parentheses around the numbers
  p_values[,2:5] <- paste0("'(", p_values[,2:5], ")")
 
  all_results <- rbind(all_results, beta_values, use.names=FALSE)
  all_results <- rbind(all_results, p_values, use.names=FALSE)

}

# save out all_results
fwrite(all_results, file = paste0("FILEPATH",version,"_structural_break_results.csv"), quote = TRUE)

