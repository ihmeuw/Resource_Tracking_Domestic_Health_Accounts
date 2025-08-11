### Functions to help with running inefficiency regression analyses
### Authors: USERNAME
### Date: 08/22/2024

## Function to run inefficiency regression with policy variables that we do not have draws for
lm_policy <- function(policy_var, ineff_df, ineff_var,ctryFE) {
  set.seed(123)
  covar_dir = "FILEPATH"
  # get data file path based on policy_var name
  if(policy_var == "Democracy"){
    print("Running regression on Democracy")
    df = fread(file.path(covar_dir, "democracy_index.csv"))
  } else if(policy_var == "pct_women_in_govt") {
    print("Running regression on Percent Women in Parliament")
    df = fread(file.path(covar_dir, "women_in_government.csv"))
  } else if(policy_var == "PHC_per_CHE") {
    print("Running regression on WHO Primary Care")
    df = fread(file.path(covar_dir, "WHO_primary_care.csv"))
  } else if(policy_var == "log_density") {
    print("Running regression on Road Networks")
    df = fread(file.path(covar_dir, "road_network_data.csv"))
  } else if(policy_var == "hhi") {
    print("Running regression on DAH Fragmentation")
    df = fread(file.path(covar_dir, "FILEPATH/dah_HHI_all_8.28.24.csv"))
    setnames(df,"hhi_all","hhi")
    # reverse directionality of fragmentation (HHI)
    df[, hhi := 10000 - hhi]
  } else if(policy_var == "social_protection_expend") {
    print("Running regression on Social Protection Expenditure")
    df = fread(file.path(covar_dir, "FILEPATH/social_protections_expenditure.csv"))
  } else if(policy_var == "educ_ineq") {
    print("Running regression on Aggregate Education Relative Inequality (GINI)")
    df = fread(file.path(covar_dir, "FILEPATH/aggregate_education_inequality_GINI_age_std.csv"))
  } else if(policy_var == "educ_ineq_f") {
    print("Running regression on Female Education Relative Inequality (GINI)")
    df = fread(file.path(covar_dir, "FILEPATH/female_education_inequality_GINI_age_std.csv"))
    setnames(df, old = "educ_ineq", new = "educ_ineq_f")
  } else if(policy_var == "pop_dens") {
    print("Running regression on Population Density")
    df = fread(file.path(covar_dir, "FILEPATH/population_density.csv"))
  } else if(policy_var == "electricity_access") {
    print("Running regression on Electricity Access")
    df = fread(file.path(covar_dir, "electricity_access.csv"))
  } else { 
    stop("Unknown policy variable")
  }
  # merge policy variable data with frontier inefficiency scores
  keep_cols = c("year_id", "location_id", policy_var)
  df = merge(ineff_df, df[, ..keep_cols], by = c("year_id", "location_id"))
  
  # normalize policy variable
  normalize_var_list <- c(policy_var)
  print(paste0("Normalize var list ", normalize_var_list))
  df[, (normalize_var_list) := lapply(.SD, function(x){
    (x - mean(x, na.rm = T))/sd(x, na.rm = T)
    }), .SDcols = normalize_var_list]
  #print(paste0("End of normalizing ", normalize_var_list))
  # set up empty lists for results
  beta_list <- list()
  intercept_list <- list()
  rsq_list <- list()
  # for each draw, run a regression of independent var on dependent var
  for (dr in unique(df$draw)) {
    errorFlag <- 0
    # for models with country fixed-effects, we can run into an error calling lm() because the location_id
    # might only have 1 value, so the factor() function will not work
      tryCatch(
        if(ctryFE==1){
          model <- lm(as.formula(paste(ineff_var, '~', policy_var,' + factor(year_id) + factor(location_id)')), 
                data = df[draw == dr])
        }else{
          model <- lm(as.formula(paste(ineff_var, '~', policy_var,' + factor(year_id)')), 
                    data = df[draw == dr])
        }
    , error = function(e) {
      print(paste0("Error in LM regression for ", policy_var, " on draw ", dr))
      errorFlag <<- 1
      print(e)
      #next
    }) # end tryCatch
    if(errorFlag==0){
      vcov_mat <- sandwich::vcovHC(model, type = "HC1") 
      beta_means <- coefficients(summary(model))[, 1]
    }
    # set beta_draws to NA values; try to upate in the tryCatch block
    # beta_draws <- matrix(NA, nrow = 10, ncol = length(beta_means))
    # exit and return NA if the beta_draws does not work
   
    tryCatch(
      beta_draws <- MASS::mvrnorm(10, beta_means, vcov_mat) 
      , error = function(e) {
          print(paste0("Error in regression for ", policy_var, " on draw ", dr))
          errorFlag <<- 1
          print(e)
          #next
         }
    )# end tryCatch
    if(errorFlag==0){
      rc__beta <- beta_draws[, policy_var] #reference the column by name
      intercept_temp <- beta_draws[, "(Intercept)"]
      beta_list <- c(beta_list,rc__beta)
      intercept_list <- c(intercept_list, intercept_temp)
      rsq_list <- c(rsq_list, summary(model)$r.squared)
    }
  }
    if(errorFlag==1){
      return(NA)
    }
      betas <- unlist(beta_list)
      #print(paste0("head betas ",head(betas)))
      intercept <- unlist(intercept_list)
      coef_dt <- data.table(model_type = "year",
                        dep_var = ineff_var,
                        indep_var = policy_var,
                        constant = mean(intercept),
                        constant_std_err = sd(intercept)/sqrt(nrow(df[draw==dr])),
                        beta = mean(betas),
                        beta_lower = quantile(betas, probs = c(0.025)),
                        beta_upper = quantile(betas, probs = c(0.975)),
                        beta_std_err = sd(betas)/sqrt(nrow(df[draw == dr])),
                        rsq = mean(unlist(rsq_list)))
      coef_dt[, sig := ifelse(beta_lower<0 & beta_upper<0, '-1',
                          ifelse(beta_lower>0 & beta_upper>0, '1', '0'))]
      coef_dt[, nsamples := nrow(df[!is.na(policy_var) & (draw == dr),])]
  
  print(paste0("Finishing regression on ",policy_var))
  return(coef_dt)
}

# function to return results from draw space:
#    global_data needs to have a column 'draw' that is the draw number
draw_space_results <- function(model_type, dep_var, indep_var, global_data, covs, ctryFE) {
  # set up empty lists for results
  beta_list <- list()
  intercept_list <- list()
  rsq_list <- list()
  # for each draw, run a regression of independent var on dependent var
  for (dr in unique(global_data$draw)) {
    if(model_type == "year"){
      if(covs == ""){
        if(ctryFE==1){
          model <- lm(as.formula(paste(dep_var, '~', indep_var,' + factor(year_id) + factor(location_id)')), global_data[draw == dr,])
        }else{
          model <- lm(as.formula(paste(dep_var, '~', indep_var,' + factor(year_id)')), global_data[draw == dr,])
        }
              }else{
        model <- lm(as.formula(paste(dep_var, '~', indep_var,' + factor(year_id) + ',covs)), global_data[draw == dr,])
      }
    }else{
      print ("unknown model type")
    }
    vcov_mat <- sandwich::vcovHC(model, type = "HC1") 
    beta_means <- coefficients(summary(model))[, 1]
    beta_draws <- MASS::mvrnorm(10, beta_means, vcov_mat) 
    # beta_draws <- mvtnorm::rmvnorm(10, beta_means, vcov_mat, method = "chol")
    rc__beta <- beta_draws[, indep_var] #reference the column by name
    intercept_temp <- beta_draws[, "(Intercept)"]
    beta_list <- c(beta_list,rc__beta)
    intercept_list <- c(intercept_list, intercept_temp)
    rsq_list <- c(rsq_list, summary(model)$r.squared)
  }
  
  betas <- unlist(beta_list)
  if(sum(is.na(betas)) > 0){
    print(paste0("NAs in betas for ", indep_var))
    intercept <- unlist(intercept_list)
    coef_dt <- data.table(model_type = model_type,
                          dep_var = dep_var,
                          indep_var = indep_var,
                          constant = 0,
                          constant_std_err = 0,
                          beta = 0,
                          beta_lower = 0,
                          beta_upper = 0,
                          beta_std_err = 0,
                          rsq = 0)
    coef_dt[, sig := ifelse(beta_lower<0 & beta_upper<0, '-1',
                            ifelse(beta_lower>0 & beta_upper>0, '1', '0'))]
    
    coef_dt[, nsamples := nrow(global_data[!is.na(indep_var) & (draw == dr),])]
  }else{
    intercept <- unlist(intercept_list)
    coef_dt <- data.table(model_type = model_type,
                        dep_var = dep_var,
                        indep_var = indep_var,
                        constant = mean(intercept),
                        constant_std_err = sd(intercept)/sqrt(nrow(global_data[draw==dr])),
                        beta = mean(betas),
                        beta_lower = quantile(betas, probs = c(0.025)),
                        beta_upper = quantile(betas, probs = c(0.975)),
                        beta_std_err = sd(betas)/sqrt(nrow(global_data[draw == dr])),
                        rsq = mean(unlist(rsq_list)))
    coef_dt[, sig := ifelse(beta_lower<0 & beta_upper<0, '-1',
                          ifelse(beta_lower>0 & beta_upper>0, '1', '0'))]
  
    coef_dt[, nsamples := nrow(global_data[!is.na(indep_var) & (draw == dr),])]
  }
  return(coef_dt)
}

# Function to create forest plot of regression coefficients
# for the inefficiency regressions, positive values are red because more
# inefficiency is worse 
plot_effects_and_se <- function(df, fig_title, ymin, ymax, var_labels){
  
  # add empty variables for section headers
  header_names = c("Financing", "", 
                   "Governance", " ", 
                   "Preventive Care", "  ", 
                   "Structural")
  
  for (name in header_names){
    header_row = data.table(model_type = "", 
                            dep_var = "ineff",
                            indep_var = name,
                            constant = NA,
                            constant_std_err = NA,
                            beta = NA, 
                            beta_lower = NA, 
                            beta_upper = NA,
                            beta_std_err = NA,
                            rsq = NA,
                            sig = "",
                            nsamples = NA)
    df = rbind(df,
               header_row)
  }
  
  new_levels = c("HWD", "log_density", "pop_dens", "electricity_access",
                 "Structural", "  ",
                 "SBA", "ANC4", "MCV", "Hib3",
                 "Preventive Care", " ",
                 "educ_ineq", "effectiveness", "stability", "reg_quality",
                 "rule_of_law", "voice", "CPI_score", "Democracy",
                 "Governance", "", 
                 "social_protection_expend", "pharma", "outpatient", "inpatient", 
                 "hhi","dah_per_the","oop_per_the","ppp_per_the","ghe_per_the",
                 "Financing")
  
  # factor indep_var to control label order
  df[, indep_var := factor(indep_var, levels = new_levels)]
  
  red <- brewer.pal(3, 'Set1')[1]
  green <- brewer.pal(3, 'Set1')[3]
  black <- '#000000'
  
  x_axis_label <- "Change in inefficiency associated with 1 SD increase"
  
  p <- ggplot(df, aes(x=indep_var, y=beta, color=factor(sig))) +
    geom_hline(yintercept = 0, color='firebrick') +
    geom_point() +
    geom_errorbar(aes(ymin=beta_lower, ymax=beta_upper), width=0) + 
    scale_color_manual("", breaks=c("-1","0","1"), values=c(green, black, red, black)) +
    coord_flip() +
    scale_x_discrete(name="", labels = var_labels) +
    scale_y_continuous(name=x_axis_label, limits = c(ymin, ymax), 
                       labels = function(x) format(x, scientific = FALSE)) +
    # ggtitle(fig_title) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor.x = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_blank(),
          legend.position = "none",
          plot.title = element_text(hjust = 1, size = 16),
          axis.title.x = element_text(size = 16,
                                      margin = margin(t = 7.5, r = 0, b = 0, l = 0)),
          axis.text.x = element_text(size = 14),
          axis.text.y = ggtext::element_markdown(size = 16, hjust = 0),
          plot.margin = margin(0.5, 1, , , "cm"))
  #plot.title.position = "plot")
  return(p)
}


# define function to run regressions on frontier inefficiencies with each independent variable 
run_regressions <- function(ineff_df, ineff_var, indep_var_list, covars, countryFE=0){
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
                                     dep_var = ineff_var,
                                     indep_var = ind_var, 
                                     global_data = input_df, 
                                     covs = covars,
                                     ctryFE = countryFE)
    all_results = rbind(all_results, temp_result)
  }
  
  # add results for variables that are not in draw space
  all_results = rbind(all_results, 
                      lm_policy("hhi", ineff_df, ineff_var,ctryFE = countryFE), # DAH fragmentation
                      lm_policy("Democracy", ineff_df, ineff_var,ctryFE = countryFE),
                      lm_policy("log_density", ineff_df, ineff_var,ctryFE = countryFE) # Road density
  )
  sp <- lm_policy("social_protection_expend", ineff_df, ineff_var,ctryFE = countryFE)
  if(length(sp)>1){
    all_results = rbind(all_results, sp)
  }
  all_results = rbind(all_results,        
                      lm_policy("educ_ineq", ineff_df, ineff_var,ctryFE = countryFE), # Aggregate Education Inequality (Gini)
                      lm_policy("pop_dens", ineff_df, ineff_var,ctryFE = countryFE), # Population Density
                      lm_policy("electricity_access", ineff_df, ineff_var,ctryFE = countryFE)
  )
  
  return(all_results)
}

