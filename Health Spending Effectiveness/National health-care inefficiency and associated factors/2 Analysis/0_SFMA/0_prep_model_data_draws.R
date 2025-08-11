##########################################################################
### Author: USERNAME
### Date: 10/18/24
### Project: Health Spending Effectiveness 
### Purpose: Combine outcomes and covariate draws into one dataset
###########################################################################


# clear environment
rm(list=ls())


GBD_dir = "FILEPATH"
out_dir = "FILEPATH"
FGH_dir = "FILEPATH"

# load libraries
library(data.table)

cut_the_flag = 1

# load THE draws
THE = fread(paste0(FGH_dir, "THE_ppp23_1995_2022.csv"))

# remove Venezuela and North Korea data (unrealistic/erroenous)
THE = THE[!location_id %in% c(7, 133)] # 7 = North Korea, 133 = Venezuela


if(cut_the_flag==1){
  ## remove THEpc < 35 - extremely small THE values impacting model
  # identify which country-year combinations to remove from draw data based on means

  THE_cut = THE_means[the_pc < 35]
  table(THE_cut$location_name)
  #save THE_cut for appendix
  fwrite(THE_cut,"FILEPATH/THE_cut_under35_RTR_FGH24.csv")
  n_pre_cut = nrow(THE)
  # remove these country-year combinations from THE draws
  THE = THE[!paste(location_id, year_id) %in% paste(THE_cut$location_id, THE_cut$year_id)]
  
} 

table(THE_cut$location_name)


# reformat THE draws: 
unique(THE$draw) # draw_1 - draw_500

# the GBD data has draw_0 - draw_249
# relabel draw_250 as draw_0 in THE data
THE[draw == "draw_250", draw := "draw_0"]

# load SDI data (socio-demographic index)
sdi = fread(paste0(GBD_dir,"sdi_1995_2023.csv"))

########################################
### HALE
########################################
# initalize outcomes directory
outcome_dir = "FILEPATH"
# load HALE draws
HALE = fread(file.path(outcome_dir, 'FILEPATH/draws_1995_2024.csv'))

# melt draws to long format:
HALE_long = melt(HALE, variable.name = 'draw', value.name = 'HALE', 
                 id.vars = c('year_id', 'location_id', 'cause_id',
                             'age_group_id', 'sex_id'))

# load PAF
PAF = fread(file.path(GBD_dir, "FILEPATH/PAF_draws_all_age_1995_2023.csv"))

# load HIV
HIV = fread(file.path(GBD_dir, "FILEPATH/HIV_draws_1995_2023.csv"))


# replace zero HIV prevalence estimates with next smallest value for log-transform
min_HIV = min(HIV[HIV_prevalence > 0]$HIV_prevalence)
HIV[HIV_prevalence == 0, HIV_prevalence := min_HIV]
range(HIV$HIV_prevalence) # 1.661782e-12 2.169265e-01
# merge HALE, spending, and covariate data
HALE_df = merge(THE, HALE_long[,.(year_id, location_id, draw, HALE)], 
                by = c('year_id', 'location_id', 'draw'))
HALE_df = merge(HALE_df, sdi[,.(year_id, location_id, sdi)], 
                by = c('year_id', 'location_id'))
HALE_df = merge(HALE_df, PAF[,.(year_id, location_id, draw, PAF)],
                by = c('year_id', 'location_id', 'draw'))
HALE_df = merge(HALE_df, HIV[,.(year_id, location_id, draw, HIV_prevalence)],
                by = c('year_id', 'location_id', 'draw'))

# check that we are at 250 draws (THE starts with 500, but GBD 2023 covariates have 250)
length(unique(HALE_df$draw))

# calculate HALE variance from draws
HALE_df[, HALE_var := var(HALE), by = c('year_id', 'location_id')]

# save out HALE model data frames, one file per draw
for (d in unique(HALE_df$draw)){
  HALE_draw = HALE_df[draw == d]
  if(cut_the_flag==1){
    print(paste('Saving out HALE model input', d, 'with THE cut under 35'))
    fwrite(HALE_draw, file.path(out_dir, "FILEPATH", paste0("model_input_", d, "_THEover35_FGH24.csv")))
  } else {
    print(paste('Saving out HALE model input', d))
    fwrite(HALE_draw, file.path(out_dir, "FILEPATH", paste0("model_input_", d, "_THE_FGH24.csv")))
  }
}

########################################
### U5M
########################################

# load U5M draws
U5M = fread('FILEPATH/U5M_envelope_draws_1995_2023.csv')
unique(U5M$draw) # "draw_0" to "draw_249"

# load PAF
PAF_U5 = fread(file.path(GBD_dir, "FILEPATH/PAF_draws_age_bins_1995_2023.csv"))
PAF_U5 = PAF_U5[age_bin == "Under 5"]

# merge U5M, spending, and covariate data
U5M_df = merge(THE, U5M[,.(year_id, location_id, draw, U5M)], 
                by = c('year_id', 'location_id', 'draw'))
U5M_df = merge(U5M_df, sdi[,.(year_id, location_id, sdi)], 
                by = c('year_id', 'location_id'))
U5M_df = merge(U5M_df, PAF_U5[,.(year_id, location_id, draw, PAF)],
                by = c('year_id', 'location_id', 'draw'))

# calculate U5M variance from draws
U5M_df[, U5M_var := var(U5M), by = c('year_id', 'location_id')]

# save out U5M model data frames, one file per draw
for (d in unique(U5M_df$draw)){
  U5M_draw = U5M_df[draw == d]
  if(cut_the_flag==1){
    print(paste('Saving out U5M model input', d, 'with THE cut under 35'))
    fwrite(U5M_draw, file.path(out_dir, "FILEPATH", paste0("model_input_", d, "_THEover35_FGH24.csv")))
  } else {
    print(paste('Saving out U5M model input', d))
    fwrite(U5M_draw, file.path(out_dir, "FILEPATH", paste0("model_input_", d, "_THE_FGH24.csv")))
  }
 
}
