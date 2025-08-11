##########################################################################
### Author: USERNAME
### Date: 11/22/24
### Project: Health Spending Effectiveness 
### Purpose: Update SFMA covariate config file for new version runs
###########################################################################

# clear environment
rm(list=ls())

# load libraries
library(data.table)

# covariate config file path
config_file = 'FILEPATH/config_covs.csv'
config_df = fread(config_file)

head(config_df)
unique(config_df$Version)


## New Sensitivity Analyses with FGH 2024
# Main model specification without cutting based on THE filter
A = config_df[Version == 'S']
A$Version <- 'A'

# First submission main model specification: SDI, PAF, HIV
B = config_df[Version == 'T']
B$Version <- 'B'

# sensitivity analysis with just SDI
C = config_df[Version == 'W']
C$Version <- 'C'

# sensitivity analysis with just PAF
D = config_df[Version == 'W']
D$Version <- 'D'

# sensitivity analysis with no covariates
E = config_df[Version == '8']
E$Version <- 'E'

# rbind new version names
config_df = rbind(config_df, 
                  A,
                  B, 
                  C,
                  D, 
                  E)

# save out new config file
fwrite(config_df, config_file)
