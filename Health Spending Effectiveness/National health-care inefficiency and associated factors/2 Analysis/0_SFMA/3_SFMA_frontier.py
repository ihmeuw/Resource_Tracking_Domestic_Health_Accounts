# @Author: USERNAME
# @Date_code_last_updated: 04/04/2025
# @Date_data_last_updated: 04/04/2025
# @Purpose: 
# Run SFMA function on draws of THEpc vs HALE or U5M
# Fits the SFMA and then extracts health inefficiencies by country-year
# @Input_files:
# Prepped input data files containing health outcome, THEpc and covariates saved in directory
# `FILEPATH` with individual draw files named
# `model_input_draw_[draw_num]_THEover35.csv`
# @Output_files: 
# Individual draw files containing frontier fit, observed outcome values, and 
# inefficiencies saved out to the following directory: 
# `FILEPATH` 
# Outcome and version names defined in: `FILEPATH/1_jobmon_launch_script.R`
# @How_to_run: 
# Launched on cluster by Jobmon launch script in an R session using latest SciComp image
# Launch Script: `FILEPATH/1_jobmon_launch_script.R`

import argparse
import numpy as np
import pandas as pd
# Load MSCA SFMA Python package
from sfma import Data, Variable, SplineVariable, SplineGetter, SplinePriorGetter, UniformPrior, SFMAModel

# # Set up to parse jobs
parser = argparse.ArgumentParser()
parser.add_argument("--draw_num", type=int)
parser.add_argument("--outcome", type=str)
parser.add_argument("--version", type = str)
args = parser.parse_args()

# Create draw string to parallelize over
draw = f'draw_{args.draw_num}'

# load model input data
input_path = f'FILEPATH/{args.outcome}/model_input_{draw}_THEover35_FGH24.csv'

df = pd.read_csv(input_path)

# load outcome & model version specific covariates from config file
config_file = f'FILEPATH/config_covs.csv'
config_data = pd.read_csv(config_file)

subset_df = config_data[config_data['Version'] == args.version]

HALE_covs = subset_df['hale_covs'].iloc[0]
# check if HALE_covs is NA
if pd.isnull(HALE_covs):
      HALE_list = [] # no covariates used in this run
else:
      HALE_list = HALE_covs.split(",")

u5m_covs = subset_df['u5m_covs'].iloc[0]
# check if U5M_covs is NA
if pd.isnull(u5m_covs):
      u5m_list = []
else:
      u5m_list = u5m_covs.split(",")

## initialize SFMA variables
x = 'the_pc' # Total Health Expenditure per capita, main predictor + spline variable
y = args.outcome # outcome variable

# calculate standard error
df['standard_error'] = np.sqrt(df[f'{y}_var'])

# log the independent variables
log_x = 'log_the_pc'
df[log_x] = np.log(df[x])
# log the outcome variable
if y == 'HALE':
      log_y = f'log_{y}' # variable to fit model on
      df[log_y] = np.log(df[y])

elif y == 'U5M': 
      log_y = f'neg_log_{y}'
      df[log_y] = -np.log(df[y])
      # if outcome is 'U5M', we need to negate the outcome variable to fit the backtier
else: 
      raise ValueError('Outcome variable not recognized')

# calculate standard error of log(Y) using delta method: 
# SE(log Y) = se(Y)/Y
df['log_SE'] = df['standard_error'] / df[y]

# initialize list of covariates to log for the frontier model
if y == 'HALE': # covariates for HALE frontier
      covs = HALE_list

elif y == 'U5M': 
      covs = u5m_list
      # Same as HALE but without HIV prevalence
else: 
      raise ValueError('Outcome variable not recognized')

# log tansform each covariate: 
log_covs = [] # initalize empty list to store log cov names in
for cov in covs:
    df[f'log_{cov}'] = np.log(df[cov])
    log_covs.append(f'log_{cov}')

# create model object with log transformed data
data = Data(obs = log_y,
            obs_se = 'log_SE'
            )

priors = [
      SplinePriorGetter(UniformPrior(lb=0.0, ub=np.inf), order=1, size=20),
      SplinePriorGetter(UniformPrior(lb=-np.inf, ub=0.0), order=2, size=20)
      ]

variables_covs = [
      SplineVariable(log_x,
                     spline=SplineGetter(knots=np.linspace(0.0, 1.0, 7),
                                         degree=2,
                                         #r_linear=True,
                                         #l_linear=True,
                                         knots_type="rel_domain",
                                         include_first_basis=True),
                                         priors=priors)
                                         ] + [Variable(name) for name in log_covs]

model = SFMAModel(data, variables_covs)
model.attach(df)

# fit model with covariates
model.fit(verbose=False, outlier_pct=0.05,
          eta_options={"method": "bounded", "bounds": [0.0, 100.0]}
          )

## Adjust outcome variables by removing covariate impact
# make predictions using model with covariates
Y_hat = model.predict(df)
# Outcome variable:
Y = df[log_y].values
# Prediction excluding the impact of the covariates: 
df_null = df.copy()
# set all covariates to 2021 mean in df copy
for cov in log_covs:
      var_mean21 = df.loc[df['year_id'] == 2021, cov].mean()
      df_null[cov] = var_mean21
# make predictions using the mean covariate values (df_null)
Y_hat_null = model.predict(df_null)
# Impact of the covariates: 
cov_hat = Y_hat - Y_hat_null
# Now remove the impact of the covariates from the outcome variable
Y_adj = Y - cov_hat

# add columns to dataframe: 
if y == 'HALE': 
      df[f'log_{y}_adj'] = Y_adj
      df['log_pred_null'] = Y_hat_null

elif y == 'U5M': 
      df[f'log_{y}_adj'] = -Y_adj
      df['log_pred_null'] = -Y_hat_null
      # negate for U5M because of backtier
else: 
      raise ValueError('Outcome variable not recognized')

# save outliers for plotting
df['outliers'] = model.weights == 0.0

## Calculate inefficiencies
df['log_ineff'] = model.get_inefficiency()
Y_ineff = Y_hat_null - model.get_inefficiency() # outcome estimate in log space adjusted by inefficiency score
# add columns to dataframe and calculate inefficiency scores in level space
if y == 'HALE': 
      df[f'log_{y}_ineff'] = Y_ineff # frontier prediciion subtracted by inefficiency score
      df['ineff'] = np.maximum(0, np.exp(df['log_pred_null']) - np.exp(df[f'log_{y}_ineff'])) # new method, level space

else:
      df[f'log_{y}_ineff'] = -Y_ineff # frontier prediciion subtracted by inefficiency score
      df['ineff'] = np.maximum(0, np.exp(df[f'log_{y}_ineff']) - np.exp(df['log_pred_null'])) # new method, level space 
      # negate for U5M because of backtier

# calculate slope at each THEpc value 
# dy/dx = (y/x) * dmat.dot(beta) 
if y == 'HALE': 
      df[f'{y}_pred'] = np.exp(df['log_pred_null']) # get the predicted outcome to calculate slope with
      df['slope'] = (df[f'{y}_pred'] / df['the_pc'])*variables_covs[0].spline.design_dmat(df['log_the_pc'], 1).dot(model.get_beta_dict()['log_the_pc'])

else:
      df[f'{y}_pred'] = np.exp(df['log_pred_null'])
      df['slope'] = -(df[f'{y}_pred'] / df['the_pc'])*variables_covs[0].spline.design_dmat(df['log_the_pc'], 1).dot(model.get_beta_dict()['log_the_pc'])
      # negate for U5M because of backtier

# save out draw results data frame with frontier predictions, inefficiency scores and slopes
output_dir = f'FILEPATH/{args.outcome}/{args.version}/result_draws/'
df.to_csv(f"{output_dir}pooled_frontier_ineffs_{draw}.csv", index=False)

# save out the frontier coefficients for each variable
coef_df = pd.DataFrame()
for key in model.get_beta_dict():
      if key in ['log_the_pc']: 
            for i in range(len(model.get_beta_dict()[key])):
                  # save each the_pc spline coefficient to individual columns
                  coef_df[key+str(i)] = model.get_beta_dict()[key][[i]] 
      else:
            coef_df[key] = model.get_beta_dict()[key]

# save out the frontier model coefficients
coef_df.to_csv(f"{output_dir}model_betas_{draw}.csv", index=False)

# create data frame with a 'grid' of the_pc values to predict on / get slope values for
the_values = list(np.linspace(10, 90, 9)) + list(np.linspace(100, 10000, 100)) + [10500.0, 11000.0, 11500.0, 12000.0] 
# 10 - 90 by 10, 100 - 10,000 by 100, 10,000 - 12,000 by 500 
slope_df = pd.DataFrame({'the_pc': the_values})
slope_df['log_the_pc'] = np.log(slope_df['the_pc'])
# create covariate columns using the 2021 mean values (for the covariate adjusted prediction)
for cov in log_covs:
      var_mean21 = df.loc[df['year_id'] == 2021, cov].mean()
      slope_df[cov] = var_mean21

# make predictions on the grid of the_pc values and calculate slopes
if y == 'HALE': 
      slope_df['log_pred'] = model.predict(slope_df)
      slope_df[f'{y}_pred'] = np.exp(slope_df['log_pred'])
      slope_df['slope'] = (slope_df[f'{y}_pred'] / slope_df['the_pc'])*variables_covs[0].spline.design_dmat(slope_df['log_the_pc'], 1).dot(model.get_beta_dict()['log_the_pc'])

else:
      slope_df['log_pred'] = -model.predict(slope_df)
      slope_df[f'{y}_pred'] = np.exp(slope_df['log_pred'])
      slope_df['slope'] = -(slope_df[f'{y}_pred'] / slope_df['the_pc'])*variables_covs[0].spline.design_dmat(slope_df['log_the_pc'], 1).dot(model.get_beta_dict()['log_the_pc'])
      # negate for U5M because of backtier

# save out slope results
slope_df.to_csv(f"{output_dir}THE_grid_slopes_{draw}.csv", index=False)
