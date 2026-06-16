
# Estimating Domestic Expenditures on Maternal Health and Family Planning


We are interested in estimating domestic expenditures on maternal health and family planning.  
We are primarily interested in public (i.e., government) expenditures and out-of-pocket expenditures on these health focus areas.  

We focus on producing estimates for UNFPA priority countries, which makes up most LMICs.  

Terminology:  

- OOP: Out-of-pocket expenditures  
- Public: Public/government expenditures  
- RH: Reproductive health  
- MH: Maternal health  
- FP: Family planning  
- Dis2: Reproductive health (based on the coding used in SHA2011) 
- Dis2.1: Maternal health (based on the coding used in SHA2011)  
- Dis2.3: Family planning (based on the coding used in SHA2011)  
- CM: Contraceptive Method(s)  
- Repr. pop: reproductive population  


File structure:  

- `1_rh_extractions`: processing of reproductive health extractions from all data sources.  
- `2_oop_prep`: calculation of components needed for the price-volume approach used to estimate OOP spending on maternal health and family planning.  
- `3_stgpr_phase1`: ST-GPR Phase 1 — models public spending on maternal health and family planning as well as utilization estimates needed for price-volume approach. 
- `4_finalize_public`: finalizes public spending estimates and prepares outputs.
- `5_oop_estimates`: calculates OOP spending estimates using the price-volume approach.  
- `6_stgpr_phase2`: ST-GPR Phase 2 — models OOP spending on maternal health.  
- `7_oop_finalize`: finalizes OOP estimates and prepares outputs.  


## Public spending on maternal health  

**Nickname:** public_dis2.1  

**Data sources:**  

- National Health Accounts (NHAs)  
- NIDI surveys  

**Estimation pipeline:**  
ST-GPR Phase 1 modeling of the proportion of public_dis2.1 out of total public expenditures (which is pulled from the Economic Indicator team's estimates of government health spending (GHES)).  

**Output:**  
Country-level estimates of public spending on maternal health as a proportion of total public health expenditures; multiplied by GHES to produce absolute spending estimates.




## Public spending on family planning

**Nickname:** public_dis2.3  

**Data sources:**  

- National Health Accounts (NHAs)
- NIDI surveys  
- Family Planning Spending Assessments (FPSAs)  

**Estimation pipeline:**  
ST-GPR Phase 1 modeling of the proportion of public_dis2.3 out of total public expenditures (which is pulled from the Economic Indicator team's estimates of government health spending (GHES)).  

**Output:**  
Country-level estimates of public spending on family planning as a proportion of total public health expenditures; multiplied by GHES to produce absolute spending estimates.


## Out-of-pocket spending on maternal health

**Nickname:** oop_mh (aggregated across service types)

**Data sources:**

- Demographic Health Surveys (DHS)  
- Literature review on antenatal care, facility delivery, and postnatal care costs  

**Estimation pipeline:**  
Price-volume approach using maternal health service utilization data (from DHS, modeled by ST-GPR Phase 1 for imputation) and service-specific OOP costs from literature.  
ST-GPR Phase 2 then models the proportion of total OOP spending that is attributable to maternal health services across countries and years.  

**Output:**  
Country-level estimates of out-of-pocket spending on maternal health services as a proportion of total OOP spending; multiplied by total OOP denominator to produce absolute spending estimates.


## Out-of-pocket spending on family planning

**Nickname:** oop_dis2.3  

**Data sources:**  

- Demographic Health Surveys (DHS)  
- Reproductive Health Supply Coalition (RHSC) reports (CGA 2019, LEAP 2020)  

**Estimation pipeline:**  
Price-volume approach using contraceptive use data from DHS (modeled by ST-GPR Phase 1 for imputation) and contraceptive OOP costs from RHSC reports.  

**Output:**  
Country-level estimates of out-of-pocket spending on family planning as a proportion of total OOP spending; multiplied by total OOP denominator to produce absolute spending estimates.


## Pipeline Details

### Overall Pipeline Structure

The pipeline has two distinct ST-GPR modeling phases:

- **Phase 1** (directory: `3_stgpr_phase1`): Models public spending on maternal health and family planning. Input data from NHAs, NIDI surveys, and FPSAs. Also produces intermediate products (utilization estimates) used for OOP estimation.

- **Phase 2** (directory: `6_stgpr_phase2`): Models out-of-pocket spending on maternal health, using inputs from the price-volume approach calculations in `5_oop_estimates`.

### General ST-GPR Pipeline 

The ST-GPR pipeline is built generically so that it can be used to model all the different variables needed for the estimation of domestic expenditures on maternal health and family planning.  

Recall, ST-GPR is a 3-stage model where we first fit some model to the data (usually a linear model, denoted the "linear prior"), then smooth the residuals across space and time before adding them back to the linear prior, and then use this smoothed prediction as a prior mean function in a Gaussian process regression.    

The steps in the pipeline here are:  

0. Data preparation: pulls in all the (pre-processed) data that will be used for input into different models and combines them into one data frame. A column called `value_id` specifies the modeled variable.  

1. Outlier detection and time-trend variance calculation: runs an outlier detection algorithm for each modeled variable to flag observations as outliers by updating the `is_outlier` column of the ST-GPR input data. Also computes a time-trend based variance for each observation to be used as the `variance` column in the ST-GPR input.  

2. Stage 1 model selection: runs a model selection process for each modeled variable to select covariates for the stage 1 of the ST-GPR model. The covariates are saved to be used in the next step.  

3. Run ST-GPR: runs the ST-GPR model for each modeled variable. First a custom stage 1 fit is produced using the model selected in the previous step. Then the standard ST-GPR input data is prepared along with the configuration file. These are passed to the ST-GPR back-end via its API, and the model is run.  

4. Plot results: generates country-level plots which show all of the stages of the modeling process along with input data, for model vetting. In the iterative process of model development, this is where the pipeline typically stops, until a model is considered final and ready for finalization.    

5. Model finalization: once a model is vetted and deemed finished, it is finalized by storing its files (input, intermediate, and outputs) in a more permanent location.  

#### Outlier Detection and Time-Trend Variance Calculation

The method used here is to fit a smooth time-trend to the data for each country and then calculate a residual for each observation, and then outlier data that is excessively far from the smooth trend.  

The approach is based on the assumption that these modeled variables should not vary by large amounts from year-to-year, after accounting for plausible time-trends (such as an increasing trend, decreasing trend, U-shaped trend, or other more flexible trends).  
The time-trend is fit using a generalized additive model (GAM) with an intercept and a smooth function of the year of observation. The basis dimension of the smooth function, which controls the flexibility of the time-trend (more basis functions means more flexibility) is set to about the maximum possible (although it may be reduced by the GAM-fitting procedure) to allow the smooth trend to be very wiggly if the data suggests, so as to not impose too many prior assumptions on the time trends.  
Standardized residuals (scaled Pearson residuals) are calculated and data that is deemed to be excessively far from the smooth trend, in terms of standard deviations away, are flagged as outliers. The assumption here is that if a flexible function could not accommodate an observed value while also fitting to the rest of the country's data, then it is an unlikely observation.  

The smooth time-trend could be thought of as the underlying latent trend of the variable across time, so that the actual observations are the result of a noisy measurement process. Under this framework, the residuals might be thought of as the noise in the measurement, and so these deviations of the observations from the smooth trend represent an estimate of an observation's variance.  
Thus, after outliers are flagged, each GAM is re-fit to the data with the outliers removed and raw residuals are calculated to be used as estimates of variance in the ST-GPR pipeline (we did not end up using this method to calculate variance for ST-GPR though).  

This approach was motivated by the ineffectiveness of using Cook's distance - I tried this standard approach but since values vary quite a bit across countries and data is sparse, it doesn't work well when the model under investigation is ran across the entire data set (as it is done in the stage-1 of the ST-GPR process). As an alternative I tried running a separate linear regression for each country and then calculating Cook's distance, but this approach was too strict - tagging many observations as outliers when they did not appear to be.  

