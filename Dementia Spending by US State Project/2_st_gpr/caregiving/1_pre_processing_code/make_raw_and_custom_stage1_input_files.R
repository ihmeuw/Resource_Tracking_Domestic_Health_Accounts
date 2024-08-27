# Makes raw and custom stage 1 data files for caregiving ST-GPR input 
# Author: Michael Breshock
# Date: 09/12/2023

# Clear environment
rm(list = ls())

# Loading libraries
library(data.table)


# load predictions made in:
# ~/alzheimers/Global_dementia_2023/1_stage1_model/caregiving_model/2_regression_predictions.R
pred = fread("FILEPATH/03_cg_predictions_04.17.24.csv")

## make custom stage 1 file: 
# select just the columns we need for st-gpr
pred_cols = pred[,.(year_id, location_id, logit_pred, sex_id, age_group_id)]

setnames(pred_cols, old = "logit_pred", new = "cv_custom_stage_1")

# save out custom stage1 predictions file to st-gpr directory: 
fwrite(pred_cols, file = "FILEPATH/04.17.24.cg_predictions.csv")

## Make raw data input file: 
clean_cg = fread(paste0("FILEPATH/02_clean_data_no_outliers_04.11.24.csv"))
# this file was made in: 
# /1_stage1_model/caregiving_model/1_cut_outliers.R

# filter to overall severity & care type, per patient estimates
patient_cg = clean_cg[estimate_method == "per patient" & 
                        severity == "" & care_type == ""]
# reduces input data from 80 -> 51; 29 observations with splits cut
cut_from_stgpr = anti_join(clean_cg, patient_cg)
# save split data that was cut from st-gpr: 
fwrite(cut_from_stgpr, file = "FILEPATH/cut_from_STGPR_04.11.24.csv")

# select variables needed for st-gpr: 
cg_raw = patient_cg[,.(year_id, location_id, logit_hours, sample_size, 
                       age_group_id, sex_id, sdi, severity, care_type)]
# change value name to match with what ST-GPR expects: 
setnames(cg_raw, old = c("logit_hours","sdi", "severity", "care_type"), 
         new = c("val","cv_sdi","cv_severity","cv_care_type"))

# adding variables for st-gpr input: 
cg_raw[, ":=" (measure = "continuous",
               measure_id = 19, 
               nid = 999999,
               is_outlier = 0)]

# calculating variance scaled by sample size: 
cg_raw[, variance := var(val)/sample_size]
nrow(cg_raw[is.na(variance)]) # 0

# save out raw caregiving st_gpr input data: 
fwrite(cg_raw, file = "FILEPATH/04.17.24.cg_data.csv")

