#### #----#                    Docstring                    #----# ####
#' Title:    05_dex_gbd_merge.R
#' Project:  Resource Tracking - Brain Health Initiative (BHI)
#' Purpose : Append the datasets pulled from GBD
#'           for causes and risk factors from DEX specific acauses.
#'           
#'     
#' Author: USERNAME
#' Date: 2023-02-28
#---------------------------------------------------------------------#

#----# Set up directory roots #----#
rm(list=ls())
if (!exists("code_repo"))  {
  code_repo <- unname(ifelse(Sys.info()['sysname'] == "Windows", "ADDRESS", paste0("ADDRESS", Sys.info()['user'][1], "ADDRESS")))
}

## Source functions
source(paste0(code_repo, 'FILEPATH'))
source(paste0(dah.roots$k, "FILEPATH"))
source(paste0(dah.roots$k, "FILEPATH"))

#----# Local CONSTANTS #----#
## Variable prep
data_yr <- 2019
currentDate <- format(Sys.time(), "%Y%m%d")
gbd_release_id <- 16
years <- 2000:(data_yr)

## Paths
bhi_share <- paste0('FILEPATH')
raw_data_path <- paste0(bhi_share, 'FILEPATH')
int_data_path <- paste0(bhi_share, 'FILEPATH')
fin_data_path <- paste0(bhi_share, 'FILEPATH')

# lists
# dex age group metadata datatable
dex_age_groups = as.data.table(list(age = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85),
                                    age_group_name = c('<5 years', '5 to 9', '10 to 14', '15 to 19', 
                                                     '20 to 24', '25 to 29', '30 to 34', '35 to 39', 
                                                     '40 to 44', '45 to 49', '50 to 54', '55 to 59', 
                                                     '60 to 64', '65 to 69', '70 to 74', '75 to 79', 
                                                     '80 to 84', '85 plus'),
                                    age_group_id = c(1, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 30, 160)))

# gbd causes that need relabel to dex cause
gbd_cause_relabel <- fread(paste0(raw_data_path, 'FILEPATH'))
gbd_cause_relabel <- gbd_cause_relabel[!is.na(`reassigned id`)]$`GBD acause`

#---------------------------------------------------------------------#

cat("\n\n")
cat(green("\t#######################################\n"),
    green("\t#######   BEGIN 4_dex_gbd_merge.R   #######\n"),
    green("\t#######################################\n\n"))


################### #----# Main #----# ###################
####----# Read and Process Data #----####
cat(' Read and Process Data\n')

## Get DEX estimates files
dex_files <- list.files(paste0(int_data_path, 'FILEPATH'), pattern = 'dex_')

## Read in and append DEX estimates
dex_raw <- rbindlist(lapply(paste0(int_data_path, 'FILEPATH', dex_files), fread))

## Read in GBD estimates
gbd_full <- fread(paste0(int_data_path, "FILEPATH"))

# reallocating dex funds from renal_failure to ckd and from septicemia to endo
dex_raw[acause == 'renal_failure', `:=` (acause = 'ckd')]
dex_raw[acause == 'septicemia', `:=` (acause = 'endo')]
dex_raw[acause == 'lri_corona', `:=` (acause = 'lri')]
gbd_acauses_dt <- data.table(unique(gbd_full[, .(acause, cause_id, cause_name)]))

# relabeling gbd acauses with the appropriate dex label
gbd_cause_dt <- data.table(unique(gbd_full[!dex_acause %like% '^rf_|cvd_hf$', .(dex_acause1 = dex_acause, acause, cause_id1 = cause_id, cause_name1 = cause_name)]))

# updating cause names
gbd_cause_dt[dex_acause1 == '_infect_agg', `:=` (cause_name1 = 'Other infectious diseases', cause_id1 = 408)]
gbd_cause_dt[dex_acause1 == '_intent_agg', `:=` (cause_name1 = 'Interpersonal violence', cause_id1 = 724)]
gbd_cause_dt[dex_acause1 == '_unintent_agg', `:=` (cause_name1 = 'Other unintentional injuries', cause_id1 = 716)]
gbd_cause_dt[dex_acause1 == 'mental_drug_agg', `:=` (cause_name1 = 'Other substance use disorders', cause_id1 = 566)]

# relabeling causes that need to be relabeled to match DEX
gbd_full <- merge(gbd_full, gbd_cause_dt, all.x = T)
gbd_full[acause %in% gbd_cause_relabel, `:=` (acause = dex_acause1, cause_id = cause_id1, cause_name = cause_name1)]
gbd_full[, c('dex_acause1', 'cause_id1', 'cause_name1') := NULL]
gbd_full[is.na(rei_acause_frct), rei_acause_frct := 0]
gbd_full <- gbd_full[, .(mean_value = sum(mean_value, na.rm = T)),
                     by = .(acause, cause_id, age_group_id, location_id, metric_id, sex_id, year_id, age_group_name,
                            cause_name, location_name, sex, dex_acause, dex_cause_id, rei_acause_frct, rei_id, measure)]

#---------------------------------------------------------------------#
####----# Merge Data #----####
cat(' Merge Data\n')

# setting labels for dex to merge
setnames(dex_raw, old = c('acause', 'age_group_years_start', 'year'), new = c('dex_acause', 'age', 'year_id'))

# setting dex age group mapping
dex_raw <- merge(dex_raw, dex_age_groups, by = 'age', all.x = T)

# pivoting DEX data wide with type of care spending as columns
dex <- dcast.data.table(data = dex_raw,
                        formula = location_id + year_id + sex_id + age_group_id + age_group_name + dex_acause ~ toc,
                        value.var = c('spend'),
                        fun.aggregate = sum)

## Make vector of spending columns
toc_val_cols <- unique(dex_raw$toc)
rm(dex_raw)

## Merge GBD estimates on to DEX to calculate spending per case for calculating global estimates
us_dt <- merge(dex,
               gbd_full[location_id == 102],
               by = c('location_id', 'year_id', 'sex_id', 'age_group_id', 'age_group_name', 'dex_acause'),
               all.x = T)

## Fix NAs to 0 and reallocate risk factor to appropriate causes
us_dt[, c(toc_val_cols) := lapply(.SD, function(x) nafill(x, fill = 0)), .SDcols = toc_val_cols]
us_dt[, c(toc_val_cols) := lapply(.SD, function(x) x * rei_acause_frct), .SDcols = toc_val_cols]

## Compute spending per case
us_dt[!is.na(mean_value) | mean_value != 0, paste0(toc_val_cols, '_pcase') := lapply(.SD, function(x) x / mean_value), .SDcols = toc_val_cols]
us_dt[is.na(mean_value) | mean_value == 0, paste0(toc_val_cols, '_pcase') := 0]

## Create US state dataset and US agg dataset
col_ids <- c(names(gbd_full)[grepl('_id', names(gbd_full)) & names(gbd_full) != 'location_id'], 'age_group_name', 'sex', 'dex_acause')
us_global <- us_dt[location_id == 102, .SD, .SDcols = c(col_ids, paste0(toc_val_cols, '_pcase'))]

# merge back to entire dataset, use the per case rates to all countries
dt_global <- merge(gbd_full, us_global, by = c(col_ids), all.x = T)
rm(us_global, gbd_full, us_dt, dex)

## Append datasets
dt <- copy(dt_global)
rm(dt_global)

## Remove any rows with all NAs in all spending columns
dt <- dt[!is.na(AM_pcase)]

#---------------------------------------------------------------------#
# aggregating risk factors up to individual cause level
dex_toc_cols <- c(paste0(toc_val_cols, '_pcase'))

gbddex_ids <- c('location_id', 'year_id', 'cause_id', 'age_group_id', 'metric_id', 'sex_id', 'acause', 
                'age_group_name', 'cause_name', 'location_name', 'sex')

keep_dt_cols <- c(gbddex_ids,
                  'dex_cause_id', 'rei_id', 'dex_acause', 'rei_acause_frct', 'mean_value', 
                  dex_toc_cols)

# computing the spending for causes in each country by multiplying the spending per case TOC columns and the number of cases in countries
dt <- dt[, .SD, .SDcols = keep_dt_cols]
dt[, gsub('_pcase', '', dex_toc_cols) := lapply(.SD, function(x) x * mean_value), .SDcols = dex_toc_cols]

# aggregating by acause now
dt[!is.na(rei_id), mean_value := 0]
dt <- dt[, lapply(.SD, sum, na.rm = T), by = gbddex_ids, .SDcols = c('mean_value', gsub('_pcase', '', dex_toc_cols))]

#---------------------------------------------------------------------#

#### ## Saving dataset ## ####
cat(' Saving dataset \n')
                                             
# saving GBD cause and rei datasets data
fwrite(dt, file = paste0(int_data_path, "FILEPATH"))

# saving Feather file versions
arrow::write_feather(dt, paste0(int_data_path, "FILEPATH"))
                                        
## End of Script ##