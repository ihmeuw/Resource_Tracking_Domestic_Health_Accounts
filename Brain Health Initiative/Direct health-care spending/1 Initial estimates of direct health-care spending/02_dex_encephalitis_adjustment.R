#### #----#                    Docstring                    #----# ####
#' Title:    02_dex_encephalitis_adjustment
#' Project:  Resource Tracking - Brain Health Initiative (BHI)
#' Purpose : Adjust DEX 3.0 spending estimates to isolate encephalitis 
#' spending from infectious disease aggregate spending using last iteration
#' of DEX 2.0 spending estimates
#'           
#' Author: USERNAME
#' Date: 2024-05-31
#---------------------------------------------------------------------#

#----# Set up directory roots #----#
rm(list=ls())
if (!exists("code_repo"))  {
  code_repo <- unname(ifelse(Sys.info()['sysname'] == "Windows", "ADDRESS", paste0("ADDRESS", Sys.info()['user'][1], "ADDRESS")))
}

#----# Source functions #----#
source(paste0(code_repo, 'FILEPATH'))
source(paste0(code_repo, 'FILEPATH'))
source(paste0(dah.roots$k, "FILEPATH"))
source(paste0(dah.roots$k, "FILEPATH"))
source(paste0(h, "FILEPATH"))
library(arrow)

# INPUTS
date <- format(Sys.Date(), '%Y%m%d')  # date for archived files
currency_convert_yr <- 2021           # year to convert all estimates
years <- 2000:2019                    # time period
gbd_release_id <- 16                  # release id for GBD estimates

# paths
bhi_share <- paste0('FILEPATH')
raw_data_path <- paste0(bhi_share, 'FILEPATH')
int_data_path <- paste0(bhi_share, 'FILEPATH')
fin_data_path <- paste0(bhi_share, 'FILEPATH')


## #############################################################################
## Read in and format DEX 2.0 estimates
## #############################################################################

## Read in results from DEX2.0
dex2.0_dt <- arrow::read_feather('FILEPATH')
dex2.0_dt <- setDT(dex2.0_dt)

## Subset to aggregate infectious causes, relevant years, and remove 'Government administration' type of care
infectious_causes <- c('diptheria', 'encephalitis', 'measles', 'whooping', 'tetanus', 'varicella', 'hepatitis', 'infectious')
dex2.0_dt <- dex2.0_dt[acause %in% infectious_causes & year %in% years & `function` != 'GA']

## Rename columns and update types of care changes
setnames(dex2.0_dt,
         c('function', 'sex', 'age'),
         c('toc', 'sex_id', 'age_group_years_start'))
dex2.0_dt[toc == 'ER', toc := 'ED']
dex2.0_dt[toc == 'LT', toc := 'NF']

## Update <1 year to <5 years
dex2.0_dt[age_group_years_start == 1, age_group_years_start := 0]

## Duplicate encephalitis and hepatitis rows and reassign all as 'infectious'
temp <- copy(dex2.0_dt[acause %in% c('encephalitis', 'hepatitis')])
dex2.0_dt[, acause := 'infectious']
dex2.0_dt <- rbind(dex2.0_dt, temp)
rm(temp)

## Collapse by cause
to_sum <- names(dex2.0_dt)[grepl('expend', names(dex2.0_dt))]
dex2.0_dt <- dex2.0_dt[, lapply(.SD, sum, na.rm = T),
                       by = c('year', 'toc', 'sex_id', 'acause', 'age_group_years_start'),
                       .SDcols = to_sum]

## Reshape long by draws
dex2.0_dt <- melt.data.table(dex2.0_dt,
                             id.vars = c('year', 'toc', 'sex_id', 'acause', 'age_group_years_start'),
                             measure.vars = to_sum,
                             variable.name = 'payer',
                             variable.factor = F)

## Subset to total spending
dex2.0_dt <- dex2.0_dt[grepl('expend_scaled', payer)]

## Reshape wide by cause
dex2.0_dt <- dcast.data.table(dex2.0_dt,
                              year + toc + sex_id + age_group_years_start + payer ~ acause,
                              value.var = 'value')

## Calculate hepatitis to infectious spend ratio and encephalitis to (infectious - hepatitis) spend ratio
dex2.0_dt[, `:=` (hep_ratio = hepatitis / infectious,
                  enceph_ratio = encephalitis / (infectious - hepatitis))]

## Add in extra rows to account for no 2017-2019 data using 3-year weighted average
dex2.0_dt <- melt.data.table(dex2.0_dt,
                             id.vars = c('year', 'toc', 'sex_id', 'age_group_years_start', 'payer'),
                             measure.vars = c('enceph_ratio', 'hep_ratio'),
                             variable.factor = F)
dex2.0_dt <- dcast.data.table(dex2.0_dt,
                              toc + sex_id + age_group_years_start + payer + variable ~ year,
                              value.var = 'value')
dex2.0_dt[, `2017` := (1/2)*`2016` + (1/3)*`2015` + (1/6)*`2014`]
dex2.0_dt[, `2018` := (1/2)*`2017` + (1/3)*`2016` + (1/6)*`2015`]
dex2.0_dt[, `2019` := (1/2)*`2018` + (1/3)*`2017` + (1/6)*`2016`]

## Calculate means
dex2.0_dt[, payer := gsub('[0-9]+', '', payer)]
dex2.0_dt <- dex2.0_dt[, lapply(.SD, mean, na.rm = T),
                       by = c('toc', 'sex_id', 'age_group_years_start', 'payer', 'variable'),
                       .SDcols = as.character(2000:2019)]

## Add in extra rows to account for 'home health' type of care
temp <- dex2.0_dt[toc == 'AM']
temp[, toc := 'HH']
dex2.0_dt <- rbind(dex2.0_dt, temp)

## Add in extra rows to account for no 'dental' type of care
temp <- dex2.0_dt[toc == 'AM']
temp[, toc := 'DV']
temp[, as.character(2000:2019) := 0]
dex2.0_dt <- rbind(dex2.0_dt, temp)

## Reshape back to original
dex2.0_dt <- melt.data.table(dex2.0_dt,
                             id.vars = c('toc', 'sex_id', 'age_group_years_start', 'payer', 'variable'),
                             variable.name = 'year',
                             variable.factor = F)
dex2.0_dt <- dcast.data.table(dex2.0_dt,
                              toc + sex_id + age_group_years_start + payer + year ~ variable,
                              value.var = 'value')

## Reclassify year column as numeric
dex2.0_dt[, year := as.numeric(year)]


## #############################################################################
## Gathering and processing estimates - DEX 3.0 health expenditure
## #############################################################################


## Get DEX estimates files
dex_files <- list.files(paste0(int_data_path, 'FILEPATH'), pattern = 'dex_')

## Read in DEX data function
read_dex <- function(file) {
  temp <- fread(file)
  temp[, `:=` (currency = NULL, currency_year = NULL, currency_converter_version = NULL)]
  return(temp)
}

for (year in 2000:2019) {
  
  ## Read in dex 3.0 estimates
  dex_raw <- read_dex(paste0(int_data_path, 'FILEPATH', year, 'FILEPATH'))
  
  ## Isolate infectious aggregate and hepatitis c
  infectious <- copy(dex_raw[acause %in% c('_infect_agg', 'hepatitis_c')])
  
  ## Reshape wide by cause
  infectious <- dcast.data.table(infectious,
                                 location_id + year + age_group_years_start + sex_id + toc ~ acause,
                                 value.var = 'spend')
  
  ## Merge on ratios
  infectious <- merge(infectious, dex2.0_dt, by = c('year', 'sex_id', 'age_group_years_start', 'toc'), all.x = T)
  
  ## Calculate hepatitis spend
  infectious[, hepatitis := hep_ratio * (`_infect_agg` + hepatitis_c)]
  
  ## If hepatitis is less than hepatitis c, reassign hepatitis spend total as hepatitis c spend
  infectious[hepatitis < hepatitis_c, hepatitis := hepatitis_c]
  
  ## Calculate encephalitis
  infectious[, encephalitis := enceph_ratio * (`_infect_agg` + hepatitis_c - hepatitis)]
  
  ## Remove encephalitis spend from infectious aggregate
  infectious[, `_infect_agg` := `_infect_agg` - encephalitis]
  
  ## Reshape long by acause to match original formatting
  infectious <- melt.data.table(infectious,
                                id.vars = c('year', 'sex_id', 'age_group_years_start', 'toc', 'location_id'),
                                measure.vars = c('_infect_agg', 'encephalitis'),
                                variable.name = 'acause',
                                variable.factor = F,
                                value.name = 'spend')
  
  ## Append to original estimates
  dex_raw <- dex_raw[acause != '_infect_agg']
  dex_raw <- rbind(dex_raw, infectious)
  
  ## Save out adjusted estimates
  fwrite(dex_raw, paste0(int_data_path, 'FILEPATH', year, '.csv'))
  rm(infectious, dex_raw)
  
}

## End of Script##