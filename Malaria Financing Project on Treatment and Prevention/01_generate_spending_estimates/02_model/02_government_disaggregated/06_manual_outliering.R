#####################
# USERNAME
# 
# Code to outlier erroneous data
#####################

rm(list = ls())

if (!exists("code_repo"))  {
  code_repo <- unname(ifelse(Sys.info()['sysname'] == "Windows", 
                             "ADDRESS", 
                             paste0("ADDRESS")))
}

source(paste0(code_repo, 'FILEPATH'))

## read data
other_ssa <- fread('FILEPATH')
other_nonssa <- fread('FILEPATH')
other <- rbind(other_ssa, other_nonssa)
diagnostics <- fread('FILEPATH')
antimalarials <- fread('FILEPATH')
hrta <- fread('FILEPATH')
itns <- fread('FILEPATH')
irs <- fread('FILEPATH')
me <- fread('FILEPATH')
pao <- fread('FILEPATH')
caa <- fread('FILEPATH')
iae <- fread('FILEPATH')
psm <- fread('FILEPATH')


## Add on IHME Loc ids
other <- get_ihme_loc(other)
diagnostics <- get_ihme_loc(diagnostics)
antimalarials <- get_ihme_loc(antimalarials)
psm <- get_ihme_loc(psm)

## flag manual outliers
other[val > 0.01, is_outlier := 1]
other[ihme_loc_id == 'BGD' & year_id %in% c(2011), is_outlier := 1]
other[ihme_loc_id == 'KOR' & year_id %in% c(2009, 2010, 2011), is_outlier := 1]
other[ihme_loc_id == 'BWA' & year_id %in% c(2015, 2018), is_outlier := 1]
other[ihme_loc_id == 'COM' & year_id == 2012, is_outlier := 1]
other[ihme_loc_id == 'TZA' & year_id == 2011, is_outlier := 1]

diagnostics[ihme_loc_id == 'COL' & year_id == 2010, is_outlier := 1]
diagnostics[ihme_loc_id == 'ARG' & year_id == 2010, is_outlier := 1]

antimalarials[ihme_loc_id == 'KOR' & year_id == 2019, is_outlier := 1]

psm[ihme_loc_id == 'ARG' & year_id == 2014, is_outlier := 1]


## split up other by super region again
other_ssa <- other[value_code == 'Other SSA']
other_nonssa <- other[value_code == 'Other nonSSA']

## write data
fwrite(other_ssa, 'FILEPATH')
fwrite(other_nonssa, 'FILEPATH')
fwrite(antimalarials, 'FILEPATH')
fwrite(diagnostics, 'FILEPATH')
fwrite(psm, 'FILEPATH')

## End of Script ##