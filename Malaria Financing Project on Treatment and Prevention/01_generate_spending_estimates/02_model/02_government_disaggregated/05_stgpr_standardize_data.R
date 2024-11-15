#####################
# USERNAME
# 
# Code to prepare disaggregated malaria data for ST-GPR modeling
#####################

#########
# Prepare workspace
#########

# load packages
pacman::p_load(readstata13, data.table, ggplot2, feather)

rm(list = ls())

if (Sys.info()["sysname"] == "Linux") {
  j <- "ADDRESS"
  h <- "ADDRESS"
} else {
  j <- "ADDRESS"
  h <- "ADDRESS"
}

source("FILEPATH")
source('FILEPATH')

#---------------------------------------------------------------

# set pwd
setwd('ADDRESS')

# Read in disaggregated data from cooks distance
data <- fread(paste0('FILEPATH'))


for (i in unique(data$value_code)) {
  
  dt <- copy(data)
  dt <- dt[value_code == i]
  setnames(dt, c('y_frac', 'flag_cd'), c('val', 'is_outlier'))
  dt[, `:=` (sample_size = 25,
             nid = 99999,
             measure_id = 18,
             variance = var(dt$val))]
  
  fwrite(dt, paste0('FILEPATH'))
}

## End of Script ##