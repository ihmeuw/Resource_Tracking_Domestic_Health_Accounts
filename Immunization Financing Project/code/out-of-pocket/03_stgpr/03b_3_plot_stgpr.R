########################################################################################
## Make Volume country-level line graph plots of ST-GPR results
## Author: Emilie Maddison
## Date: 15 May 2020
## Description: Produces PDFs, with each page representing the countries within
##              each super-region. Time series line graphs
########################################################################################
rm(list = ls())

## Set filepaths
if (Sys.info()[1] == "Linux") {
  j <- FILEPATH
  h <- FILEPATH
  k <- FILEPATH
} else if (Sys.info()[1] == "Windows") {
  j <- FILEPATH
  h <- FILEPATH
  k <- FILEPATH
}

code.dir <- FILEPATH
loc.dir <- FILEPATH
out.dir <- FILEPATH

source(paste0(FILEPATH, "helper_functions.R"))
source("FILEPATH/utility.r")
library(ggplot2)

## Today's date
date1 <- format(Sys.time(), "%Y%m%d")
date_input_data <- '20200714'

config_file <- fread(paste0(out.dir, "oop_volume_config.csv"))
config_file <- config_file[, .(model_version, model_index_id, stage_1_model_formula)]
dt_list <- unique(config_file$model_version)

run_dictionary <- fread(paste0(out.dir, "oop_volume_runid_modelid_dictionary.csv"))
run_dictionary2 <- data.table(merge(run_dictionary, config_file,
                        by.x = 'model_id', by.y = 'model_index_id',
                        all.x = T))
run_dictionary2 <- run_dictionary2[date == date1 & run_id >= 151040, ]

for (m in c(284:288)) {
  n <- run_dictionary2[model_id == m, run_id]
  print(n)
  
  df1 <- get_data(n)

  raw_data <- fread(paste0(out.dir, "log_volume_proportion_", 
                           run_dictionary2[model_id == m, model_version], 
                           "_", date_input_data, 
                           ".csv"))
  raw_data <- raw_data[, val := exp(val)]
  adj_val <- (min(raw_data[, val]))
  raw_data <- raw_data[, val := val - adj_val]
  
  gpr_locs <- merge(df1, locs[, .(location_id)], by = 'location_id', all.y = T)
  gpr_locs <- unique(gpr_locs$location_id)

  df <- merge(df1, locs[, .(location_id, location_name, super_region_name)], 
            by = 'location_id', all.x = T)
  df <- merge(df, raw_data[, .(location_id, year_id, val)], 
            by = c('location_id', 'year_id'), all.x = T)
  
  
  
  df <- df[, `:=`(gpr_mean = exp(gpr_mean) - adj_val,
                  gpr_lower = exp(gpr_lower) - adj_val,
                  gpr_upper = exp(gpr_upper) - adj_val)]
  sr_locs <- unique(locs$super_region_name)
  pdf(paste0(loc.dir, run_dictionary2[model_id == m, model_version], "_volume_model_", 
             m, "_", n, ".pdf"))
  
  for (i in c(1:7)) {
    a <- ggplot(df[super_region_name == sr_locs[i],], 
                aes(x = year_id, y = gpr_mean, color = super_region_name)) +
          geom_line() +
          geom_ribbon(aes(ymin = gpr_lower, ymax = gpr_upper, fill = super_region_name),
                      alpha = 0.5) +
          geom_point(aes(y = val), color = 'black') +
          labs(caption = run_dictionary2[run_id == n, stage_1_model_formula]) +
          theme_bw() +
          theme(
                legend.position = "top",
                legend.title = element_blank()) +
          facet_wrap(~location_name)
    print(a)
  }
  dev.off()
}
 
# df_model <- get_data(148919)
# fwrite(df_model, paste0(loc.dir, "oop_scalar_stgpr_", date1, ".csv"))

## End of Script ##