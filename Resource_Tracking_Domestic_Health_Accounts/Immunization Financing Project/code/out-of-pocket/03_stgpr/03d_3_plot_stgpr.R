########################################################################################
## Make OOP scalar line graph plots of ST-GPR results
## Author: Emilie Maddison
## Date: 17 May 2020
## Description: Produces PDFs, with each page representing the countries within
##              each super-region. Time series line graphs
########################################################################################
## -------------------------------------------------------------------------------------
## 1. Set up environment
## -------------------------------------------------------------------------------------

## Clear environment
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

code.dir <-FILEPATH

out.dir <- FILEPATH
# out.dir <- FILEPATH

## Load libraries
source(paste0(FILEPATH, "helper_functions.R"))
## St-GPR functions
source("FILEPATH/utility.r")
library(ggplot2)

## Today's date
date1 <- format(Sys.time(), "%Y%m%d")
date1 <- '20200628'

## -------------------------------------------------------------------------------------
## 2. Prep data
## -------------------------------------------------------------------------------------

## 135 locations of interest
lm_locs <- get_ig(locs)
lm_locs <- lm_locs[income_group != 'H']

## Read in dictionary
run_dictionary <- fread(paste0(FILEPATH,
                               'oop_scalar_runid_modelid_dictionary.csv'))

## Read in config file to get stage1 model parameters
config_file <- fread("FILEPATH/oop_scalar_config.csv")

config_file <- config_file[, .(model_version, model_index_id, stage_1_model_formula)]

run_dictionary <- fread(paste0(out.dir, "oop_scalar_runid_modelid_dictionary.csv"))
run_dictionary2 <- data.table(merge(run_dictionary, config_file,
                        by.x = 'model_id', by.y = 'model_index_id',
                        all.x = T))

## Subset to today's runs
# run_dictionary2 <- run_dictionary2[date == date1, ]
run_dictionary2 <- run_dictionary2[model_id == 20, ]

## Read in raw (pre-St-GPR data)
raw_data <- fread(paste0("FILEPATH/oop_scalar_",
                         date1, ".csv"))
head(raw_data)

## Delete the tiny adjustment made to fix zeros for logit space
adj_val <- min(raw_data$val)

## -------------------------------------------------------------------------------------
## 3. Graph data
## -------------------------------------------------------------------------------------

## Fetch data and produce line graph of time tren for every country,
## by super-region, with raw data overlaid.
for (n in run_dictionary2$run_id) {
  print(n)
  df1 <- get_data(n)

  ## Subtract the adjustment value we added pre-ST-GPR to deal with zeros
  df1[, `:=`(gpr_mean = gpr_mean - adj_val,
             gpr_lower = gpr_lower - adj_val,
             gpr_upper = gpr_upper - adj_val)]
  
  # gpr_locs <- merge(df1, lm_locs[, .(location_id)], by = 'location_id', all.y = T)
  # gpr_locs <- unique(gpr_locs$location_id)

  df <- merge(df1, lm_locs[, .(location_id, location_name, super_region_name)], 
            by = 'location_id', all.y = T)

  df <- merge(df, raw_data[, .(location_id, year_id, val)], 
            by = c('location_id', 'year_id'), all.x = T)

  ## Reassign Argentina
  df[location_id == 97, super_region_name := "Latin America and Caribbean"]
  sr_locs <- sort(unique(df$super_region_name))

  pdf(paste0("FILEPATH/oop_scalar_model_", n, "_all3.pdf"),
      width = 12, height = 8)
  for (i in c(1:5)) {
    a <- ggplot(df[super_region_name == sr_locs[i],], 
                aes(x = year_id, y = gpr_mean, color = super_region_name)) +
          geom_line() +
          geom_ribbon(aes(ymin = gpr_lower, ymax = gpr_upper, fill = super_region_name), 
                      alpha = 0.5) +
          geom_point(aes(y = val), color = 'black') +
          xlab('') +    
          ylab("OOP scalar fraction") +
          labs(caption = run_dictionary2[run_id == n, stage_1_model_formula]) +
          theme_bw() +
          theme(
                legend.position = "top",
                legend.title = element_blank(),
                axis.text.x = element_text(angle = 45, hjust = 1)) +
          facet_wrap(~location_name, labeller = label_wrap_gen(28))
    print(a)
  }
  a <- ggplot(df[super_region_name == sr_locs[6],], 
              aes(x = year_id, y = gpr_mean, color = super_region_name)) +
    geom_line() +
    geom_ribbon(aes(ymin = gpr_lower, ymax = gpr_upper, fill = super_region_name), 
                alpha = 0.5) +
    geom_point(aes(y = val), color = 'black') +
    xlab('') +    
    ylab("OOP scalar fraction") +
    labs(caption = run_dictionary2[run_id == n, stage_1_model_formula]) +
    theme_bw() +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)) +
    facet_wrap(~ location_name, ncol = 10, labeller = label_wrap_gen(16))
  print(a)
  dev.off()
}

## Write out data
fwrite(df1,
       paste0("FILEPATH/oop_scalar_stgpr_",
              date1, ".csv"))

## END OF SCRIPT ##