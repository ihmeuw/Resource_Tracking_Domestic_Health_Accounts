################################################
#' @author USERNAME
#' 
#' @description Save results for Malaria government STGPR
################################################
pacman::p_load(data.table, dplyr, reticulate)
rm(list = ls())

source('FILEPATH')
source('FILEPATH')

## *************************************************

malaria_version <- "NAME"

## *************************************************

malaria_country_list <- fread('FILEPATH')

## set working directory 
setwd("FILEPATH")

## read runinfo
runinfo <- fread("FILEPATH")
run_id <- "NUMBER"
## find which run_id you want
## *******************************************
run_id <- runinfo[version == malaria_version, run_id]
## *******************************************

for (i in runinfo[version == malaria_version]$me_name) {
  
  run_id <- runinfo[version == malaria_version & me_name == i, run_id]
  
  ## list all files in results directory
  results_dir <- paste0("FILEPATH")
  files <- list.files(results_dir)
  
  ## function to read, melt, and select
  read_stgpr_draws <- function(x){
    draw <- fread(x)
    draw <- melt.data.table(draw, id.vars = c("location_id", "year_id", "age_group_id", "sex_id"))
    draw <- draw[, .(location_id, year_id, variable, value, value_code = i)]
    return(draw)
  }
  
  # read, melt, select from, and bind all results
  results <- rbindlist(lapply(paste0(results_dir, files), read_stgpr_draws))
  results <- results[location_id %in% malaria_country_list$location_id]
  
  results <- get_ihme_loc(results)
  results <- get_region(results)
  
  if (grepl('nonssa', i)) {
    results <- results[super_region_name != 'Sub-Saharan Africa']
  } else if (grepl('ssa', i)) {
    results <- results[super_region_name == 'Sub-Saharan Africa']
  }
  
  if (i == 'fs_malaria_domestic_public_diagnostics') {
    all_results <- copy(results)
  } else {
    all_results <- rbind(all_results, results)
  }
  

}


## Add IHME loc id
temp <- copy(all_results)
temp[, super_region_name := NULL]

## Pull in total GHES
ghes <- get_he_data('ghes_totes')

ghes <- ghes[, .(ghes_totes = mean(data_var)),
             by = .(ihme_loc_id, year_id)]

## Merge onto results
temp <- merge(temp, ghes, by = c('ihme_loc_id', 'year_id'))

## Calculate values
temp[, value_frac := value]
temp[, value := value * ghes_totes]

## Combine the "Other" value codes
temp[value_code %in% c('fs_malaria_domestic_public_other_nonssa', 'fs_malaria_domestic_public_other_ssa'),
     value_code := 'fs_malaria_domestic_public_other']


# Write out results
write_feather(temp, paste0('FILEPATH'))


temp[, value_code := gsub('fs_malaria_domestic_public_', '', value_code)]

temp <- dcast.data.table(temp, ihme_loc_id + location_id + year_id + variable ~ value_code, value.var = 'value')
temp[, total := `anti-malarial_medicines` + communication_and_advocacy + diagnostics + `human_resources_&_technical_assistance` + `infrastructure_&_equipment` + `insecticide_&_spraying_materials` + `itns_&_pbos` + monitoring_and_evaluation + `planning,_administration,_overheads` + `procurement_&_supply_management` + other]

temp[, `:=` (`anti-malarial_medicines_prop` = `anti-malarial_medicines` / total,
             communication_and_advocacy_prop = communication_and_advocacy / total,
             diagnostics_prop = diagnostics / total,
             `human_resources_&_technical_assistance_prop` = `human_resources_&_technical_assistance` / total,
             `infrastructure_&_equipment_prop` = `infrastructure_&_equipment` / total,
             `insecticide_&_spraying_materials_prop` = `insecticide_&_spraying_materials` / total,
             `itns_&_pbos_prop` = `itns_&_pbos` / total,
             monitoring_and_evaluation_prop = monitoring_and_evaluation / total,
             `planning,_administration,_overheads_prop` = `planning,_administration,_overheads` / total,
             `procurement_&_supply_management_prop` = `procurement_&_supply_management` / total,
             other_prop = other / total)]


# read in total malaria ghes
malaria_spend <- fread('FILEPATH')
malaria_spend <- merge(temp, malaria_spend, by = c('location_id', 'year_id', 'variable'))

malaria_spend[, `:=` (`anti-malarial_medicines` = `anti-malarial_medicines_prop` * mal_ghes,
                  communication_and_advocacy = communication_and_advocacy_prop * mal_ghes,
                  diagnostics = diagnostics_prop * mal_ghes,
                  `human_resources_&_technical_assistance` = `human_resources_&_technical_assistance_prop` * mal_ghes,
                  `infrastructure_&_equipment` = `infrastructure_&_equipment_prop` * mal_ghes,
                  `insecticide_&_spraying_materials` = `insecticide_&_spraying_materials_prop` * mal_ghes,
                  `itns_&_pbos` = `itns_&_pbos_prop` * mal_ghes,
                  monitoring_and_evaluation = monitoring_and_evaluation_prop * mal_ghes,
                  `planning,_administration,_overheads` = `planning,_administration,_overheads_prop` * mal_ghes,
                  `procurement_&_supply_management` = `procurement_&_supply_management_prop` * mal_ghes,
                  other = other_prop * mal_ghes)]

malaria_spend <- malaria_spend[, .(ihme_loc_id, location_id, year_id, variable, `anti-malarial_medicines`, communication_and_advocacy, diagnostics, `human_resources_&_technical_assistance`, `infrastructure_&_equipment`, `insecticide_&_spraying_materials`, `itns_&_pbos`, monitoring_and_evaluation, `planning,_administration,_overheads`, `procurement_&_supply_management`, other)]


malaria_spend <- melt.data.table(malaria_spend, id.vars = c('ihme_loc_id', 'location_id', 'year_id', 'variable'),
                                 variable.factor = F,
                                 variable.name = 'value_code')

fwrite(malaria_spend, paste0('FILEPATH'))

## End of Script ##