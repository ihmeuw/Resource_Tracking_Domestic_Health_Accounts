###################
## USERNAME
## 
## Code for calculating GHES for malaria on inpatient and outpatient visits
## and adding to current estimates which we believe reflect program spending
## 
###################

pacman::p_load(data.table, tidyverse, openxlsx)

rm(list = ls())

if (Sys.info()["sysname"] == "Linux") {
  j <- "ADDRESS"
  h <- "ADDRESS"
} else {
  j <- "ADDRESS"
  h <- "ADDRESS"
}

source("FILEPATH")
source("FILEPATH")
source('FILEPATH')
source("FILEPATH")

### get location sets
location <- get_location_metadata(location_set_id = 22, gbd_round_id = 7)
ihme_loc <- location[level == 3,.(location_id,ihme_loc_id)]
loc_name <- location[level == 3,.(location_id,ihme_loc_id, location_name)]
loc <- location[level == 3, location_id]


## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Get existing government estimates
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setwd("ADDRESS")

## converted in previous code 
ghes <- fread("FILEPATH")

## get location id
ghes <- merge(ghes, ihme_loc, by = "ihme_loc_id")

## reorder and select columns
ghes <- ghes[, .(location_id, ihme_loc_id, year_id, value, source_type, source_id)]

## select data to adjust (WHO adds in patient care estimates for 2010-)
ghes_adjust <- ghes[source_type %like% "wmr|gfatm" & year_id <=2009]

## select data not to adjust
ghes_no_adjust <- ghes[(!source_type %like% "wmr|gfatm") | (source_type %like% 'wmr|gfatm' & year_id >= 2010)]

## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## get data used to calculate GHES inpatient and outpatient spend
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## ==================
## Get ip/op costs
## ==================

## read in files
ipop_cost_paths <- c("FILEPATH", 
                     "FILEPATH")

## write quick function to get costs
get_draw_means <- function(x) {

  ## read data
  temp <- fread(x)
  
  ## filter to retro and remove scenario/V1
  temp <- temp[year_id %in% 2000:2019, -c("scenario", "V1")]
  
  ## set name based on file path 
  name <- x %>% str_remove("\\/.+\\/") %>% str_remove("_usd.+")
  
  ## melt
  temp <- temp %>% melt.data.table(id.vars = c("location_id", "year_id"), value.name = "unit_cost")
  
  # get mean of draw
  collapse <- temp[, .(mean = mean(unit_cost)), 
                   by = .(location_id, year_id)]
  
  # get ihme loc id for conversion
  collapse <- merge(collapse, ihme_loc, by = "location_id")
  
  # convert currency
  converted <- currency_conversion(data = collapse,
                                   col.loc = 'ihme_loc_id',
                                   col.value = 'mean',
                                   currency = 'USD',
                                   currency.year = '2017',
                                   base.year = 2021,
                                   base.unit = 'USD', 
                                   converter.version = 6.2)
  
  # clean up columns
  out <- converted[, .(location_id, year_id, mean)]
  
  # set name of mean to original variable name
  setnames(out, "mean", name)
  
  return(out)
}

## apply functions to list
ipop_cost_list <- lapply(ipop_cost_paths, function(x) get_draw_means(x))

input <- Reduce(function(x, y) merge(x, y, by = c("location_id", "year_id")), ipop_cost_list, accumulate = F)

## ===============================
## get extracted cost literature
## ===============================

cost_lit <- data.table(read.xlsx(paste0(j,  "FILEPATH")))
cost_lit[, Year := as.numeric(substr(Year, 1, 4))]
cost_lit[is.na(currency_year), currency_year := Year]
cost_lit <- cost_lit[!is.na(mean_median_cost)]

## fix country names
cost_lit[Country == "Democratic Repbulic of the Congo", Country := "Democratic Republic of the Congo"]
cost_lit[Country %like% "China", Country := "China"]

no_drug_diag <- cost_lit[!is.na(median_non_drug_not_diagnostic_cost)]

no_drug_diag[, no_drug_diag_ratio := median_non_drug_not_diagnostic_cost/mean_median_cost]
op_no_drug_diag_ratio <- no_drug_diag[grepl("opt", outpatient.or.inpatient), median(no_drug_diag_ratio)]
ip_no_drug_diag_ratio <- no_drug_diag[grepl("ipt", outpatient.or.inpatient), median(no_drug_diag_ratio)]

op_no_25 <- no_drug_diag[grepl("opt", outpatient.or.inpatient), quantile(no_drug_diag_ratio, .25)]
ip_no_25 <- no_drug_diag[grepl("ipt", outpatient.or.inpatient), quantile(no_drug_diag_ratio, .25)]

op_no_75 <- no_drug_diag[grepl("opt", outpatient.or.inpatient), quantile(no_drug_diag_ratio, .75)]
ip_no_75 <- no_drug_diag[grepl("ipt", outpatient.or.inpatient), quantile(no_drug_diag_ratio, .75)]

## calculate percentiles for sensitivity analysis
cost_lit <- merge(cost_lit, loc_name, by.y = "location_name", by.x = "Country")

cost_lit[,
         `:=`(currency_copy = currency, 
              currency_year_copy = currency_year)]

cost_lit <- currency_conversion(data = cost_lit,
                                col.loc = "ihme_loc_id",
                                col.value = c("mean_median_cost", "median_non_drug_not_diagnostic_cost"),
                                col.currency = "currency",
                                col.currency.year = "currency_year",
                                base.year = 2021, 
                                base.unit = "USD", 
                                converter.version = 6.2)

setnames(cost_lit, c("Year", "mean_median_cost"), c("year_id", "lit_cost"))

cost_lit_all <- copy(cost_lit)

cost_lit <- merge(cost_lit, input, by = c("location_id", "year_id"))

cost_lit[grepl("opt", outpatient.or.inpatient), mal_ghes_cost := lit_cost/op_costs]
cost_lit[grepl("ipt", outpatient.or.inpatient), mal_ghes_cost := lit_cost/ip_costs]

op_mal_cost <- cost_lit[grepl("opt", outpatient.or.inpatient), median(mal_ghes_cost)]
ip_mal_cost <- cost_lit[grepl("ipt", outpatient.or.inpatient), median(mal_ghes_cost)]

op_25 <- cost_lit[grepl("opt", outpatient.or.inpatient), quantile(mal_ghes_cost, .25)]
ip_25 <- cost_lit[grepl("ipt", outpatient.or.inpatient), quantile(mal_ghes_cost, .25)]

op_75 <- cost_lit[grepl("opt", outpatient.or.inpatient), quantile(mal_ghes_cost, .75)]
ip_75 <- cost_lit[grepl("ipt", outpatient.or.inpatient), quantile(mal_ghes_cost, .75)]


## ===============================
## get other variables for calculating government treatment costs
## ===============================

#### elimination status
es <- fread(paste0(j, "FILEPATH"))
input <- merge(input, es, by = "location_id")

#### admissions
mal_ip <- fread(paste0("FILEPATH"))

#### incident cases
incidence <- fread('FILEPATH')
draw_cols <- colnames(incidence)[colnames(incidence) %like% 'draw']
incidence <- melt.data.table(incidence, id.vars = c('location_id', 'year_id'), measure.vars = draw_cols)
incidence <- incidence[location_id %in% loc,
                       .(val = mean(value)),
                       by = .(location_id, year_id)]

setnames(incidence, "val", "incident_cases")

#### any treatment seeking
ts_any <- fread("FILEPATH")
ts_any <- merge(ts_any, ihme_loc, by = "ihme_loc_id")
ts_any[, ihme_loc_id := NULL]

#### public treatment seeking
ts_pub <- fread("FILEPATH")
ts_pub <- ts_pub[, .(public_treat_seek_pct = mean(public_treat_seek_pct)),
                 by = .(location_id, year_id)]

l <- list(input, mal_ip, incidence, ts_any, ts_pub)

output <- Reduce(function(x, y) merge(x, y, by = c("location_id", "year_id")), l, accumulate = F)


## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Adjust government
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

### merge on nmcp data
output <- merge(output, ghes_adjust, by = c("location_id", "year_id"))

#### inpatient costs
output[, ip_cost_ghes := (ip_costs * ip_mal_cost * ip_no_drug_diag_ratio)]
output[, inpatient_spend := incident_cases * mal_ip_cases * (public_treat_seek_pct/treat) * ip_cost_ghes]

## calculate percentiles for sensitivity analysis
output[, ip_cost_ghes_25 := (ip_costs * ip_25 * ip_no_25)]
output[, ip_cost_ghes_75 := (ip_costs * ip_75 * ip_no_75)]
output[, ip_spend_25 := incident_cases * mal_ip_cases * (public_treat_seek_pct/treat) * ip_cost_ghes_25]
output[, ip_spend_75 := incident_cases * mal_ip_cases * (public_treat_seek_pct/treat) * ip_cost_ghes_75]

#### outpatient costs
output[, op_cost_ghes := (op_costs * op_mal_cost * op_no_drug_diag_ratio)]
output[, outpatient_spend := ((public_treat_seek_pct * incident_cases) - (incident_cases * mal_ip_cases * (public_treat_seek_pct/treat))) * op_cost_ghes]

## calculate percentiles for sensitivity analysis
output[, op_cost_ghes_25 := (op_costs * op_25 * op_no_25)]
output[, op_cost_ghes_75 := (op_costs * op_75 * op_no_75)]
output[, op_spend_25 := ((public_treat_seek_pct * incident_cases) - (incident_cases * mal_ip_cases * (public_treat_seek_pct/treat))) * op_cost_ghes_25]
output[, op_spend_75 := ((public_treat_seek_pct * incident_cases) - (incident_cases * mal_ip_cases * (public_treat_seek_pct/treat))) * op_cost_ghes_75]

#### calculate top up value
out_25 <- copy(output)
out_75 <- copy(output)
output[, value := value + inpatient_spend + outpatient_spend]
out_25[, value := value + ip_spend_25 + op_spend_25]
out_75[, value := value + ip_spend_75 + op_spend_75]

## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## prep for writing and write data
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#### subset columns for output
out_list <- lapply(list(output, out_25, out_75), function(x) x[, .(location_id, year_id, value, source_type, source_id)])

#### bind no adjust back in 
ghes_no_adjust <- ghes_no_adjust[,.(location_id, year_id, value, source_type, source_id)]
out_list <- lapply(out_list, function(x) rbind(x, ghes_no_adjust))

paths <- c("FILEPATH", 
           "FILEPATH", 
           "FILEPATH")

for (i in c(1:3)) {
  fwrite(out_list[[i]], paths[i])
}

## End of Script ##