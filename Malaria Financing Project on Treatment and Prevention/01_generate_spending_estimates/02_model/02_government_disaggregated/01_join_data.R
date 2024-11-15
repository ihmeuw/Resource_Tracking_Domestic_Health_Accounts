#######################################################
# USERNAME
# 
# Read in and standardize disaggregated government spending data
#######################################################

##===================##
## Prepare workspace ##
##===================##
pacman::p_load(data.table, tidyverse, openxlsx, gridExtra, cowplot, readxl)

rm(list = ls())

if (Sys.info()["sysname"] == "Linux") {
  j <- "ADDRESS"
  h <- "ADDRESS"
  k <- 'ADDRESS'
} else {
  j <- "ADDRESS"
  h <- "ADDRESS"
  k <- 'ADDRESS'
}

source("FILEPATH")
source('FILEPATH')
source("FILEPATH")
malaria_country_list <- fread('FILEPATH')

# Read in data
nha <- fread('FILEPATH')
who <- fread('FILEPATH')
nmcp <- fread('FILEPATH')

# Remove NHA values of 0
nha <- nha[value != 0]

# Add nha source column
nha[, source_type := 'nha']

# Update NHA value codes
nha[value_code == 'ITNs', value_code := 'ITNs & PBOs']
nha[value_code == 'Human resources & technical assistance', value_code := 'Human Resources & Technical Assistance']
nha[value_code == 'Insecticides and spray materials', value_code := 'Insecticide & spraying materials']
nha[value_code == 'Monitoring and evaluation', value_code := 'Monitoring and Evaluation']
nha[value_code == 'Antimalarial medicines', value_code := 'Anti-malarial medicines']
nha[value_code == 'Diagnostic testing', value_code := 'Diagnostics']

## Remove WHO values of zero
who <- who[value_ghes > 0]
who <- who[, .(ihme_loc_id, year_id, value_code = who_name, value = value_ghes, source_type = 'who')]

## Update WHO value codes
who[value_code == 'ITNs/PBOs', value_code := 'ITNs & PBOs']
who[value_code == 'Human Resources', value_code := 'Human Resources & Technical Assistance']

# Clean NMCP data
nmcp <- nmcp[funding_source == 'Domestic' & `aggregation level (unit cost vs total)` == 'total']
nmcp <- nmcp[!(`health_function/spending category` %in% c('unspecified', 'unspecified (total)'))]
nmcp[, value := as.numeric(gsub(',', '', value))]
nmcp[, mult_factor := as.numeric(gsub(',', '', mult_factor))]
nmcp <- nmcp[value > 0]
nmcp[, value := value * mult_factor]
nmcp <- nmcp[, .(location_name_id, ihme_loc_id, year_start, value_code = `health_function/spending category`, value, currency, base_year = year_start)]

# Fix all NMCP categories to align
nmcp <- nmcp[!(value_code == 'Other' & ihme_loc_id == 'SLB')]
nmcp[value_code %in% c('monitoring and evaluation', 'Monitoring and evaluation'), value_code := 'Monitoring and Evaluation']
nmcp[value_code %in% c('Insecticide and spraying materials (excluding distribution costs)'), value_code := 'Insecticide & spraying materials']
nmcp[value_code %in% c('Diagnostics (excluding distribution costs)', 'Malaria RDT Kit', 'Diagnostic testing', 'Diagnostic Testing') , value_code := 'Diagnostics']
nmcp[value_code %in% c('Human resources and technical assistance', 'Personnel costs', 'Staffing'), value_code := 'Human Resources & Technical Assistance']
nmcp[value_code %in% c('Antimalarial medicines (excluding distribution costs)', 'Antimalarial Drugs and Supplies', 'Antimicrobial medicines', 'Drugs and medical supplies'), value_code := 'Anti-malarial medicines']
nmcp[value_code %in% c('Infrastructure and other equipment'), value_code := 'Infrastructure & equipment']
nmcp[value_code %in% c('Procurement and supply management costs (transport, fees, etc.)', 'Transportation costs'), value_code := 'Procurement & supply management']
nmcp[value_code == 'Training' & ihme_loc_id %in% c('TKM', 'TUR'), value_code := 'Human Resources & Technical Assistance']
nmcp[value_code == 'Programme management and administration costs', value_code := 'Planning, administration, overheads']

## Collapse training and human resources and technical assistance
nmcp <- nmcp[, .(value = sum(value)),
             by = .(location_name_id, ihme_loc_id, year_start, value_code, currency, base_year)]

## Remove value codes not leveraged
nmcp <- nmcp[(value_code %in% unique(who$value_code))]

## currency convert NMCP
nmcp <- currency_conversion(nmcp,
                            col.loc = 'ihme_loc_id',
                            col.value = 'value',
                            col.currency = 'currency',
                            col.currency.year = 'base_year',
                            base.year = 2021,
                            base.unit = 'usd',
                            converter.version = 6.2)
nmcp <- nmcp[, .(ihme_loc_id, year_id = year_start, value_code, value, source_type = 'nmcp')]

# Combine datasets
dt <- rbind(nha, who, nmcp)

# Keep relevant countries
dt <- dt[ihme_loc_id %in% malaria_country_list$ihme_loc_id]

# Assign sub-saharan africa other and non-sub-saharan africa other
dt <- get_region(dt)
dt[super_region_name == 'Sub-Saharan Africa' & value_code == 'Other', value_code := paste0(value_code, ' SSA')]
dt[super_region_name != 'Sub-Saharan Africa' & value_code == 'Other', value_code := paste0(value_code, ' nonSSA')]

## Write out data
fwrite(dt, 'FILEPATH')

## End of Script ##