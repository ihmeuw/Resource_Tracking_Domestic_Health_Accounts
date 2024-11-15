## ---------------------------------------------------------------------------------------------------------------------
## WHO country profiles
## Author: USERNAME
## Date: February 7, 2022
## Description: Process WHO spending data for modelling
## 
## ---------------------------------------------------------------------------------------------------------------------
### ### ### ### ### ### ###
## 1. Set up environment ## --------------------------------------------------------------------------------------------
### ### ### ### ### ### ###
rm(list = ls())

# Set up directory roots
if (Sys.info()["sysname"] == "Linux") {
  j <- "ADDRESS"
  h <- paste0("ADDRESS")
  k <- "ADDRESS"
} else {
  j <- "ADDRESS"
  h <- "ADDRESS"
  k <- "ADDRESS"
}

library(cowplot)
library(data.table)
library(ggplot2)
library(gridExtra)
library(lme4)
library(readxl)
library(feather)

source(paste0(h, "FILEPATH"))
source(paste0(h, "FILEPATH"))
source(paste0(k, "FILEPATH"))

in.dir <- paste0(j, "ADDRESS")
out.dir <- paste0(j, "ADDRESS")

malaria <- read_feather('FILEPATH')
malaria <- data.table(malaria)
malaria[, dah_ghes := dah + ghes]
malaria <- malaria[, .(the = mean(the), ghes = mean(ghes), dah = mean(dah), dah_ghes = mean(dah_ghes)),
                   .(location_id, year_id)]
malaria <- get_ihme_loc(malaria)

### ### ### ### ### ### ###  ### ###
## 2. Prepare and explore dataset ## -----------------------------------------------------------------------------------
### ### ### ### ### ### ### ###  ###

who_dt1 <- data.table(read_excel(paste0(in.dir, "FILEPATH"), 
                                sheet = "data"))

setnames(who_dt1, c('iso', 'Year'), c('ihme_loc_id', 'year_id'))


who_dt1 <- get_region(who_dt1)
who_dt <- copy(who_dt1)
## categories
names(who_dt)

#`Government Contribution`
donor_contribution_list <- c("Global Fund (including all PRs)", "World Bank", "USAID /PMI", 
                       "Other bilaterals ( AusAUD,CIDA,DFID,JICA etc)", "WHO", "UNICEF", 
                       "Others (EU, other UN, NGOs, foundations etc)")

contribution_list <- c("Government Contribution", donor_contribution_list)

disaggregated_list <- c("Human Resources & Technical Assistance", "Training", "ITNs/PBOs", "Insecticide & spraying materials", 
              "Diagnostics", "Anti-malarial medicines", "Procurement & supply management", "Infrastructure & equipment", 
              "Communication and advocacy", "Monitoring and Evaluation", "Planning, administration, overheads", "Other")

## Create proportions between reported contributions and disagreggated categories
who_dt[, lapply(.SD, sum, na.rm=TRUE), by = ihme_loc_id, .SDcols = disaggregated_list] 

who_dt[, disaggregated_total := rowSums(.SD, na.rm = T), .SDcols = disaggregated_list]
who_dt[, contribution_total := rowSums(.SD, na.rm = T), .SDcols = contribution_list]
who_dt[, donor_contribution_total := rowSums(.SD, na.rm = T), .SDcols = donor_contribution_list]


##  Convert currency, WHO reported currency is in nominal USD
who_dt[, year := year_id]
who_dt <- currency_conversion(who_dt,
                              col.loc = 'ihme_loc_id',
                              col.value = c('Government Contribution', 'disaggregated_total', 'contribution_total', 'donor_contribution_total'),
                              currency = 'USD',
                              col.currency.year = 'year',
                              base.year = 2021,
                              base.unit = 'USD',
                              converter.version = 5.2)


who_dt[, prop_categorized := disaggregated_total / contribution_total]
who_dt[, prop_gov := disaggregated_total / `Government Contribution`]
who_dt[, prop_donor := disaggregated_total / donor_contribution_total]

fwrite(who_dt[, .(ihme_loc_id, year_id, 
                  disaggregated_total, contribution_total, 
                  donor_contribution_total, government_contribution = `Government Contribution`)],
       file = paste0(in.dir, "FILEPATH"))

## ---------------------------------------------------------------------------------------------------------------------

who_spend <- who_dt1[, .(ihme_loc_id, super_region_name, Country, year_id,
                        `Government Contribution`, `Global Fund (including all PRs)`, `World Bank`, `USAID /PMI`,
                        `Other bilaterals ( AusAUD,CIDA,DFID,JICA etc)`, WHO, UNICEF,
                        `Others (EU, other UN, NGOs, foundations etc)`)]

who_spend[, total_spend := rowSums(.SD, na.rm = TRUE),
          .SDcols = c("Government Contribution", "Global Fund (including all PRs)", "World Bank", "USAID /PMI",
          "Other bilaterals ( AusAUD,CIDA,DFID,JICA etc)", "WHO", "UNICEF",
          "Others (EU, other UN, NGOs, foundations etc)")]

who_spend <- merge(who_spend, malaria, by = c('ihme_loc_id', 'year_id'), all.x = T)

who_cat <- who_dt1[, .(ihme_loc_id, super_region_name, Country, year_id,
                      `Human Resources & Technical Assistance`, Training, `ITNs/PBOs`,
                      `Insecticide & spraying materials`, Diagnostics,
                      `Anti-malarial medicines`, `Procurement & supply management`, `Infrastructure & equipment`,
                      `Communication and advocacy`, `Monitoring and Evaluation`, `Planning, administration, overheads`,
                      Other)]

## Excluded obvious erroneous estimates
who_cat[Country == 'Nepal' & year_id == 2009,  
        `:=`(`Insecticide & spraying materials` = NA, `Procurement & supply management` = NA)]

who_cat[ihme_loc_id == 'CIV' & year_id %in% c(2015),
        `:=`(`ITNs/PBOs` = NA)] 

who_cat[ihme_loc_id == 'VEN' & year_id %in% c(2018),
        `:=`(`Human Resources & Technical Assistance` = NA)] 

who_cat <- who_cat[!(ihme_loc_id == 'CIV' & year_id %in% c(2016, 2018)), ]
who_cat <- who_cat[!(ihme_loc_id == 'BFA' & year_id %in% c(2012)), ]
who_cat <- who_cat[!(ihme_loc_id == 'GNB' & year_id %in% c(2009)), ]
who_cat <- who_cat[!(ihme_loc_id == 'TLS' & year_id %in% c(2020)), ]

who_cat <- who_cat[!(ihme_loc_id == 'AFG' & year_id %in% c(2018)), ]
who_cat <- who_cat[!(ihme_loc_id == 'BDI' & year_id %in% c(2009, 2018)), ]
who_cat <- who_cat[!(ihme_loc_id == 'BEN' & year_id %in% c(2014, 2016, 2019)), ]
who_cat <- who_cat[!(ihme_loc_id == 'BFA' & year_id %in% c(2009, 2010, 2011)), ]
who_cat <- who_cat[!(ihme_loc_id == 'BLZ' & year_id %in% c(2014)), ]
who_cat <- who_cat[!(ihme_loc_id == 'BOL' & year_id %in% c(2010)), ]
who_cat <- who_cat[!(ihme_loc_id == 'BWA' & year_id %in% c(2016)), ]
who_cat <- who_cat[!(ihme_loc_id == 'CAF' & year_id %in% c(2013)), ]
who_cat <- who_cat[!(ihme_loc_id == 'DOM' & year_id %in% c(2008, 2009, 2020)), ]
who_cat <- who_cat[!(ihme_loc_id == 'DZA' & year_id %in% c(2008, 2012)), ]
who_cat <- who_cat[!(ihme_loc_id == 'ECU' & year_id %in% c(2014)), ]
who_cat <- who_cat[!(ihme_loc_id == 'GIN' & year_id %in% c(2011)), ]
who_cat <- who_cat[!(ihme_loc_id == 'GNB' & year_id %in% c(2019)), ]
who_cat <- who_cat[!(ihme_loc_id == 'GTM' & year_id %in% c(2017)), ]
who_cat <- who_cat[!(ihme_loc_id == 'HND' & year_id %in% c(2009)), ]
who_cat <- who_cat[!(ihme_loc_id == 'IDN' & year_id %in% c(2008)), ]
who_cat <- who_cat[!(ihme_loc_id == 'MOZ' & year_id %in% c(2017)), ]
who_cat <- who_cat[!(ihme_loc_id == 'MYS' & year_id %in% c(2008)), ]
who_cat <- who_cat[!(ihme_loc_id == 'NPL' & year_id %in% c(2014)), ]
who_cat <- who_cat[!(ihme_loc_id == 'PAN' & year_id %in% c(2009)), ]
who_cat <- who_cat[!(ihme_loc_id == 'PRY' & year_id %in% c(2012, 2014)), ]
who_cat <- who_cat[!(ihme_loc_id == 'RWA' & year_id %in% c(2012)), ]
who_cat <- who_cat[!(ihme_loc_id == 'SAU' & year_id %in% c(2011)), ]
who_cat <- who_cat[!(ihme_loc_id == 'STP' & year_id %in% c(2015)), ]
who_cat <- who_cat[!(ihme_loc_id == 'TZA' & year_id %in% c(2017)), ]
who_cat <- who_cat[!(ihme_loc_id == 'ZAF' & year_id %in% c(2009)), ]
who_cat <- who_cat[!(ihme_loc_id == 'PAK' & year_id %in% c(2016)), ]
who_cat <- who_cat[!(ihme_loc_id == 'PER' & year_id %in% c(2015)), ]
who_cat <- who_cat[!(ihme_loc_id == 'NAM' & year_id %in% c(2020)), ]

who_cat[, total_cat := rowSums(.SD, na.rm = TRUE),
        .SDcols = c("Human Resources & Technical Assistance", "Training", "ITNs/PBOs",
        "Insecticide & spraying materials", "Diagnostics",
        "Anti-malarial medicines", "Procurement & supply management", "Infrastructure & equipment",
        "Communication and advocacy", "Monitoring and Evaluation", "Planning, administration, overheads",
        "Other")]

## Combine "Human Resources & Technical Assistance" and "Training" to align with IHME categories
who_cat[, `Human Resources` :=rowSums(.SD, na.rm = TRUE), 
        .SDcols = c("Human Resources & Technical Assistance", "Training")]
who_cat[, `:=`(`Human Resources & Technical Assistance` = NULL, `Training` = NULL)]

who_cat <- who_cat[total_cat != 0,]
who_cat <- melt(who_cat, id.vars = c('ihme_loc_id', 'super_region_name', 'Country', 'year_id', 'total_cat'))

who_cat[, col_year := year_id]

## Reported currency is in nominal USD.
who_cat1 <- currency_conversion(who_cat,
                               col.loc = 'ihme_loc_id',
                               col.value = c('value', 'total_cat'),
                               currency = 'USD',
                               col.currency.year = 'col_year',
                               base.year = 2021,
                               base.unit = 'USD',
                               converter.version = 5.2)

who_cat <- copy(who_cat1)
fwrite(who_cat, paste0('FILEPATH'))

### Cleaning up dataset

who_cat2 <- setdiff(who_cat0, who_cat)
who_cat <- merge(who_cat, malaria, by = c('ihme_loc_id', 'year_id'), all.x = TRUE)

sr_list <- c("North Africa and Middle East", "Sub-Saharan Africa",  "High-income",
             "Central Europe, Eastern Europe, and Central Asia", "South Asia", "Latin America and Caribbean", 
             "Southeast Asia, East Asia, and Oceania")


########################################################################################################################

sum_cat <- who_total_cat[, .(total_cat = sum(total_cat, na.rm = T), the = sum(the, na.rm = T)), by = ihme_loc_id]
sum_cat[, total_diff := the - total_cat]
sum_cat[total_diff < 0, high_priority := 'Yes']

who_total_cat[, diff := the - total_cat]
who_total_cat[diff < 0, priority := 'Yes']

sum_cat <- merge(sum_cat, who_total_cat, by = 'ihme_loc_id', all = T)
fwrite(sum_cat, 'FILEPATH')

who_spend[Country %in% c('Niger')]
who_spend[Country %in% c('South Sudan', 'Niger')]
who_cat[Country %in% c('Nepal') & year_id == 2009]


## End of Script ## 
