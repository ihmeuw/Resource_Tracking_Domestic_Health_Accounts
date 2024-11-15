rm(list = ls())
library(data.table)
library(tidyverse)
library(readxl)

## --------------------------------------------------------
## Read data
## --------------------------------------------------------

## read NHA, Global fund, GHED, and PAHO data
NHA_GF <- data.table(read_xlsx("FILEPATH", sheet = "extraction"))

NHA <- NHA_GF[source_type == "nha"]
GF <- NHA_GF[source_type %like% "gfatm"]

GHED <- data.table(fread('FILEPATH'))

PAHO <- fread('FILEPATH')

## read old WMR data
WRM_old <- fread("FILEPATH")
WRM_old[, year_id := as.numeric(year_id)]
WRM_old[, year_start := as.numeric(year_start)]
WRM_old[is.na(year_start), year_start := year_id]

WRM_old[is.na(year_start)]

## read new WMR data and recent iterations
WMR_2019 <- fread("FILEPATH")
WMR_2019[, source_id := NA]
WMR_2020 <- fread('FILEPATH')
WMR <- fread('FILEPATH')

## --------------------------------------------------------
## Address duplicate problem specific to global fund data
## --------------------------------------------------------

## split apart funding landscape data
GF_F <- GF[source_type == "gfatm_f"][order(source_id)]
GF_C_P <- GF[source_type != "gfatm_f"]

## remove duplicate data from funding landscape data (duplicates were extracted from different source_ids)
DUPS <- GF_F[, .(location_name_id, year_start, value_code, value)] %>% duplicated()
GF_F <- GF_F[!DUPS]

## re-bind data
GF <- rbind(GF_C_P, GF_F)

## --------------------------------------------------------
## getting Malaria GHES data from each source
## --------------------------------------------------------
NHA_ghes <- NHA[value_code %chin% c("fs_malaria_domestic_public")]
GF_ghes <- GF[value_code %chin% c("fs_malaria_domestic_public")]
WMR_2019_ghes <- WMR_2019[value_code %chin% c('fs_malaria_domestic_public')]
WMR_2020_ghes <- WMR_2020[value_code %chin% c('fs_malaria_domestic_public')]
WMR_ghes <- WMR[value_code %chin% c("fs_malaria_domestic_public")]
WMR_old_ghes <- WRM_old[value_code %chin% c("fs_malaria_domestic_public")]
GHED <- GHED[value_code %chin% c('fs_malaria_domestic_public')]
PAHO <- PAHO[value_code %chin% c('fs_malaria_domestic_public')]

## clean WMR so they match
setnames(WMR_old_ghes, "ihme_loc_id", "location_name_id")
WMR_old_ghes[, `:=`(mult_factor = 1, 
                   currency_year = year_start, 
                   source_type = "wmr_pre_19")]

WMR_2019_ghes[, source_type := "wmr_19"]
WMR_2020_ghes[, source_type := 'wmr_21']
WMR_ghes[, source_type := 'wmr_22']

## --------------------------------------------------------
## prefer new data for WMR
## --------------------------------------------------------

WMR_old_ghes[, country_year := str_c(location_name_id, '_', year_start)]
WMR_2019_ghes[, country_year := str_c(str_sub(location_name_id, -3, -1), '_', year_start)]
WMR_2020_ghes[, country_year := str_c(str_sub(location_name_id, -3, -1), '_', year_start)]
WMR_ghes[, country_year := str_c(str_sub(location_name_id, -3, -1), '_', year_start)]


WMR_old_ghes <- WMR_old_ghes[!(country_year %in% WMR_2019_ghes$country_year)]
WMR_old_ghes <- WMR_old_ghes[!(country_year %in% WMR_2020_ghes$country_year)]
WMR_old_ghes <- WMR_old_ghes[!(country_year %in% WMR_ghes$country_year)]
WMR_2019_ghes <- WMR_2019_ghes[!(country_year %in% WMR_2020_ghes$country_year)]
WMR_2019_ghes <- WMR_2019_ghes[!(country_year %in% WMR_ghes$country_year)]
WMR_2020_ghes <- WMR_2020_ghes[!(country_year %in% WMR_ghes$country_year)]

## --------------------------------------------------------
## binding data together
## --------------------------------------------------------
GHED[, source_id := 1]
WMR_2020_ghes[, source_id := 2]
WMR_ghes[, source_id := 3]
PAHO[, source_id := 4]
setnames(PAHO, 'Value.x', 'value')


ghes_list <- list(NHA_ghes, 
                  GF_ghes, 
                  WMR_ghes, 
                  WMR_old_ghes,
                  WMR_2019_ghes,
                  WMR_2020_ghes,
                  GHED,
                  PAHO)

ghes <- lapply(ghes_list, function(x) x[,.(ihme_loc_id = str_extract(location_name_id, "[:upper:]{3}"), 
                                           year_id = year_start,
                                           value_code, 
                                           value, 
                                           mult_factor, 
                                           units, 
                                           currency_year, 
                                           source_type, 
                                           source_id)])
ghes <- rbindlist(ghes)

## --------------------------------------------------------
## removing duplicates
## --------------------------------------------------------
ghes <- ghes[!duplicated(ghes)]

## --------------------------------------------------------
## converting currency
## --------------------------------------------------------
ghes[, value := as.numeric(str_remove_all(value, ",| "))]

ghes[, `:=`(value = value*mult_factor, 
            mult_factor = NULL)]


## filling currency_year with year (if NA)
ghes[is.na(currency_year), 
     currency_year := year_id]

## convert currency
source(paste0('FILEPATH'))
ghes_conv <- currency_conversion(ghes, 
                                 col.loc = "ihme_loc_id",
                                 col.currency = "units",
                                 col.currency.year = "currency_year",
                                 col.value = "value",
                                 base.year = 2021,
                                 base.unit = "USD",
                                 converter.version = 6.2)

## --------------------------------------------------------
## writing data
## --------------------------------------------------------

fwrite(ghes_conv, "FILEPATH")
