##############
## Pull DHS proportions
## Author: Sawyer Crosby
## Edited by: Ian Cogswell
#############

## Clearing environment
rm(list = ls())

## Setting up OS flexibility
if (Sys.info()[1] == "Linux") {
  j <- FILEPATH
  h <- FILEPATH
  k <- FILEPATH
}else if (Sys.info()[1] == "Windows") {
  j <- FILEPATH
  h <- FILEPATH
  k <- FILEPATH
}

## Loading libraries
library(data.table)
library(tidyverse)
library(foreign)
library(plyr)
library(readstata13)


DHS <- fread("FILEPATH/2_dhs_compiled_factor.csv")[, filename]
DHS_files <- unique(DHS)

##
ihme_loc_ids <- substr(DHS_files, start = 1, stop = 3)
length(unique(ihme_loc_ids))


##

for (i in c(128:length(DHS_files))) {

file_path <- paste0(FILEPATH, DHS_files[i])

# Open the model dataset
dta <- data.table(read.dta13(file_path, convert.factors = FALSE))

colnames(dta) <- tolower(colnames(dta))

dta2 <- dta[, .(v005, h12j, h12k, h12l, h12m)]
dta2[, h12j := recode(h12j, 
                     "1" = 1, 
                     "Yes" = 1,
                     "0" = 0,
                     "No" = 0,
                     "private hosp./clinic" = 1,
                     "Private hosp./clinic" = 1,
                     "private clinic" = 1,
                     "9" = NaN)]

dta2[, h12k := recode(h12k, 
                     "1" = 1,
                     "Yes" = 1,
                     "0" = 0,
                     "No" = 0,
                     "private pharmacy" = 1,
                     "Private pharmacy" = 1,
                     "pharmacy/drugstore" = 1,
                     "private clinic" = 1,
                     "dedicated drug store" = 1,
                     "9" = NaN)]

dta2[, h12l := recode(h12l, 
                     "1" = 1, 
                     "Yes" = 1, 
                     "0" = 0,
                     "No" = 0,
                     "private doctor" = 1, 
                     "Private doctor" = 1,
                     "Pharmacy" = 1, 
                     "9" = NaN)]

dta2[, h12m := recode(h12m, 
                     "1" = 1, 
                     "Yes" = 1, 
                     "0" = 0,
                     "No" = 0,
                     "Private mobile clin." = 1,
                     "private mobile clin." = 1, 
                     "9" = NaN)]

dta2[, any_yes := ifelse(is.na(h12j) & is.na(h12k) & is.na(h12l) & is.na(h12m), NA, 0)]


dta2[any_yes == 0, any_yes := rowSums(.SD, na.rm = T), .SDcols = c('h12j', 'h12k', 'h12l', 'h12k')]
dta2[any_yes > 0, private_facility_use := 1]
dta2[any_yes == 0, private_facility_use := 0]
dta3 <- dta2[is.na(private_facility_use), v005 := 0]

# Create weight variable
dta3[, wt := v005 / 1000000]
dta3[, country := str_sub(DHS_files[i], 1, 3)]

# Tabulate indicator by country
## survey command

proportion <- ddply(dta3, ~country, summarise, mean = weighted.mean(private_facility_use, wt))[, c("mean")]
proportion_data <- c(dta3$country[1], gsub(".*[/]([^.]+)[/].*", "\\1", file_path), proportion)



if (i == 1) {
  dhs_proportions <- proportion_data
} else {
  dhs_proportions <- rbind(dhs_proportions, proportion_data)
}

}

dhs_proportions <- as.data.table(dhs_proportions)
setnames(dhs_proportions, c("V1", "V2", "V3"), c("ihme_loc_id", "year_id", "private_facility_use_prop"))

fwrite(dhs_proportions, file = paste0("FILEPATH/private_facility_use_data_20200422b.csv"))


       