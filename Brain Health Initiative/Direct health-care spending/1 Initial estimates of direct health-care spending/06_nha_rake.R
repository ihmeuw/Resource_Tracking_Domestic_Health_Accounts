#### #----#                    Docstring                    #----# ####
#' Title: 06_nha_rake.R
#' Project:  Resource Tracking - Brain Health Initiative (BHI)
#' Purpose : Map DEX TOC to NHA TOC and rake to the NHA envelope
#'           
#' Author: USERNAME
#' Date: 2024-03-08
#---------------------------------------------------------------------#


#----# Set up directory roots #----#
rm(list=ls())
if (!exists("code_repo"))  {
  code_repo <- unname(ifelse(Sys.info()['sysname'] == "Windows", "ADDRESS", paste0("ADDRESS", Sys.info()['user'][1], "ADDRESS")))
}

## Source functions
source(paste0(code_repo, 'FILEPATH'))
source(paste0(code_repo, 'FILEPATH'))
source(paste0(code_repo, 'FILEPATH'))
source(paste0(dah.roots$k, "FILEPATH"))
source(paste0(dah.roots$k, "FILEPATH"))

#----# Local CONSTANTS #----#
## Variable prep
data_yr <- 2019
currentDate <- format(Sys.time(), "%Y%m%d")
gbd_release_id <- 16
years <- 2000:(data_yr)

## Paths
bhi_share <- paste0('FILEPATH')
raw_data_path <- paste0(bhi_share, 'FILEPATH')
int_data_path <- paste0(bhi_share, 'FILEPATH')
fin_data_path <- paste0(bhi_share, 'FILEPATH')


#---------------------------------------------------------------------#

cat("\n\n")
cat(green("\t#######################################\n"),
    green("\t#######   BEGIN 5_nha_rake.R   #######\n"),
    green("\t#######################################\n\n"))


################### #----# Main #----# ###################
####----# Read and Process Data #----####
cat(' Read and Process Data\n')

## Read in PHC - NHA estimates
nha <- fread(paste0(int_data_path, 'FILEPATH'))

## Read in PHC - NHA mapping
toc_map <- fread(paste0(raw_data_path, 'FILEPATH'))

## Read in DEX-GBD merge file
gbd_dex <- fread(paste0(int_data_path, 'FILEPATH'))

## TOCs
toc_cols <- c('AM', 'ED', 'DV', 'HH', 'NF', 'RX', 'IP')


####----# TOC mapping #----####
cat(' TOC mapping\n')

## Remove total rows from NHA estimates
nha <- nha[sha_hc_name != 'HC Total' & sha_hp_name == 'HP Total']

## Aggregating hc by all hp estimates
nha <- nha[, .(value = sum(value, na.rm = T)), by = .(ihme_loc_id, location_id, year_id, hc, sha_hc_name)]

## Allocate money for NEC estimates
nha <- nha[sha_hc_name %in% toc_map[DEX_TOC != 'None']$hc_name]
nha <- dcast.data.table(nha, ihme_loc_id + location_id + year_id ~ hc, value.var = 'value')

nha[, total_1 := `1.1` + `1.2` + `1.3.1` + `1.3.2` + `1.3.3` + `1.3.nec` + `1.4`]
nha[, total_2 := `2.1` + `2.2` + `2.3` + `2.4`]
nha[, total_3 := `3.1` + `3.2` + `3.3` + `3.4`]

hc1 <- c('1.1', '1.2', '1.3.1', '1.3.2', '1.3.3', '1.3.nec', '1.4')
hc2 <- c('2.1', '2.2', '2.3', '2.4')
hc3 <- c('3.1', '3.2', '3.3', '3.4')

for (col in hc1) {
  nha[, proportion := get(col) / total_1]
  nha[, eval(col) := get(col) + proportion * `1.nec`]
  nha[, `:=` (proportion = NULL)]
}
for (col in hc2) {
  nha[, proportion := get(col) / total_2]
  nha[, eval(col) := get(col) + proportion * `2.nec`]
  nha[, `:=` (proportion = NULL)]
}
for (col in hc3) {
  nha[, proportion := get(col) / total_3]
  nha[, eval(col) := get(col) + proportion * `3.nec`]
  nha[, `:=` (proportion = NULL)]
}

nha[, `:=` (total_1 = NULL, total_2 = NULL, total_3 = NULL)]

## Reshape long again
nha <- melt.data.table(nha,
                       id.vars = c('ihme_loc_id', 'location_id', 'year_id'),
                       variable.name = 'hc',
                       variable.factor = F)

## Merge NHA and TOC mapping
nha <- merge(nha, toc_map, by = c('hc'), all.x = T)

## Remove rows that are not mappable
nha <- nha[!DEX_TOC %in% c('None', 'GA', 'split1', 'split2', 'split3')]

## Fix rows for updates to toc names
nha[DEX_TOC == 'ER', DEX_TOC := 'ED']
nha[DEX_TOC == 'LT', DEX_TOC := 'NF']

## Aggregate by DEX TOC
nha <- nha[, .(value = sum(value, na.rm = T)), by = .(ihme_loc_id, location_id,  year_id, DEX_TOC)]


####----# Rake NHA estimates to total health expenditure #----####
cat(' Rake NHA estimates to total health expenditure\n')

## Pull total health expenditure and keep relevant years
the <- setDT(read_feather('FILEPATH'))
the <- the[year %in% years & scenario == 0]

## Currency conversion
the <- currency_conversion(the,
                           col.loc = 'iso3',
                           col.value = 'the_totes',
                           currency = 'USD',
                           currency.year = 2022,
                           base.unit = 'USD',
                           base.year = 2021,
                           converter.version = 7)

## Calculate means from draws
the <- the[, .(the = mean(the_totes)),
           by = .(ihme_loc_id = iso3, year_id = year)]

## Reshape NHA estimates wide by TOC
nha <- dcast.data.table(nha, ihme_loc_id + location_id + year_id ~ DEX_TOC, value.var = 'value')

## Merge on THE
nha <- merge(nha, the, by = c('ihme_loc_id', 'year_id'), all.x = T)

## Rake NHA estimates to THE
nha[, c(toc_cols) := lapply(.SD, function(x) x / rowSums(.SD, na.rm = T) * the), .SDcols = toc_cols]

## Reshape NHA estimates long by TOC
nha <- melt.data.table(nha,
                       id.vars = c('ihme_loc_id', 'location_id', 'year_id'),
                       measure.vars = toc_cols,
                       variable.name = 'DEX_TOC',
                       variable.factor = F)


####----# Add in 9 missing NHA countries #----####
cat(' Add in 9 missing NHA countries\n')
## Cook Islands, Monaco, Nauru, Niue, Palau, Saint Kitts and Nevis, San Marino, Tokelau, Tuvalu

## Get income groups
income_groups <- fread('FILEPATH')
income_groups <- income_groups[YEAR == data_yr]
setnames(income_groups, c('ISO3_RC', 'INC_GROUP'), c('ihme_loc_id', 'income_group'))

## Subset to the missing countries and relevant years
the <- get_location_id(get_location_name(get_region(the)))
the <- merge(the, income_groups[, .(ihme_loc_id, income_group)], by = c('ihme_loc_id'), all.x = T)
the <- the[year_id %in% years & location_name %in% c('Cook Islands', 'Monaco', 'Nauru', 'Niue', 'Palau', 'Saint Kitts and Nevis', 'San Marino', 'Tokelau', 'Tuvalu')]

## Subset NHA estimates to applicable super regions
nha_temp <- copy(nha)
nha_temp <- get_region(nha_temp)
nha_temp <- merge(nha_temp, income_groups[, .(ihme_loc_id, income_group)], by = c('ihme_loc_id'), all.x = T)
nha_temp <- nha_temp[super_region_name %in% unique(the$super_region_name)]

## Reshape wide by toc to calculate proportions
nha_temp <- dcast.data.table(nha_temp, super_region_name + income_group + ihme_loc_id + location_id + year_id ~ DEX_TOC, value.var = 'value')

## Calculate proportions
nha_temp[, c(toc_cols) := lapply(.SD, function(x) x / rowSums(.SD, na.rm = T)), .SDcols = toc_cols]

## Calculate mean toc by super region and year
nha_temp <- nha_temp[, lapply(.SD, mean, na.rm = T), by = .(super_region_name, income_group, year_id), .SDcols = toc_cols]

## Merge in total health expenditure
nha_temp[, year_id := as.numeric(year_id)]
the <- merge(the, nha_temp, by = c('super_region_name', 'income_group', 'year_id'), all.x = T)

## Calculate type of care spending
the[, c(toc_cols) := lapply(.SD, function(x) x * the), .SDcols = toc_cols]

## Reshape long by toc to append back on to NHA estimates
the <- melt.data.table(the,
                       id.vars = c('ihme_loc_id', 'location_id', 'year_id'),
                       measure.vars = toc_cols,
                       variable.name = 'DEX_TOC',
                       variable.factor = F,
                       value.name = 'value')

## Append on to NHA estimates
nha <- rbind(nha, the)
rm(the, nha_temp)


####----# GBD_DEX raking #----####
cat(' GBD_DEX raking\n')

## Calculate crude totals by location-year-toc
# USA
gbd_dex_states <- copy(gbd_dex)
gbd_dex_states <- gbd_dex_states[location_id == 102]

# Global
gbd_dex <- gbd_dex[location_id != 102]
gbd_dex <- melt.data.table(gbd_dex,
                           id.vars = c('location_id', 'year_id', 'cause_id', 'age_group_id', 'metric_id', 'sex_id', 'acause', 'age_group_name', 'cause_name',
                                       'location_name', 'sex', 'mean_value'),
                           measure.vars = toc_cols,
                           variable.name = 'DEX_TOC',
                           variable.factor = F)

gbd_dex_agg <- copy(gbd_dex)
gbd_dex_agg <- gbd_dex_agg[, .(value = sum(value, na.rm = T)), by = .(location_id, year_id, DEX_TOC)]

## Merge NHA and GBD_DEX
setnames(nha, 'value', 'nha_value')
nha <- nha[location_id != 102]
nha <- merge(nha, gbd_dex_agg, by = c('location_id', 'year_id', 'DEX_TOC'), all.x = T)
rm(gbd_dex_agg)

## Calculate raking ratio
nha[, raking_ratio := nha_value / value]
nha <- nha[, .(location_id, year_id, DEX_TOC, raking_ratio)]

## Merge back to GBD_DEX
gbd_dex <- merge(gbd_dex, nha, by = c('location_id', 'year_id', 'DEX_TOC'), all.x = T)

## Calculate raked values
gbd_dex[, nha_raked_value := value * raking_ratio]
gbd_dex[, `:=` (value = NULL, raking_ratio = NULL)]

## Append state values back on
gbd_dex <- dcast.data.table(gbd_dex, location_id + year_id + cause_id + age_group_id + metric_id + sex_id + sex + acause + age_group_name + cause_name + location_name + mean_value ~ DEX_TOC, value.var = 'nha_raked_value')
gbd_dex <- rbind(gbd_dex, gbd_dex_states)
rm(gbd_dex_states)


####----# Save estimates #----####
cat(' Save estimates\n')

## Write out final estimates
fwrite(gbd_dex, paste0(int_data_path, 'FILEPATH'))

## End of Script ##