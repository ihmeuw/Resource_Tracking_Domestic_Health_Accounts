#############################################################
#'
#' 
#' @description pull DAH data for TB in multiple formats
#' -Global DAH
#' -Country-level DAH for Malaria
#' -Country-level DAH broken down by:
#'     -Source
#'     -Channel
#'     -Program Area
#############################################################

rm(list = ls())

if (!exists("code_repo"))  {
  code_repo <- unname(ifelse(Sys.info()['sysname'] == "Windows", 
                             "FILEPATH", 
                             paste0("FILEPATH")))
}


source(paste0(code_repo, "FILEPATH"))

out.dir <-paste0(mal.roots$malaria_share, "FILEPATH")

## parameters
fgh_year <- 2021
cds_year <- 2020


##########################
## Read in DAH database ##
##########################


## read in data
dah_db <- fread(paste0("FILEPATH", fgh_year, "FILEPATH", fgh_year, "FILEPATH"))

## remove double counting
dah_db <- dah_db[ELIM_CH %in% c(0, NA) & ELIM_DONOR %in% c(0, NA)]

## assign sources
# BMGF
dah_db[DONOR_NAME == "BMGF", source := "BMGF"]
dah_db[CHANNEL == "BMGF", source := "BMGF"]

# governments
isos <- c('USA', 'GBR', 'DEU', 'FRA', 'CAN', 'AUS', 'JPN', 'ESP', 'NLD', 'AUT', 'BEL',
          'DNK', 'FIN', 'GRC', 'IRL', 'ITA', 'KOR', 'LUX', 'NZL', 'PRT', 'SWE', 'CHE', 'CHN')
dah_db[INCOME_SECTOR == 'PUBLIC' & ISO_CODE %in% isos, source := ISO_CODE]

dah_db[INCOME_SECTOR == "PUBLIC" & ISO_CODE %in% c("CZE", "HUN", "ISL", "POL", "SVK", "SVN"), source := "OTHERDAC"]
dah_db[INCOME_SECTOR == "PUBLIC" & is.na(source), source := "OTHERPUB"]

# corporate donations
dah_db[INCOME_SECTOR == "INK", source := "PRIVINK"]


View(unique(dah_db[is.na(source), .(INCOME_SECTOR, ISO_CODE, DONOR_NAME)]))

## get lower case column names 
colnames(dah_db) <- tolower(colnames(dah_db))


## subset columns
keep_cols <- c("year", "source", "channel", "iso3_rc", "level", "inkind", colnames(dah_db)[grepl("mal_", colnames(dah_db))])
dah_db <- dah_db[year %in% c(2000:cds_year), keep_cols, with = FALSE]
setnames(dah_db, c("year", "iso3_rc"), c("year_id", "ihme_loc_id"))


##===================##
## get malaria estimates ##
##===================##

dah_db[, `:=`(fs_mal_dah = get(paste0('mal_dah_', substr(fgh_year, 3, 4))))]


##================##
## Get global DAH ##
##================##

## subset to only global and unallocable dah & include INKIND
global <- dah_db[ihme_loc_id %in% c('G', 'QZA')  | inkind == 1]

## melt columns long and sum up Malaria DAH
global_w_inkind <- global %>% group_by(year_id, inkind) %>% dplyr::summarise(global_dah = sum(fs_mal_dah, na.rm = T)) %>% data.table
global <- global %>% group_by(year_id) %>% dplyr::summarise(global_dah = sum(fs_mal_dah, na.rm = T)) %>% data.table


## write output
fwrite(global, "FILEPATH")
fwrite(global_w_inkind, "FILEPATH")

##=======================##
## get country level DAH ##
##=======================##

## remove global and unallocable DAH
non_global_dah <- dah_db[!ihme_loc_id %in% c('G', 'QZA') & inkind != 1]

## sum to total per country-year
non_global_dah1 <- non_global_dah[, .(fs_mal_dah = sum(fs_mal_dah)), .(ihme_loc_id, year_id)]

## melt data to get health focus areas long
mal_cols <- colnames(non_global_dah)[colnames(non_global_dah) %like% 'mal']

non_global_dah <- melt.data.table(non_global_dah,
                                  id.vars = c('ihme_loc_id', 'year_id'),
                                  measure.vars = c(mal_cols),
                                  variable.name = 'program_area',
                                  variable.factor = F)

non_global_dah <- non_global_dah[, .(value = sum(value)),
                                 by = .(ihme_loc_id, year_id, program_area)]

## write file
fwrite(non_global_dah1, paste0(out.dir, "FILEPATH"))
fwrite(non_global_dah, paste0(out.dir, "FILEPATH"))

## End of Script ##