#
# process unfpa priority countries
#

library(data.table)
library(readxl)
source(here::here("src", "r", "params.R"))


CONFIG <- list(
    dirs = list(
        input = get_path("raw", "input"),
        covars = get_path("int", "data", "covars")
    )
)


#
# load gbd countries
#
locs <- fread(file.path(CONFIG$dirs$covars, "gbd_locations.csv"))[level == 3, ]

## adjust names to match unfpa spellings
locs[, unfpa_name := fcase(
    location_name == "Bolivia (Plurinational State of)", "Bolivia",
    location_name == "Côte d'Ivoire", "Cote dIvoire",
    location_name == "Democratic People's Republic of Korea", "Dem Rep Korea",
    location_name == "Iran (Islamic Republic of)", "Iran",
    location_name == "Lao People's Democratic Republic", "Lao PDR",
    location_name == "Micronesia (Federated States of)", "Micronesia",
    location_name == "Palestine", "State of Palestine",
    location_name == "Türkiye", "Turkey",
    location_name == "Venezuela (Bolivarian Republic of)", "Venezuela",
    rep_len(TRUE, .N), location_name
)]


#
# read in the unfpa priority countries
#
unfpa <- as.data.table(readxl::read_excel(
    file.path(CONFIG$dirs$input, "misc", "unfpa_priority_countries.xlsx"),
))
names(unfpa) <- gsub(" ", "_", tolower(names(unfpa)))

## merge with gbd locs to add gbd identifiers where applicable
unfpa <- merge(unfpa,
               locs[, .(unfpa_name, ihme_loc_id, location_name)],
               by.x = "country", by.y = "unfpa_name",
               all.x = TRUE)

## some unfpa countries are not gbd locs.
unfpa[, is_gbd := !is.na(ihme_loc_id)]

## create iso3 column with official ISO code for all locs
unfpa[, iso3 := fcase(
    country == "Anguilla", "AIA",
    country == "Aruba", "ABW",
    country == "British Virgin Islands", "VGB",
    country == "Cayman Islands", "CYM",
    country == "Curaçao", "CUW",
    country == "Kosovo", "XKK",  ## Kosovo has no offical ISO, this is current unicode 
    country == "Montserrat", "MSR",
    country == "Sint Maarten", "SXM",
    country == "Turks and Caicos Islands", "TCA",
    rep_len(TRUE, .N), ihme_loc_id
)]

## fill in ihme_loc_id for the non-gbd locs:
##   - most assigned to "_QZA" (unallocated)
##   - Kosovo gets assigned to Serbia
unfpa[is.na(ihme_loc_id), `:=` (
    ihme_loc_id = "_QZA",
    location_name = "Unallocated"
)]
unfpa[country == "Kosovo", `:=` (
    ihme_loc_id = "SRB",
    location_name = "Serbia"
)]


setnames(unfpa,
         c("location_name", "country", "region", "country_id"),
         c("ihme_location_name", "unfpa_country", "unfpa_region", "unfpa_country_id")
         )

#
# Combine UNFPA locs with GBD locs
#
## To complete the data set, add in the gbd locs that are not unfpa priority
## countries, for completeness
## (so the final set of countries includes all gbd locs plus additional unfpa
##  locs that are not gbd locs).

fin <- merge(
    locs[, .(ihme_loc_id, tmp = location_name)],
    unfpa,
    by = "ihme_loc_id",
    all = TRUE
)

fin[, is_unfpa := !is.na(unfpa_country)]
fin[is_unfpa == FALSE, is_gbd := TRUE]
fin[is_unfpa == FALSE, `:=` (
    ihme_location_name = tmp,
    iso3 = ihme_loc_id
)]
fin[, tmp := NULL]


#
# Add on region info and fill in regions for non-GBD locs
#
fin <- merge(fin,
             locs[, .(ihme_loc_id, region_name, super_region_name)],
             by = "ihme_loc_id", all.x = TRUE)

fin[, region_name := fcase(
    unfpa_country %in% c("Anguilla", "Aruba", "British Virgin Islands",
                         "Cayman Islands", "Curaçao", "Montserrat",
                         "Sint Maarten"),
    "Caribbean",
    # note - T&C is not technically in the Caribbean but GBD classifies Bahamas
    # as Caribbean, and T&C is nearby, both on opposite side of Cuba from the
    # Caribbean sea
    unfpa_country == "Turks and Caicos Islands", "Caribbean",
    unfpa_country == "Kosovo", "Central Europe", ## same region as Serbia
    rep_len(TRUE, .N), region_name
)]


fin[, super_region_name := fcase(
    region_name %in% c("Caribbean"), "Latin America and Caribbean",
    region_name %in% c("Central Europe"), "Central Europe, Eastern Europe, and Central Asia",
    rep_len(TRUE, .N), super_region_name
)]


#
# Add on world bank income group
#
wb <- fread(file.path(CONFIG$dirs$covars, "all_wb_ig.csv"))
wb[income_group == "", income_group := NA_character_]

fin <- merge(fin, wb[year == 2022, .(iso3, income_group)],
             by = "iso3",
             all.x = TRUE)

## fill in IG for locations without it - based on OECD recipients found here:
# https://www.oecd.org/dac/financing-sustainable-development/development-finance-standards/DAC-List-of-ODA-Recipients-for-reporting-2024-25-flows.pdf
fin[, income_group := fcase(
    ihme_location_name == "Cook Islands", "H",
    ihme_location_name == "Niue", "UM",
    ihme_location_name == "Tokelau", "LM",
    # As of June 2024, WB has a note saying Venezuela is temporarily unclassified
    # while they wait for revised national accounts data. In recent years
    # (2015-2019) they have been classified as Upper Middle.
    # OECD classifies as UM.
    ihme_location_name == "Venezuela (Bolivarian Republic of)", "UM",
    unfpa_country == "Kosovo", "UM", ## same as Serbia
    unfpa_country == "Anguilla", "H", ## https://www.paho.org/en/anguilla?page=2
    unfpa_country == "Montserrat", "H", ## based on OECD
    rep_len(TRUE, .N), income_group
)]


fwrite(fin, file.path(CONFIG$dirs$covars, "ihme_unfpa_locs.csv"))

