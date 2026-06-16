#
# process RHSC data containing contraceptive use/cost data
#
library(data.table)
library(openxlsx)
source(here::here("src", "r", "params.R"))



CONFIG <- list(
    files = list(
        locs = get_path("int", "data", c("covars", "gbd_locations.csv"))
    ),
    dirs = list(
        input = get_path("raw", "input"),
        output = get_path("int", "data", c("domestic", "int", "oop_fp"))
    )
)

dir.create(CONFIG$dirs$output, recursive = TRUE, showWarnings = FALSE)



#
# MAIN ====
#


#
# LEAP 2021 data - process for merging with cga19 data
#
leap21 <- fread(file.path(CONFIG$dirs$input, "rhsc", "custom_fp_158.csv"))
leap21 <- leap21[year == 2019, ] # other years are projections
leap21[, indicator := fcase(
    indicator == "Costs", "Cost",
    indicator == "Quantities", "Quantity",
    indicator == "Users", "User"
)]
leap21[, sector := fcase(
    sector == "Private non-subsidized", "Private Non-Sub",
    sector == "Private subsidized", "Private Sub",
    sector == "Public", "Public"
)]
leap21 <- leap21[, .(geo = region,
                     year,
                     indicator = tolower(indicator),
                     sector = gsub(" |-", "_", tolower(sector)),
                     method = tolower(method),
                     type,
                     value = as.numeric(value))]
## aggregate across type (method == "Other" is the only method with sub-types)
leap21 <- leap21[, .(value = sum(value)),
                 by = .(geo, year, indicator, sector, method)]



#
# Process CGA 2019 data
#
cga19 <- as.data.table(openxlsx::read.xlsx(
    file.path(CONFIG$dirs$input, "rhsc", "RHSC CGA 2019 Data for IHME.xlsx"),
    sheet = "Data"))
names(cga19) <- tolower(names(cga19))

cga19[, `:=` (
    sector = gsub(" |-", "_", tolower(sector)),
    indicator = tolower(indicator),
    method = tolower(method),
    value = as.numeric(value),
    gnigroup = NULL
)]




#
# Combine & finalize
#
rhsc_data <- rbind(leap21, cga19)
setnames(rhsc_data, c("geo", "year"), c("country", "year_id"))


# merge on gbd location info
rhsc_data[, country := fcase(
    country == "Bosnia & Herz", "Bosnia and Herzegovina",
    country == "CAR", "Central African Republic",
    country == "Cape Verde", "Cabo Verde",
    country == "Congo, DR", "Democratic Republic of the Congo",
    country == "Côte d'Ivoire", "Cote d'Ivoire",
    country == "Dem. People's Republic of Korea", "Democratic People's Republic of Korea",
    country == "Dominican Rep. ", "Dominican Republic",
    country == "Eq. Guinea", "Equatorial Guinea",
    country == "Marshall Isl.", "Marshall Islands",
    country == "Papua N. Guinea", "Papua New Guinea",
    country == "Russian Fed.", "Russian Federation",
    country == "Solomon Isl.", "Solomon Islands",
    country == "St. Lucia", "Saint Lucia",
    country == "St. Vincent & Gren.", "Saint Vincent and the Grenadines",
    country == "São Tomé & Prin.", "Sao Tome and Principe",
    country == "Bolivia", "Bolivia (Plurinational State of)",
    country == "Iran", "Iran (Islamic Republic of)",
    country == "Lao PDR", "Lao People's Democratic Republic",
    country == "Macedonia", "North Macedonia",
    country == "Moldova", "Republic of Moldova",
    country == "Nauru ", "Nauru",
    country == "Korea DPR", "Democratic People's Republic of Korea",
    country == "Swaziland", "Eswatini",
    country == "Syria", "Syrian Arab Republic",
    country == "Tanzania", "United Republic of Tanzania",
    country == "The Gambia", "Gambia",
    country == "Turkey", "Turkiye",
    country == "Türkiye", "Turkiye",
    country == "Vietnam", "Viet Nam",
    country == "State of Palestine", "Palestine",
    rep_len(TRUE, .N), country
)]

locs <- fread(file.path(CONFIG$files$locs))[level == 3,] ## countries
rhsc_data <- merge(rhsc_data,
                   locs[, .(country = location_ascii_name, ihme_loc_id)],
                   by = "country", all.x = TRUE)

stopifnot( rhsc_data[is.na(ihme_loc_id), .N] == 0 )

rhsc_data[, descr := "Level value of indicator for method sourced from sector"]
rhsc_data[, denominator := "none"]
fwrite(rhsc_data, file.path(CONFIG$dirs$output, "rhsc_clean.csv"))
