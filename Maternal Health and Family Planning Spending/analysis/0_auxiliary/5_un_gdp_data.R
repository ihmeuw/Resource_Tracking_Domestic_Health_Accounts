#
# loads and processes the UN GDP data in constant 2015 USD for countries
#
library(data.table)
library(openxlsx)

source(here::here("src", "r", "params.R"))


# Load the data
raw <- openxlsx::read.xlsx(
    get_path("raw", "input", c("un_gdp", "Download-GDPconstant-USD-countries.xlsx")),
    startRow = 3
)
setDT(raw)


# clean
gdp <- raw[IndicatorName == "Gross Domestic Product (GDP)"]
gdp <- melt(gdp,
            id.vars = c("CountryID", "Country", "IndicatorName"),
            variable.name = "year")
names(gdp) <- tolower(names(gdp))
gdp[, indicatorname := "GDP in constant 2015 USD"]


# merge on iso codes
un_meta <- fread(get_path("raw", "input", c("un_gdp", "UNSD — Methodology.csv")))
un_meta <- un_meta[, c("Country or Area", "ISO-alpha3 Code")]
names(un_meta) <- c("country", "iso3")

## manually change some country names since the GDP data doesn't abide by standard
un_meta[, country := fcase(
    country %like% "Côte d’Ivoire", "Côte d'Ivoire",
    country %like% "Democratic Republic of the Congo", "D.R. of the Congo", 
    country %like% "Lao People's Democratic Republic", "Lao People's DR",
    country %like% "Micronesia", "Micronesia (FS of)",
    country %like% "Saint Vincent and the Grenadines", "St. Vincent and the Grenadines",
    country %like% "United Republic of Tanzania", "U.R. of Tanzania: Mainland",
    country %like% "Democratic People's Republic of Korea", "D.P.R. of Korea",
    rep_len(TRUE, .N), country
)]


gdp <- merge(
    gdp, un_meta,
    by = "country",
    all.x = TRUE
)
gdp[country == "Kosovo", iso3 := "XKK"]
gdp <- gdp[!is.na(iso3)]


# merge on unfpa priority indicator
unfpa_locs <- fread(get_path("int", "data", c("covars", "ihme_unfpa_locs.csv")))

gdp <- merge(
    gdp, unfpa_locs[, .(iso3, is_unfpa)],
    by = "iso3",
    all.x = TRUE
)


## no data for NIU or TKL - unfpa priority countries

# save
fwrite(gdp, get_path("int", "data", c("covars", "un_gdp_data.csv")))
