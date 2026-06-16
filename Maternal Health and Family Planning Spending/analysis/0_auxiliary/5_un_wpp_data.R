#
# loads the bulk download of UN World Population Prospect data, cleans it, and
# saves it for use in pipelines
#

library(data.table)
library(openxlsx)

source(here::here("src", "r", "params.R"))


#
# Only saving some of the many indicators
#
keep_cols <- c(
    "country" = "Region, subregion, country or area *",
    "iso3" = "ISO3 Alpha-code",
    "type" = "Type",
    "year" = "Year",
    "total_pop_1000" = "Total Population, as of 1 July (thousands)",
    "pop_density" = "Population Density, as of 1 July (persons per square km)",
    "pop_sex_ratio" = "Population Sex Ratio, as of 1 July (males per 100 females)",
    "tfr" = "Total Fertility Rate (live births per woman)",
    "mean_age_childbear" = "Mean Age Childbearing (years)",
    "life_exp_birth" = "Life Expectancy at Birth, both sexes (years)",
    "infant_mortality_rate" = "Infant Mortality Rate (infant deaths per 1,000 live births)"
)
ind_to_short <- setNames(names(keep_cols), keep_cols)


#
# Load raw data from un wpp
#

## retrospective estimates
wpp <- openxlsx::read.xlsx(
    get_path("raw", "input",
             c("wpp", "WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx")),
    startRow = 17,
    sep.names = " ",
    sheet = "Estimates"
)
setDT(wpp)
wpp <- wpp[, ..keep_cols]
wpp <- wpp[between(Year, 1995, 2023) & Type == "Country/Area"]


## prospective estimates
wpp_est <- openxlsx::read.xlsx(
    get_path("raw", "input",
             c("wpp", "WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx")),
    startRow = 17,
    sep.names = " ",
    sheet = "Medium variant"
)
setDT(wpp_est)
wpp_est <- wpp_est[, ..keep_cols]
wpp_est <- wpp_est[between(Year, 2024, 2050) & Type == "Country/Area"]


## combine
wpp[, is_forecast := FALSE]
wpp_est[, is_forecast := TRUE]
wpp_full <- rbind(wpp, wpp_est)


#
# Finalize & save
#

wpp_full[, Type := NULL]
setnames(wpp_full,
         c("Region, subregion, country or area *", "ISO3 Alpha-code", "Year"),
         c("country", "iso3", "year"))

wpp_full <- melt(wpp_full,
                 id.vars = c("country", "iso3", "year", "is_forecast"),
                 variable.name = "indicator",
                 variable.factor = FALSE)

wpp_full[, indicator_short := ind_to_short[indicator]]


output_dir <- get_path("int", "data", c("covars"))
fwrite(wpp_full,
       file.path(output_dir, "un_wpp_data.csv"))
