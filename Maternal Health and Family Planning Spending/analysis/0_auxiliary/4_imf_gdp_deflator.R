
library(data.table)
source(here::here("src", "r", "params.R"))

gdp_defl <- fread("/FILEPATH/imf_usgdp_deflators_0424.csv")
names(gdp_defl) <- tolower(names(gdp_defl))
setnames(gdp_defl, "year", "year_id")

gdp_defl <- melt(gdp_defl,
                 id.vars = c("year_id", "iso3", "country",
                             "us_gdp_current", "us_deflator_2017", "us_gdp_constant_2017"),
                 variable.name = "base_year_series",
                 variable.factor = FALSE,
                 value.name = "deflator")

gdp_defl[, base_year_series := gsub("gdp_deflator_", "", base_year_series)]
gdp_defl[, c("country", "us_gdp_current", "us_deflator_2017",
             "us_gdp_constant_2017") := NULL]

fwrite(gdp_defl,
       get_path("int", "data", c("covars", "us_gdp_deflators.csv")))
