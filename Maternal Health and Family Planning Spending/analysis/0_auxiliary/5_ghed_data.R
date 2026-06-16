library(data.table)

source(here::here("src", "r", "params.R"))
source(here::here("src", "r", "internals.R"))

locs <- fread(get_path("int", "data", c("covars", "gbd_locations.csv")))


ghed <- openxlsx::read.xlsx(get_path("raw", "input", c("ghed", "GHED_data.xlsx")))
setDT(ghed)



# current health spending
# (in million current NCU)
che <- ghed[, .(iso3 = code, year_id = year,
                che = che * 1e6,
                dis2 = dis2 * 1e6,               # che for reprod. health
                dis21 = dis21 * 1e6,             # che for maternal conditions
                dis2_pvtd = dis2_pvtd * 1e6,     # domestic private expenditure on RH
                dis21_pvtd = dis21_pvtd * 1e6)]  # domestic private expenditure on MH
che <- melt(che, id.vars = c("iso3", "year_id"), variable.name = "indicator")
che[, value_lcu := value]

# convert from NCU to 2022 USD
che[, curr_year := year_id] ## consumed by currency convert
che <- currency_convert(
    che,
    col.loc = "iso3", col.value = "value",
    # from
    currency = "LCU", col.currency.year = "curr_year",
    # to
    base.unit = "USD", base.year = 2022
)

stopifnot( che[is.na(value) & !is.na(value_lcu), .N] == 0 )

che[, `:=`(
    currency = "2022 USD",
    source = "GHED 2024 - Current Health Expenditure"
)]


fwrite(
    che,
    get_path("int", "data", c("covars", "ghed_che.csv"))
)
