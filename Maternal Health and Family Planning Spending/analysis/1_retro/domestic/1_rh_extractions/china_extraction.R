library(data.table)
library(arrow)
source(here::here("src", "r", "params.R"))
source(here::here("src", "r", "internals.R"))


PATHS <- list(
    files = list(
        gbd_locs = get_path("int", "data", c("covars", "gbd_locations.csv")),
        ihme_unfpa_locs = get_path("int", "data", c("covars", "ihme_unfpa_locs.csv"))
    ),
    dirs = list(
        input = get_path("raw", "input"),
        output = get_path("int", "data", c("domestic", "int"))
    )
)



china_public_fp <- function(min_yr, max_yr) {
    dt <- fread(file.path(PATHS$dirs$input, "extractions", "china_extractions.csv"))
    dt[, `:=`(
        # scale values to single dollars
        public_fp_lcu = public_fp * mult_factor,
        public_mh_ch_lcu = public_mh_ch * mult_factor
    )]
    dt[, `:=`(
        # setup cols for currency conversion
        ihme_loc_id = "CHN",
        curr_year = year,
        public_fp = public_fp_lcu,
        public_mh_ch = public_mh_ch_lcu,
        currency = NULL
    )]
    ## currency convert public_fp and public_mh_ch
    dt <- currency_convert(
        dt,
        # what
        col.loc = "ihme_loc_id", col.value = "public_fp",
        # from
        currency = "lcu", col.currency.year = "curr_year",
        # to
        base.unit = "usd", base.year = 2023
    )
    dt[, curr_year := year]
    dt <- currency_convert(
        dt,
        # what
        col.loc = "ihme_loc_id", col.value = "public_mh_ch",
        # from
        currency = "lcu", col.currency.year = "curr_year",
        # to
        base.unit = "usd", base.year = 2023
    )
    
    dt_fp <- dt[, .(
        src = "china",
        iso3 = "CHN",
        year_id = year,
        value_code = "public_dis2.3",
        value = public_fp,
        units = "2023 USD"
    )]
    fwrite(
        dt_fp, file.path(PATHS$dirs$output, "china_public_fp.csv")
    )
}
