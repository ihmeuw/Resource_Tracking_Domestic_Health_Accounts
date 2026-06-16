
library(jsonlite)
library(data.table)
library(arrow)

source(here::here("src", "r", "params.R"))

CONFIG <- list(
    files = list(
        locs = get_path("int", "data", c("covars", "gbd_locations.csv")),
        iso2iso3 = get_path("raw", "input", c("misc", "country_iso2to3.csv"))
    ),
    dirs = list(
        covars = get_path("int", "data", c("covars")),
        output = get_path("int", "data", c("domestic", "int", "oop_mh"))
    )
)
CONFIG$dirs$cache <- file.path(CONFIG$dirs$output, "raw_dhs")

invisible(lapply(CONFIG$dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

ISO2ISO3 <- fread(CONFIG$files$iso2iso3)


query_dhs_api <- function(indicator, rows_per_page = 10000) {
    url <- paste0("https://api.dhsprogram.com/rest/dhs/data/",
                  paste(indicator, collapse = ","),
                  "?f=json&perpage=",
                  rows_per_page)
    resp <- fromJSON(url)
    if (resp$RecordsReturned != resp$RecordCount) {
        stop("Not all records were returned, need to increase perpage")
    }
    return(as.data.table(resp$Data))
}


load_dhs_indicator <- function(indicator_name,
                               indicator_dict,
                               cache_path,
                               requery = FALSE) {
    cache_file <- file.path(cache_path, paste0(indicator_name, ".arrow"))
    if (!requery && file.exists(cache_file)) {
        return(read_feather(cache_file))
    }
    dt <- query_dhs_api(indicator_dict[[indicator_name]])
    write_feather(dt, cache_file)
    return(dt)
}


get_preferred_values <- function(dt) {
    #' If a particular indicator has multiple values because it is disaggregated
    #' in multiple ways, IsPreferred will identify which is the preferred value
    #' to display by default.
    #' Practically, for c-section and place-of-delivery data, the denominator is
    #' the number of live or still births in the 2, 3, or 5-years preceding the
    #' survey. All are reported in later survey-years, but 2-years preceding are
    #' the ones flagged by IsPreferred.
    #' For clarity, we make explicity that we favor the values calculated over
    #' the 2-years preceding the survey.
    #' For some older surveys, the data is only disaggregated by 3 or 5 years, in
    #' which case we use those values. 
    dt[, n_points := .N, by = .(DHS_CountryCode, SurveyYear, Indicator)]
    dt[, two_year := grepl("^Two years preceding", ByVariableLabel)]
    dt <- dt[n_points == 1 | two_year == 1]
    
    stopifnot(
        dt[, .N, by = .(DHS_CountryCode, SurveyYear, Indicator)][, unique(N)] == 1
    )
    dt[, c("n_points", "two_year") := NULL]
    return(dt)
    
}

merge_iso3_codes <- function(dt) {
    dt <- merge(dt, ISO2ISO3, by.x = "DHS_CountryCode", by.y = "alpha2")
    setnames(dt, "alpha3", "ihme_loc_id")
    if( dt[is.na(ihme_loc_id), .N] != 0 ) {
        stop("Some countries do not have an ISO3 code for indicator: ",
             dt$Indicator[1])
    }
    return(dt)
}


finalize_dhs_cols <- function(dt) {
    dt <- dt[, .(
        ihme_loc_id,
        year_id = SurveyYear,
        indicator = Indicator,
        indicator_id = IndicatorId,
        value = as.numeric(Value) / 100 # convert from percentage
    )]
    return(dt)
}



load_total_births <- function(covars_path) {
    # load live births in 1000s 
    lb <- fread(file.path(CONFIG$dirs$covars, "live_births.csv"))
    lb <- lb[year_id >= 1990, .(ihme_loc_id, year_id, live = mean_value * 1000)]
     
    # load stillbirth ratio (stillbirths / livebirths)
    sbr <- fread(file.path(CONFIG$dirs$covars, "still_birth_ratio.csv"))
    sbr <- sbr[year_id >= 1990, .(ihme_loc_id, year_id, sbr = mean_value)]
    
    # combine and compute still births, and total births
    b <- merge(lb, sbr, by = c("ihme_loc_id", "year_id"), all = TRUE)
    stopifnot(
        b[is.na(live), .N] == 0, b[is.na(sbr), .N] == 0
    )
    
    b[, still := live * sbr]
    b[, total_births := live + still]
    return(b)
}


#
# MAIN =======================================================================
#



indicators <- list(
    "c_section" = "RH_DELA_C_CSC",
    # "stillbirth_rate" = "CM_PNMR_C_SBR",
    "place_of_delivery" = c(
        "RH_DELP_C_PUB",
        "RH_DELP_C_PRV",
        "RH_DELP_C_HOM",
        "RH_DELP_C_OTH",
        "RH_DELP_C_DKM",
        "RH_DELP_C_DHF"
    )
)


#
# load data sets, querying dhs api as needed
#
sets <- lapply(names(indicators), function(nm) {
    load_dhs_indicator(nm, indicators, CONFIG$dirs$cache)
})
names(sets) <- names(indicators)

#
# apply standardizations
#
sets <- lapply(sets, get_preferred_values)
sets <- lapply(sets, merge_iso3_codes)
sets <- lapply(sets, finalize_dhs_cols)

#
# apply data set specific standardizations
#

# c-section
sets$c_section[, value_id := "c_section"]
sets$c_section[, c("indicator", "indicator_id") := NULL]

# place of delivery:
# - we will keep proportion in public and private facility, and at home, but
#   drop percentage at "other" and "don't know", since these are relatively
#   small (max values of 6.8% and 8.5% respectively) and cannot be costed.
# - we also drop the "health facility" category since this just sums across
#   public and private facilities.
sets$place_of_delivery <- sets$place_of_delivery[
    indicator_id %in% c("RH_DELP_C_PUB", "RH_DELP_C_PRV", "RH_DELP_C_HOM")
]
sets$place_of_delivery[, value_id := fcase(
    indicator_id == "RH_DELP_C_PUB", "public_facility_delivery",
    indicator_id == "RH_DELP_C_PRV", "private_facility_delivery",
    indicator_id == "RH_DELP_C_HOM", "home_delivery"
)]
sets$place_of_delivery[, c("indicator", "indicator_id") := NULL]


#
# combine
#
# note - confirmed that if we separately model each indicator instead of
#   multiplying across by c-section here we can preserve some raw data values.
#
full <- rbindlist(sets)


#
# add denominator
#
births <- load_total_births(CONFIG$dirs$covars)

full <- merge(full,
              births[, .(ihme_loc_id, year_id, total_births)],
              by = c("ihme_loc_id", "year_id"),
              all.x = TRUE)

full[, denominator := "Total births (live and still)"]

message("* Saving dhs_mh_clean.csv to ", CONFIG$dirs$output)
fwrite(full, file.path(CONFIG$dirs$output, "dhs_mh_clean.csv"))

