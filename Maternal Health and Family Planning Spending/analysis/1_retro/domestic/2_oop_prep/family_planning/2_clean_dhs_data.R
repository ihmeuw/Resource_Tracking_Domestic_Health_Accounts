#
# clean Demographic Health Survery data obtained from statcompiler
#
library(data.table)
library(openxlsx)
source(here::here("src", "r", "params.R"))

CONFIG <- list(
    files = list(
        locs = get_path("int", "data", c("covars", "gbd_locations.csv")),
        pops = get_path("int", "data", c("covars", "population.csv")) 
    ),
    dirs = list(
        input = get_path("raw", "input"),
        output = get_path("int", "data", c("domestic", "int", "oop_fp"))
    )
)


# map from DHS contracetive indicator title to short name
method_indicator_to_name_map = c(
    "Current use of female sterilization (all women)" = "Female sterilization",
    "Current use of male sterilization (all women)" = "Male sterilization",
    "Current use of pill (all women)" = "Pill",
    "Current use of monthly pill (all women)" = "Pill",
    "Current use of IUD (all women)" = "IUD",
    "Current use of injections (all women)" = "Injectables",
    "Current use of implants (all women)" = "Implants",
    "Current use of condom (all women)" = "Condom",
    "Current use of female condom (all women)" = "Female Condom",
    "Current use of diaphragm (all women)" = "Diaphragm",
    "Current use of foam or jelly (all women)" = "Foam or jelly",
    "Current use of emergency contraception (all women)" = "Emergency contraception"
)

# map from DHS contraceptive method name to RHSC contraception category
method_name_to_category_map = c(
    "Total" = "total",
    "Female sterilization" = "sterilization",
    "Male sterilization" = "sterilization",
    "Pill" = "pill",
    "IUD" = "iud",
    "Injectables" = "injectable",
    "Implants" = "implant",
    "Condom" = "condom",
    "Female condom" = "condom",
    "Diaphragm" = "other",
    "Foam or jelly" = "other",
    "Emergency contraception" = "other"
)

# rename source indicators of interest
source_indicator_map = c(
    "Current users most recent supply or information from a public source" = "public_source",
    "Current users most recent supply or information from a private medical source" = "private_source",
    "Current users most recent supply or information from other non-medical sources" = "nonmedical_source"
)


survey_to_year <- function(x) {
    # ======================================================================== #
    #' Converts suvey names like "DHS 2015-16" to the year 2016 or "DHS 2017" to 2017
    #' Works only if it is true that multi-year dates are in YYYY-ZZ format where
    #' YYYY is the start year and ZZ is the end year
    x <- trimws(gsub("DHS|MIS|AIS", "", x))
    x <- vapply(strsplit(x, "-", fixed = TRUE),
                function(x) {
                    if (length(x) == 1) {
                        as.numeric(x)
                    } else {
                        year1 <- x[1]
                        pref <- as.numeric(substr(x[1], 1, 2))
                        post <- as.numeric(substr(x[2], 1, 2))
                        if (grepl("99$", year1)) pref <- pref + 1
                        return(pref * 100 + post)
                    }
                }, numeric(1))
    return(x)
}


sum_check_na <- function(x) {
    if (all(is.na(x))) {
        return(NA_real_)
    } else {
        return(sum(x, na.rm = TRUE))
    }
}


load_method_data <- function() {
    #
    # Process contraceptive method data
    #
    method_data <- as.data.table(openxlsx::read.xlsx(
        file.path(CONFIG$dirs$input, "dhs", "statcompiler_contraceptive_method.xlsx"),
        skipEmptyRows = TRUE, sep.names = " "
    ))
    
    ## file is not in tidy format - drop extra rows and set column names
    header_row <- which(method_data[, 1] == "Country")
    method_data <- method_data[(header_row + 1):nrow(method_data), ]
    names(method_data) <- c("country", "survey", names(method_data)[-c(1,2)])
    
    final_row <- min(which(method_data[, country] %like% "Current use of any"))
    method_data <- method_data[1:(final_row - 1), ]
    
    ## pivot indicator from wide to long
    method_data <- melt(method_data, id.vars = c("country", "survey"),
                        variable.name = "indicator", value.name = "value",
                        variable.factor = FALSE)
    
    ## drop rows with missing values
    method_data <- method_data[, value := as.numeric(value) / 100] ## % to decimal
    method_data <- method_data[!is.na(value), ]
    
    ## extract year_id from survey - if survey spans multiple years, use last year
    method_data[, year_id := survey_to_year(survey)]
    method_data[, survey := NULL]
    
    return(method_data)
}


load_source_data <- function() {
    #
    # Process contraceptive source data
    #
    source_data <- as.data.table(openxlsx::read.xlsx(
        file.path(CONFIG$dirs$input, "dhs", "statcompiler_contraceptive_source.xlsx"),
        skipEmptyRows = TRUE, sep.names = " "
    ))
    
    ## file is not in tidy format - drop extra rows and set column names
    header_row <- which(source_data[, 1] == "Country")
    source_data <- source_data[(header_row + 1):nrow(source_data), ]
    names(source_data) <- c("country", "survey", names(source_data)[-c(1,2)])
    
    final_row <- min(which(source_data[, country] %like% "Current users most recent supply"))
    source_data <- source_data[1:(final_row - 1), ]
    
    ## pivot indicator from wide to long
    source_data <- melt(source_data, id.vars = c("country", "survey"),
                        variable.name = "indicator", value.name = "value",
                        variable.factor = FALSE)
    ## keep only indicators of interest
    source_data <- source_data[indicator %in% names(source_indicator_map), ]
    source_data[, source := source_indicator_map[indicator]]
    
    ## drop rows with missing values
    source_data[, value := as.numeric(value) / 100] ## convert from % to decimal
    source_data <- source_data[!is.na(value), ]
    
    ## extract year_id from survey - if survey spans multiple years, use last year
    source_data[, year_id := survey_to_year(survey)]
    
    ## cast source type from long to wide
    source_data <- dcast(
        source_data[, .(country, year_id, source, value)],
        country + year_id ~ source, value.var = "value"
    )
    
    # ASSUMPTION
    ## assume that non-medical facilities have same costs as private facilities and
    ## thus can be aggregated
    source_data[, private_source := rowSums(.SD, na.rm = TRUE),
                .SDcols = c("private_source", "nonmedical_source")]
    source_data[, nonmedical_source := NULL]
    return(source_data)
}


load_method_x_source_data <- function() {
    #
    # Process contraceptive use data
    #
    # table is of format:
    # |                  |         Source A        |        Source B
    # | Country | Survey | Method A | Method B |...| Method A | Method B |... 
    # |---------|--------|----------|----------|---|----------|----------|---
    # cells give us the percentage of women currently using modern contraceptive
    # method <method> who received their most recent supply or information from 
    # source <source>, e.g.,
    # (# of women using pill from private source) / (# of women using pill)
    #
    ms_raw <- as.data.table(openxlsx::read.xlsx(
        file.path(CONFIG$dirs$input, "dhs", "statcompiler_contraceptive_source_x_method.xlsx"),
        skipEmptyRows = TRUE, sep.names = " "
    ))
    top_row <- names(ms_raw)
    
    head_row <- which(ms_raw[, 1] == "Country")[1]
    names(ms_raw) <- unlist(ms_raw[head_row, ])
    ms_raw <- ms_raw[(head_row + 1):nrow(ms_raw), ]
    end_row <- min(which(ms_raw[, Country] %like% "Current users"))
    ms_raw <- ms_raw[1:(end_row - 1), ]
    
    sources <- top_row[top_row != ""]
    ## give unique name since some sources repeat for private and public
    sources <- paste0(sources, "__", seq(1, length(sources)))
    top_row[top_row != ""] <- sources
    ms_data <- list()
    for (i in seq_along(sources)) {
        start_ix <- which(top_row == sources[i])
        end_ix <- if (i == length(sources)) {
            ncol(ms_raw)
        } else {
            which(top_row == sources[i + 1]) - 1
        }
        subs <- ms_raw[, c(1,2, start_ix:end_ix), with = FALSE]
        subs[, source := sources[i]]
        ms_data[[i]] <- subs
    }
    # row bind and fill missing cols since not all source indicators contain
    #   all contraceptive methods
    ms_data <- rbindlist(ms_data, fill = TRUE)
    
    # filter to sources of interest
    ms_data[, source := vapply(strsplit(source, "__"), `[[`, character(1), 1)]
    ms_data <- ms_data[source %in% names(source_indicator_map)]
    ms_data[, source := source_indicator_map[source]]
    
    # melt contraceptive type from wide to long
    ms_data <- melt(ms_data,
                    id.vars = c("Country", "Survey", "source"),
                    variable.name = "method", value.name = "value",
                    variable.factor = FALSE)
    
    # extract year_id from survey - if survey spans multiple years, use last year
    ms_data[, `:=` (
        year_id = survey_to_year(Survey),
        Survey = NULL
    )]
    setnames(ms_data, "Country", "country")
    
    ms_data[, value := as.numeric(value) / 100] ## convert from % to decimal
    ms_data <- ms_data[!is.na(value)]
    
    return(ms_data)
}




#
# MAIN ====
#
method_data <- load_method_data()
ms_data <- load_method_x_source_data()

# map method indicator to method name, for merging with method x source data
method_data <- method_data[indicator %in% names(method_indicator_to_name_map)]
method_data[, method := method_indicator_to_name_map[indicator]]
method_data[, indicator := NULL]
setnames(method_data, "value", "value_method")


#
# combine data types to unravel percentages:
#
# E.g., a specific cell in the Source/Method table is:
#   (# of women using pill from private source) / (# of women using pill)
# We also have the following cell from the Method table which is:
#   (# of women using pill) / (all women)
#
# So if we multiply, the (# of women using pill) cancels out and we are left with:
#   (# of women using pill from private source) / (all women)
#
ms_data <- merge(ms_data[method != "total"],
                 method_data,
                 by = c("country", "year_id", "method"),
                 all.x = TRUE)

ms_data[, value := value * value_method]
ms_data[, value_method := NULL]
ms_data <- ms_data[!is.na(value)]

#
# convert method categories to the categories used by RHSC
# (for which we have price data)
#
ms_data[, method := method_name_to_category_map[method]]

# aggregate the new categories
#
# note - summing across values which represent
#     (number of women using method from source) / (all women)
#   so if we sum two method-source combos we get
#     (number of women using method A from X or B from Y) / (all women).
#   Thus it is valid to sum these percentages by these categories.
ms_data <- ms_data[, .(value = sum(value)),
                   by = .(country, year_id, method, source)]


#
# cast source from long to wide
#
ms_data <- dcast(
    ms_data,
    country + year_id + method ~ source,
    value.var = "value"
)
# ASSUMPTION
## assume that non-medical sources have same costs as private sources and
## thus can be aggregated
## (this is a fair assumption since the non-medical sub-categories are "shop",
##  "church", "friend/relative", or "other", which are likely to obtain the
##  contraceptive from private sources themselves).
ms_data[, private_source := rowSums(.SD, na.rm = TRUE),
        .SDcols = c("private_source", "nonmedical_source")]
ms_data[, nonmedical_source := NULL]


#
# Finalize
#

# merge on gbd location info
locs <- fread(CONFIG$files$locs)[level == 3,] # countries
ms_data[, country := fcase(
    country == "Bolivia", "Bolivia (Plurinational State of)",
    country == "Congo Democratic Republic", "Democratic Republic of the Congo",
    country == "Kyrgyz Republic", "Kyrgyzstan",
    country == "Moldova", "Republic of Moldova",
    country == "Tanzania", "United Republic of Tanzania",
    country == "Turkey", "Turkiye",
    rep_len(TRUE, .N), country
)]
ms_data <- merge(ms_data,
                 locs[, .(country = location_ascii_name, ihme_loc_id)],
                 by = "country",
                 all.x = TRUE)

stopifnot( ms_data[is.na(ihme_loc_id), .N] == 0 )


# save
ms_data[, descr := "proportion of all women using method from source-type"]
ms_data[, denominator := "all women age 15-49"]
## note - the denominator according to DHS is "all women" but examining the age
## breakdowns and other DHS info, it is clear that if they are interviewing
## women they are within the age 15-49. E.g., see:
## https://dhsprogram.com/data/Guide-to-DHS-Statistics/Organization_of_DHS_Data.htm#:~:text=The%20Biomarker%20Questionnaire%20collects%20information,and%20men%20age%2015%2D49.
## https://dhsprogram.com/data/Guide-to-DHS-Statistics/Current_Use_of_Contraceptive_Methods.htm

fwrite(ms_data, file.path(CONFIG$dirs$output, "dhs_clean.csv"))








# =============================================================================
#
# deprecated method, pulling in individual files which were then
# merged and multiplied, to crudely estimate the proportion of contraceptive
# methods by source
#
if (FALSE) {
# map from DHS indicator title to RHSC contracpetion category
method_indicator_map = c(
    "Current use of female sterilization (all women)" = "sterilization",
    "Current use of male sterilization (all women)" = "sterilization",
    "Current use of pill (all women)" = "pill",
    "Current use of IUD (all women)" = "iud",
    "Current use of injections (all women)" = "injectable",
    "Current use of diaphragm, foam or jelly (all women)" = "other",
    "Current use of implants (all women)" = "implant",
    "Current use of condom (all women)" = "condom",
    "Current use of female condom (all women)" = "condom",
    "Current use of lactational amenorrhea (all women)" = "lactational_amenorrhea",
    "Current use of emergency contraception (all women)" = "other",
    "Current use of diaphragm (all women)" = "other",
    "Current use of foam or jelly (all women)" = "other",
    "Current use of monthly pill (all women)" = "pill",
    "Current use of standard days method (all women)" = "standard_days_method",
    "Current use of other modern methods (all women)" = "other"
)


method_data <- load_method_data()

# drop traditional methods & any aggregate categories
#    (like any method, any traditional, etc.)
method_data <- method_data[indicator %in% names(method_indicator_map), ]
method_data[, method := method_indicator_map[indicator]]
# drop no-cost methods
method_data <- method_data[! method %in% c("lactational_amenorrhea",
                                           "standard_days_method"),]
stopifnot( method_data[is.na(method), .N] == 0 )

## aggregate by contraception type
### first ensure that no country has multiple observations of the same indicator
### (from different surveys)
test <- method_data[, .(n = .N),
                    by = .(country, indicator, year_id)]
if (test[n > 1, .N] > 0) {
    stop("Some country-years have multiple observations of the same contraceptive indicator.",
         " This should be resolved before aggregating.")
}

method_data <- method_data[, .(value = sum(value)),
                            by = .(country, method, year_id)]

stopifnot( method_data[!between(value, 0, 1), .N] == 0 )


# combine: merge source onto method data to calculate proportion of methods by source
source_data <- load_source_data()

dhs <- merge(
    method_data, source_data,
    by = c("country", "year_id"), all.x = TRUE
)

dhs[, value_private := value * private_source]
dhs[, value_public := value * public_source]


# clean country names
locs <- fread(CONFIG$files$locs)[level == 3,] # countries
dhs[, country := fcase(
    country == "Bolivia", "Bolivia (Plurinational State of)",
    country == "Congo Democratic Republic", "Democratic Republic of the Congo",
    country == "Kyrgyz Republic", "Kyrgyzstan",
    country == "Moldova", "Republic of Moldova",
    country == "Tanzania", "United Republic of Tanzania",
    country == "Turkey", "Turkiye",
    rep_len(TRUE, .N), country
)]


dhs <- merge(dhs,
             locs[, .(country = location_ascii_name, ihme_loc_id)],
             by = "country",
             all.x = TRUE)

stopifnot( dhs[is.na(ihme_loc_id), .N] == 0 )
}
