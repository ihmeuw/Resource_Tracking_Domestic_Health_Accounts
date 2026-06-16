#
# helper functions to expose internal IHME code in a sane way
#
library(pacman) # pacman is required by currency_conversion.R

source_gbd_utils <- function() {
    #' Source GBD shared functions
    source("/FILEPATH/get_ids.R")
    source("/FILEPATH/get_draws.R")
    source("/FILEPATH/get_outputs.R")
    source("/FILEPATH/get_population.R")
    source("/FILEPATH/get_model_results.R")
    source("/FILEPATH/get_location_metadata.R")
    source("/FILEPATH/get_covariate_estimates.R")
}


source_stgpr_api <- function() {
    #' Source ST-GPR API
    source("/FILEPATH/public.R")
}


._source_currency_converter <- function() {
    env <- new.env()
    # source to a local env to avoid polluting the global env and to silence
    # the loader's print statements
    . <- capture.output(source("/FILEPATH/currency_conversion.R",
                               local = env))
    return(env)
}



currency_convert <- function(...) {
    # ======================================================================== #
    #' Perform currency conversion using Economic Indicator team's public function
    #' 
    #' Think of the arguments as specifying WHAT to convert, FROM what currency,
    #' and TO which currency. The converter values vary by country and year, hence
    #' we must specify these in arguments, either for the full data set or by
    #' specifying a column so that the transformation varies by row.
    #' 
    #' Example - convert from local currency unit (LCU) with base-year determined
    #' by the values of column "year_id" to US dollars with 2022 base-year:
    #' currency_convert(
    #'     data = data,
    #      # what
    #'     col.loc = "ihme_loc_id", col.value = "value",
    #'     # from
    #'     currency = "lcu", col.currency.year = "year_id",
    #'     # to {base currency} {base year}
    #'     base.unit = "usd", base.year = 2022
    #' )
    #'
    #' @param data [data.frame/data.table] A dataframe or datatable. MUST SPECIFY
    #' @param col.loc [str] Column name that identify iso3 code, usually `ihme_loc_id`. MUST SPECIFY
    #' @param col.value [str, vector] Column names that needs to be converted, take single or multiple. MUST SPECIFY
    #' @param col.currency [str] Column that has the raw currency. This column takes values ['lcu','usd','eur','ppp'] and is not cap sensitive. LEAVE BLANK IF \param{currency} is specified
    #' @param currency [str] Raw currency. LEAVE BLANK IF \param{col.currency} is specified
    #' @param col.currency.year [str] Column that has raw currency year. LEAVE BLANK IF \param{currency.year} is specified
    #' @param currency.year [int] Year of raw currency . LEAVE BLANK IF \param{col.currency.year} is specified
    #' @param base.year  [int] Default is set to `the latest`. Year you want to base the output unit
    #' @param base.unit [str] Default is `usd`. Please choose 'ppp' or 'usd'.
    #' @param inflate.usd [logical] Default is `FALSE`. If TRUE, nominal LCUs are converted to nominal USD, then inflated in USD
    #' @param simplify  [logical] Default is `TRUE`. Return the simplified data with values converted, no other helper variables
    #' @param LBR_WHO [logical] Default is `FALSE`. By default, converts Liberia data using the IHME series, which is based on the standard currency of Liberian dollars. Set this to TRUE if your Liberia input data comes from WHO. If set to TRUE, uses WHO converters only for Liberia, since WHO reports Liberia data in a different non-standard currency.
    env <- ._source_currency_converter()
    sink(nullfile()) ## hide print statement in currency_conversion
    out <- env$currency_conversion(
        ...,
        converter.version = 7L
    )
    sink()
    return(out)
}






