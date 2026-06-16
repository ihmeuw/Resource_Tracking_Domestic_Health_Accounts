

._UTILS_ENV <- new.env(parent = emptyenv())

._UTILS_ENV$params_yml <- here::here("data", "meta", "params.yml")
._UTILS_ENV$paths_yml <- here::here("data", "meta", "paths.yml")


._UTILS_ENV$load_paths <- function() {
    out <- yaml::read_yaml(._UTILS_ENV$paths_yml)
    return(out)
}


._UTILS_ENV$load_params <- function() {
    out <- yaml::read_yaml(._UTILS_ENV$params_yml)
    names(out) <- tolower(names(out))
    return(out)
}


._UTILS_ENV$data <- list(params = ._UTILS_ENV$load_params(),
                         paths = ._UTILS_ENV$load_paths())



._UTILS_ENV$get_or_error <- function(key, dict, error_msg) {
    out <- NULL
    if (is.list(dict))
        out <- dict[[tolower(key)]]
    if (is.null(out)) stop(error_msg, call. = FALSE)
    return(out)
}

#' Return the requested DAH parameter as specified in metadata
#' @param param [character] The name/key of the parameter to return.
#' @param sub_key [character] The sub-key of the parameter to return. If none exist, ignore.
#'   If sub-keys do exist for the given parameter but sub_key is not specified,
#'   this function returns a list of all the possible sub_keys and their values.
#' @returns The value of the requested parameter, or a [list] of all sub-keys and
#'   their values (if sub_key is not specified and sub-keys exist).
#' @export
get_param <- function(param, sub_key="") {
    param_dict <- ._UTILS_ENV$get_or_error(param,
                                           ._UTILS_ENV$data$params,
                                           paste("Parameter not found:", param))
    if (sub_key != "") {
        param_dict <- ._UTILS_ENV$get_or_error(sub_key,
                                               param_dict,
                                               paste("Sub-key not found:", sub_key))
    }
    return(param_dict)
}


#' Function to return the file path for a specified type & name
#' @param type [character] Primary string key used to filter paths.yml
#' @param name [character] Secondary string key used to filter specific file path
#' @param append [character] Optionally provide a character vector to append to
#'   the path. E.g., `append = "file.csv"` or `append = c("folder", "file.csv")`
#'   If `name` is not provided, this is ignored.
#' @param ... [list] Additional named arguments used to override default parameters.
#'   E.g., `report_year = 2021` overrides the default, which is the current report year.
#'   Valid overrides include "report_year", "prev_report_year", "abrv_year",
#'   "prev_abrv_year", and "crs_update_mmyy"
#' @param check [logical] If TRUE (and `name` is specified), the function will
#'   error if the file does not exist. Default FALSE.
#' @param is_backup [logical] Typically for internal use only,
#'   specifies primary or backup file location
#' @returns The file path ([character]) for the specified type and name or,
#'   if name is not specified, a [list] of all the file paths for each of the
#'   type's names.
#' @export
get_path <- function(type,
                     name = "",
                     append = "",
                     ...,
                     check = FALSE) {
    override <- list(...)
    append <- paste(append, collapse = "/")
    path_dict <- ._UTILS_ENV$get_or_error(type,
                                          ._UTILS_ENV$data$paths,
                                          paste("Path type not found:", type))
    if (name != "") {
        out <- ._UTILS_ENV$get_or_error(name,
                                       path_dict,
                                       paste("Key not found:", name))
        out <- file.path(out, append)
        out <- ._UTILS_ENV$clean_filepath(out, override)
        out <- gsub("//+", "/", out)
        if (check && !file.exists(out))
            stop(paste("File does not exist:", out), call. = FALSE)
    } else {
        # if name not provided, return list of all clean paths for the type
        out <- lapply(path_dict, \(fp) file.path(fp, append))
        out <- lapply(out, ._UTILS_ENV$clean_filepath, override = override)
        out <- lapply(out, \(fp) gsub("//+", "/", fp))
    }
    return(out)
}


# replace keywords with data in the given filepath, optionally override the
# default data with the params in the supplied list
._UTILS_ENV$clean_filepath <- function(filepath, override = list()) {
    params <- ._UTILS_ENV$data$params # local copy
    # replace params with override params where applicable
    names(override) <- tolower(names(override))
    for (p in names(override)) {
        params[[p]] <- override[[p]]
    }
    for (ptrn in names(params)) {
        filepath <- gsub(paste0("\\[", ptrn, "\\]"),
                         params[[ptrn]],
                         filepath,
                         fixed = FALSE,
                         ignore.case = TRUE)
    }
    return(filepath)
}
