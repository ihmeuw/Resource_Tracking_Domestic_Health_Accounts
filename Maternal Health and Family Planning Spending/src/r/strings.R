

library(stringi)
library(logger)


stop_and_log <- function(msg, call. = FALSE, ...) {
    log_error(msg, ...)
    stop(msg, call. = call.)
}


#' @title strclean
#' @description
#' Standardize strings by translating to a common encoding, removing
#' non-alphanumeric characters, and other cleaning operations.
#' 
#' @param x a character vector to apply the function to.
#' @param case character, should the case of the strings be converted to
#'     "upper" or "lower", or "none" if no conversion is desired.
#' @param pad character, a string to pad the strings with (i.e., add to either
#'     side of the string). Defaults to " ".
#' @param replace a list specifying characters to replace, and their replacements.
#'     Defaults to list("[^[:alnum:] ]" = " "), which replace non-alphanumeric
#'     characters other than a spcae with a space.
#' @param trans_id character, the transliteration ID from stringi::stri_trans_list()
#'     to use in transliteration (see ?stringi::stri_trans_general).
#'     Defaults to "latin-ascii" as this is common in our data.
#' @param ... additional arguments to pass to stringi::stri_trans_general.
#' 
#' @example
#' clean_cols <- c("colA", "colB")
#' dt[, (clean_cols) := lapply(.SD, function(x) {
#'     strclean(x, upper = TRUE, pad = TRUE)
#' }),
#' .SDcols = clean_cols]
strclean <- function(x,
                     case = c("upper", "lower", "none"),
                     pad = " ",
                     replace = list("[^[:alnum:] ]" = " "),
                     trans_id = "latin-ascii",
                     ...) {
    case <- match.arg(case)
    x <- stringi::stri_trans_general(x, id = trans_id, ...)
    if (case != "none") {
        x <- switch(case,
                    upper = stringi::stri_trans_toupper(x),
                    lower = stringi::stri_trans_tolower(x))
    }
    for (pat in names(replace)) {
        x <- stringi::stri_replace_all_regex(x,
                                             pattern = pat,
                                             replacement = replace[[pat]])
    }
    if (pad != "")
        x <- paste0(pad, x, pad)
    return(x)
}






#' @title concat string columns in a data.table
#' @description
#' Concatenate multiple string columns in a data.table into a single column,
#' taking care to remove duplicates and drop empty strings
#' 
#' @param SD a data.table, probably a subset of a larger data.table, to operate on.
#' @param collapse character, the string to use to separate the concatenated
#'     strings. Defaults to " ; ".
#' @param uniq logical, should duplicate strings be removed before concatenation?
#' @param clean logical, should the strings be cleaned using strclean() before?
#' @param ... additional arguments to pass to strclean().
#' @return returns the concatenated strings as a character vector with an entry
#'     for each row in the input.
dt_str_concat <- function(SD,
                          collapse = " ; ",
                          uniq = TRUE,
                          clean = FALSE,
                          ...,
                          verbose = FALSE) {
    if (clean) {
        if (verbose) log_info("  Cleaning strings before concatenation")
        SD <- as.data.table(lapply(SD, strclean, ...))
    }
    inner <- function(row) {
        r <- as.character(row)
        r <- if (uniq) unique(r) else r
        r <- r[trimws(r) != ""]
        if (length(r) == 0) return("")
        paste(r, collapse = collapse)
    }
    if (verbose) log_info("  Collapsing strings")
    SD[, inner(.SD), by = seq(NROW(SD))][, 2] # element 1 is by, 2 is result
}









#' @title perform keyword search
#' @description
#' Search for fixed keywords across the search_cols in a data.frame, which will be
#' modified (new columns added).
#' @param df a data.frame to search for keywords in.
#' @param search_cols a character vector of column names to search for keywords in.
#' @param keyword_list a named list where each entry's name is the name of the
#'     category or entity being searched for, and the corresponding value is a
#'     character vector of keywords to search for.
#'     E.g., list("hiv" = c(" hiv ", " aids "), "mal" = c(" malaria ", " bednet "))
#' @param clean_search_cols logical, should the search_cols be standardized using
#'     strclean()?
#' @param clean_keywords logical, should the keywords in the keyword_list be
#'     standardized using strclean()?
#' @param verbose logical, should messages be printed to the console?
keyword_search <- function(df,
                           search_cols,
                           keyword_list,
                           clean_search_cols = TRUE,
                           clean_keywords = TRUE,
                           verbose = TRUE) {
    logthresh <- log_threshold()
    if (!verbose) log_threshold(logger::WARN)
    #
    # runs the keyword search for the CHW keywords
    #
    fmat <- function(...) format(..., big.mark = ",")
    fstrcnt <- function(pat, x) stringi::stri_count(txt, fixed = pat)
    #
    # verify inputs
    dt <- data.table::copy(df)
    if (!inherits(dt, "data.frame"))
        stop_and_log("dt must be related to a data.frame. inherits(dt, 'data.frame') == FALSE")
    data.table::setDT(dt)

    if (!all(search_cols %in% names(dt)))
        stop_and_log("columns in search_cols must be a subset of the columns of df")
    if (!is.list(keyword_list))
        stop_and_log("keyword_list must be a list")

    log_info(" ** Keyword Search")
    #
    # create uppper vars
    if (clean_search_cols) {
        log_info("  - cleaning strings in search_cols...")
        dt[, (paste0("upper_", search_cols)) := lapply(.SD, function(x) {
            strclean(x)
        }), .SDcols = search_cols]
        upper_cols <- paste0("upper_", search_cols)
    } else {
        upper_cols <- search_cols
    }

    kws <- keyword_list
    if (clean_keywords) {
        log_info("  - cleaning keywords...")
        kws <- lapply(kws, strclean, pad = "") # don't pad since this is specified by user
    }
    log_info("  - beginning keyword search...")

    # prep columns to count matches per hfa-language,
    dt[, (names(kws)) := 0]
    # and to store matched patterns
    dt[, (paste0(names(kws), "_match")) := ""]
    #
    # iter over each hfa-language and search for keywords across each search col
    for (hfa_lang in names(kws)) {
        kws_vec <- kws[[hfa_lang]]
        if (length(kws_vec) == 0) {
            log_warn("no keywords found for {hfa_lang}, check keyword parsing")
            next
        }
        log_info("  *** {hfa_lang}: {fmat(length(kws_vec))} keywords")
        #
        # vectorized across patterns and text (per column)
        for (col in upper_cols) {
            # evaluate keyword across the text in col, extract num matches
            txt <- dt[[col]]
            # match_counts:
            # essentially a matix, nrow = length(kws_vec), ncol = length(txt),
            # each element is number of matches of kw_i to txt_j
            match_counts <- lapply(kws_vec, fstrcnt, x = txt)
            match_idxs <- lapply(match_counts, \(x) which(x > 0))
            #
            # transpose index list
            names(match_idxs) <- seq_along(match_idxs)  # stack requires names
            by_txt <- with(stack(match_idxs), split(ind, values))
            #
            # expand list to be length of txt, and extract match values
            mres <- lapply(as.character(seq(1, length(txt))), \(txt_ix) {
                ixs <- by_txt[[txt_ix]] # get pattern indices for given txt
                # get match counts for given txt
                if (is.null(ixs)) return(list(0, ""))
                cnt <- vapply(match_counts[ixs],
                              "[[",
                              integer(1),
                              as.integer(txt_ix))
                return(list(
                    sum(cnt), # total matches for txt_i to all patterns
                    paste(kws_vec[ixs], collapse = ";") # matched patterns
                ))
            })
            # extract into vectors
            mlens <- vapply(mres, "[[", double(1), 1)
            mstrs <- vapply(mres, "[[", character(1), 2)
            #
            # add the number of matches to the hfa count
            dt[, eval(hfa_lang) := get(hfa_lang) + mlens]
            #
            # if there are matches, append the keyword(s) to the match column
            dt[, paste0(hfa_lang, "_match") := paste(
                get(paste0(hfa_lang, "_match")),
                mstrs,
                sep = ";"
            )]
            log_info("    - found {fmat(sum(mlens))} matches across rows in '{col}'")
        }
        # rm starting semicolon caused by concatenation of strings
        dt[, (paste0(hfa_lang, "_match")) := gsub("^;", "",
                                                  get(paste0(hfa_lang, "_match"))
        )]
        log_info("")
    }
    # concat all matched keywords for convenience
    dt[, all_matches_kws := dt_str_concat(.SD, collapse = ";", verbose = FALSE),
       .SDcols = paste0(names(kws), "_match")]
    setnames(dt, names(kws), paste0(names(kws), "_count"))
    log_info(" ** KWS Done!")
    log_threshold(logthresh)
    return(dt)
}
