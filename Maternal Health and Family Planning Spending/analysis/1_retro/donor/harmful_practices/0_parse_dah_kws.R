#
# Load the DAH team's keyword search (a Stata DO file) and parse it into a JSON
# file for general use.
#
library(jsonlite)
source(here::here("src", "r", "params.R"))

kws_path <- "/FILEPATH/FGH_2021_HEALTH_FOCUS_AREAS_all.do"


# This parser is specific to the structure of the keyword search DO file and
# may need updating if that structure changes.
parse_keywords_do <- function(fp) {
    lines <- readLines(fp)
    output <- list()

    in_keywords <- FALSE
    first_line <- FALSE
    n <- 1
    while (n < length(lines)) {
        line <- trimws(lines[n])
        if (grepl("^global ", line) && !grepl("^global hfa", line)) {
            in_keywords <- TRUE
            first_line <- TRUE
            hfa_lang <- strsplit(line, "\\W")[[1]][2]
            output[[hfa_lang]] <- character(0)
        }
        if (in_keywords) {
            if (line == "" || grepl("^//", line) || grepl("^\\*", line)) {
                # this is filler before the next hfa_lang
                in_keywords <- FALSE
                n <- n + 1
                next
            }
            # example line: 'global rmh_level  ABCDEFGHI '
            line <- gsub("^global.+ABCDEFGHI", "", line)
            words <- strsplit(line, "\" \"", fixed = TRUE)[[1]] # keywords in quotes
            words <- gsub(" ///", "", words) # rm line continuation if any
            words <- gsub("[^[:alnum:] ]", "", words) # rm quotes and other slashes
            words <- gsub("\\s+", " ", words) # rm double spaces caused by above removals
            output[[hfa_lang]] <- c(output[[hfa_lang]], words)
            first_line <- FALSE
        }
        n <- n + 1
    }
    return(output)
}



#
# MAIN
#
kws <- parse_keywords_do(kws_path)

kws_json <- jsonlite::prettify(jsonlite::toJSON(kws))
writeLines(kws_json, here::here("data", "meta", "dah_kws.json"))

