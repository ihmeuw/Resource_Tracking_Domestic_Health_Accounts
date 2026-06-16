#
# set defaults for logging with {logger}
#
library(logger)


._custom_appender_tee <- function(file = NULL) {
    force(file)
    if (is.null(file)) {
        inner <- function(lines = NULL, chkfile = FALSE) {
            if (!is.null(lines))
                appender_stdout(lines)
            else if (chkfile)
                return(NULL)
        }
    } else {
        inner <- function(lines = NULL, chkfile = FALSE) {
            if (!is.null(lines)) {
                appender_stdout(lines)
                appender_file(file = file, append = TRUE)(lines)
            } else if (chkfile) {
                return(file)
            }
        }
    }
    return(inner)
}


log_threshold(logger::INFO)
log_formatter(logger::formatter_glue)
log_layout(logger::layout_glue)
log_appender(._custom_appender_tee(NULL))

set_log_file <- function(file) {
    log_appender(._custom_appender_tee(file))
}

get_log_file <- function() {
    log_appender()(lines = NULL, chkfile = TRUE)
}
