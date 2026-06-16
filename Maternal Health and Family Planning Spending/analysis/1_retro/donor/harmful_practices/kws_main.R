#
#       Perform keyword search across batched data
# Use --partition flag to use the script to load the data and convert to batches.
# Otherwise, it loads a single, specified batch of data and runs the KWS on it.
#
# HE 2024-04-24
#
suppressPackageStartupMessages({
    library(argparse)
    library(jsonlite)
    library(arrow)
    library(data.table)

    source(here::here("src", "r", "strings.R"))
    source(here::here("src", "r", "logging.R"))
})


# argparse
aparse <- argparse::ArgumentParser(
    description = paste0("Prep and execution of keyword search across data sets,",
                         " controlled by config files in the working directory.")
)
aparse$add_argument(
    "--wdir", type = "character",
    help = "Working directory where config files are stored and where data will be stored. Always required."
)
aparse$add_argument(
    "--partition", action = "store_true",
    help = paste0("Flag to run the partitioning of data sets into smaller chunks",
                  " for faster processing. Otherwise the script runs in keyword ",
                  "search mode.")
)
aparse$add_argument(
    "--name", type = "character",
    help = "Name of the data source to process (e.g., crs) - required for keyword search and partition task."
)
aparse$add_argument(
    "--batchid", type = "character",
    help = "Batch ID to process (e.g., 1) if running keyword search."
)
aparse$add_argument(
    "--loglvl", type = "character",
    help = "Log level to use (e.g., INFO, DEBUG). Default is INFO.",
    default = "INFO"
)


# logger
logger::log_layout(layout = logger::layout_glue_colors)
logger::log_threshold(logger::INFO)



# =============================================================================
# CONTROL

## TODO: this could be defined elsewhere and sourced by this script,
## so that different keyword functions can be specified by the user to pull in
## different keyword sets
prep_keywords <- function() {
    # dah keywords
    #
    dah_keywords <- jsonlite::read_json(here::here("data", "meta", "dah_kws.json"))
    dah_keywords <- lapply(dah_keywords, unlist)
    # instead of keeping hfa-lang combos seperate, concat into one list since
    # we are not using the lang info in the keyword search
    fp_keywords <- unlist(dah_keywords[c(
        grep("^rmh_fp", names(dah_keywords), value = TRUE)
    )])
    mh_keywords <- unlist(dah_keywords[c(
        grep("^rmh_mh", names(dah_keywords), value = TRUE)
    )])
    # harmful practices keywords
    #
    hp_keywords <- jsonlite::read_json(here::here("data", "meta", "hp_kws.json"))
    hp_keywords <- lapply(hp_keywords, unlist)
    # all keywords
    #
    keywords <- append(
        hp_keywords,
        list(
            fp = unname(fp_keywords),
            mh = unname(mh_keywords)
        )
    )
    return(keywords)
}



# =============================================================================
# HELPERS
#


#
# load all the config files in the working directory
#
load_configs <- function(wdir) {
    configs <- list()
    files <- grep("_kws_config.json$", dir(wdir), value = TRUE)
    for (f in files) {
        cfg <- lapply(jsonlite::read_json(file.path(wdir, f)), unlist)
        configs[[cfg$name]] <- cfg
    }
    return(configs)
}


#
# load data specified in config, partition into n batches (n specified by config)
# and write to apache feather files in out_dir
#
make_batches <- function(config, out_dir) {
    log_info(" ** Reading '{config$name}' data from: {config$data_path}")
    
    file_ext <- tolower(tools::file_ext(config$data_path))
    if (file_ext == "csv") {
        dt <- fread(config$data_path) 
    } else if (file_ext == "parquet") {
        dt <- as.data.table(arrow::read_parquet(config$data_path))
    } else if (file_ext == "feather") {
        dt <- as.data.table(arrow::read_feather(config$data_path))
    } else {
        stop("Unsupported file format: ", file_ext, ". Encountered while reading: ",
             config$data_path)
    }
    
    log_info(" ** Dims: {nrow(dt)} x {ncol(dt)}")
    
    nbatch <- config$nbatch
    
    if (nbatch == 1) {
        log_info(" ** Only one batch specified, writing to file")
        out_path <- file.path(out_dir, sprintf("%s_pre_1.feather", config$name))
        arrow::write_feather(dt, out_path)
        return(invisible(NULL))
    }

    batch_size <- ceiling(nrow(dt) / nbatch)
    ixs <- unique(c(seq(1, nrow(dt), by = batch_size), nrow(dt)))

    log_info(" ** Partitioning data into {nbatch} batches of ~{format(batch_size, big.mark = ',')} rows each")
    for (i in seq_len(nbatch)) {
        log_info("  *** Batch {i} of {nbatch}")
        start_ix <- ixs[i]
        end_ix <- if (i != nbatch) ixs[i + 1] - 1 else ixs[i + 1]
        out_path <- file.path(out_dir,
                              sprintf("%s_pre_%s.feather", config$name, i))
        arrow::write_feather(dt[start_ix:end_ix, ],
                             out_path)
    }
    return(invisible(NULL))
}


#
# Run the keyword search on the specific batch specified by the filepth,
# on the columns specified in the config, using the keyword_list.
# Output is saved in feather format to the file at out_path.
#
chunk_do_kws <- function(filepth, config, keyword_list, out_path) {
    dt <- arrow::read_feather(filepth)
    #
    ## collapse search cols into one string, also standardize string for kws
    log_info(" ** Cleaning search cols: ",
             paste(config$kws_cols, collapse = ", "))
    dt[, srchstr := dt_str_concat(.SD, clean = TRUE, collapse = ";",
                                  verbose = FALSE),
       .SDcols = config$kws_cols]
    #
    ## run keyword search
    dt <- keyword_search(dt,
                         search_cols = "srchstr",
                         keyword_list = keyword_list,
                         clean_search_cols = FALSE, # done above
                         clean_keywords = TRUE,     # reqd for consistency
                         verbose = TRUE)
    #
    log_info(" ** Saving: {out_path}")
    arrow::write_feather(dt, out_path)
}





# =============================================================================
# MAIN
#
main <- function() {
    log_info("  █████████████")
    log_info("  █    KWS    █")
    log_info("  █████████████")

    arg_list <- aparse$parse_args(commandArgs(trailingOnly = TRUE))
    #
    # requirements
    #
    if (is.null(arg_list$wdir))
        stop_and_log("Working directory required but not specified")

    if (is.null(arg_list$name))
        stop_and_log("Name of data source required but not specified")

    configs <- load_configs(arg_list$wdir)
    if (! arg_list$name %in% names(configs))
        stop_and_log("Config not found for data source: {arg_list$name}")

    config <- configs[[arg_list$name]]

    if (arg_list$partition) {
        #
        # phase 1:
        # data needs to be batched so that the keyword search can be run across each
        #
        log_info("* Action: partition data into batches | name: {arg_list$name}")
        #
        outdir <- file.path(arg_list$wdir, "pre")
        dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
        make_batches(config, outdir)
        #
        #
    } else {
        #
        # phase 2:
        # keyword search is run on a single batch of data
        #
        if (is.null(arg_list$batchid)) {
            stop_and_log("Batch ID required but not specified")
        }
        log_info("* Action: run keyword search | name: {arg_list$name} | batch-id: {arg_list$batchid}")
        #
        batchf <- file.path(arg_list$wdir, "pre", sprintf("%s_pre_%s.feather",
                                                          arg_list$name,
                                                          arg_list$batchid))
        if (!file.exists(batchf))
            stop_and_log("Batch file not found: ", batchf)
        #
        # prep keywords
        #
        log_info(" ** Prepping keywords")
        keyword_list <- prep_keywords()
        #
        outdir <- file.path(arg_list$wdir, "post")
        dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
        out_path <- file.path(outdir, sprintf("%s_post_%s.feather",
                                              arg_list$name, arg_list$batchid))
        #
        # run kws
        #
        chunk_do_kws(batchf,
                     config = config,
                     keyword_list = keyword_list,
                     out_path = out_path)
    }
    
}



main()


