library(data.table)
source(here::here("src", "r", "params.R"))
source(here::here("src", "r", "logging.R"))


PATHS <- list(
    crs = get_path("raw", "crs", "CRS_1973_2023.parquet"),
    undp_iati = get_path("int", "data", c("iati", "clean", "undp.csv")),
    undp_api = get_path("int", "data", c("undp", "clean", "undp_projects.csv")),
    undp_out = get_path("int", "data", c("undp", "clean", "undp_full.csv")),
    unwomen  = get_path("int", "data", c("iati", "clean", "unwomen.csv")),
    working = get_path("int", "data", c("hp_kws"))
)



#
# HELPERS =====================================================================
#
mk_cfg_pth <- function(save_dir, name) {
    if (!file.exists(save_dir)) {
        stop("Can't save config to non-existent dir: ", save_dir)
    }
    file.path(save_dir, paste0(name, "_kws_config.json"))
}

write_config <- function(
        name,
        nbatch,
        kws_cols,
        data_path,
        save_dir
) {
    jsonlite::write_json(list(
        name = name,
        nbatch = nbatch,
        kws_cols = kws_cols,
        data_path = data_path
    ), mk_cfg_pth(save_dir, name))
}



#
# DATA LOADERS ================================================================
#
PREP <- list()

PREP$crs <- function(save_dir, force = FALSE) {
    if (file.exists(mk_cfg_pth(save_dir, "crs")) && !force) {
        log_info("CRS: Skipping: config file already exists")
        return(FALSE)
    }
    log_info(" Reading CRS data from: ", PATHS$crs)
    crs <- arrow::read_parquet(PATHS$crs)
    log_info("  Dims: {paste(format(dim(crs), big.mark = ',', trim = TRUE), collapse = ' x ')}")

    batch_size <- ceiling(nrow(crs) * 0.05)
    nbatch <- ceiling(nrow(crs) / batch_size)

    write_config(
        name = "crs",
        nbatch = nbatch,
        kws_cols = c("project_title", "short_description",
                     "long_description", "keywords"),
        data_path = PATHS$crs,
        save_dir = save_dir
    )
    log_info("  Config saved.")
    return(TRUE)
}


PREP$undp <- function(save_dir, force = FALSE) {
    #
    # 2 source of data - IATI plus the UNDP API (primarily for older projects)
    #
    if (file.exists(mk_cfg_pth(save_dir, "undp")) && !force) {
        log_info("UNDP: Skipping: config file already exists")
        return(FALSE)
    }
    log_info("UNDP: Read data from:")
    log_info("    {PATHS$undp_iati}")
    log_info("    {PATHS$undp_api}")
    log_info("  Combining sources...")

    u1 <- fread(PATHS$undp_iati)
    u2 <- fread(PATHS$undp_api)
    # coerce u2's features to match up with u1 (iati) so they can be bound
    setnames(u2,
             c("project_id", "title", "description",
               "expense", "budget", "country", "iso",
               "sector", "sdg", "donor", "year"),
             c("project_id", "title_narr", "description_long_narr",
               "trans_value", "project_budget", "recip", "recip_iso3",
               "sector", "sdg", "donor", "trans_year"))
    #
    # fill in some cols to match the iati undp data
    u2[, `:=`(
        sector = gsub(" __ ", "&", sector, fixed = TRUE),
        trans_type = "Expenditure", ## match iati
        trans_currency = "USD",
        outgoing_flag = TRUE,
        recip_pct = 100,
        provider = "United Nations Development Programme (UNDP)", ## match iati
        provider_type = "Multilateral", ## match iati
        provider_is_narr = FALSE,
        finance_type = "Standard grant",
        flow_type = "ODA"
    )]
    for (nm in names(u2)) {
        # for cols that don't match between undp api and iati, add prefix
        if (! nm %in% names(u1))
            setnames(u2, nm, paste0("api_", nm))
    }
    # combine
    u1[, source := "iati"]
    u2[, source := "undp_api"]
    undp <- rbind(u1, u2, fill = TRUE)
    log_info("  Saving to: {PATHS$undp_out}")
    fwrite(undp, PATHS$undp_out)

    log_info("  Dims: {paste(format(dim(undp), big.mark = ',', trim = TRUE), collapse = ' x ')}")

    write_config(
        name = "undp",
        nbatch = 1,
        kws_cols = c(
            "title_narr",
            "description_long_narr",
            "description_objectives_narr",
            "description_target_group_narr",
            "trans_description_narr"
        ),
        data_path = PATHS$undp_out,
        save_dir = save_dir
    )
    log_info("  Config saved.")
    return(TRUE)
}


PREP$unwomen <- function(save_dir, force = FALSE) {
    #
    # only one source of data - IATI
    #
    if (file.exists(mk_cfg_pth(save_dir, "unwomen")) && !force) {
        log_info("UN Women: Skipping: config file already exists")
        return(FALSE)
    }
    log_info("UN Women: Read data from: ", PATHS$unwomen)
    unwomen <- fread(PATHS$unwomen)
    log_info("  Dims: {paste(format(dim(unwomen), big.mark = ',', trim = TRUE), collapse = ' x ')}")

    write_config(
        name = "unwomen",
        nbatch = 1,
        kws_cols = c(
            "title_narr",
            "description_long_narr",
            "description_objectives_narr",
            "description_target_group_narr",
            "trans_description_narr"
        ),
        data_path = PATHS$unwomen,
        save_dir = save_dir
    )
    log_info("  Config saved.")
    return(TRUE)
}



#
# MAIN
#
control <- yaml::read_yaml(here::here("analysis", "1_retro", "donor",
                                      "harmful_practices", "control.yml"))
control <- control$prep_inputs

dir.create(PATHS$working, showWarnings = FALSE, recursive = TRUE)
for (src in names(control)) {
    if (!src %in% names(PREP)) {
        stop("No data loader for: ", src)
    }
    PREP[[src]](save_dir = PATHS$working, force = control[[src]]$force)
    log_info("")
}
