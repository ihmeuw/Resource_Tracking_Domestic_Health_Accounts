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
        working = get_path("int", "data", c("domestic", "int")),
        covars = get_path("int", "data", c("covars")),
        output = get_path("int", "data", c("domestic", "int", "draws"))
    )
)

dir.create(PATHS$dirs$output, recursive = TRUE, showWarnings = FALSE)


#
# Auxiliary data
#

# Locations
GBD_LOCS <- fread(PATHS$files$gbd_locs) ## countries only
ihme_unfpa_locs <- fread(PATHS$files$ihme_unfpa_locs)
GBD_LOCS <- merge(GBD_LOCS,
                  unique(ihme_unfpa_locs[, .(ihme_loc_id, is_unfpa)]),
                  by = "ihme_loc_id",
                  all.x = TRUE)

# Raw input data
INPUT_DT <- fread(file.path(PATHS$dirs$working, "stgpr", "stgpr_all_data.csv"))


china_public_fp <- function(min_yr, max_yr) {
    # load China's public_fp values (extracted directly from official budget)
    dt <- fread(file.path(PATHS$dirs$working, "china_public_fp.csv"))
    
    dt_fp <- dt[between(year_id, min_yr, max_yr), .(
        year_id,
        ihme_loc_id = "CHN",
        location_id = GBD_LOCS[ihme_loc_id == "CHN", location_id[1]],
        sex_id = 3,
        age_group_id = 22,
        source = "China budget",
        measure_id = 18,
        metric_id = 3,
        val = value,
        run_id = NA_character_,
        is_unfpa = ihme_unfpa_locs[ihme_loc_id == "CHN", is_unfpa[1]]
    )]
    if (dt_fp[, max(year_id)] < max_yr) {
        # if st-gpr values extend past last China extraction, hold last val
        # constant
        add_yrs <- (dt_fp[, max(year_id)] + 1):max_yr
        dt_fp_add <- dt_fp[year_id == max(year_id),
                           .(year_id = add_yrs),
                           by = setdiff(names(dt_fp), "year_id")]
        dt_fp_add[, source := "China budget (held constant)"]
        dt_fp <- rbind(
            dt_fp,
            dt_fp_add
        )
    }
    ## simualte draws
    dt_fp <- dt_fp[, .(draw = 0:999), by = names(dt_fp)]
    return(dt_fp)
}



save_final_draws <- function(.value_id, .output_file) {
    message("\n*** Saving final draws for ", .value_id, " ***")
    message("    Loading draws")
    fp <- file.path(PATHS$dirs$working, "stgpr", "model_values",
                    .value_id, "final", "draws.feather")
    last_save <- file.mtime(fp)
    draws <- arrow::read_feather(fp)
    ndraw <- length(unique(draws$draw))
    message("    - Number of draws: ", ndraw)
    message("    - Last update: ", last_save)
    message("    - Filtering to countries")
    draws <- merge(draws,
                   GBD_LOCS[, .(location_id, ihme_loc_id, level, is_unfpa)],
                   by = "location_id",
                   all.x = TRUE)
    draws <- draws[level == 3, -"level"]
    denom_name <- INPUT_DT[value_id == .value_id, denom_name[1]]
    message("    - Merging denominator: ", denom_name)
    denom <- arrow::read_feather(file.path(PATHS$dirs$working, "stgpr",
                                           "denominators",
                                           paste0(denom_name, ".feather")))
    denom <- denom[, .(location_id, year_id, denominator = get(denom_name))]
    draws <- merge(draws, denom,
                   by = c("location_id", "year_id"), all.x = TRUE)
    draws[, denom_name := denom_name]
    draws[, denom_currency := "2023 USD"]
    
    
    ## if government spend on FP, update China values
    if (.value_id == "public_dis2.3") {
        message("    - Updating China values based on official budget data!")
        # we assume China's official budget values are their estimate, so replace
        # model results with fake draws that contain budget values
        chn <- china_public_fp(
            min_yr = min(draws$year_id),
            max_yr = max(draws$year_id)
        )
        chn[, run_id := draws[, run_id[1]]]
        # merge on denominator
        chn <- merge(chn, denom,
                     by = c("location_id", "year_id"), all.x = TRUE)
        ## values from function are in absolute terms, convert to proportions
        chn[, val := val / denominator]
        chn[, denom_name := denom_name]
        chn[, denom_currency := "2023 USD"]
        
        draws <- rbind(
            draws[ihme_loc_id != "CHN"],
            chn
        )
    }
    
    message("    - Saving to ", .output_file)
    stopifnot(draws[is.na(denominator), .N] == 0)
    arrow::write_feather(draws, .output_file)
}



#
# Public spending on maternal health and family planning
#
save_final_draws("public_dis2.1",
                 file.path(PATHS$dirs$output, "public_mh_draws.feather"))


save_final_draws("public_dis2.3",
                 file.path(PATHS$dirs$output, "public_fp_draws.feather"))
