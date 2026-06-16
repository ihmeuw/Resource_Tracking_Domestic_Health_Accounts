#
# post-processing of keyword searched CRS database
#
library(data.table)
library(arrow)
source(here::here("src", "r", "params.R"))
source(here::here("src", "r", "logging.R"))


CONFIG <- list(
    dirs = list(
        post = get_path("int", "data", c("hp_kws", "post")),
        out = get_path("int", "data", c("oda", "int"))
    ), 
    hfas = c("fgm", "gbv", "ecm", "other", "mh", "fp")
   
)

concat_post_files <- function(result_dir, org_name) {
    hfa_counts <- paste0(CONFIG$hfas, "_count")
    out <- list()
    for (file in dir(result_dir, pattern = org_name)) {
        out[[file]] <- arrow::read_feather(file.path(result_dir, file))
    }
    out <- data.table::rbindlist(out) 

    return(out)
}



post_kws_crs <- function() {
    log_info("* Post-processing CRS data with keyword search results")
    
    crs <- concat_post_files(CONFIG$dirs$post, "crs")
    crs[, usd_disbursement := usd_disbursement * 1e6]
    
    # convert crs to unique project-level data
    # (donor-channel-purpose-recipient)
    id_cols <- c("year", "crs_id",
                 "donor_name", "donor_code", "agency_name", "agency_code",
                 "channel_name", "channel_code",
                 "sector_name", "sector_code", "purpose_name", "purpose_code",
                 "project_title", "short_description", "long_description", "keywords",
                 "all_matches_kws",
                 "flow_name", "flow_code",
                 "recipient_name", "recipient_code")
    
    crs <- crs[, .(
        usd_disbursement = sum(usd_disbursement, na.rm = TRUE),
        # sum counts since the total proportions will be maintained
        # (string search columns are included in id columns)
        fgm_count = sum(fgm_count, na.rm = TRUE),
        gbv_count = sum(gbv_count, na.rm = TRUE),
        ecm_count = sum(ecm_count, na.rm = TRUE),
        other_count = sum(other_count, na.rm = TRUE),
        fp_count = sum(fp_count, na.rm = TRUE),
        mh_count = sum(mh_count, na.rm = TRUE)
    ), by = id_cols]

    crs[, harmful_count := rowSums(.SD, na.rm = TRUE),
        .SDcols = paste0(c("fgm", "gbv", "ecm", "other"), "_count")]

    # save
    log_info("* Saving post-processed CRS data")
    arrow::write_feather(
        crs,
        file.path(CONFIG$dirs$out, "crs_post_kws.feather")
    )
    log_info("* Done")
}


# MAIN
post_kws_crs()
