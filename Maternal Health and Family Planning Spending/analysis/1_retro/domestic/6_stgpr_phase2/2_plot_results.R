#
# creates country level diagonistic plots for the given model runs
#

library(data.table)

source(here::here("src", "r", "params.R"))
source(here::here("analysis", "1_retro", "domestic", "stgpr_helpers.R"))

CONFIG <- list(
    files = list(
        gbd_locs = get_path("int", "data", c("covars", "gbd_locations.csv"))
    ),
    dirs = list(
        working = get_path("int", "data", c("domestic", "int", "stgpr")),
        model_output = get_path("int", "stgpr_output")
    )
)


#
# MAIN ====
#
message("*** PLOT MODEL RESULTS ***\n")



gbd_locs <- fread(CONFIG$files$gbd_locs)
run_specs <- yaml::read_yaml(here::here("analysis", "1_retro", "domestic", "stgpr_run.yml"))


acceptable_value_ids <- c(
    "oop_antenatal",
    "oop_public_facility_delivery",
    "oop_public_facility_c_section",
    "oop_private_facility_delivery",
    "oop_private_facility_c_section",
    "oop_home_delivery",
    "oop_postnatal"
)

for (val_id in names(run_specs)) {
    if (! val_id %in% acceptable_value_ids) {
        next()
    }
    specs <- run_specs[[val_id]]
    if (isTRUE(specs$run)) {
        # load last run-id
        last_run_id <- fread(file.path(CONFIG$dirs$working, "model_values",
                                       val_id, "last_run.csv")
                             )$run_id
        make_country_result_plots(
            run_id = last_run_id,
            gbd_locs = gbd_locs,
            working_dir = CONFIG$dirs$working,
            model_dir = CONFIG$dirs$model_output
        )
        message("")
    }
}

message("*** Done.")
