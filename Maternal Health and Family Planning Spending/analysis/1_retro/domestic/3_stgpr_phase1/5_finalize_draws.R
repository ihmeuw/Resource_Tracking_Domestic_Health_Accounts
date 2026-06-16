#
# Store final model output files in the modeled value's final directory.
# Existing "final" versions will be overwritten.
#
library(data.table)
source(here::here("src", "r", "params.R"))
source(here::here("analysis", "1_retro", "domestic", "stgpr_helpers.R"))

PATHS <- list(
    files = list(
        gbd_locs = get_path("int", "data", c("covars", "gbd_locations.csv"))
    ),
    dirs = list(
        working = get_path("int", "data", c("domestic", "int", "stgpr")),
        model_output = get_path("int", "stgpr_output")
    )
)




#
# MAIN
#
message("*** FINALIZE MODEL RESULTS ***\n")

gbd_locs <- fread(PATHS$files$gbd_locs)
gbd_locs <- gbd_locs[level == 3, ] # only save country-level results


run_specs <- yaml::read_yaml(here::here("analysis", "1_retro", "domestic", "stgpr_run.yml"))



for (val_id in names(run_specs)) {
    specs <- run_specs[[val_id]]
    if (isTRUE(specs$run)) {
        # load last run-id
        last_run_id <- fread(file.path(PATHS$dirs$working, "model_values",
                                       val_id, "last_run.csv")
                             )$run_id
        finalize_model_run(
            run_id = last_run_id,
            gbd_locs = gbd_locs,
            working_dir = PATHS$dirs$working,
            model_dir = PATHS$dirs$model_output
        )
        
        message("====================\n")
    }
}
