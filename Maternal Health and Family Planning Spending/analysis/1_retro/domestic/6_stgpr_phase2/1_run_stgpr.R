#
# run st-gpr models
#
# send models off to run by generating necessary inputs, loading configs, and
# registering and running the model via the stgpr api
#

library(data.table)
source(here::here("src", "r", "params.R"))
source(here::here("src", "r", "stgpr_utils.R"))
source(here::here("analysis", "1_retro", "domestic", "stgpr_configs.R"))
source(here::here("src", "r", "internals.R"))
source_stgpr_api()


options(warn = 1) # print warnings as they occur
set.seed(3980)


PATHS <- list(
    dirs = list(
        working = get_path("int", "data", c("domestic", "int", "stgpr")),
        covars = get_path("int", "data", c("covars"))
    )
)



.calc_cv_runs <- function(cfg) {
    # ======================================================================== #
    #' calculate number of model runs that will be required to run
    #' cross-validation with the given parameter set
    # ======================================================================== #
    if (cfg$do_cross_validation == FALSE) {
        return(1)
    }
    np <- 1 # number of parameter sets
    for (param in c("st_lambda", "st_zeta", "st_omega", "gpr_scale")) {
        np <- np * length(strsplit(cfg[[param]], ",", fixed = TRUE)[[1]])
    }
    holdouts <- as.numeric(cfg$holdouts)
    if (holdouts > 0) {
        np <- np * holdouts
    } # else no holdouts so in-sample selection
    return(np)
}



run_stgpr <- function(
        value_id,
        stage1_file,
        base_config = stgpr_config_base,
        full_data_file = "stgpr_all_data.csv",
        working_dir = PATHS$dirs$working,
        nparallel = 10,
        test_only = FALSE
) {
    # ======================================================================== #
    #' Register and submit an ST-GPR model for a given model variable.
    #' 
    #' This function first runs the custom first stage for the model based on
    #' the formula specified in the variable's entry in stage1_formulas.csv
    #' (created by 2_select_stage1s.R).
    #' It then finalizes input data and the configuration file for ST-GPR, and
    #' registers and sends off the model to be run on the cluster. 
    #'
    #' @param value_id the short-code/nickname for the variable being
    #'     modeled, used to query the data in full_data_file
    #' @param stage1_file Path to the custom stage1 file. Required.
    #' @param base_config the base/constant config shared across many model
    #'     types, as loaded from stgpr_config.R
    #' @param full_data_file the name of the file containing all variables to be
    #'     modeled. Must contain columns required for st-gpr, namely:
    #'     "nid", "location_id", "year_id", "age_group_id", "sex_id",
    #'     "val", "variance", "sample_size", "is_outlier".
    #'     Other required st-gpr columns are added by these functions.
    #'     Must also contain a column named "value_id" to identify the
    #'     variable being modeled.
    #' @param working_dir the directory in which to store the model pre-processing
    #'     and post-processing files (model outputs are stored elsewhere by the
    #'     st-gpr backend).
    #' @param nparallel the number of batches to run in parallel in some of the
    #'     model stages.
    #' @param test_only if TRUE, only test the creation of model inputs (config,
    #'     data, and custom first stage), but do not register and submit the model.
    vid <- value_id
    dir.create(file.path(working_dir, "model_values", vid),
               recursive = TRUE,
               showWarnings = FALSE)
    full_config <- append(base_config, load_current_config(value_id))
    message("*** Model for: ", vid)
    
    message("*** Loading cutom stage 1")
    full_config[["path_to_custom_stage_1"]] <- stage1_file
    full_config[["custom_stage1_formula"]] <- "custom_file_passed"
    stage1 <- list(model = "custom_file_passed")
    stage1_fml <- data.table(value_id = vid,
                                 formula = "custom_file_passed")
    
    message("*** Preparing ST-GPR inputs:")
    
    run_data <- save_stgpr_input(
        working_dir = working_dir,
        full_data_file = full_data_file,
        config_template = full_config,
        value_id = vid,
        val_col = full_config$val_col,
        val_measure = full_config$val_measure,
        val_metric = full_config$val_metric,
        val_transform = full_config$val_transform,
        save_data = !test_only # set false for testing
    )
    run_data$stage1 <- stage1
    run_data$run_id <- NA_character_
    
    message("model name:     ", run_data$config$me_name[1])
    message("measure:        ", run_data$data$measure[1])
    message("metric:         ", run_data$data$metric[1])
    message("input data:     ", run_data$config$path_to_data[1])
    message("custom stage1:  ", run_data$config$path_to_custom_stage_1[1])
    message("cross val:      ", run_data$config$do_cross_validation[1])
    message("num runs:       ", .calc_cv_runs(run_data$config))
    message("nparallel:      ", nparallel)
    message("note:           ", run_data$config$note[1])
    
    if (test_only) {
        message("Test only, not registering model.")
        return(run_data)
    }
    # register model version
    if (run_data$saved == FALSE) {
        stop("Config and input data are not being updated, you probably don't want to run.")
    }
    message("*** Registering model:")
    run_id <- register_stgpr_model(path_to_config = run_data$config_path)
    if (is.na(run_id)) {
        warning("Failed to register stgpr model, returning current data")
        return(run_data)
    }
    
    # create run directory and copy initial files
    dir.create(file.path(working_dir, "run", run_id),
               showWarnings = FALSE, recursive = TRUE)
    file.copy(run_data$config_path,
              file.path(working_dir, "run", run_id, "config.csv"))
    fwrite(stage1_fml,
           file.path(working_dir, "run", run_id, "stage1_formula.csv"))
    
    # run model
    message("*** Sending model to run:")
    stgpr_sendoff(
        gbd_model_version_id = run_id,
        project = "proj_fgh",
        log_path = file.path(working_dir, "run", run_id),
        nparallel = nparallel
    )

    
    run_data$run_id <- run_id
    return(run_data)
}




#
# MAIN ========================================================================
#
message("")
message("SUBMIT ST-GPR MODELS")
message("")

run_specs <- yaml::read_yaml(here::here("analysis", "1_retro", "domestic", "stgpr_run.yml"))


## in this pipeline, these are the only value_id's which should still be run
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
        if (is.null(specs$nparallel) | specs$nparallel < 1) {
            specs$nparallel <- 10
        }
        
        stage1_file <- specs$custom_stage1_file
        if (specs$custom_stage1_file == "") {
            warning("Custom stage-1 file is expected for ", val_id, " but not found. Skipping run.")
            next()
        } else {
            stage1_file <- file.path(PATHS$dirs$working, "model_values", val_id,
                                     specs$custom_stage1_file)
        }
        
        run <- run_stgpr(
            value_id = val_id,
            nparallel = specs$nparallel,
            stage1_file = stage1_file
        )
        # save run_id
        if (is.na(run$run_id) || is.null(run$run_id)) {
            message("*** ", val_id, ": failed to register")
        } else {
            fwrite(data.frame(run_id = run$run_id),
                   file.path(PATHS$dirs$working, "model_values", val_id,
                             "last_run.csv"))
        }
    } else {
        message("*** ", val_id, ": skipping")
    }
    message("")
}
message("Done.")

