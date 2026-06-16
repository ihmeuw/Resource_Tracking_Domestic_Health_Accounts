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


calc_variance <- function(dt, val_col = "val") {
    # ======================================================================== #
    # ======================================================================== #
    req_cols <- c("location_id", "data_src", "trend_variance", val_col)
    if (!all(req_cols %in% names(dt)))
        stop("Missing required columns: ",
             paste0(req_cols[!req_cols %in% names(dt)], collapse = ", "))
    
    # estimate variance for each location
    dt[is_outlier == 0,
       loc_var := var(get(val_col)),
       by = .(location_id)]
    ## if we can't estimate variance for the location, use the mean
    dt[, mean_loc_var := mean(loc_var, na.rm = TRUE)]
    dt[is.na(loc_var),
       loc_var := mean_loc_var]
    
    dt[, variance := loc_var]
    # variance must be positive in the input data, but outliers won't be used
    dt[is_outlier == 1, variance := mean_loc_var]
    # clean up
    dt[, c("loc_var", "mean_loc_var") := NULL]
    
    ## add trend variance for spending models
    ## - use sqrt(trend_variance) to penalize bigger variances more heavily
    ## (think of plot(x, sqrt(x)) for x in [0, 0.02])
    dt[model_group == "rh_spending",
       variance := variance + sqrt(trend_variance)]
    
    ## for this China data, we want minimal variance since it comes from official
    ## budget
    dt[value_id == "public_dis2.3" & ihme_loc_id == "CHN" & data_src == "china",
       variance := val * 1e-5]
    
    
    ggplot(dt[is_outlier == 0],
           aes(x = year_id, y = val * model_value_denom)) +
        geom_point() +
        geom_errorbar(aes(ymin = model_value_denom * (val - 2 * sqrt(variance)),
                          ymax = model_value_denom * (val + 2 * sqrt(variance))),
                      alpha = 0.2) +
        facet_wrap(~ihme_loc_id, scales = "free_y")
    return(dt)
}



    
run_stgpr <- function(
        value_id,
        base_config = stgpr_config_base,
        full_data_file = "stgpr_all_data.csv",
        stage1_file = "",
        working_dir = PATHS$dirs$working,
        health_exp_dir = PATHS$dirs$covars,
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
    #' @param health_exp_dir the directory containing the health expenditure data
    #'     which is loaded to be used as a potential covariate.
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
    
        
    # GENERATE CUSTOM STAGE 1
    message("*** Custom stage 1:")
    if (stage1_file == "") {
        stage1_fml <- fread(file.path(working_dir, "model_values",
                                      vid, "stage1_formula.csv"))
        fml <- stage1_fml[value_id == vid, formula]
        
        stage1 <- build_custom_stage1_lme4(
            working_dir = working_dir,
            full_data_file = full_data_file,
            val_col = full_config$val_col,
            val_transform = full_config$val_transform,
            value_id = vid,
            model_formula = fml,
            health_exp_dir = health_exp_dir,
            gbd_covar_names = full_config$gbd_covar_names,
            location_set_id = full_config$location_set_id,
            year_id = seq(full_config$year_start,
                          full_config$year_end),
            age_group_id = full_config$age_group_id,
            sex_id = full_config$sex_id,
            gbd_release_id = full_config$release_id,
            rebuild = "none",
            predict_re = full_config$predict_re,
            smooth_fit = full_config$smooth_stage1,
            loess_opts = full_config$loess_opts,
            ensemb_fit = full_config$ensemb_stage1,
            ensemb_opts = full_config$ensemb_opts,
            output_csv = "custom_stage1_preds.csv"
        )
        full_config[["path_to_custom_stage_1"]] <- stage1$path_to_custom_stage_1
        full_config[["custom_stage1_formula"]] <- stage1$model_formula
    } else {
        full_config[["path_to_custom_stage_1"]] <- stage1_file
        full_config[["custom_stage1_formula"]] <- "custom_file_passed"
        stage1 <- list(model = "custom_file_passed")
        stage1_fml <- data.table(value_id = vid,
                                 formula = "custom_file_passed")
    }
    
    message("*** Preparing ST-GPR inputs:")
    ## have to run st-gpr with specific release id
    full_config$release_id <- full_config$stgpr_release_id
    
    run_data <- save_stgpr_input(
        working_dir = working_dir,
        full_data_file = full_data_file,
        config_template = full_config,
        value_id = vid,
        val_col = full_config$val_col,
        val_measure = full_config$val_measure,
        val_metric = full_config$val_metric,
        val_transform = full_config$val_transform,
        variance_fn = calc_variance,
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


for (val_id in names(run_specs)) {
    specs <- run_specs[[val_id]]
    if (isTRUE(specs$run)) {
        if (is.null(specs$nparallel) | specs$nparallel < 1) {
            specs$nparallel <- 10
        }
        
        ## use existing custom stage 1 data if provided
        stage1_file <- specs$custom_stage1_file
        if (specs$custom_stage1_file != "") {
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
