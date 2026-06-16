library(data.table)
library(arrow)
library(lme4)
library(ggplot2)
library(patchwork)

source(here::here("src", "r", "params.R"))
source(here::here("src", "r", "internals.R"))
source_gbd_utils()


#
# misc ========================================================================
#
logit <- function(x) return(log(x / (1 - x)))

inv_logit <- function(x) return(exp(x) / (1 + exp(x)))


#
# stage 1 modeling ============================================================
#

save_covars <- function(covar_names,
                        cache_path,
                        gbd_locs,
                        location_set_id,
                        year_id,
                        age_group_id,
                        sex_id,
                        release_id,
                        rebuild = FALSE) {
    # ======================================================================== #
    #' Query covariate data and save to an apache feather file
    #' 
    #' Function first checks the cache_path to see if file exists and determines
    #' if there are any new covariates to pull, to avoid re-querying database. 
    #' 
    #' @param covar_names character vector of covariate names ("covariate_name_short")
    #' @param cache_path character path to save apache feather file
    #' @param gbd_locs data.table of location metadata
    #' @param location_set_id integer location set id
    #' @param year_id integer year id
    #' @param age_group_id integer age group id
    #' @param sex_id integer sex id
    #' @param release_id integer release id
    #' @param rebuild logical whether to rebuild the data (re-query all covariates)
    # ======================================================================== #
    # first check if file already exits so that we don't need to re-query covars
    #   each time
    if (!is.null(cache_path) && file.exists(cache_path) && !rebuild) {
        curr_data <- arrow::read_feather(cache_path)
        curr_covars <- unique(curr_data$covariate_name_short)
        new_covars <- setdiff(covar_names, curr_covars)
        # no new covariates? no need to re-query or re-save
        if (length(new_covars) == 0) {
            message("save_covars: no new covariates to query.")
            return(invisible())
        }
    } else {
        new_covars <- covar_names
        curr_data <- NULL
    }
    
    # query database for all new covars
    message("save_covars: querying and saving data for: ",
            paste(new_covars, collapse = ", "))
    covar_ids <- get_ids("covariate")
    covar_data <- lapply(new_covars, function(name) {
        id <- covar_ids[covariate_name_short == name, covariate_id]
        message("   * ", name, " (", id, ")")
        o <- get_covariate_estimates(covariate_id = id,
                                     location_set_id = location_set_id,
                                     year_id = year_id,
                                     age_group_id = age_group_id,
                                     sex_id = sex_id,
                                     release_id = release_id)
        if (nrow(o) == 0) {
            stop("save_covars: no data returned for query of covariate: ", name)
        }
        return(o)
    })
    covar_data <- rbindlist(covar_data)

    # merge on location metadata
    covar_data <- merge(covar_data,
                        gbd_locs[, .(location_id, ihme_loc_id,
                                     region_name, super_region_name, level)],
                        by = "location_id",
                        all.x = TRUE)
    
    # if we have existing data, append new data to it
    if (!is.null(curr_data))
        covar_data <- rbind(curr_data, covar_data)
    
    # save/re-save
    arrow::write_feather(covar_data, cache_path)
}


merge_covars <- function(in_dt, covar_path) {
    # ======================================================================== #
    #' Merge covariate data with input data
    #' @param in_dt data.table input data
    #' @param covar_path character path to covariate data cache
    #' @return data.table with covariate data merged
    # ======================================================================== #
    # load cached covariate estimates (covariates are in long format - ie, rows)
    covar_data <- arrow::read_feather(covar_path)
    # remove covariate-specific columns other than name and value
    covar_data[, c("lower_value", "upper_value",
                   "model_version_id", "covariate_id") := NULL]
    # pivot covariate data from long to wide (ie, covars as columns)
    covar_data <- dcast(covar_data, ... ~ covariate_name_short,
                        value.var = "mean_value")
    # merge covariate data with input data
    bycols <- c("location_id", "year_id")
    for (col in c("age_group_id", "sex_id", "ihme_loc_id")) {
        if (col %in% names(in_dt)) {
            bycols <- c(bycols, col)
        }
    }
    nr <- nrow(in_dt)
    in_dt <- merge(in_dt, covar_data, by = bycols, all.x = TRUE)
    if (nrow(in_dt) != nr) {
        stop("merge_covars: faulty merge, resulted in different number of rows.")
    }
    return(in_dt)
}



load_population <- function(cache_path,
                            location_set_id,
                            year_id,
                            release_id,
                            rebuild = FALSE) {
    # ======================================================================== #
    #' Get population data
    #' @param cache_path character path to population data cache
    #' @param location_set_id integer location set id
    #' @param year_id integer year id(s)
    #' @param release_id integer release id
    #' @param rebuild logical whether to rebuild cache (re-query location
    #'    metadata if it already exists on disk)
    # ======================================================================== #
    if (!is.null(cache_path) && file.exists(cache_path) && !rebuild) {
        pops <- arrow::read_feather(cache_path)
    } else {
        pops <- get_population(
            age_group_id = "all", # All ages
            sex_id = "all", # Both
            location_id = "all",
            location_set_id = location_set_id,
            year_id = year_id,
            release_id = release_id 
        )
        arrow::write_feather(pops, cache_path)
    }
    return(pops)
}

merge_population <- function(in_dt,
                             cache_path,
                             gbd_locs,
                             location_set_id,
                             year_id,
                             release_id,
                             rebuild = FALSE) {
    # ======================================================================== #
    #' Merge population data with input data
    #' @param in_dt data.table input data
    #' @param cache_path character path to population data cache
    #' @param location_set_id integer location set id
    #' @param year_id integer year id
    #' @param release_id integer release id
    #' @param rebuild logical whether to rebuild cache (re-query population
    #'     data if it already exists on disk)
    # ======================================================================== #
    pops <- load_population(cache_path,
                            location_set_id, year_id, release_id, rebuild)
    # merge on total and reproductive population
    all_pops <- pops[
            age_group_id == 22 & sex_id == 3, #all ages, both sexes
    ]
    repr_pops <- pops[
        age_group_id == 24 & sex_id == 2, # 15-49 years, female
    ]
    in_dt <- merge(in_dt,
                   all_pops[, .(location_id, year_id, pop = population)],
                   by = c("location_id", "year_id"),
                   all.x = TRUE)
    in_dt <- merge(in_dt,
                   repr_pops[, .(location_id, year_id, repr_pop = population)],
                   by = c("location_id", "year_id"),
                   all.x = TRUE)
    # create variable containing fraction of parent location's population
    in_dt <- merge(in_dt,
                   gbd_locs[, .(location_id, parent_id)],
                   by = "location_id",
                   all.x = TRUE)
    in_dt <- merge(in_dt,
                   all_pops[, .(location_id, year_id, parent_pop = population)],
                   by.x = c("parent_id", "year_id"),
                   by.y = c("location_id", "year_id"),
                   all.x = TRUE)
    in_dt[, pop_frac_parent := pop / parent_pop]
    return(in_dt)
}


merge_health_expenditure <- function(in_dt, gbd_locs, file_path) {
    # ======================================================================== #
    #' Merge health expenditure data with input data
    #'
    #' No cache is used since health expenditure data is already in csv files.
    #' Requires population data, so in_dt should have been passed through
    #' merge_population() before going through this function.
    #' Population data is required to scale health expenditure data by
    #' population weights for sub-national estimation (sub-national estimates
    #' are required in the stage 1 predictions and other covariates already
    #' include sub-national estimates).
    #'
    #' @param in_dt data.table input data
    #' @param gbd_locs data.table GBD location data
    #' @param file_path character path to directory with health expenditure data
    #' @return data.table with health expenditure data merged
    # ======================================================================== #
    # extract population data from in_dt:
    pops <- unique(in_dt[, .(year_id, location_id, parent_id, pop_frac_parent)])
    year_id_range <- range(in_dt$year_id)
    
    # iterate over each health expenditure series
    series <- c("oop_totes", "ghes_totes", "ppp_totes", "dah_totes", "the_totes")
    for (s in series) {
        if (s %in% names(in_dt))
            in_dt[, (s) := NULL]
        # load health expenditure data
        tmp <- fread(file.path(file_path, paste0(s, ".csv")))
        tmp <- tmp[between(year_id, year_id_range[1], year_id_range[2]),
                   .(location_id, year_id, he = get(s))]
        
        # compute region aggregates
        tmp <- merge(tmp,
                     gbd_locs[, .(location_id, region_id, super_region_id)],
                     by = "location_id",
                     all.x = TRUE)
        tmp_r <- tmp[, .(he = sum(he)), by = .(year_id,
                                               location_id = region_id)] 
        tmp_sr <- tmp[, .(he = sum(he)), by = .(year_id,
                                                location_id = super_region_id)]
        tmp_g <- tmp[, .(he = sum(he)), by = .(year_id)]
        tmp_g[, location_id := 1] # Global
        tmp[, c("region_id", "super_region_id") := NULL]
        tmp <- rbind(tmp, tmp_r, tmp_sr, tmp_g)
        
        # merge on health expenditure
        in_dt <- merge(in_dt,
                       tmp[, .(location_id, year_id, he)],
                       by = c("location_id", "year_id"),
                       all.x = TRUE)
        warning(
            "Temporary fix for France in place: not using real France health spending data"
            ## since for this project we really only care about level3
            ## LMICs, France is just a nuisance
        )
        in_dt[location_id %in% c(338, 350, 363, 364, 387, 97896),
              he := 1e9] #old  France level 3
        
        # for level 4 sub-nationals, merge on parent health expenditure and
        #    apply population fraction
        in_dt <- merge(in_dt,
                       tmp[, .(location_id, year_id, parent_he = he)],
                       by.x = c("parent_id", "year_id"),
                       by.y = c("location_id", "year_id"),
                       all.x = TRUE)
        in_dt[is.na(he), he := parent_he * pop_frac_parent]
        in_dt[, parent_he := NULL] 
        
        ## continuation of France tmp fix
        in_dt[parent_id %in% c(338, 350, 363, 364, 387, 97896) & is.na(he),
              he := 1e9 * pop_frac_parent]
        
        # repeat for level 5 subnationals, based on imputed level 4 he
        in_dt <- merge(in_dt,
                       in_dt[, .(location_id, year_id, parent_he = he)],
                       by.x = c("parent_id", "year_id"),
                       by.y = c("location_id", "year_id"),
                       all.x = TRUE)
        in_dt[is.na(he), he := parent_he * pop_frac_parent]
        in_dt[, parent_he := NULL]
        
        # repeat for level 6 subnationals, based on imputed level 5 he
        in_dt <- merge(in_dt,
                       in_dt[, .(location_id, year_id, parent_he = he)],
                       by.x = c("parent_id", "year_id"),
                       by.y = c("location_id", "year_id"),
                       all.x = TRUE)
        in_dt[is.na(he), he := parent_he * pop_frac_parent]
        in_dt[, parent_he := NULL]
        
        if (in_dt[is.na(he), .N] > 0)
            warning("Missing values in health expenditure data: ", s)
        setnames(in_dt, "he", s)
    }
    return(in_dt)
}




load_locations <- function(cache_path,
                          location_set_id,
                          release_id,
                          rebuild = FALSE) {
    # ======================================================================== #
    #' Get location metadata
    #' @param cache_path character path to location metadata cache
    #' @param location_set_id integer location set id
    #' @param release_id integer release id
    #' @param rebuild logical whether to rebuild cache (re-query location
    #'    metadata if it already exists on disk)
    # ======================================================================== #
    if (!is.null(cache_path) && file.exists(cache_path) && !rebuild) {
        locs <- arrow::read_feather(cache_path)
    } else {
        locs <- get_location_metadata(location_set_id = location_set_id,
                                      release_id = release_id)
        arrow::write_feather(locs, cache_path)
    }
    return(locs)
}



transform_value <- function(in_dt, val_col, transf = c("identity", "log", "logit")) {
    # ======================================================================== #
    #' Transform the value column (dependent/outcome variable) using the same
    #' transformations applied internally by ST-GPR.
    #'
    #' @param in_dt data.table input data
    #' @param val_col character name of value column
    #' @param transf character transformation to apply to value column - either
    #'     "identity", "log", or "logit"
    #' @return data.table with transformed value column
    # ======================================================================== #
    if (!val_col %in% names(in_dt)) {
        stop("transform_value: value column not found in input data.")
    }
    transf <- match.arg(transf)
    if (transf == "none") {
        in_dt[, `:=`(val = get(val_col), offset = 0)]
    }
    
    # calculate offset
    offset_factor <- if (transf == "log") 0.01 else 0.001
    in_dt[, offset := offset_factor * median(get(val_col))]
    
    # apply offset and transformation
    in_dt[, val := get(val_col)]
    if (transf == "log") {
        in_dt[, val := log(val + offset)]
    } else if (transf == "logit") {
        in_dt[, val := logit((1 - offset) * (val + offset))]
    } else {
    }
    return(in_dt) 
}


untransform_value <- function(x, transf = c("identity", "log", "logit")) {
    # ======================================================================== #
    #' Reverse the transformation of the value column
    #' @param x numeric vector to un-transform
    #' @param transf character transformation that was applied to the value
    # ======================================================================== #
    transf <- match.arg(transf)
    # apply transformation
    if (transf == "log") {
        return(exp(x))
    } else if (transf == "logit") {
        return(inv_logit(x))
    } else {
        return(x)
    }
    return(in_dt)
}


prep_data_for_stage1 <- function(in_dt,
                                 val_col,
                                 transf = c("none", "log", "logit"),
                                 covar_save_path,
                                 pops_save_path,
                                 locs_save_path,
                                 health_exp_dir,
                                 location_set_id,
                                 year_id,
                                 release_id,
                                 rebuild_locs = FALSE,
                                 rebuild_pops = FALSE,
                                 year_0 = 2000) {
    # ======================================================================== #
    #' Prepare data for stage 1 by adding covariates and population data
    #' 
    #' Stage 1 of the ST-GPR model pipeline is meant to set a prior, essentially
    #' for the mean function of the gaussian process regression (a later stage).
    #' It typically involves fitting a linear model based on covariates which
    #' can be used to extrapolate to locations where data is missing, and
    #' hierarchical modeling is common since it allows for partial pooling across
    #' countries with various quantities of data.
    #'
    #' @param in_dt data.table containing input data
    #' @param val_col character name of value column with outcome data in in_dt
    #' @param transf character transformation to apply to value column - either
    #'     "none", "log", or "logit"
    #' @param covar_save_path character path to covariate cache
    #' @param pops_save_path character path to population cache
    #' @param locs_save_path character path to location metadata cache
    #' @param health_exp_dir character path to directory with health expenditure
    #'     data files
    #' @param location_set_id integer location set id
    #' @param year_id integer year id(s)
    #' @param release_id integer release id
    #' @param rebuild_locs logical whether to rebuild location metadata cache
    #' @param rebuild_pops logical whether to rebuild population cache
    #' @param year_0 integer the year considered to be the first year in the
    #'     series - a time_idx variable is created as year_id - year_0
    # ======================================================================== #
    # load location metadata
    gbd_locs <- load_locations(locs_save_path,
                              location_set_id = location_set_id,
                              release_id = release_id,
                              rebuild = rebuild_locs)
    
    # add covariates from covariate cache
    in_dt <- merge_covars(in_dt, covar_path = covar_save_path)
    in_dt <- merge(in_dt,
                   gbd_locs[, .(location_id, region_id, super_region_id)],
                   by = "location_id",
                   all.x = TRUE)
    in_dt[, `:=`(
        level_1 = as.factor(super_region_id),
        level_2 = as.factor(region_id),
        level_3 = as.factor(location_id)
    )]
    in_dt[, time_ix := year_id - year_0]
    
    # add population data
    in_dt <- merge_population(in_dt,
                              pops_save_path,
                              gbd_locs = gbd_locs,
                              location_set_id = location_set_id,
                              year_id = year_id,
                              release_id = release_id,
                              rebuild = rebuild_pops)
    # add health expenditure data
    in_dt <- merge_health_expenditure(in_dt, gbd_locs, health_exp_dir)
    
    # transform the outcome variable
    in_dt <- transform_value(in_dt, val_col = val_col, transf = transf)
    
    return(in_dt)
}




make_prediction_df <- function(location_ids, pred_years, in_dt = NULL) {
    # ======================================================================== #
    #' Create a prediction data.table with all combinations of location and year
    #' @param location_ids integer vector of location ids
    #' @param pred_years integer vector of prediction years
    #' @param in_dt optional data.table of original input data - if provided it
    #'     will be merged onto the prediction data.table for comparison of
    #'     observed vs. predicted values
    #' @return data.table with all combinations of location and year
    # ======================================================================== #
    pred_dt <- data.table::CJ(
        location_id = unique(location_ids),
        year_id = unique(pred_years)
    )
    if (!is.null(in_dt)) {
        # if input/observed data is provided, merge onto prediction df
        pred_dt <- merge(pred_dt,
                         in_dt[, .(location_id, year_id, val)],
                         by = c("location_id", "year_id"),
                         all.x = TRUE)
    }
    return(pred_dt)
}



predict_stage1 <- function(pred_dt, model, ...) {
    # ======================================================================== #
    #' Generate stage 1 predictions using the fitted model
    #' @param pred_dt data.table created by make_prediction_df() with necessary
    #'     covariates added
    #' @param model model object that has a predict method
    #' @param ... additional arguments to pass to the predict method
    #' @return data.table with stage 1 predictions added
    # ======================================================================== #
    # predict stage 1
    pred_dt[, fit := predict(model, newdata = pred_dt, ...)]
    if (pred_dt[is.na(fit), .N] > 0)
        warning("There are missing values in the predictions. This is likely ",
                "due to missingness in covariates.")
    
    return(pred_dt)
}


add_stgpr_stage1_preds <- function(pred_dt, run_id) {
    # ======================================================================== #
    #' Add ST-GPR stage 1 predictions to the prediction data.table for comparison
    #' @param pred_dt data.table, probably created by make_prediction_df()
    #' @param run_id character ST-GPR run id
    #' @return data.table with ST-GPR stage 1 predictions added
    # ======================================================================== #
    stage1 <- as.data.table(rhdf5::h5read(
        file.path(get_path("int", "stgpr_output"), run_id, "temp_0.h5"),
        "stage1"
        ))
    pred_dt <- merge(pred_dt, stage1,
                     by = c("location_id", "year_id"), all.x = TRUE)
    return(pred_dt)
}


preds_to_custom_stage1 <- function(pred_dt, value_col_lvl) {
    # ======================================================================== #
    #' Convert custom model predictions into the custom stage 1 format
    #' 
    #' ST-GPR can be passed "custom stage 1 results", which it will use instead
    #' of running its own stage 1. This function converts the custom predictions,
    #' probably created by running a custom model over the output of
    #' make_prediction_df() and prep_data_for_stage1(), into the format expected
    #' by ST-GPR.
    #'
    #' @param pred_dt data.table with custom model predictions
    #' @param value_col_lvl character name of the value column in the pred_dt.
    #'     ST-GPR expects this to be in level-space, not transformed space (i.e.,
    #'     apply untrasform_value() to the value column before passing to this)
    # ======================================================================== #
    req_cols <- c("location_id", "year_id", "age_group_id", "sex_id",
                  value_col_lvl)
    if (! "age_group_id" %in% names(pred_dt))
        pred_dt[, age_group_id := 22] # All Ages
    if (! "sex_id" %in% names(pred_dt))
        pred_dt[, sex_id := 3] # Both Sexes
    if (! all(req_cols %in% names(pred_dt))) {
        stop("preds_to_custom_stage1: required columns not found in input data.")
    }
    if (pred_dt[is.na(get(value_col_lvl)), .N] > 0)
        stop("preds_to_custom_stage1: missing values in value column: ", value_col_lvl)
    
    # set unique since if observed data was merged with preds there could be
    #   duplicate rows if the same loc-year had multiple observations
    out <- unique(pred_dt[, ..req_cols])
    setnames(out, value_col_lvl, "cv_custom_stage_1")
    return(out)
}




build_custom_stage1_lme4 <- function(
        working_dir,
        full_data_file,
        val_col,
        val_transform,
        value_id,
        gbd_covar_names,
        model_formula = "val ~ 1",
        health_exp_dir,
        location_set_id,
        year_id,
        age_group_id,
        sex_id,
        gbd_release_id,
        rebuild = c("none", "covars", "pops", "locs"),
        predict_re = FALSE,
        smooth_fit = FALSE,
        loess_opts = list(span = 0.5, degree = 2),
        ensemb_fit = FALSE,
        ensemb_opts = list(
            model_formula = "val ~ 1 + (1 | level_3/level_2/level_1)",
            weights = c("main" = 1/2, "simple" = 1/2),
            REML = FALSE
        ),
        output_csv = "custom_stage1_preds.csv",
        prep_only = FALSE
) {
    # ======================================================================== #
    #' Fit ST-GPR stage 1 model using lme4::lmer and generate custom stage 1
    #' prediction file.
    #' 
    #' The first stage of ST-GPR is to generate a "linear prior" for all
    #' countries, across years, by using observed data to fit a linear (hierarchical)
    #' model that predicts the modeled variable based on shared covariates that
    #' are observed for all locations and years of interest.
    #' 
    #' @param working_dir character, path to directory where files will be saved
    #' @param full_data_file character, path to file with all input data
    #' @param val_col character, name of column in `full_data_file` with modeled variable
    #' @param val_transform character, transformation to apply to `val_col` before modeling
    #' @param value_id character, the code name for the modeled variable,
    #'     used to subset the input data.
    #' @param gbd_covar_names character, names of GBD covariates to use in the
    #'     model, which will be queries from the GBD database.
    #' @param model_formula character, formula to use in the model. Can include
    #'     the specified GBD covariates, as well as health expenditure variables
    #'     (oop_totes, ppp_totes, ghes_totes, the_totes), populations variables
    #'     ("pop" for country-year's population, repr_prop for country-year's
    #'     reproductive population, etc.). See the `pred_dt` in the output of
    #'     this function for the full list of available variables.
    #' @param health_exp_dir character, path to directory with health expenditure
    #'     data files.
    #' @param location_set_id integer, the location set ID to use for the analysis.
    #' @param year_id integer, the year ID to use for the analysis.
    #' @param age_group_id integer, the age group ID to use for the analysis.
    #' @param sex_id integer, the sex ID to use for the analysis.
    #' @param gbd_release_id integer, the GBD release ID to use for the analysis.
    #' @param rebuild character, whether to rebuild the covariates, populations,
    #'     or locations data files. Can be "none", or any combination of the other
    #'     options. After loading, they are cached in the working_dir and won't
    #'     be re-queried unless requested.
    #' @param predict_re logical, whether to predict the random effects for each
    #'    location-year in the stage 1 model (this mirrors ST-GPR's own stage 1
    #'    parameter). 
    #' @param smooth_fit logical, whether to smooth the stage-1 fit using LOESS
    #' @param loess_opts list, `span` and `degree` options to pass to
    #'     `loess` if `smooth_fit` is TRUE - see `?loess` for details.
    #' @param ensemb_fit logical, whether to ensemble the predictions of the main
    #'     stage-1 model with those of another model (by default, a simpler model
    #'     of the mean value for a country, that only includes the intercept and
    #'     random effects).
    #' @param ensemb_opts a list with 2 elements: "model_formula" which specifies the
    #'     formula of the alternative model used in the ensemble, and "weights",
    #'     a character vector of length 2 which specifies the weights to use in
    #'     the ensemble for the main and alternative models, respectively (default
    #'     is 2/3 and 1/3). 
    #' @param output_csv character, path of the output CSV file used to save the
    #'    custom stage 1 predictions (within working_dir).
    # ======================================================================== #
    rebuild <- match.arg(rebuild, several.ok = TRUE)
    if ("none" %in% rebuild)
        rebuild <- "none" # prevent rebuild = c("none", "covars"), etc.
    config <- list(
        # arguments
        val_col = val_col,
        val_transform = val_transform,
        value_id = value_id,
        gbd_covar_names = gbd_covar_names,
        model_formula = model_formula,
        rebuild_covars = if ("covars" %in% rebuild) TRUE else FALSE,
        rebuild_pops = if ("pops" %in% rebuild) TRUE else FALSE,
        rebuild_locs = if ("locs" %in% rebuild) TRUE else FALSE,
        predict_re = predict_re,
        smooth_fit = smooth_fit,
        loess_opts = loess_opts,
        ensemb_fit = ensemb_fit,
        ensemb_opts = ensemb_opts,
        # gbd params
        location_set_id = location_set_id, # default used by ST-GPR = 22
        year_id = year_id, # first year of data to last estimate year (for release_id 16, 2024 is required)
        age_group_id = age_group_id, # all ages = 22
        sex_id = sex_id, # both sexes = 3
        release_id = gbd_release_id, # GBD 2023 - the required release_id for ST-GPR runs = 16
        # files
        working_dir = working_dir,
        full_data_file = full_data_file,
        health_exp_dir = health_exp_dir,
        output_csv = output_csv,
        prep_only = prep_only
    )
    config <- within(config, {
        covar_save_path  = file.path(config$working_dir, "covariate_cache.feather")
        pops_save_path   = file.path(config$working_dir, "population_cache.feather")
        expend_save_path = file.path(config$working_dir, "expenditure_cache.feather")
        locs_save_path   = file.path(config$working_dir, "location_cache.feather")
    })

    #
    # load auxiliary data sets
    #
    gbd_locs <- load_locations(config$locs_save_path,
                               location_set_id = config$location_set_id,
                               release_id = config$release_id,
                               rebuild = config$rebuild_locs)
    
    #
    # cache all covariates of interest
    #
    message("Querying covariates...")
    save_covars(config$gbd_covar_names,
                config$covar_save_path,
                gbd_locs = gbd_locs,
                location_set_id = config$location_set_id,
                year_id = config$year_id,
                age_group_id = config$age_group_id,
                sex_id = config$sex_id,
                release_id = config$release_id,
                rebuild = config$rebuild_covars)
    
    
    #
    # prep input data: load data and merge with covariates
    #
    message("Prepping data frames...")
    full <- fread(file.path(config$working_dir, config$full_data_file))
    input_dt <- full[value_id == config$value_id]
    input_dt <- prep_data_for_stage1(
        in_dt = input_dt,
        val_col = config$val_col,
        transf = config$val_transform,
        covar_save_path = config$covar_save_path,
        pops_save_path = config$pops_save_path,
        locs_save_path = config$locs_save_path,
        health_exp_dir = config$health_exp_dir,
        location_set_id = config$location_set_id,
        year_id = config$year_id,
        release_id = config$release_id,
        rebuild_locs = config$rebuild_locs,
        rebuild_pops = config$rebuild_pops
    )
    
    #
    # create prediction data set
    #
    ## create prediction grid and merge on features
    loc_ids <- gbd_locs[level >= 3, location_id] # countries and sub-nationals
    pred_dt <- make_prediction_df(loc_ids, config$year_id)
    pred_dt[, val := 0] # needed for prep_data_for_stage1
    
    pred_dt <- prep_data_for_stage1(
        in_dt = pred_dt,
        val_col = "val",
        transf = "identity",
        covar_save_path = config$covar_save_path,
        pops_save_path = config$pops_save_path,
        locs_save_path = config$locs_save_path,
        health_exp_dir = config$health_exp_dir,
        location_set_id = config$location_set_id,
        year_id = config$year_id,
        release_id = config$release_id,
        rebuild_locs = FALSE,
        rebuild_pops = FALSE
    )
    pred_dt[, val := NULL]
    
    config$input_dt <- input_dt
    config$pred_dt <- pred_dt
    if (config$prep_only) {
        message("Returning prepped data frames.")
        return(config)
    }
    
    
    #
    # fit model
    #
    message("Fitting model...")
    model_stg1 <- lme4::lmer(config$model_formula,
                             REML = TRUE,
                             data = input_dt[is_outlier == 0])
    
    
    #
    # create predictions
    #
    message("Generating predictions...")
    ## predict
    re.form <- if (config$predict_re) NULL else NA
    pred_dt <- predict_stage1(pred_dt,
                              model = model_stg1,
                              allow.new.levels = TRUE,
                              re.form = re.form)

    if (config$smooth_fit) {
        message("Smoothing predictions...")
        pred_dt[, fit_unsmoothed := fit]
        pred_dt[, fit := fitted(loess(fit_unsmoothed ~ year_id,
                                      span = config$loess_opts$span,
                                      degree = config$loess_opts$degree)),
                by = location_id]
    }
    if (config$ensemb_fit) {
        W <- config$ensemb_opts$weights
        if (length(W) != 2) {
            stop("Ensemble weights must be a vector of length 2. Check ensemb_opts.")
        }
        if (sum(W) != 1) {
            stop("Provided ensemble weights do not sum to 1. Check ensemb_opts.")
        }
        message("Ensembling predictions with simpler model...")
        model_simple <- lme4::lmer(config$ensemb_opts$model_formula,
                                   data = input_dt[is_outlier == 0],
                                   REML = config$ensemb_opts$REML)
        pred_dt[, fit_main   := fit]
        pred_dt[, fit_simple := predict(model_simple,
                                        newdata = pred_dt,
                                        allow.new.levels = TRUE,
                                        re.form = re.form)]
        
        pred_dt[, fit := (W[1] * fit_main) + (W[2] * fit_simple)]
    }
    
    #
    # create level version of the outcome variable and convert to custom stage 1
    #
    pred_dt[, `:=`(
        fit_lvl = untransform_value(fit, transf = config$val_transform)
    )]
    custom_stg1 <- preds_to_custom_stage1(pred_dt, "fit_lvl")
    message("Saving custom stage 1 predictions.")
    outpath <- file.path(config$working_dir, "model_values",
                         config$value_id, config$output_csv)
    fwrite(custom_stg1, outpath)
    
    config$model <- model_stg1
    config$pred_dt <- pred_dt
    config$path_to_custom_stage_1 <- outpath
    return(config)
}






#
# STGPR RUN HELPERS ===========================================================
#

stgpr_default_variance <- function(dt, val_col = "val") {
    # ======================================================================== #
    #' Default function for computing data variance for ST-GPR input data
    #'
    #' Computes the variance for each row by computing the empirical variance
    #'   of the `val_col` column for each location (across time).
    #' Locations with only one observation (and thus no empirical variance) are
    #' assigned the average variance across the locations for which the variance
    #' was calculatable.
    #' @param dt data.table: input data with columns `location_id` and `eval(val_col)`.
    # ======================================================================== #
    req_cols <- c("location_id", val_col)
    if (!all(req_cols %in% names(dt)))
        stop("Missing required columns: ",
             paste0(req_cols[!req_cols %in% names(dt)], collapse = ", "))

    dt[, variance := var(get(val_col)), by = .(location_id)]
    ## if only one value for this location, or if calculated variance is very small
    dt[is.na(variance) | variance < min(get(val_col)), 
       variance := mean(dt[, variance], na.rm = TRUE)]
    return(dt)
}


stgpr_bernoulli_variance <- function(dt,
                                     val_col = "model_value",
                                     denom_col = "model_value_denom") {
    # ======================================================================== #
    #' Compute the variance for each observation based on a bernoulli assumption.
    #' 
    #' Let Y be our observed random variable that counts the amount of spending
    #' from a financing source (say OOP spending) that goes to a specific
    #' disease area (say family planning) in a given country-year.
    #' Let Y := X_1 + ... + X_n, where X_i is a random variable that denotes
    #' whether a single dollar of spending goes to the specific disease area or
    #' not, and n is the total amount of health spending from the financing
    #' source (n >= Y).
    #' Assume that X_i ~ Bernoulli(p), where p is probability of a dollar of
    #' spending going to the specific disease area.
    #' Let the observed proportion of spending to the disease area be the
    #' empirical estimate of p for a given country-year. Another way to write the
    #' proportion is p_hat = (X_i + ... + X_n) / n. Then the variance of p_hat is:
    #' Var(p_hat) = Var(1/n \* sum(X_i)) = 1/n^2 \* sum(Var(X_i))
    #'     = 1/n^2 \* sum(p (1 - p)) 
    #'     = 1/n^2 \* (n p (1 - p))
    #'     = (p (1-p)) / n
    #' This formula is used to compute the variance for each row in the input.
    #' 
    #' @param dt data.table: input data with columns `val_col` and `denom_col`.
    #' @param val_col character: column name of the proportion.
    #' @param denom_col character: column name of the proportion's denominator,
    #'     i.e., the total amount of spending from the financing source in the
    #'     country-year.
    if(dt[get(val_col) < 0 | get(val_col) > 1, .N] > 0)
        stop("Values in `val_col` must be between 0 and 1 for bernoulli variance calculation")
    dt[, variance := (get(val_col) * (1 - get(val_col))) / get(denom_col)]
    return(dt)
}


stgpr_trend_variance <- function(dt) {
    # ======================================================================== #
    #' Use the "trend variance" as the data variance.
    #' 
    #' The trend variance is calculated in the outlier detection step by fitting
    #' a flexible, smooth function of time to the data and computing the
    #' raw residuals.
    #' This step needs to have been run in order for this function to work.
    #' 
    #' @param dt data.table: input data with column `trend_variance`.
    if (! "trend_variance" %in% names(dt)) {
        stop("You specified stgpr_trend_variance but the trend variance has not", 
             " been calculated for the provided input data.")
    }
    dt[, variance := abs(trend_variance)]
    min_var <- min(dt[variance > 0, variance])
    max_var <- max(dt[variance < 1, variance])
    dt[variance < .Machine$double.eps, variance := min_var]
    dt[variance > 1, variance := max_var]
    return(dt)
}


stgpr_existing_variance <- function(dt) {
    if (! "variance" %in% names(dt))
        stop("You specified stgpr_existing_variance but the 'variance' column",
             " is not present in the input data.")
    return(dt)
}


.validate_stgpr_input <- function(inp_dt) {
    # ======================================================================== #
    #' Validate input data for ST-GPR
    # ======================================================================== #
    req_cols <- c("nid", "location_id", "year_id", "age_group_id", "sex_id",
                  "measure_id", "val", "variance", "sample_size", "is_outlier")
    
    # check for missing columns
    if (!all(req_cols %in% names(inp_dt)))
        stop("Missing required columns: ",
             paste0(req_cols[!req_cols %in% names(inp_dt)], collapse = ", "))
    
    # check for missing values
    for (col in req_cols)
        if (any(is.na(inp_dt[[col]])))
            stop("Column '", col, "' has missing values")
    
    # validate choice of measure
    measure <- unique(inp_dt$measure_id)
    if (length(measure) != 1)
        stop("Multiple measures found: ", paste0(measure, collapse = ", "))
    if (! measure %in% 18:19)
        stop("measure_id must be 18 (proportion) or 19 (continuous)")
    if (!all(inp_dt$is_outlier %in% c(0, 1)))
        stop("is_outlier must be 0 or 1")
    
    # validate variance
    if (any(inp_dt$variance <= 0))
        stop("Variance must be positive")
    if (measure == 18 && any(inp_dt$variance > 1))
        stop("Variance must be between 0 and 1 for proportion data")
    
    # validate sample_size
    if (any(inp_dt$sample_size <= 0) || any(inp_dt$sample_size > 8e9))
        stop("sample_size must be positive and less than 8 billion")
}


.validate_stgpr_config <- function(cfg) {
    # ======================================================================== #
    #' Validate ST-GPR configuration file
    # ======================================================================== #
    req_params_general <- c(
        "release_id", "description", "metric_id", "measure_id",
        "modelable_entity_id", "prediction_units"
    )
    req_params_stage1 <- c(
        # note: these are not technically required but are practically necessary
        "gbd_covariates", "stage_1_model_formula"
    )
    req_params_stage2 <- c(
        "st_lambda", "st_omega", "st_zeta"
    )
    req_params_stage3 <- c(
        "gpr_scale"
    )
    if (!inherits(cfg, "data.frame"))
        stop("The ST-GPR must be a data.frame like object for saving as a CSV")
    err <- FALSE
    for (param in req_params_general) {
        if (!param %in% names(cfg)) {
            err <- TRUE
            warning("Missing required parameter: ", param,
                    call. = FALSE)
        }
    }
    if (! "path_to_data" %in% names(cfg) &&
        (! "bundle_id" %in% names(cfg) || ! "crosswalk_id" %in% names(cfg))) {
        err <- TRUE
        warning("Missing required parameter: either path_to_data or bundle_id/crosswalk_id",
                call. = FALSE)
    }
    if (err) stop("Missing required parameters - see warnings")
    if ("density_cutoffs" %in% names(cfg)) {
        nbuckets <- length(
            strsplit(cfg[["density_cutoffs"]], split = ",", fixed = TRUE)[[1]]
        )
        if (!grepl("^0", cfg[["density_cutoffs"]]))
            nbuckets <- nbuckets + 1
        for (hyperparam in c("st_lambda", "st_omega", "st_zeta", "gpr_scale")) {
            hp <- strsplit(cfg[[hyperparam]], split = ",", fixed = TRUE)[[1]]
            if (length(hp) != nbuckets)
                stop("Number of hyperparameters in ", hyperparam, " does not match density_cutoffs")
        }
    }
}


save_stgpr_input <- function(working_dir, # stgpr working directory
                             full_data_file,
                             config_template,
                             val_col,     # the column that contains the value to be modeled
                             value_id,
                             variance_fn = stgpr_existing_variance, # function to compute variance per row
                             val_measure = c("continuous", "proportion"),
                             val_metric = c("number", "rate"),
                             val_transform = c("identity", "logit", "log"),
                             update_config = NULL,
                             save_data = TRUE) {
    # ======================================================================== #
    #' Finalize, validate, and save input data and configuration file for ST-GPR
    #' 
    #' @param working_dir character: directory to save data and configuration
    #' @param full_data_file character: file name for the csv containing all the
    #'     input data.
    #' @param config_template list: template for ST-GPR configuration
    #' @param val_col character: column containing the variable to be modeled
    #'     (Known as 'val' in ST-GPR docs.)
    #' @param value_id character: name/code-name of the value being
    #'     modeled, used to filter full_data_file.
    #' @param variance_fn function: function to compute variance for each
    #'     observations. Function must accept the input data.table and return
    #'     the data.table with a new column 'variance'. The default is
    #'     `stgpr_existing_variance()` which assumes the 'variance' column is
    #'     already in the input data (and checks that this is true).
    #' @param val_measure character: type of measure for the variable being
    #'     modeled. Either 'continuous' or 'proportion'.
    #' @param update_config list: specify ST-GPR config parameters that you want
    #'     to overwrite in the template config.
    #' @param save_data logical: defaults TRUE. Use FALSE for testing if you
    #'     just want the function to return the data without saving.
    # ======================================================================== #

    #
    # validate args
    #
    fp <- file.path(working_dir, full_data_file)
    if (!file.exists(fp))
        stop("input data does not exist: ", fp)
    dat <- fread(fp)
    if (! val_col %in% names(dat))
        stop("val_col not in input data")
    val_measure <- match.arg(val_measure)
    val_metric <- match.arg(val_metric)
    val_transform <- match.arg(val_transform)
    .value_id <- value_id
    
    #
    # prep input data
    #
    dat <- dat[value_id == .value_id]
    if (nrow(dat) == 0) stop("Empty data set created after filtering by value_id.")
    dat[, val := get(val_col)]
    
    # compute variance
    dat <- variance_fn(dat)
    
    # identify measure_id & metric_id
    dat[, measure_id := fcase(
        val_measure == "continuous", 19, # get_ids("measure")
        val_measure == "proportion", 18
    )]
    dat[, measure := val_measure]
    dat[, metric_id := fcase(
        val_metric == "number", 1, # get_ids("metric")
        val_metric == "rate", 3
    )]
    dat[, metric := val_metric]
    dat[, data_transform := val_transform]
    
    # select final columns
    dat <- dat[,c(
        "nid", "location_id", "year_id", "age_group_id", "sex_id",
        "measure_id", "measure", "metric_id", "metric", "data_transform",
        "val", "variance", "sample_size",
        "is_outlier"
    ), with = FALSE]
    .validate_stgpr_input(dat)
    
    # create name and path for data
    nm <- paste0(.value_id, "-", val_transform)
    dat[, name := nm]
    data_path <- file.path(working_dir, "model_values",
                           .value_id, paste0(nm, ".csv"))
    
    #
    # create stgpr config
    #
    cfg <- as.list(config_template)
    cfg <- append(cfg, cfg[["cross_validation"]]) ## unnest
    cfg[["cross_validation"]] <- NULL
    cfg[lengths(cfg) > 1] <- NULL
    if (is.list(update_config)) {
        for (key in names(update_config))
            cfg[[key]] <- update_config[[key]]
    }
    if ("path_to_custom_stage_1" %in% names(cfg)) {
        cfg[["gbd_covariates"]] <- NULL
        cfg[["stage_1_model_formula"]] <- NULL
        if (!file.exists(cfg[["path_to_custom_stage_1"]]))
            stop("path_to_custom_stage_1 provided but does not exist")
    }
    if ("do_cross_validation" %in% names(cfg)) {
        cv <- cfg[["do_cross_validation"]]
        if (!is.null(cv) && cv == TRUE) {
            cfg[["st_lambda"]] <- get("cv_st_lambda", cfg)
            cfg[["st_omega"]] <- get("cv_st_omega", cfg)
            cfg[["st_zeta"]] <- get("cv_st_zeta", cfg)
            cfg[["gpr_scale"]] <- get("cv_gpr_scale", cfg)
            cfg[["density_cutoffs"]] <- NULL # can't have both
            if (is.null(cfg[["holdouts"]])) {
                cfg[["holdouts"]] <- "0"
            }
        } else {
            # if holdouts is >0, the model will run cross-validation even though
            # there is only one set of hyper-parameters (so waste of time)
            cfg[["holdouts"]] <- "0"
        }
    }
    
    
    ## y-axis used in st-gpr viz
    setDT(cfg) # convert to data.table
    cfg[, `:=` (
        path_to_data = data_path,
        prediction_units = paste(val_measure, "of", .value_id),
        measure_id = dat$measure_id[1],
        metric_id = dat$metric_id[1],
        data_transform = dat$data_transform[1],
        me_name = nm
    )]
    .validate_stgpr_config(cfg)
    config_path <- file.path(working_dir, "model_values",
                             .value_id, paste0(nm, "_config.csv"))
    
    #
    # save and return
    #
    if (save_data) {
        fwrite(dat, data_path)
        fwrite(cfg, config_path)
    }
    return(list(data = dat,
                config = cfg,
                config_path = config_path,
                saved = save_data))
}



plot_crossval_stats <- function(run_id) {
    # ======================================================================== #
    #' Get cross-validation statistics for a ST-GPR run
    #' 
    #' @param run_id character: ID of the ST-GPR run
    # ======================================================================== #
    
    # get cross-validation stats
    raw <- fread(file.path(
        get_path("int", "stgpr_output"), run_id, "fit_stats.csv"
    ))
    cv_stats <- raw[var == "gpr_mean",
                    .(parameter_set,
                      st_zeta = zeta,
                      st_lambda = lambdaa,
                      st_omega = omega,
                      gpr_scale = scale,
                      in_sample_rmse, oos_rmse, best)]
    rmse_type <- ""
    if (any(is.na(cv_stats$oos_rmse))) {
        cv_stats[, rmse := in_sample_rmse]
        rmse_type <- "in_sample_rmse"
    } else {
        cv_stats[, rmse := oos_rmse]
        rmse_type <- "oos_rmse"
    }
    cv_stats[, c("in_sample_rmse", "oos_rmse") := NULL]
    cv_stats <- melt(cv_stats,
                     id.vars = c("parameter_set", "rmse", "best"),
                     variable.name = "param")
    
    p <- ggplot(cv_stats, aes(x = value, y = rmse, color = as.factor(best))) +
        geom_point(alpha = 0.5) +
        scale_color_manual(values = c("0" = "black", "1" = "red"),
                           labels = c("0" = "", "1" = "best")) +
        facet_wrap(~param, scales = "free") +
        labs(x = "parameter value", y = rmse_type, color = "",
             title = paste("Cross-validation statistics for", run_id),
             subtitle = paste0(rmse_type, " best: ", cv_stats[best == 1, round(rmse, 5)]))
    
    return(list(stats = raw, plot = p))
}
