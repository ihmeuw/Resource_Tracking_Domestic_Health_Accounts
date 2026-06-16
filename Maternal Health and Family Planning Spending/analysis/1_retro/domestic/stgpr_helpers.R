
library(data.table)
library(arrow)
library(rhdf5)
library(ggplot2)
library(patchwork)

source(here::here("src", "r", "stgpr_utils.R"))
source(here::here("analysis", "1_retro", "domestic", "stgpr_configs.R"))
source(here::here("src", "r", "internals.R"))
source_gbd_utils()
source_stgpr_api()


# GENERAL HELPERS =============================================================

get_fit_stats <- function(run_id,
                          model_dir) {
    # load the fit stats for a given run (giving performance for a given set of
    #   hyperparameters)
    fit_stats <- fread(file.path(model_dir, run_id, "fit_stats.csv"))
    return(fit_stats)
}


load_stage <- function(run_id,
                       gbd_locs,
                       stage = c("stage1", "spacetime", "gpr", "final")) {
    # load the estimates for a given stage of the model
    return(
        get_estimates(gbd_model_version_id = run_id,
                      entity = stage,
                      location_id = unique(gbd_locs[level == 3, location_id]))
    )
}


load_draws <- function(model_dir, run_id) {
    run_dir <- file.path(model_dir, run_id)
    output <- fread(file.path(run_dir, "output_0_0.csv"))
    id_cols <- grep("draw_", names(output), value = TRUE, invert = TRUE)
    draws <- melt(output,
                  id.vars = id_cols,
                  variable.name = "draw",
                  value.name = "val")
    draws[, draw := as.integer(gsub("draw_", "", draw))]
    ## for now ignore new GBD2025 point estimate methods and stick to old
    ## draw-level results
    draws[, c("point_estimate", "lower", "upper") := NULL]
    return(draws)

}


# VISUALIZE RESULTS ===========================================================

# TODO: should add gpr_amp_factor and gpr_amp_cutoff to the hyperparam plot

make_country_result_plots <- function(run_id,
                                      gbd_locs,
                                      param_set = NULL,
                                      working_dir,
                                      model_dir) {
    # ======================================================================== #
    #' Generates a pdf with a plot for each country, showing ST-GPR results
    #' 
    #' The first plot in the pdf shows the hyperparameters of the specified
    #' parameter set that were used in the fitting of the given model.
    #' The rest of the plots show, for each country, the results of each
    #' modeling stage.
    #' By default the "best" parameter set is used and estimates are pulled
    #' using the ST-GPR api. If parameter set is specified (e.g, to compare
    #' results from cross-validation runs), the estimates are pulled directly
    #' from the model output directory.
    #' 
    #' @param run_id the id of the run to be analyzed
    #' @param gbd_locs the GBD location set, used to merge on location names
    #' @param param_set the parameter set to be analyzed. If NULL, the best
    #'    parameter set will be used (as determined by cross-validation).
    #' @param working_dir your working directory for stgpr runs, where the
    #'    pdf will be saved in a subdirectory called "figures"
    #' @param model_dir the path to the st-gpr model output directory 
    # ======================================================================== #
    model_cfg <- fread(file.path(working_dir, "run", run_id, "config.csv"))
    value_id <- strsplit(model_cfg$me_name[1], "-")[[1]][1]
    model_note <- if (is.null(model_cfg$note)) "" else model_cfg$note[1]
    run_path <- file.path(model_dir, run_id)
    if (!file.exists(file.path(run_path, "model_complete.csv"))) {
        warning("Run ", run_id, " is not complete. Cannot generate plots.")
        return(NULL)
    }
    
    #
    # load st-gpr hyperparameters and determine which model to use
    #
    fit_stats <- get_fit_stats(run_id, model_dir = model_dir)
    is_best <- FALSE
    if (is.null(param_set)) {
        is_best <- TRUE
        params <- get_parameters(run_id)
        params <- params[, .(st_zeta, st_lambda, st_omega, gpr_scale)]
        fit_stats <- fit_stats[best == 1 & var == "gpr_mean"]
        param_set <- fit_stats$parameter_set
        params[, `:=` (
            in_sample_rmse = fit_stats$in_sample_rmse,
            oos_mean = fit_stats$oos_mean,
            parameter_set = param_set 
        )]
    } else {
        params <- fit_stats[parameter_set == param_set &
                                ko == 0 &  ## ko==0: full data, no subset
                                var == "gpr_mean", 
                            .(parameter_set,
                              st_zeta = zeta, st_lambda = lambdaa, st_omega = omega,
                              gpr_scale = scale,
                              in_sample_rmse, oos_mean)]
    }
    params <- melt(params,
                   variable.name = "param",
                   value.name = "value",
                   id.vars = c("parameter_set", "in_sample_rmse", "oos_mean"))
    setorder(params, param)
    params[, `:=`(value = as.factor(value), order = 1:.N)]
    params[, param := factor(param, levels = params$param)]
    message("value_id: ", value_id)
    message("run: ", run_id, " | param set: ", param_set, " | best: ", is_best)
    message("note: ", model_note)
    
    #
    # LOAD RESULTS FROM EACH STAGE
    #
    message("loading model stages...")
    #
    # load first-stage results (linear model prior)
    #
    stage1 <- load_stage(run_id, gbd_locs, stage = "stage1")
    setnames(stage1, "val", "stage1")
    #
    # load second-stage results (space-time smoothing)
    #
    if (is_best) {
        stage2 <- load_stage(run_id, gbd_locs, stage = "spacetime")
        setnames(stage2, "val", "stage2")
    } else {
        ## rhdf5::h5ls(file.path(run_path, "output_0_0.h5"))
        stage2_file <- file.path(run_path, paste0("output_0_", param_set, ".h5"))
        blk0_items <- rhdf5::h5read(stage2_file, "st/block0_items")
        blk0_vals <- as.data.table(
            t(rhdf5::h5read(stage2_file, "st/block0_values"))
        )
        names(blk0_vals) <- blk0_items
        
        blk1_items <- rhdf5::h5read(stage2_file, "st/block1_items")
        blk1_vals <- as.data.table(
            t(rhdf5::h5read(stage2_file, "st/block1_values"))
        )
        names(blk1_vals) <- blk1_items
        
        stage2 <- cbind(blk0_vals, blk1_vals)
        stage2[, st := untransform_value(st, transf = model_cfg$data_transform[1])]
        setnames(stage2, "st", "stage2")
    }
    #
    # load final st-gpr results (mean, lower, and upper estimates)
    #
    if (is_best) {
        stage3 <- load_stage(run_id, gbd_locs, stage = "gpr")
        setnames(stage3, "val", "mean")
    } else {
        # load draw-level results and create summary
        stage3_dir <- file.path(run_path, "draws_temp_0", param_set)
        draw_files <- list.files(stage3_dir,
                                 pattern = ".csv",
                                 full.names = TRUE)
        draw_data_ls <- lapply(draw_files, fread)
        draws <- rbindlist(draw_data_ls)
        draws <- melt(draws,
                      id.vars = c("year_id", "location_id", "sex_id", "age_group_id"),
                      variable.name = "draw", value.name = "val")
        stage3 <- draws[, .(
            mean = mean(val),
            lower = quantile(val, 0.025),
            upper = quantile(val, 0.975)
        ), by = .(year_id, location_id, sex_id, age_group_id)]
    }
    #
    # combine all stages 
    #
    ests <- merge(stage3, stage1,
                  by = c("location_id", "year_id", "sex_id", "age_group_id"),
                  all.x = TRUE)
    ests <- merge(ests, stage2,
                  by = c("location_id", "year_id", "sex_id", "age_group_id"),
                  all.x = TRUE)
    # add extra location info
    # (inner-join so that only provided locations are included)
    ests <- merge(
        ests,
        gbd_locs[, .(location_id, ihme_loc_id, location_name, level)],
        by = "location_id"
    )
    #
    # load raw data points
    #
    raw_dat <- get_input_data(gbd_model_version_id = run_id,
                              data_stage_name = "original")
    ## inner-join so that only provided locations are included
    raw_dat <- merge(
        raw_dat,
        gbd_locs[, .(location_id, ihme_loc_id, region_name, super_region_name)],
        by = "location_id"
    )
    raw_dat[, is_outlier := as.factor(as.logical(is_outlier))]
    
    #
    # CREATE FIGURES
    #
    # create figure path
    figure_path <- file.path(working_dir, "run", run_id, "figures")
    dir.create(figure_path, showWarnings = FALSE)
    
    message("saving country plots at: ", figure_path)
    
    # open pdf
    pdf_name <- paste0("country_results_run_", run_id, "_param_", param_set)
    if (is_best) pdf_name <- paste0(pdf_name, "_isbest")
    pdf(file.path(figure_path, paste0(pdf_name, ".pdf")),
        width = 9, height = 6)
    
    # create best params plot
    p0 <- ggplot(params, aes(x = param, y = order)) +
        geom_point(size = 5) +
        scale_y_continuous(breaks = params$order,
                           labels = params$value) +
        labs(title = paste0("hyperparameter set [run ", run_id,
                            ", param set ", param_set,"]"),
             subtitle = paste0(if (is_best) "(BEST) " else "",
                               "In-sample RMSE: ",
                               round(params$in_sample_rmse[1], 4),
                               " | Mean out-of-sample RMSE: ",
                               round(params$oos_mean[1], 4)),
             x = "", y = "final value",
             caption = model_note) +
        coord_flip() +
        theme_linedraw(12) +
        theme(panel.grid.minor = element_blank())
    print(p0)
    
    # create plot for each country
    dat_isos <- sort(unique(ests[level == 3, ihme_loc_id]))
    pb <- txtProgressBar(min = 0, max = length(dat_isos), style = 3)
    
    for (i in seq_along(dat_isos)) {
        setTxtProgressBar(pb, i)
        dat_iso <- dat_isos[i]
        dat_reg <- gbd_locs[ihme_loc_id == dat_iso, region_name]
        dat_supreg <- gbd_locs[ihme_loc_id == dat_iso, super_region_name]
        
        p1 <- ggplot(ests[ihme_loc_id == dat_iso],
                     aes(x = year_id)) +
            scale_color_manual(values = c("final" = "green",
                                          "stage1" = "orange",
                                          "st" = "blue",
                                          "country" = "red",
                                          "region" = "#74c476",
                                          "super-region" = "#beaed4"),
                               breaks = c("stage1", "st", "gpr",
                                          "country", "region", "super-region")) +
            scale_shape_manual(values = c(
                "FALSE" = 16, "TRUE" = 1
            )) +
            theme_linedraw() +
            labs(color = "", x = "year", y = value_id) +
            theme(legend.position = "none")
        
        p1 <- p1 +
            geom_ribbon(aes(ymin = lower, ymax = upper),
                        fill = "grey50", alpha = 0.25) +
            geom_line(aes(y = mean, color = "final"), linetype = "solid")
        
        stage1_layer <-
            geom_line(aes(y = stage1, color = "stage1"), linetype = "dotdash")
        stage2_layer <-
            geom_line(aes(y = stage2, color = "st"), linetype = "dotdash")
        
        country_layer <-
            geom_point(
                data = raw_dat[ihme_loc_id == dat_iso],
                aes(y = val, color = "country", shape = is_outlier),
                alpha = 1
            )
        region_layer <- geom_point(
            data = raw_dat[region_name == dat_reg & ihme_loc_id != dat_iso],
            aes(y = val, color = "region", shape = is_outlier),
            alpha = 0.7
        )
        super_region_layer <- geom_point(
            data = raw_dat[super_region_name == dat_supreg &
                               ihme_loc_id != dat_iso & region_name != dat_reg],
            aes(y = val, color = "super-region", shape = is_outlier),
            alpha = 0.5
        )
        p2 <- p1 + stage1_layer + stage2_layer + country_layer
        p3 <- p1 + stage1_layer + stage2_layer + region_layer + country_layer
        p4 <- p1 + stage1_layer + stage2_layer +
            super_region_layer + region_layer + country_layer +
            theme(legend.position = "right")
        titl <- paste(
            ests[ihme_loc_id == dat_iso, c(ihme_loc_id[1],
                                           location_id[1],
                                           location_name[1])],
            collapse = ": "
        )
        titl <- paste0(titl, " [run ", run_id, ", param set ", param_set,
                       if (is_best) " (best)" else "", "]")
        allp <- p1 + p2 + p3 + p4 +
            plot_layout(ncol = 2,
                        guides = "collect",
                        axis_titles = "collect",
                        axes = "collect") +
            plot_annotation(title = titl)
        suppressWarnings( print(allp) )
    }
    dev.off()
    close(pb)
    
    message("Done.")
}


# SAVE DRAWS ==================================================================


finalize_model_run <- function(run_id,
                               gbd_locs,
                               min_draws = 1000,
                               working_dir,
                               model_dir) {
    # ======================================================================== #
    #' @param run_id the id of the run to be saved
    #' @param gbd_locs the GBD location set, used to merge on location names and
    #'     potentially to subset the results
    #' @param working_dir your working directory for stgpr runs, where the
    #'    pdf will be saved in a subdirectory called "figures"
    #' @param model_dir the path to the st-gpr model output directory 
    # ======================================================================== #
    model_cfg <- fread(file.path(working_dir, "run", run_id, "config.csv"))
    value_id <- strsplit(model_cfg$me_name[1], "-")[[1]][1]
    message("Finalizing run ", run_id)
    message("value_id: ", value_id)
    message("note: ", model_cfg$note[1])
    
    if (!file.exists(file.path(model_dir, run_id, "model_complete.csv"))) {
        warning("Run ", run_id, " is not complete. Cannot finalize.")
        return(NULL)
    }
    
    #
    # load fit stats and model params
    #
    fit_stats <- fread(file.path(model_dir, run_id, "fit_stats.csv"))
    params <- get_parameters(run_id)
    
    #
    # load results from each stage
    #
    message("loading model stages...")
    # load first-stage results (linear model prior)
    stage1 <- load_stage(run_id, gbd_locs, stage = "stage1")
    setnames(stage1, "val", "stage1")
    
    # load second-stage results (space-time smoothing)
    stage2 <- load_stage(run_id, gbd_locs, stage = "spacetime")
    setnames(stage2, "val", "spacetime")
    
    # load final st-gpr draws
    draws <- load_draws(model_dir, run_id)
    ndraw <- length(unique(draws$draw))
    if (ndraw < min_draws)
        stop("You are finalizing results from a model with less than ",
             min_draws, " draws (only ", ndraw, " draws)")
    draws[, run_id := run_id]
    
    #
    # summarize draws and combine all stages 
    #
    summ <- draws[, .(
        gpr_mean = mean(val),
        gpr_lower = quantile(val, 0.025),
        gpr_upper = quantile(val, 0.975)
    ), by = .(year_id, location_id, sex_id, age_group_id, measure_id, metric_id)]
    
    summ <- merge(summ, stage1,
                  by = c("location_id", "year_id", "sex_id", "age_group_id"),
                  all.x = TRUE)
    summ <- merge(summ, stage2,
                  by = c("location_id", "year_id", "sex_id", "age_group_id"),
                  all.x = TRUE)
    
    # add extra location info
    ## inner-join so that only provided locations are included
    summ <- merge(
        summ,
        gbd_locs[, .(location_id, ihme_loc_id, location_name, level)],
        by = "location_id"
    )
    
    # get raw data points
    raw_dat <- get_input_data(gbd_model_version_id = run_id,
                              data_stage_name = "original")
    
    # save everything
    message("saving results...")
    out_dir <- file.path(working_dir, "model_values", value_id, "final")
    dir.create(out_dir, showWarnings = FALSE, recursive = FALSE)
    
    fwrite(model_cfg, file.path(out_dir, "config.csv"))
    fwrite(fit_stats, file.path(out_dir, "fit_stats.csv"))
    fwrite(params, file.path(out_dir, "parameters.csv"))
    arrow::write_feather(summ, file.path(out_dir, "summary.feather"))
    arrow::write_feather(raw_dat, file.path(out_dir, "input_data.feather"))
    arrow::write_feather(draws, file.path(out_dir, "draws.feather"))
    
    message("Finalized run ", run_id, " to ", out_dir)
}
