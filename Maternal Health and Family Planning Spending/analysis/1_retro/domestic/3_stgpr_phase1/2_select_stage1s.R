#
# run covariate/model selection selection and save stage 1 formulas
# - covariate selection is done via a stepwise algorithm (lasso regression was
#   also considered)
# - covariates are generally transformed to help with model convergence and fit
#
library(data.table)
library(ggplot2)
library(lme4)
library(tictoc)
source(here::here("src", "r", "params.R"))
source(here::here("src", "r", "stgpr_utils.R"))
source(here::here("analysis", "1_retro", "domestic", "stgpr_configs.R"))

options(warn = 1) # print warnings as they occur
set.seed(3980)


PATHS <- list(
    dirs = list(
        working = get_path("int", "data", c("domestic", "int", "stgpr")),
        covars = get_path("int", "data", c("covars"))
    )
)

PATHS$dirs$figures <- file.path(PATHS$dirs$working, "stage1_select_figures")
dir.create(PATHS$dirs$figures, recursive = TRUE, showWarnings = FALSE)



#
# Helpers =====================================================================
#
make_formula_for_lme4 <- function(covar_names,
                                  rand_slopes = TRUE,
                                  rand_int = TRUE) {
    # build formula
    ## fixed effects
    rhs <- paste(covar_names, collapse = " + ")
    ## random effects
    if (rand_slopes) {
        r_slopes <- vapply(covar_names, function(x) {
            paste0("(0 + ", x, " | level_3)") # super-region
        }, character(1))
        r_slopes <- paste(r_slopes, collapse = " + ")
        rhs <- paste(rhs, "+", r_slopes)
    }
    if (rand_int) {
        r_int <- "(1 | level_1/level_2/level_3)" # super-region/region/country
        rhs <- paste(rhs, "+", r_int)
    }
    
    fml <- paste("val ~", rhs)
    return(fml)
}



lasso_selection <- function(possible_covars,
                            input_dt,
                            top_n = Inf) {
    # lasso regression for covariate selection
    
    # center and scale all covariates (after any transformation)
    scaled_covars <- vapply(possible_covars,
                            \(x) paste0("scale(", x, ")"),
                            character(1))
    fml_lasso <- paste("val ~", paste(scaled_covars, collapse = " + "))
    
    # lasso regression - cross validate to tune lambda, then fit model
    X <- as.matrix(model.frame(fml_lasso, data = input_dt))[, -1] # drop outcome
    y <- input_dt$val
    cv_lambda <- glmnet::cv.glmnet(x = X,
                                   y = y,
                                   alpha = 1) # alpha = 1: lasso
    lasso_mod <- glmnet::glmnet(x = X,
                                y = y,
                                alpha = 1,
                                lambda = cv_lambda$lambda.min)
    
    # extract top covariates based on coefficient size
    betas <- data.table(coef = rownames(lasso_mod$beta),
                        beta = as.numeric(lasso_mod$beta[, 1]))
    betas[, ix := .I]
    betas <- betas[abs(beta) > .Machine$double.eps][order(-abs(beta))]
    final_betas <- head(betas[order(-abs(beta))], n = top_n)
    
    # return formula for selected model
    lasso_covars <- possible_covars[final_betas$ix]
    lasso_final_fml <- paste("val ~", paste(lasso_covars, collapse = " + "))
    
    return(list(covars = lasso_covars, fml = lasso_final_fml))
}


aic_selection <- function(possible_covars, aic_mods) {
    # select best model across the 3 stepwise methods
    aics <- vapply(aic_mods, AIC, numeric(1))
    best_model <- names(aic_mods)[which.min(aics)]
    
    mod_aic_fin <- aic_mods[[best_model]]
    aic_covars <- names(mod_aic_fin$model)[-1] # drop intercept
    aic_final_fml <- paste("val ~", paste(aic_covars, collapse = " + "))
    
    return(list(covars = aic_covars, fml = aic_final_fml))
}


compare_model_oos <- function(selection_list,
                              input_dt,
                              nrep = 100) {
    # predict out-of-sample to compare performance of models
    # repeat nrep times so that the train and test data are different each time
    locs <- unique(input_dt$ihme_loc_id)
    nloc <- length(locs)
    mses <- matrix(NA, nrow = nrep, ncol = length(selection_list))
    colnames(mses) <- names(selection_list)
    
    formula_list <- lapply(selection_list, \(x) x$fml)
    
    for (i in seq(1, nrep)) {
        # partition data into training and test sets (split locations)
        tr_locs <- sample(locs, size = ceiling(nloc * 0.7), replace = FALSE)
        Xtr <- input_dt[ihme_loc_id %in% tr_locs]
        Xte <- input_dt[! ihme_loc_id %in% tr_locs]
        
        # fit linear models using each formula on the training subset
        mods <- lapply(formula_list, lm, data = Xtr)
        # eval each model on the test subset
        mse <- vapply(mods, function(mod) {
            pr <- predict(mod, newdata = Xte)
            r <- pr - Xte$val
            return(mean(r^2))
        }, numeric(1))
        mses[i, ] <- mse
    }
    # which model has lower MSE on average?
    winner <- names(selection_list)[ which.min(colMeans(mses)) ]
    return(winner)
}


fit_stage1_components <- function(cfg, fml) {
    ## note: troubleshooting convergence warnings:
    ##   https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
    message("* Testing full model with location random effects...")
    model_final <- lmer(fml, data = cfg$input_dt[is_outlier == 0])
    
    # generate predicted values
    cfg$pred_dt <- predict_stage1(cfg$pred_dt,
                                  model = model_final,
                                  re.form = NULL, # includes all RE 
                                  allow.new.levels = TRUE)
    
    # smooth fit for comparison (on transformed scale)
    cfg$pred_dt[, fit_smooth_l := fitted(loess(fit ~ year_id,
                                             span = 0.5,
                                             degree = 2)),
                by = .(location_id)]
    
    
    # fit simple model of mean value to ensemble with covariate model
    # https://stats.stackexchange.com/questions/48671/what-is-restricted-maximum-likelihood-and-when-should-it-be-used
    model_simple <- lmer(stgpr_config_base$ensemb_opts$model_formula,
                         data = cfg$input_dt[is_outlier == 0],
                         REML = stgpr_config_base$ensemb_opts$REML)
    cfg$pred_dt[, fit_simple_l := predict(model_simple,
                                          newdata = .SD,
                                          re.form = NULL,
                                          allow.new.levels = TRUE)]
    
    # convert response back to original scale
    cfg$pred_dt[, fit_lvl := untransform_value(fit, cfg$val_transform)]
    cfg$pred_dt[, fit_smooth := untransform_value(fit_smooth_l, cfg$val_transform)]
    cfg$pred_dt[, fit_simple := untransform_value(fit_simple_l, cfg$val_transform)]
    # ensemble:
    W <- stgpr_config_base$ensemb_opts$weights
    stopifnot(sum(W) == 1, length(W) == 2)
    cfg$pred_dt[, fit_ensemb := untransform_value(
        fit_smooth_l * W[1] + fit_simple_l * W[2],
        cfg$val_transform
    )]
    
    cfg$covar_model <- model_final
    return(cfg)
}



#
# Plotting functions ==========================================================
#


qp <- function(iso, points = TRUE) {
    p <- ggplot(res$pred_dt[ihme_loc_id == iso], aes(x = year_id)) +
        geom_line(aes(y = fit_smooth), color = "orange") +
        geom_line(aes(y = fit_simple), color = "red") +
        geom_line(aes(y = fit_ensemb), color = "black")
    if (points) p <- p + geom_point(data = res$input_dt[ihme_loc_id == iso],
                                    aes(y = model_value))
    return(p)
}

plot_custom_stage1 <- function(stage1_config,
                               plot_pdf_path,
                               isos = "all",
                               pdf_width = 12,
                               pdf_height = 6) {
    pred_dt <- stage1_config$pred_dt[level == 3] # national estimates only
    input_dt <- stage1_config$input_dt
    if (isos[1] == "all") {
        isos <- sort(unique(pred_dt$ihme_loc_id))
    } else {
        isos <- sort(unique(pred_dt[ihme_loc_id %in% isos, ihme_loc_id]))
    }
    
    pb <- txtProgressBar(min = 0, max = length(isos), style = 3, initial = 0)
    on.exit(close(pb))
    
    pdf(plot_pdf_path, width = pdf_width, height = pdf_height)
    for (i in seq_along(isos)) {
        iso <- isos[i]
        p <- ggplot(pred_dt[ihme_loc_id == iso], aes(x = year_id, y = fit_lvl)) +
            geom_line(aes(y = fit_lvl, color = "fit"),
                      alpha = 0.7, linetype = "dotdash") +
            geom_line(aes(y = fit_smooth, color = "fit smoothed"),
                      alpha = 0.7, linetype = "dotdash") +
            geom_line(aes(y = fit_simple, color = "fit simple"),
                      alpha = 0.7, linetype = "dotdash") +
            geom_line(aes(y = fit_ensemb, color = "ensemble"),
                      alpha = 0.7) +
            scale_color_manual(values = c("fit" = "blue",
                                          "fit smoothed" = "orange",
                                          "fit simple" = "red",
                                          "ensemble" = "black")) +
            theme_linedraw() +
            labs(title = paste0(iso, ": ", pred_dt[ihme_loc_id == iso, location_name][1]),
                 x = "year_id", y = "model_value (untransformed)",
                 color = "model")
        p2 <- ggplot(pred_dt[ihme_loc_id == iso],
                     aes(x = year_id, y = fit_ensemb)) +
            geom_line() +
            geom_point(data = input_dt[ihme_loc_id == iso],
                       aes(x = year_id, y = model_value,
                           shape = as.logical(is_outlier))) +
            scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 1)) +
            theme_linedraw() +
            labs(title = iso, x = "year_id", y = "", shape = "is_outlier")
        suppressWarnings( print(p + p2) )
        setTxtProgressBar(pb, i)
    }
    dev.off()
    return(invisible())
}


plot_stage1_covars <- function(stage1_config,
                               plot_pdf_path,
                               isos = "all",
                               pdf_width = 12,
                               pdf_height = 6) {
    pred_dt <- stage1_config$pred_dt[level == 3] # national estimates only
    if (isos[1] == "all") {
        isos <- sort(unique(pred_dt$ihme_loc_id))
    } else {
        isos <- sort(unique(pred_dt[ihme_loc_id %in% isos, ihme_loc_id]))
    }
    # use model metadata to build model matrix with transformed covariates
    if (class(stage1_config$covar_model) == "lmerMod") {
        fml_raw <- as.character(stage1_config$covar_model@call$formula)
        fml_raw <- gsub("\\+ \\(.*\\)", "", fml_raw) # rm random effects
    } else if (class(stage1_config$covar_model) == "lm") {
        fml_raw <- as.character(stage1_config$covar_model$call$formula)
    } else {
        stop("model must be of class lme4 or lm but is ", class(stage1_config$covar_model))
    }
    fml <- as.formula(paste("~ 0 +", fml_raw[3]))
    covars_wide <- as.data.table(model.matrix(fml, data = pred_dt))
    covars_wide <- cbind(covars_wide,
                         pred_dt[, .(location_id, year_id, ihme_loc_id,
                                     location_name, fit)])
    covars <- melt(covars_wide,
                   id.vars = c("location_id", "year_id", "ihme_loc_id",
                               "location_name", "fit"),
                   variable.name = "covar_name", value.name = "covar_value")
    
    # prep input data for plotting
    input_dt <- stage1_config$input_dt[level == 3]
    input_dt <- merge(input_dt[, .(ihme_loc_id, year_id, data_src, val)],
                      covars_wide,
                      by = c("ihme_loc_id", "year_id"),
                      all.x = TRUE)
    input_dt <- melt(input_dt,
                     id.vars = c("ihme_loc_id", "year_id", "data_src", "val",
                                 "location_id", "location_name", "fit"),
                     variable.name = "covar_name",
                     value.name = "covar_value")
    
    # make country plots
    pb <- txtProgressBar(min = 0, max = length(isos), style = 3, initial = 0)
    on.exit(close(pb))
    
    pdf(plot_pdf_path, width = pdf_width, height = pdf_height)
    for (i in seq_along(isos)) {
        iso <- isos[i]
        p1 <- ggplot(covars[ihme_loc_id == iso],
               aes(x = year_id, y = covar_value)) +
            geom_line(alpha = 0.6) +
            geom_point(size = 1) +
            facet_wrap(~covar_name, scales = "free_y") +
            labs(x = "year", y = "covariate value") +
            theme_light() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        p2 <- ggplot(covars[ihme_loc_id == iso],
               aes(x = covar_value, y = fit)) +
            geom_line(aes(color = "stage1 fit"), linewidth = 0.5) +
            geom_point(data = input_dt[ihme_loc_id == iso],
                       aes(y = val, color = "observed"), size = 1, alpha = 0.5) +
            facet_wrap(~covar_name, scales = "free") +
            scale_color_manual(
                values = c("stage1 fit" = "black", "observed" = "red")
            ) +
            labs(x = "covariate value", y = "value (transformed)", color = "") +
            theme_linedraw()
        
        p <- p1 + p2 +
            plot_annotation(
                title = paste0(iso, ": ", covars[ihme_loc_id == iso, location_name][1]),
            )
        suppressWarnings( print(p) )
        setTxtProgressBar(pb, i)
    }
    dev.off()
    return(invisible())
}







#
# Run selection ===============================================================
#


# names of covariates to test for inclusion in models
mh_covars <- c(
    "I(mcare_coverage_prop    )",
    "I(ANC4_coverage_prop     )",
    "I(SBA_coverage_prop      )",
    "I(IFD_coverage_prop      )",
    "I(PNC_coverage_prop      )"
)
fp_covars <- c(
    "I(contra_mod_prev_prop   )",
    "I(contra_demand_satisfied)"
)
healthsys_covars <- c(
    "log(LDI_pc)",
    "log(maternal_educ_yrs_pc)",
    "log(hospital_beds_per1000)",
    "I(sanitation_prop)",
    "I(haqi)",
    "log(physicians_pc)",
    "log(nurses_midwives_pc)",
    # population
    "log(TFR)",
    "log(pop/1000)",
    "log(repr_pop/pop)"
)
healthspend_covars <- c(
    "I( oop_totes/the_totes )",
    "I( ghes_totes/the_totes )",
    "log(the_totes/pop)"
)


alldata <- fread(file.path(PATHS$dirs$working, "stgpr_all_data.csv"))
setorder(alldata, model_group, value_id)

run_specs <- yaml::read_yaml(here::here("analysis", "1_retro", "domestic", "stgpr_run.yml"))

value_ids_vec <- names(run_specs)
final_formulas <- character(length(value_ids_vec))


for (i in seq_along(value_ids_vec)) {
    if (
        isFALSE(run_specs[[value_ids_vec[i]]]$stage1_select)
    ) {
        message("Skipping model selection for: ", value_ids_vec[i])
        next
    }
    cat("\n")
    tictoc::tic(msg = "* Done")
    message("***Running model selection for: ", value_ids_vec[i])
    
    
    covar_list <- c(mh_covars, fp_covars, healthsys_covars, healthspend_covars)
    if (value_ids_vec[i] %like% "dis2.1") {
        covar_list <- c(mh_covars, healthsys_covars, healthspend_covars)
    } else if (value_ids_vec[i] %like% "dis2.3") {
        covar_list <- c(fp_covars, healthsys_covars, healthspend_covars)
    } else if (value_ids_vec[i] == "private_other") {
        # not much data, so just keep very simple
        covar_list <- c("log(LDI_pc)", "I(haqi)", fp_covars)
    }
    
    #
    # generate data needed for fitting and prediction
    #
    message("* Data prep")
    res <- build_custom_stage1_lme4(
        working_dir = PATHS$dirs$working,
        full_data_file = "stgpr_all_data.csv",
        val_col = stgpr_config_base$val_col,
        val_transform = stgpr_config_base$val_transform,
        value_id = value_ids_vec[i],
        gbd_covar_names = stgpr_config_base$gbd_covar_names,,
        health_exp_dir = PATHS$dirs$covars,
        location_set_id = stgpr_config_base$location_set_id,
        year_id = seq(stgpr_config_base$year_start,
                      stgpr_config_base$year_end),
        age_group_id = stgpr_config_base$age_group_id,
        sex_id = stgpr_config_base$sex_id,
        gbd_release_id = stgpr_config_base$release_id,
        rebuild = "none", ## change to "covars" if needing to re-query covariates
        prep_only = TRUE
    )
    
    #
    # covariate selection
    #
    train_dt <- res$input_dt[is_outlier == 0]
    
    # lasso
    lasso <- lasso_selection(covar_list, train_dt, top_n = Inf)
    
    # aic
    ## (bug - can't run stepwise selection inside a function because it needs
    ##  to access train_dt in global env ... ?)
    aic_env <- new.env()
    aic <- with(aic_env, {
        model_full <- lm(paste("val ~", paste(covar_list, collapse = " + ")),
                         data = train_dt)
        aic_mods <- list()
        for (mthd in c("both", "backward", "forward"))
            aic_mods[[mthd]] <- step(model_full, direction = mthd, trace = FALSE)
        
        aic <- aic_selection(covar_list, aic_mods)
        return(aic)
    })
    rm(aic_env)
    
    ## compare models and select best based on out-of-sample performance
    mod_list <- list("lasso" = lasso, "aic" = aic)
    winner <- compare_model_oos(mod_list, train_dt, nrep = 100)
    final_covars <- mod_list[[winner]]$covars
    
    message("* [", winner, "] Selected ", length(final_covars) , " covariates:\n\t",
            paste(final_covars, collapse = " | "))
    
    if (value_ids_vec[i] %in% c("public_dis2.1", "public_dis2.3")) {
        if (! "log(LDI_pc)" %in% final_covars) {
            final_covars <- c(final_covars, "log(LDI_pc)")
            message("  - adding log(LDI_pc) for public spending models")
        }
        if (value_ids_vec[i] == "public_dis2.3" &
            ! "I(contra_mod_prev_prop)" %in% final_covars) {
            final_covars <- c(final_covars, "I(contra_mod_prev_prop)")
            message("  - adding I(contra_mod_prev_prop) for public dis2.3 model")
        }
    }
    
    # create final formula with covariates and random effects
    fml <- make_formula_for_lme4(final_covars,
                                 rand_slopes = FALSE,
                                 rand_int = TRUE)
    
    #
    # fit stage 1 to generate predictions/test hierarchical version
    #
    res <- fit_stage1_components(cfg = res, fml = fml)

    # save stage 1 formula
    message("* Saving stage1_formula.csv")
    out_file <- file.path(PATHS$dirs$working, "model_values",
                          res$value_id, "stage1_formula.csv")
    fwrite(data.table(value_id = res$value_id, formula = fml), out_file)
    
    tictoc::toc()
    flush.console()
    Sys.sleep(1) # wait a second to let the console buffer catch up
}

message("*** Done!")
