#
# ST-GPR pipeline and modeling configurations
#
# This file specifies the "base" config which contains key constants shared
# across this pipeline.
#
# Below it also specifies individual model configs where parameters may vary
# based on the variable being modeled (the "model_value" in this pipeline's
# terminology).
#
# Config templates/pseudo-configs are specified as S4 classes which allows for
# consistency across the model configs and for easier validation of the config
# before it is used in the pipeline.
# The template and config helper functions have evolved organically with the
# needs of this pipeline and thus are kept in this file, coupled with the
# implemented model configs, instead of in a separate file.
# Hidden variables (those starting with a period) are not meant to be accessed
# directly by the pipeline. The only objects exposed to the pipeline are:
#   stgpr_config_base
#   load_current_config()
#

# note:
# st_lambda (time)
#   smoother: 0.04 , less smooth: 0.5   
#
# st_zeta (space)
#   smoother: 0.1, less smooth: 0.001
#
# gpr_scale
#   smoother: 20, less smooth: 5
#
# st_omega (age): n/a
#

# base config used across this pipeline
stgpr_config_base <- list(
    #
    # data params
    #
    year_start = 2000,
    year_end = 2026,      # year end is fixed for each stgpr_release_id
    age_group_id = 22,    # all ages
    sex_id = 3,           # all sexes
    location_set_id = 22, # gbd computation location hierarchy
    # actual release id used for data pipelines
    release_id = 34,
    # release id used for st-gpr modeling
    # (st-gpr has to be run with the latest releast_id)
    stgpr_release_id = 34,
    #
    # model params
    #
    loess_opts = list(span = 0.5, degree = 2),
    # note - set REML to FALSE to use maximum likelihood instead of restricted
    # ML. In this scenario, we don't really need restricted ML, which doesn't
    # typically converge for this simple model with these data, anyways.
    # https://stats.stackexchange.com/questions/48671/what-is-restricted-maximum-likelihood-and-when-should-it-be-used
    ensemb_opts = list(
        model_formula = "val ~ 1 + (1 | level_1/level_2/level_3)",
        weights = c("main" = 1/2, "simple" = 1/2),
        REML = FALSE
    ),
    #
    # other required params
    #
    modelable_entity_id = 24920, # this is the generic ME id
    #
    # data pipeline params
    #
    val_col = "model_value",
    val_transform = "logit",
    val_measure = "proportion",
    val_metric = "rate",
    # covariates that need to be queried from the GBD database
    gbd_covar_names = c("LDI_pc",
                        "GDPpc_id_b2010",
                        "maternal_educ_yrs_pc",
                        "hospital_beds_per1000",
                        "sanitation_prop",
                        "haqi",
                        "physicians_pc",
                        "nurses_midwives_pc",
                        "sdi",
                        "mx_warterror_10years",
                        "mx_famine",
                        "stillbirth_livebirth_ratio_28wks",
                        "SBA_coverage_prop",
                        "mcare_coverage_prop",
                        "ANC4_coverage_prop",
                        "PNC_coverage_prop",
                        "IFD_coverage_prop",
                        "TFR",
                        "contra_mod_prev_prop",
                        "contra_demand_satisfied")
)


#
# Model config templates ======================================================
#
# Templates specify the required structure of the model configurations and allow
# for easier validation of the config before running the model.


.crossValidTemplate <- setClass("crossValidTemplate", representation(
    holdouts = "numeric",
    cv_st_lambda = "character",
    cv_st_omega = "character",
    cv_st_zeta = "character",
    cv_gpr_scale = "character",
    # if FALSE, these params get tossed. if TRUE, they replace the main params
    do_cross_validation = "integer"
))

.stgprConfigTemplate <- setClass("stgprConfigTemplate", representation(
    st_lambda = "character",     # character so can be comma separated list if density cutoffs are used
    st_omega = "character",
    st_zeta = "character",
    gpr_scale = "character",
    gpr_amp_cutoff = "character",
    gpr_amp_factor = "character",
    gpr_draws = "character",
    add_nsv = "logical",
    predict_re = "logical",
    smooth_stage1 = "logical",
    ensemb_stage1 = "logical",
    custom_density_cutoffs = "character", # comma seperated list
    cross_validation = "crossValidTemplate",
    description = "character",
    note = "character",
    linked_runs = "character"
))


.check_config <- function(config_list) {
    #' Check that the config list is a valid config
    chk <- .stgprConfigTemplate(
        st_lambda = config_list$st_lambda,
        st_omega = config_list$st_omega,
        st_zeta = config_list$st_zeta,
        gpr_scale = config_list$gpr_scale,
        gpr_amp_cutoff = config_list$gpr_amp_cutoff,
        gpr_amp_factor = config_list$gpr_amp_factor,
        gpr_draws = config_list$gpr_draws,
        add_nsv = config_list$add_nsv,
        predict_re = config_list$predict_re,
        smooth_stage1 = config_list$smooth_stage1,
        ensemb_stage1 = config_list$ensemb_stage1,
        custom_density_cutoffs = config_list$custom_density_cutoffs,
        cross_validation = .crossValidTemplate(
            holdouts = as.numeric(config_list$cross_validation$holdouts),
            cv_st_lambda = config_list$cross_validation$cv_st_lambda,
            cv_st_omega = config_list$cross_validation$cv_st_omega,
            cv_st_zeta = config_list$cross_validation$cv_st_zeta,
            cv_gpr_scale = config_list$cross_validation$cv_gpr_scale,
            do_cross_validation = as.integer(config_list$cross_validation$do_cross_validation)
        ),
        description = config_list$description,
        note = config_list$note,
        linked_runs = config_list$linked_runs
    )
    return(invisible())
}


#' List holds all model configs
.CURRENT_CONFIGS <- list()


#' Helper function to load a config by name
load_current_config <- function(value_id) {
    cfg <- .CURRENT_CONFIGS[[value_id]]
    if (is.null(cfg)) {
        stop("no config found for '", value_id, "'")
    }
    tryCatch(.check_config(cfg),
             error = function(e) {
                 stop("error in current config: ", e$message, call. = FALSE)
             })
    # unnest the cross_validation
    cfg <- c(cfg, cfg$cross_validation)
    cfg$cross_validation <- NULL
    return(cfg)
}



#
# Model configs ===============================================================
#


# HEALTH SPENDING CONFIGS ====
.CURRENT_CONFIGS$public_dis2.1 <- {list(
    st_lambda = "0.1",
    st_omega = "1",
    st_zeta = "0.05",
    gpr_scale = "10",
    gpr_amp_cutoff = NA_character_,
    gpr_amp_factor = "3",
    gpr_draws = "1000",
    add_nsv = TRUE,
    predict_re = TRUE,
    smooth_stage1 = TRUE,
    ensemb_stage1 = TRUE,
    custom_density_cutoffs = "",
    cross_validation = list(
        holdouts = "0",
        cv_st_lambda = "",
        cv_st_omega = "",
        cv_st_zeta = "",
        cv_gpr_scale = "",
        do_cross_validation = FALSE
    ),
    description = "Proportion of public health expenditures on maternal health",
    note = "Final model, 1000 draws.",
    linked_runs = "217317,217351,217402,217453,217584,217759,217765,217779,217788,217793,217810,217918,218987,219488,219714,219725,219889"
)}


.CURRENT_CONFIGS$public_dis2.3 <- {list(
    st_lambda = "0.1",
    st_omega = "1",
    st_zeta = "0.05",
    gpr_scale = "10",
    gpr_amp_cutoff = NA_character_,
    gpr_amp_factor = "3",
    gpr_draws = "1000",
    add_nsv = TRUE,
    predict_re = TRUE,
    smooth_stage1 = TRUE,
    ensemb_stage1 = TRUE,
    custom_density_cutoffs = "",
    cross_validation = list(
        holdouts = 0,
        cv_st_lambda = "",
        cv_st_omega = "",
        cv_st_zeta = "",
        cv_gpr_scale = "",
        do_cross_validation = FALSE
    ),
    description = "Proportion of public health expenditures on family planning",
    note = "Final model, 1000 draws.",
    linked_runs = "217919,218917,218951,218988,219715,219726,219890"
)}



# CONTRACEPTIVE USE CONFIGS ====


.CURRENT_CONFIGS$contracpetive_use <- {list(
    st_lambda = "0.1",
    st_omega = "0.5",
    st_zeta = "0.01",
    gpr_scale = "10",
    gpr_amp_cutoff = NA_character_,
    gpr_amp_factor = "2",
    gpr_draws = "1000",
    add_nsv = TRUE,
    predict_re = TRUE,
    smooth_stage1 = TRUE,
    ensemb_stage1 = TRUE,
    custom_density_cutoffs = "",
    cross_validation = list(
        holdouts = "0",
        cv_st_lambda = "",       #
        cv_st_omega = "",        #
        cv_st_zeta = "",         #
        cv_gpr_scale = "",       #
        do_cross_validation = FALSE
    ),
    description = "Proportion of women of reproductive age using contraceptive",
    note = "Final model, 1000 draws.",
    linked_runs = "217811,217920,218962,219716,219727"
)}




# use the same config for all contraceptive types
.contraceptive_types <- c("private_condom", "private_implant", "private_injectable",
                          "private_iud", "private_other", "private_pill",
                          "private_sterilization")

for (ct in .contraceptive_types) {
    .CURRENT_CONFIGS[[ct]] <- .CURRENT_CONFIGS$contracpetive_use
    .CURRENT_CONFIGS[[ct]]$description <- paste(.CURRENT_CONFIGS[[ct]]$description,
                                                ct)
}
rm(ct, .contraceptive_types)



# MATERNAL HEALTH UTILIZATION CONFIGS ====


.CURRENT_CONFIGS$maternal_health <- {list(
    st_lambda = "0.1",
    st_omega = "0.5",
    st_zeta = "0.01",
    gpr_scale = "10",
    gpr_amp_cutoff = NA_character_,
    gpr_amp_factor = "2",
    gpr_draws = "1000",
    add_nsv = TRUE,
    predict_re = TRUE,
    smooth_stage1 = TRUE,
    ensemb_stage1 = TRUE,
    custom_density_cutoffs = "",
    cross_validation = list(
        holdouts = "0",
        cv_st_lambda = "",
        cv_st_omega = "",
        cv_st_zeta = "",
        cv_gpr_scale = "",
        do_cross_validation = FALSE
    ),
    description = "Proportion of total births for maternal health indicator:",
    note = "Final model, 1000 draws.",
    linked_runs = "220455"
)}




# use the same config for all maternal health services
.mh_types <- c("public_facility_delivery", "private_facility_delivery",
               "home_delivery", "c_section")

for (mh in .mh_types) {
    .CURRENT_CONFIGS[[mh]] <- .CURRENT_CONFIGS$maternal_health
    .CURRENT_CONFIGS[[mh]]$description <- paste(.CURRENT_CONFIGS[[mh]]$description,
                                                mh)
}
rm(mh, .mh_types)



# OOP MATERNAL HEALTH CONFIGS ====

.CURRENT_CONFIGS$oop_maternal <- {list(
    st_lambda = "0.1",
    st_omega = "0.5",
    st_zeta = "0.01",
    gpr_scale = "10",
    gpr_amp_cutoff = NA_character_,
    gpr_amp_factor = "2",
    gpr_draws = "1000",
    add_nsv = TRUE,
    predict_re = FALSE,    # n/a
    smooth_stage1 = FALSE, # n/a
    ensemb_stage1 = FALSE, # n/a
    custom_density_cutoffs = "",
    cross_validation = list(
        holdouts = "0",
        cv_st_lambda = "",
        cv_st_omega = "",
        cv_st_zeta = "",
        cv_gpr_scale = "",
        do_cross_validation = FALSE
    ),
    description = "Proportion of total OOP spending for maternal health service:",
    note = "Final model, 1000 draws.",
    linked_runs = "221308"
)}



# use the same config for all maternal health categories
.types <- c(
    "oop_antenatal",
    "oop_public_facility_delivery",
    "oop_public_facility_c_section",
    "oop_private_facility_delivery",
    "oop_private_facility_c_section",
    "oop_home_delivery",
    "oop_postnatal"
)

for (ty in .types) {
    .CURRENT_CONFIGS[[ty]] <- .CURRENT_CONFIGS$oop_maternal
    .CURRENT_CONFIGS[[ty]]$description <- paste(.CURRENT_CONFIGS[[ty]]$description,
                                                ty)
}
rm(ty, .types)
