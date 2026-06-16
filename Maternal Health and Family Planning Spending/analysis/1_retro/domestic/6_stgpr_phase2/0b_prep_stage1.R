#
# Prepare stage-1 estimates (already modeled) for use in ST-GPR backend
#
library(data.table)
library(arrow)
source(here::here("src", "r", "params.R"))
source(here::here("src", "r", "stgpr_utils.R"))


CONFIG <- list(
    dirs = list(
        covars = get_path("int", "data", "covars"),
        oop_mh = get_path("int", "data", c("domestic", "int", "oop_mh")),
        stgpr = get_path("int", "data", c("domestic", "int", "stgpr"))
    )
)

locs <- load_locations(cache_path = file.path(CONFIG$dirs$stgpr,
                                              "location_cache.feather"),
                       location_set_id = stgpr_config_base$location_set_id,
                       release_id = stgpr_config_base$release_id,
                       rebuild = FALSE)

locs[level > 3, lvl3_parent := vapply(
    strsplit(path_to_top_parent, ","),
    function(x) as.integer(x[4]), ## global, super-region, region, COUNTRY, ...
    integer(1)
)]

ihme_unfpa_locs <- fread(file.path(CONFIG$dirs$covars, "ihme_unfpa_locs.csv"))


#
# OOP-FP
#
# Future work: apply the same OOP-MH approach to OOP-FP and run it through ST-GPR.



#
# OOP-MH
#
stage1 <- fread(file.path(CONFIG$dirs$oop_mh, "oop_mh_estimates.csv"))
stage1[, value_id := paste0("oop_", value_id)]
stage1 <- merge(stage1, locs[, .(location_id, ihme_loc_id)],
                by = "ihme_loc_id", all.x = TRUE)


# create grid including all level 3 (countries) and sub-nationals
stage1_grid <- data.table::CJ(
    location_id = unique(locs[level >= 3, location_id]),
    year_id = sort(unique(stage1$year_id)),
    value_id = unique(stage1$value_id),
    age_group_id = 22,
    sex_id = 3
)
stage1_grid <- merge(stage1_grid,
                     locs[, .(location_id, ihme_loc_id, lvl3_parent)],
                     by = "location_id", all.x = TRUE)


stage1_grid <- merge(
    stage1_grid,
    stage1[, .(location_id, year_id, value_id, cv_custom_stage_1 = prop_oop_totes)],
    by = c("year_id", "location_id", "value_id"),
    all.x = TRUE
)
## ensure UNFPA countries all have estimates
stage1_grid <- merge(
    stage1_grid,
    ihme_unfpa_locs[is_unfpa == TRUE & is_gbd == TRUE, .(ihme_loc_id, flag = 1)],
    by = "ihme_loc_id",
    all.x = TRUE
)
stopifnot(
    stage1_grid[is.na(cv_custom_stage_1) & flag == 1, .N] == 0
)
## use mean for missing/we don't care about (non-unfpa locs and sub-nationals)
stage1_grid[, mean := mean(cv_custom_stage_1, na.rm = TRUE),
            by = .(year_id, value_id)]
stage1_grid[is.na(cv_custom_stage_1), cv_custom_stage_1 := mean]

stage1_grid[, c("lvl3_parent", "ihme_loc_id", "flag", "mean") := NULL]

# ## confirm that all sub-nats have an imputed value
# stopifnot( stage1_grid[is.na(cv_custom_stage_1), .N] == 0 )
# 
# stage1_grid[, c("impute", "lvl3_parent") := NULL]

# save stage1 predictions for each value_id 
for (vid in unique(stage1_grid$value_id)) {
    
    outdir <- file.path(CONFIG$dirs$stgpr, "model_values", vid)
    dir.create(outdir, showWarnings = FALSE)
    
    message("* Saving stage1 estimates for ", vid)
    tmp <- stage1_grid[value_id == vid, -"value_id"]
    fwrite(tmp, file.path(outdir, "prepped_custom_stage1.csv"))
    rm(tmp)
}
