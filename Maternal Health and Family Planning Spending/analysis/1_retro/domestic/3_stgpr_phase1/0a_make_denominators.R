#
# prep input data for st-gpr steps
#
library(data.table)
library(arrow)
source(here::here("src", "r", "params.R"))
source(here::here("src", "r", "stgpr_utils.R"))
source(here::here("analysis", "1_retro", "domestic", "stgpr_configs.R"))


CONFIG <- list(
    dirs = list(
        covars = get_path("int", "data", "covars"),
        working = get_path("int", "data", c("domestic", "int", "stgpr"))
    )
)
CONFIG$dirs$denoms <- file.path(CONFIG$dirs$working, "denominators")
CONFIG$dirs$figures <- file.path(CONFIG$dirs$denoms, "figures")

invisible(lapply(CONFIG$dirs, dir.create, recursive = TRUE, showWarnings = FALSE))


#
# auxiliary data
#
message("* Loading auxiliary data")
gbd_locs <- load_locations(cache_path = file.path(CONFIG$dirs$working,
                                              "location_cache.feather"),
                           location_set_id = stgpr_config_base$location_set_id,
                           release_id = stgpr_config_base$release_id,
                           rebuild = FALSE)

pops <- load_population(cache_path = file.path(CONFIG$dirs$working,
                                               "population_cache.feather"),
                        location_set_id = stgpr_config_base$location_set_id,
                        year_id = seq(stgpr_config_base$year_start,
                                      stgpr_config_base$year_end),
                        release_id = stgpr_config_base$release_id,
                        rebuild = FALSE)


unfpa_locs <- fread(get_path("int", "data", c("covars", "ihme_unfpa_locs.csv")))

plot_isos <- sort(unique(unfpa_locs[is_unfpa == TRUE & is_gbd == TRUE, iso3]))



#
# government health spending
#
message("* Government health spending")
ghes <- fread(get_path("int", "data", c("covars", "ghes_totes.csv")))
setorder(ghes, year_id)

## create smoothed counterfactual for 2020-2022,
##     which are strongly affected by COVID
ghes[, ghes_no_covid := fifelse(year_id %in% 2020:2022, NA_real_, ghes_totes)]

smooth_fn <- function(SD) {
    xp <- SD$year_id
    yp <- SD$ghes_no_covid
    x <- xp[!is.na(yp)]
    y <- yp[!is.na(yp)]
    mod <- mgcv::gam(log(y) ~ s(x, bs = "cs"))
    exp(predict(mod, newdata = data.table(x = xp)))
}

ghes[, ghes_smooth := smooth_fn(.SD),
     .SDcols = c("year_id", "ghes_no_covid"),
     by = ihme_loc_id] 


## use smoothed value for COVID years
##   (2020-2022, and burn-in/out of 1 year to avoid sharp transitions)
ghes[, ghes_fin := fifelse(year_id >= 2020 & year_id <= 2022,
                           ghes_smooth,
                           ghes_totes)]
ghes[, ghes_fin := fifelse(year_id == 2019 | year_id == 2023,
                           (2/3) * ghes_smooth + (1/3) * ghes_totes,
                           ghes_fin)]

ghes[, is_smoothed := fifelse(year_id >= 2019 & year_id <= 2023, TRUE, FALSE)]



pdf(file.path(CONFIG$dirs$figures, "ghes.pdf"), width = 12, height = 8)
for (iso in plot_isos) {
    p <- ggplot(ghes[ihme_loc_id == iso],
           aes(x = year_id)) +
        geom_line(aes(y = ghes_totes, color = "raw")) +
        geom_line(aes(y = ghes_smooth, color = "smoothed"), linetype = "dashed") +
        geom_line(aes(y = ghes_fin, color = "final"), alpha = 0.5) +
        scale_color_manual(values = c("raw" = "red", "smoothed" = "blue", "final" = "green")) +
        labs(
            title = iso
        ) +
        theme_bw()
    print(p)
}
invisible(capture.output(dev.off()))



ghes[, c("ghes_totes", "ghes_smooth", "ghes_no_covid", "lower", "upper") := NULL]
setnames(ghes, "ghes_fin", "ghes_totes")

arrow::write_feather(ghes,
                     file.path(CONFIG$dirs$denoms, "ghes_totes.feather"))



#
# out-of-pocket spending on health
#
message("* Out-of-pocket spending")
oop <- fread(get_path("int", "data", c("covars", "oop_totes.csv")))
oop[, `:=`(
    is_smoothed = FALSE,
    lower = NULL,
    upper = NULL
)]

pdf(file.path(CONFIG$dirs$figures, "oop.pdf"), width = 12, height = 8)
for (iso in plot_isos) {
    p <- ggplot(oop[ihme_loc_id == iso],
           aes(x = year_id)) +
        geom_line(aes(y = oop_totes, color = "raw")) +
        labs(
            title = iso
        ) +
        theme_bw()
    print(p)
}
invisible(capture.output(dev.off()))

arrow::write_feather(oop,
                     file.path(CONFIG$dirs$denoms, "oop_totes.feather"))


#
# Reproductive population
#
message("* Reproductive population")
pops <- merge(pops,
              gbd_locs[, .(location_id, ihme_loc_id, level)],
              by = "location_id", all.x = TRUE)
repr_pops <- pops[
        level == 3 &         ## countries only (not aggregates or subnational)
        age_group_id == 24 & ## 15-49 years old
        sex_id == 2          ## female
]

repr_pops[, c("level", "age_group_id", "sex_id") := NULL]
setnames(repr_pops, "population", "repr_pops")

pdf(file.path(CONFIG$dirs$figures, "repr_pops.pdf"), width = 12, height = 8)
for (iso in plot_isos) {
    p <- ggplot(repr_pops[ihme_loc_id == iso],
           aes(x = year_id)) +
        geom_line(aes(y = repr_pops)) +
        labs(
            title = iso
        ) +
        theme_bw()
    print(p)
}
invisible(capture.output(dev.off()))

arrow::write_feather(repr_pops,
                     file.path(CONFIG$dirs$denoms, "repr_pops.feather"))




#
# Total births (live + still)
#
message("* Total births")

# load live births in 1000s 
lb <- fread(file.path(CONFIG$dirs$covars, "live_births.csv"))
lb <- lb[year_id >= 1990, .(ihme_loc_id, year_id, live = mean_value * 1000)]
 
# load stillbirth ratio (stillbirths / livebirths)
sbr <- fread(file.path(CONFIG$dirs$covars, "still_birth_ratio.csv"))
sbr <- sbr[year_id >= 1990, .(ihme_loc_id, year_id, sbr = mean_value)]

# combine and compute still births, and total births
b <- merge(lb, sbr, by = c("ihme_loc_id", "year_id"), all = TRUE)
stopifnot(
    b[is.na(live), .N] == 0, b[is.na(sbr), .N] == 0
)

b[, still := live * sbr]
b[, total_births := live + still]

b_plot <- melt(b, id.vars = c("ihme_loc_id", "year_id"))

pdf(file.path(CONFIG$dirs$figures, "total_births.pdf"), width = 12, height = 8)
for (iso in plot_isos) {
    p <- ggplot(b_plot[ihme_loc_id == iso],
           aes(x = year_id, y = value)) +
        geom_line() +
        scale_y_continuous(labels = scales::comma) +
        facet_wrap(~ variable, scales = "free_y") +
        labs(
            title = iso
        ) +
        theme_bw()
    print(p)
}
invisible(capture.output(dev.off()))

# finalize
b <- merge(b, gbd_locs[, .(ihme_loc_id, location_id)], by = "ihme_loc_id")
b <- b[, .(year_id, location_id, ihme_loc_id, total_births)]
arrow::write_feather(b,
                     file.path(CONFIG$dirs$denoms, "total_births.feather"))
