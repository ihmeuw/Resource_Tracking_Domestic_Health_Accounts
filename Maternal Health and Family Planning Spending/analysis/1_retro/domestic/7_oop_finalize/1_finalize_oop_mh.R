library(data.table)
library(arrow)
source(here::here("src", "r", "params.R"))


PATHS <- list(
    files = list(
        gbd_locs = get_path("int", "data", c("covars", "gbd_locations.csv")),
        ihme_unfpa_locs = get_path("int", "data", c("covars", "ihme_unfpa_locs.csv"))
    ),
    dirs = list(
        working = get_path("int", "data", c("domestic", "int")),
        covars = get_path("int", "data", c("covars")),
        output = get_path("int", "data", c("domestic", "int", "draws"))
    )
)

dir.create(PATHS$dirs$output, recursive = TRUE, showWarnings = FALSE)


#
# Auxiliary data
#

# Locations
GBD_LOCS <- fread(PATHS$files$gbd_locs) ## countries only
ihme_unfpa_locs <- fread(PATHS$files$ihme_unfpa_locs)
GBD_LOCS <- merge(GBD_LOCS,
                  unique(ihme_unfpa_locs[, .(ihme_loc_id, is_unfpa)]),
                  by = "ihme_loc_id",
                  all.x = TRUE)

# Raw input data
INPUT_DT <- fread(file.path(PATHS$dirs$working, "stgpr", "stgpr_all_data.csv"))


load_final_draws <- function(.value_id) {
    message("\n*** Loading final draws for ", .value_id, " ***")
    fp <- file.path(PATHS$dirs$working, "stgpr", "model_values",
                    .value_id, "final", "draws.feather")
    last_save <- file.mtime(fp)
    draws <- arrow::read_feather(fp)
    ndraw <- length(unique(draws$draw))
    message("    - Number of draws: ", ndraw)
    message("    - Last update: ", last_save)
    message("    Filtering to countries")
    draws <- merge(draws, GBD_LOCS[, .(location_id, level, is_unfpa)],
                   by = "location_id", all.x = TRUE)
    draws <- draws[level == 3, -"level"]

    return(draws)
}


oop_mh_value_ids <- c(
    "oop_antenatal",
    "oop_public_facility_delivery",
    "oop_public_facility_c_section",
    "oop_private_facility_delivery",
    "oop_private_facility_c_section",
    "oop_home_delivery",
    "oop_postnatal"
)


oop <- arrow::read_feather(file.path(PATHS$dirs$working, "stgpr",
                                     "denominators", "oop_totes.feather"))


#
# OOP spending on maternal health
#
draws <- list()
for (vid in oop_mh_value_ids) {
    draws[[vid]] <- load_final_draws(vid)
    draws[[vid]][, value_id := vid]
}
draws <- rbindlist(draws)
draws <- merge(draws,
               oop[, .(location_id, year_id, denominator = oop_totes)],
               by = c("location_id", "year_id"), all.x = TRUE)

draws[, denom_name := "oop_totes"]
draws[, denom_currency := "2023 USD"]
draws <- draws[is_unfpa == TRUE]


# Aggregate across service types to get total OOP-MH
draws[, oop_mh := val * denominator]
oop_mh <- draws[, .(oop_mh = sum(oop_mh)),
                by = .(location_id, year_id, sex_id, age_group_id,
                       measure_id, metric_id, draw, is_unfpa)]

oop_mh <- merge(oop_mh,
                oop[, .(location_id, year_id, denominator = oop_totes)],
                by = c("location_id", "year_id"),
                all.x = TRUE)
oop_mh[, val := oop_mh / denominator]
oop_mh[, value_id := "oop_mh"]


# Save
message("    Saving draws to ", PATHS$dirs$output)
stopifnot(oop_mh[is.na(denominator), .N] == 0)
stopifnot(draws[is.na(denominator), .N] == 0)

arrow::write_feather(oop_mh,
                     file.path(PATHS$dirs$output, "oop_mh_draws.feather"))

arrow::write_feather(draws,
                     file.path(PATHS$dirs$output, "oop_mh_by_service_draws.feather"))



if (interactive()) {
# plots
oop_mh <- merge(oop_mh, GBD_LOCS[, .(location_id, ihme_loc_id, super_region_name)],
                by = "location_id", all.x = TRUE)

x <- oop_mh[is_unfpa == TRUE,
            .(oop_mh = mean(oop_mh)),
            by = .(ihme_loc_id, year_id, super_region_name, denominator)]
x[ihme_loc_id %in% c("CHN", "IND", "BRA", "MEX", "ARG"),
  super_region_name := "High OOP Countries"]
x[ihme_loc_id %in% c("CHL", "URY"),
  super_region_name := "Latin America and Caribbean"]

x[, prop := oop_mh / denominator]
x[, .(
    val = median(prop),
    lb = quantile(prop, 0.25),
    ub = quantile(prop, 0.75)
), by = .(super_region_name)] |>
    ggplot(aes(x = super_region_name, y = val, fill = super_region_name)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.2, alpha = 0.5) +
    geom_text(aes(label = formatC(val, digits = 2)),
              vjust = -0.5, size = 3) +
    labs(
        title = "OOP-MH proportion of total OOP",
        subtitle = "Median and IQR by super region",
        fill = "",
    ) +
    theme_minimal() +
    theme(
        legend.position = "top",
        axis.text.x = element_blank()
    )

}
