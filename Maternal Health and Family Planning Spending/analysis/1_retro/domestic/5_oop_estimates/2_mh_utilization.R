#
# Finalize maternal health service indicators estimated via ST-GPR (either by us
#     or by another team at IHME)
#
library(data.table)
library(arrow)
library(ggplot2)
source(here::here("src", "r", "params.R"))
source(here::here("src", "r", "internals.R"))


PATHS <- list(
    files = list(
        gbd_locs = get_path("int", "data", c("covars", "gbd_locations.csv")),
        ihme_unfpa_locs = get_path("int", "data", c("covars", "ihme_unfpa_locs.csv")),
        figure_pdf = get_path("fin", "outputs", c("figures", "finalize_mh_utilization.pdf"))
    ),
    dirs = list(
        working = get_path("int", "data", c("domestic", "int")),
        covars = get_path("int", "data", c("covars")),
        draws = get_path("int", "data", c("domestic", "int", "draws"))
    )
)

dir.create(PATHS$dirs$draws, recursive = TRUE, showWarnings = FALSE)
if (!interactive()) pdf(PATHS$files$figure_pdf, width = 12, height = 10)


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
    message("Loading final draws for ", .value_id)
    fp <- file.path(PATHS$dirs$working, "stgpr", "model_values",
                    .value_id, "final", "draws.feather")
    last_save <- file.mtime(fp)
    draws <- arrow::read_feather(fp)
    ndraw <- length(unique(draws$draw))
    message("    - Number of draws: ", ndraw)
    message("    - Last update: ", last_save)
    
    message("    Filtering to countries")
    draws <- merge(draws,
                   GBD_LOCS[, .(location_id, ihme_loc_id, is_unfpa, level)],
                   by = "location_id", all.x = TRUE)
    draws <- draws[level == 3, -"level"]
    
    denom_name <- INPUT_DT[value_id == .value_id, denom_name[1]]
    message("    Merging on denominator: ", denom_name)
    denom <- arrow::read_feather(file.path(PATHS$dirs$working,
                                           "stgpr",
                                           "denominators",
                                           paste0(denom_name, ".feather")))
    denom <- denom[, c("location_id", "year_id", denom_name), with = FALSE]
    draws <- merge(draws, denom,
                   by = c("location_id", "year_id"), all.x = TRUE)
    
    draws <- draws[, c("location_id", "ihme_loc_id", "year_id", "draw",
                       "val", denom_name, "is_unfpa"),
                   with = FALSE]
    return(draws)
}


finalize_facility_est <- function(facility_dt, c_section_dt, cs_draw_level = FALSE) {
    #' Finalize facility estimate by splitting into delivery and c-section and
    #' converting from proportion to count.
    #' @param facility_dt data.table with columns location_id, year_id, draw,
    #'   total_births, and val. "val" records the proportion of births that
    #'   occur in the given facility type.
    #' @param c_section_dt data.table with columns location_id, year_id, draw,
    #'   and val, if passing in the draw-level estimates, or
    #'   location_id, year_id, and mean_value, if passing in the summarized estimates.
    #' @param cs_draw_level logical indicating whether the c-section data is at the
    #'   draw level. Default FALSE.
    #'
    if (cs_draw_level == TRUE) {
        if (! "draw" %in% names(c_section_dt))
            stop("cs_draw_level = TRUE requires 'draw' column in c_section_dt")
        val_col <- "val"
        by_cols <- c("location_id", "year_id", "draw")
        keep_cols <- c(by_cols, val_col)
    } else {
        val_col <- "mean_value"
        by_cols <- c("location_id", "year_id")
        keep_cols <- c(by_cols, val_col)
    }
    facility_dt <- merge(
        facility_dt,
        c_section_dt[, ..keep_cols],
        by = by_cols,
        all.x = TRUE
    )
    setnames(facility_dt, val_col, "prop_c_section")
    
    # proportion of facility births that are non-c-section
    facility_dt[, prop := val * (1 - prop_c_section)]
    facility_dt[, count := total_births * prop]
    
    # proportion of facility births that are c-section
    c_section_dt <- copy(facility_dt)
    c_section_dt[, prop := val * prop_c_section]
    c_section_dt[, count := total_births * prop]
    
    keep_cols <- c("location_id", "year_id", "draw", "count")
    return(list(delivery = facility_dt[, ..keep_cols],
                c_section = c_section_dt[, ..keep_cols]))
}



#
# MAIN ====
#
message("\n* Finalizing out-of-pocket spending on maternal health draws")

#
# 1. process antenatal and postnatal indicators
#

# load total births (live + still) for use below
# ASSUMPTION: ignore uncertainty in birth counts
lbirths <- fread(file.path(PATHS$dirs$covars, "live_births.csv"))
sbr <- fread(file.path(PATHS$dirs$covars, "still_birth_ratio.csv"))

births <- merge(
    lbirths[year_id > 1990,
            .(ihme_loc_id, year_id, live = mean_value * 1000)],
    sbr[, .(ihme_loc_id, year_id, sbr = mean_value)],
    by = c("ihme_loc_id", "year_id"),
    all.x = TRUE
)
births[, still := live * sbr] # sbr = still births / live births
births[, births := live + still]


# antenatal care
# - indicator produced by IHME is a percentage of women who had a live birth
# - use live births as denominator, since we can't get data on number of women
#   who gave birth.
#   This means we assume 1 live birth per women, ignoring
#   multiple-birth events (should be a relatively small proportion) or
#   multiple completed pregnancies in a year (likely a very small proportion)
message("** Antenatal care")
anc4 <- fread(file.path(PATHS$dirs$covars, "anc4_coverage.csv"))
anc4 <- merge(anc4,
              births[, .(year_id, ihme_loc_id, live)],
              by = c("ihme_loc_id", "year_id"), all.x = TRUE)
anc4 <- anc4[between(year_id, 2000, 2026),
             .(location_id,
               year_id,
               value_id = "antenatal",
               mean = mean_value * live,
               lower = lower_value * live,
               upper = upper_value * live)]


# post-natal care
# - indicator produced by IHME is a percentage of women who had a live birth
# - again, use live births as denominator, assuming 1 birth per women
message("** Postnatal care")
pnc <- fread(file.path(PATHS$dirs$covars, "pnc_coverage.csv"))
pnc <- merge(pnc,
             births[, .(year_id, ihme_loc_id, live)],
             by = c("ihme_loc_id", "year_id"), all.x = TRUE)

pnc <- pnc[between(year_id, 2000, 2026),
           .(location_id,
             year_id,
             value_id = "postnatal",
             mean = mean_value * live,
             lower = lower_value * live,
             upper = upper_value * live)]


#
# 2. process delivery indicators
#
message("** Delivery indicators")
public_deliv <- load_final_draws("public_facility_delivery")
private_deliv <- load_final_draws("private_facility_delivery")
home_deliv <- load_final_draws("home_delivery")

# load c-section rates estimated by IHME
# ASSUMPTION: we will ignore uncertainty in the proportion of c-sections
c_section <- fread(file.path(PATHS$dirs$covars, "csection_coverage.csv"))

# merge proportion c-section onto public and private deliveries, and
#  split into c-section and non-c-section deliveries
#  (assuming constant proportion across facility types)
#
pub <- finalize_facility_est(public_deliv, c_section)
pub$delivery[, value_id := "public_facility_delivery"]
pub$c_section[, value_id := "public_facility_c_section"]
pub <- rbindlist(pub)

priv <- finalize_facility_est(private_deliv, c_section)
priv$delivery[, value_id := "private_facility_delivery"]
priv$c_section[, value_id := "private_facility_c_section"]
priv <- rbindlist(priv)

# (assume 0 c-section proportion at homes)
home_deliv[, value_id := "home_delivery"]
home_deliv[, `:=`(
    count = val * total_births,
    value_id = "home_delivery",
    ihme_loc_id = NULL, total_births = NULL, val = NULL, is_unfpa = NULL
)]

## bind all delivery types and cast indicators from rows to columns
all_deliv <- rbind(pub, priv, home_deliv)
all_deliv <- all_deliv[year_id <= 2026]

deliv_summ <- all_deliv[, .(mean = mean(count),
                            lower = quantile(count, 0.025),
                            upper = quantile(count, 0.975)),
                        by = c("location_id", "year_id", "value_id")]



#
# 3. merge all maternal health indicators
#
# NOTE:
# - ANC and PNC estimates are not at the draw level, but delivery estimates are
# - so we ignore uncertainty in anc and pnc, but our uncertainty intervals become
#   approximate/not statistically rigorous as soon as we start merging and
#   adding these estimates together anyways
all_deliv <- dcast(all_deliv,
                   location_id + year_id + draw ~ value_id,
                   value.var = "count")

mh <- merge(all_deliv, anc4[, .(location_id, year_id, antenatal = mean)],
            by = c("location_id", "year_id"),
            all.x = TRUE)

mh <- merge(mh, pnc[, .(location_id, year_id, postnatal = mean)],
            by = c("location_id", "year_id"),
            all.x = TRUE)

mh <- melt(mh,
           id.vars = c("location_id", "year_id", "draw"),
           value.name = "num_users",
           variable.name = "value_id",
           variable.factor = FALSE)

#
# save draws
#
message("** Saving draws")
mh[, is_draw := fifelse(
    value_id %in% c("antenatal", "postnatal"), FALSE, TRUE
)]


# add location info
mh <- merge(mh, GBD_LOCS[, .(location_id, ihme_loc_id, is_unfpa)],
            by = "location_id", all.x = TRUE)


# TODO: note - may be easier/better to just get the ANC and PNC draws so that
#     we have actual utilization draws for all categories.
arrow::write_feather(mh,
                     file.path(PATHS$dirs$draws, "mh_utilization_draws.feather"))



#
# save summary
#
message("** Saving summary")
# add location info
summ <- rbind(
    deliv_summ, anc4, pnc
)


summ <- merge(summ, GBD_LOCS[, .(location_id, ihme_loc_id, is_unfpa)],
              by = "location_id", all.x = TRUE)
summ <- summ[is_unfpa == TRUE]


# confirm that same country-years are covered by all indicators
stopifnot(
    length( summ[is_unfpa == TRUE, .N, by = .(value_id)][, unique(N)] ) == 1
)

arrow::write_feather(summ,
                     file.path(PATHS$dirs$working,
                               "oop_mh", "mh_utilization_summ.feather"))
