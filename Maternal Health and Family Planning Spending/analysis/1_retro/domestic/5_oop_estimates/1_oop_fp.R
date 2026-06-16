#
# Finalize out-of-pocket spending on family planning by combining estimates of
# contraceptive use with estimates of OOP costs per-user
#
library(data.table)
library(arrow)
source(here::here("src", "r", "params.R"))
source(here::here("src", "r", "internals.R"))


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
    denom <- denom[, .(location_id, year_id, get(denom_name))]
    setnames(denom, "V3", denom_name)
    draws <- merge(draws, denom,
                   by = c("location_id", "year_id"), all.x = TRUE)
    
    return(draws)
}


load_prep_rhsc <- function() {
    message("*** Loading RHSC data ***")
    rhsc <- fread(file.path(PATHS$dirs$working, "oop_fp", "rhsc_imputed.csv"))
    rhsc[, imputed := NULL]
    rhsc_cost <- rhsc[var %like% "cost_"]
    rhsc_prop <- rhsc[var %like% "prop_"]
    
    # currency conversion - convert all years of data to same currency-year
    rhsc_cost[, curr_year := year_id] ## consumed by currency_convert()
    rhsc_cost <- currency_convert(
        rhsc_cost,
        # what
        col.loc = "ihme_loc_id", col.value = "value",
        # from
        currency = "USD", col.currency.year = "curr_year",
        # to
        base.unit = "USD", base.year = 2023
    )
    
    rhsc <- rbind(rhsc_cost, rhsc_prop)
    
    # create complete grid of country-years
    grid <- data.table::CJ(
        year_id = 2000:2024,
        ihme_loc_id = unique(rhsc$ihme_loc_id),
        method = unique(rhsc$method),
        var = unique(rhsc$var)
    )
    grid <- merge(grid, rhsc,
                  by = c("year_id", "ihme_loc_id", "method", "var"),
                  all.x = TRUE)
    
    ## method 1: average values across 2018 and 2019
    grid[, value_mean := mean(value, na.rm = TRUE),
         by = .(ihme_loc_id, method, var)]
    
    ## method 2: backfill and forward fill
    grid[, val_2018 := value[which(year_id == 2018)],
         by = .(ihme_loc_id, method, var)]
    grid[, val_2019 := value[which(year_id == 2019)],
         by = .(ihme_loc_id, method, var)]
    
    grid[, value_fill := value]
    grid[is.na(value_fill) & year_id < 2018, value_fill := val_2018]
    grid[is.na(value_fill) & year_id > 2019, value_fill := val_2019]
    grid[, c("val_2018", "val_2019") := NULL]
    
    stopifnot(
        grid[var %like% "prop", max(value_mean, na.rm = TRUE)] <= 1,
        grid[var %like% "prop", max(value_fill, na.rm = TRUE)] <= 1
    )
    
    grid[, units := "2023 USD"]
    return(grid)
}


.rhsc_calculation <- function(contra_dt, rhsc_dt, users_col) {
    contra_dt[, method := gsub("private_", "", value_id)]
    contra_dt <- merge(contra_dt, rhsc_dt,
                       by = c("ihme_loc_id", "year_id", "method"),
                       all.x = TRUE)
    contra_dt[, est_users := get(users_col)]
    
    # scale estimated users by proportions of private users that are subsidized
    #  vs. non-subsidized
    contra_dt[, est_users_sub := est_users * prop_priv_sub_user]
    contra_dt[, est_users_nonsub := est_users * prop_priv_nonsub_user]
    
    # multiply by respective costs-per-user
    contra_dt[, cost_priv_sub := est_users_sub * cost_per_user_priv_sub]
    contra_dt[, cost_priv_nonsub := est_users_nonsub * cost_per_user_priv_nonsub]
    
    # sum costs
    contra_dt[, oop_spending := cost_priv_sub + cost_priv_nonsub]
    return(contra_dt)
}

compute_oop_spending <- function(.value_id, rhsc_dt) {
    # ======================================================================== #
    #' Compute out-of-pocket spending on the given contraceptive
    #' 
    #' We have estimated the proportion of women using contraceptive methods from
    #' private sources. From RHSC, we have estimates of the private-sector
    #' costs-per-user of these contraceptives by method and by whether the user
    #' is subsidized (by the public sector) or not, and also estimates of the
    #' proportion of private users that are subsidized vs. non-subsidized.
    #' 
    #' Thus, we multiply the estimated proportion of women by the population to
    #' get an estimate of the number of private users of each contraceptive.
    #' Then we disaggregate this total number of users into private subsidized
    #' and private non-subsidized so we can multiply by the approporiate cost-
    #' per-user.
    #' 
    #' This is done at the draw level for each value_id. Summing across all
    #' value_ids will give the total out-of-pocket spending on family planning.
    contra_dt <- load_final_draws(.value_id)
    contra_dt[, value_id := .value_id]
    
    message("    Computing out-of-pocket spending")
    # convert estimate from proportion of women of reproductive age to count
    contra_dt[, est_users := val * repr_pops]
    
    # merge on rhsc data
    contra_dt <- .rhsc_calculation(contra_dt, rhsc_dt, "est_users")
    
    # finalize
    o <- contra_dt[, .(year_id, location_id, ihme_loc_id, method, draw,
                       oop_users = est_users, oop_spending, repr_pops)]
    return(o)
}


#
# MAIN ====
#

#
# load rhsc cost data (data exists for years 2018 and 2019)
#
rhsc <- load_prep_rhsc()
## 2 potential methods of computing cost:
##   - avg of 2018 and 2019, or backfill/forward fill
rhsc_mean <- dcast(rhsc[, .(year_id, ihme_loc_id, method, var, value_mean)],
                   year_id + ihme_loc_id + method ~ var,
                   value.var = "value_mean")
rhsc_fill <- dcast(rhsc[, .(year_id, ihme_loc_id, method, var, value_fill)],
                   year_id + ihme_loc_id + method ~ var,
                   value.var = "value_fill")


#
# apply OOP-FP calculation at the draw-level, by method
#
val_ids <- unique(INPUT_DT[model_group == "contraceptive_use", value_id])

data_list <- list()
for (val_id in val_ids) {
    message("*** Computing out-of-pocket spending for ", val_id, " ***")
    data_list[[val_id]] <- compute_oop_spending(val_id, rhsc_mean)
}

oop_fp <- rbindlist(data_list)
oop_fp[, draw := gsub("draw_", "", draw)]
oop_fp <- oop_fp[year_id <= 2026]




# save draws at the contraceptive method level
arrow::write_feather(oop_fp,
                     file.path(PATHS$dirs$output, "oop_fp_bymethod_draws.feather"))


#
# aggregate across contraceptive method and save final draws 
#
oop_fp_fin <- oop_fp[, .(val = sum(oop_spending)),
                     by = .(location_id, year_id, draw)]

## add total OOP spending on health as denominator
oop_totes <- fread(file.path(PATHS$dirs$covars, "oop_totes.csv"))
oop_fp_fin <- merge(oop_fp_fin,
                    oop_totes[, .(location_id, year_id,
                                  denominator = oop_totes)],
                    by = c("location_id", "year_id"),
                    all.x = TRUE)
oop_fp_fin[, val := val / denominator]

## add id information for consistency across project
oop_fp_fin[, `:=` (
    sex_id = 3,        # both sexes
    age_group_id = 22, # all ages
    measure_id = 18,    # proportion 
    metric_id = 3,     # rate
    denom_name = "oop_totes",
    denom_currency = "2023 USD"
)]


message("\nSaving final draws to ", PATHS$dirs$output)
arrow::write_feather(oop_fp_fin,
                     file.path(PATHS$dirs$output, "oop_fp_draws.feather"))



# save version of raw data that undergoes same transformation
raw_dat <- INPUT_DT[model_group == "contraceptive_use",
                    .(year_id, location_id, ihme_loc_id,
                      value_id, num_users = value, is_outlier)]
raw_dat <- .rhsc_calculation(raw_dat, rhsc_mean, users_col = "num_users")
fwrite(raw_dat, file.path(PATHS$dirs$working, "oop_fp_raw_data_transformed.csv"))




#
# Explore Results =============================================================
#
if (interactive()) {


#
# compare price/costing methods
#
a <- oop_fp[, .(tot = sum(oop_spending)), by = .(ihme_loc_id, year_id, draw)]
a <- a[, .(est_a = mean(tot)), by = .(year_id, ihme_loc_id)]

b <- oop_fp2[, .(tot = sum(oop_spending)), by = .(ihme_loc_id, year_id, draw)]
b <- b[, .(est_b = mean(tot)), by = .(year_id, ihme_loc_id)]

ab <- merge(a, b, by = c("year_id", "ihme_loc_id"))

ab[, .(est_a = sum(est_a), est_b = sum(est_b)),
   by = .(year_id)] |>
    ggplot(aes(x = year_id)) +
    geom_line(aes(y = est_a, color = "Average Cost")) +
    geom_line(aes(y = est_b, color = "Step Cost")) +
    labs(title = "Comparison of OOP-FP estimates",
         y = "Total OOP-FP spending",
         x = "Year", color = "") +
    theme_minimal() +
    theme(legend.position = "top")
    





#
# compare results with total OOP health spending to ensure consistency
#
summ <- oop_fp_fin[, .(
    mean = mean(val, na.rm = TRUE),
    lower = quantile(val, 0.025, na.rm = TRUE),
    upper = quantile(val, 0.975, na.rm = TRUE)
), by = .(year_id, location_id, denominator)] # denom is constant by loc-year
summ <- merge(summ,
              GBD_LOCS[, .(location_id, ihme_loc_id, location_name)],
              by = "location_id",
              all.x = TRUE)

summ[mean > 1, unique(ihme_loc_id)]
summary(summ$mean)
summ[mean > 0.4, unique(ihme_loc_id)] ## GRL, ZWE only

summ[ihme_loc_id == "ZWE" & year_id > 2018] |>
    ggplot(aes(x = year_id)) +
    geom_line(aes(y = mean * denominator), color = "blue") +
    geom_line(aes(y = denominator), color = "red")




#
# country plots
#
tot <- oop_fp[, .(total = sum(oop_spending, na.rm = TRUE)),
              by = .(ihme_loc_id, year_id, draw)]
tot <- tot[, .(
    mean = mean(total, na.rm = TRUE),
    lower = quantile(total, 0.025, na.rm = TRUE),
    upper = quantile(total, 0.975, na.rm = TRUE)
), by = .(year_id, ihme_loc_id)]

tot[, method := "1_oop_family_planning"]
msumm <- oop_fp[, .(
    mean = mean(oop_spending, na.rm = TRUE),
    lower = quantile(oop_spending, 0.025, na.rm = TRUE),
    upper = quantile(oop_spending, 0.975, na.rm = TRUE)
), by = .(year_id, ihme_loc_id, method)]

plot_dt <- rbind(msumm, tot)
plot_dt <- rbind(
    plot_dt,
    oop_totes[between(year_id, min(plot_dt$year_id), max(plot_dt$year_id)),
              .(year_id, ihme_loc_id,
                method = "2_oop_total_health_spending",
                mean = oop_totes, lower, upper)]
)
plot_dt <- plot_dt[year_id <= 2022]


pdf_path <- file.path(PATHS$dirs$working, "oop_fp", "figures", "country_results.pdf")
dir.create(dirname(pdf_path), showWarnings = FALSE, recursive = TRUE)

plot_isos <- sort(unique(plot_dt$ihme_loc_id))
pb <- txtProgressBar(min = 0, max = length(plot_isos), style = 3)

pdf(pdf_path, width = 15, height = 9)
for (i in seq_along(plot_isos)) {
    iso <- plot_isos[i]
    setTxtProgressBar(pb, i)
    p <- plot_dt[ihme_loc_id == iso] |>
        ggplot(aes(x = year_id, y = mean)) +
        geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25) +
        geom_line() +
        geom_point(data = raw_dat[ihme_loc_id == iso,],
                   aes(x = year_id, y = oop_spending,
                       shape = as.logical(is_outlier))) +
        scale_y_continuous(labels = \(y) y / 1e3) +
        scale_shape_manual(
            values = c("TRUE" = 1, "FALSE" = 16),
        ) +
        facet_wrap(~method, scales = "free_y") +
        labs(
            title = paste0(iso, ": out-of-pocket spending"),
            x = "year", y = "Thousands of 2023 USD",
            shape = "is_outlier"
        ) +
        theme_linedraw()
    print(p)
}
close(pb)
dev.off()

}
