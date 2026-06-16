#
# Automated outlier detection and variance computation 
#
# The method looks for large deviations from a smooth time trend.
# The time trend is fit using a GAM which allows for a flexible, non-linear
# path of the modeled variable over time.
# Data points which deviate significantly from this flexible time trend are
# deemed to be unlikely and are flagged as potential outliers.
#
# For each country:
# 1. The time trend is first fit within the country, and any data points
#    that are greater than 1.5 standard deviations from the time trend
#    are flagged (i.e., the standardized residual is over 1.5 sigmas).
#    Also, any data points that have been hand-marked as outliers are flagged in
#    the data.
# 2. Then the time trend is re-fit using data from the country's entire region,
#    once with the inclusion of data that was flagged by step 1 and once without.
#    Step 2 only flags new data points if they are greater than 2.5 standard
#    deviations from the time trend in both model fits.
# 3. The model is re-fit to the data a final time with all outliers dropped. This
#    final smooth trend is considered an estimate of the latent trend, and the
#    raw residuals are calculated to be used as data variance. (If a trend is not
#    able to be computed due to less than 3 data points, the variance is imputed
#    based on regional and super-regional medians).
#
# Note: 'variance' is a reference to sampling variance (as opposed to non-sampling)
#  and it is added to the diagonal of the covariance matrix (i.e., function/kernel)
#  of the GP (just as in my demos).
# Non-sampling variance increases the variance on datapoints if there is found
#    to be added noise not captured by sampling variance alone.
# This is an optional config parameter (add_nsv).
#
library(data.table)
library(jsonlite)
library(mgcv)
source(here::here("src", "r", "params.R"))
source(here::here("src", "r", "stgpr_utils.R"))
source(here::here("analysis", "1_retro", "domestic", "stgpr_configs.R"))

set.seed(2837)


PATHS <- list(
    files = list(
        gbd_locs = get_path("int", "data", c("covars", "gbd_locations.csv"))
    ),
    dirs = list(
        working = get_path("int", "data", c("domestic", "int", "stgpr")),
        covars = get_path("int", "data", c("covars"))
    )
)

model_values_dir <- file.path(PATHS$dirs$working, "model_values")
dir.create(model_values_dir, showWarnings = FALSE)


construct_outlier_data <- function(outlier_list) {
    # ======================================================================== #
    #' Construct a data.table of hand-identified outliers
    # ======================================================================== #
    dat <- data.table()
    for (vid in names(outlier_list)) {
        outliers <- outlier_list[[vid]]
        for (outlier in outliers) {
            dat <- rbind(dat, data.table(
                value_id = vid,
                ihme_loc_id = outlier$iso,
                year_id = outlier$year,
                data_src = outlier$data_src
            ))
        }
    }
    dat[, outlier_flag := 1]
    return(dat)
}


mark_hand_outliers <- function(dat, outliers) {
    # ======================================================================== #
    #' Mark hand-identified outliers in the data
    # ======================================================================== #
    dat <- copy(dat)
    dat <- merge(dat, outliers,
                 by = c("value_id", "ihme_loc_id", "year_id", "data_src"),
                 all.x = TRUE)
    dat[outlier_flag == 1, outl := 3]
    dat[, outlier_flag := NULL]
    return(dat)
}


get_trend_resids <- function(dat, scale = TRUE) {
    # ======================================================================== #
    #' Calculate the residuals from a GAM fit to the data
    #' 
    #' A smooth trend is fit to the data using a GAM, which is made as flexible
    #' as possible. The trend could be thought of as the underlying, latent trend
    #' or the "process model" that underlies the observed data, which are
    #' observed through a noisy measurement process.
    #' 
    #' @param dat the data.table subset for a given location and given value_id
    #' @param scale logical indicating whether to return the scaled pearson
    #'      residuals or the working residuals. Default is TRUE (scaled pearson)
    #'      which can be interpreted as the residuals in units of standard deviations.
    #'      If FALSE, the working residuals are returned, which are just the raw
    #'      difference between the fitted trend and the data, on the response scale. 
    basis_dim <- length(unique(dat$year_id)) - 1
    if (basis_dim <= 1) {
        resids <- NA_real_
    } else {
        suppressWarnings({
            m <- mgcv::gam(model_value ~ s(year_id, k = basis_dim),
                           data = dat)
        })
        rtype <- if (scale) "scaled.pearson" else "response"
        resids <- resid(m, type = rtype)
    }
    resids[is.infinite(resids)] <- NA_real_
    return(resids)
}


get_lintrend_cooksd <- function(dat) {
    m <- lm(model_value ~ year_id, data = dat)
    cooksd <- cooks.distance(m)
    return(cooksd)
}


plot_outliers <- function(dat, loc) {
    # ======================================================================== #
    #' Plot the data and trend fit for a given location to see what is outliered
    # ======================================================================== #
    ex <- dat[ihme_loc_id == loc]
    preds <- data.table(year_id = seq(min(ex$year_id), max(ex$year_id)))
    preds[, `:=`(pred1 = NA_real_, pred2 = NA_real_)]
    basis_dim1 <- length(unique(ex$year_id)) - 1
    if (basis_dim1 > 1) {
        # initial trend model - with all country data
        suppressWarnings({
            m1 <- mgcv::gam(model_value ~ s(year_id, k = basis_dim1),
                            data = ex)
        })
        preds[, pred1 := predict(m1, newdata = preds)]
            
        basis_dim2 <- length(unique(ex[outl == 0, year_id])) - 1
        if (basis_dim2 > 1) {
            # final trend model - with non-outliers only
            suppressWarnings({
                m2 <- mgcv::gam(model_value ~ s(year_id, k = basis_dim2),
                                data = ex[outl == 0])
            })
            preds[, pred2 := predict(m2, newdata = preds)]
        }
    }
    ex[, outl_label := fcase(
        outl == 0, "non-outlier",
        outl == 1, "country outlier",
        outl == 2, "regional outlier",
        outl == 3, "hand flagged"
    )]
    p <- ggplot(ex, aes(x = year_id, y = model_value)) +
        geom_line(data = preds, aes(x = year_id, y = pred1, linetype = "initial trend"),
                  alpha = 0.6) +
        geom_line(data = preds, aes(x = year_id, y = pred2, linetype = "final trend"),
                  alpha = 0.6) +
        geom_errorbar(data = ex[outl == 0],
                      aes(ymin = model_value - trend_resid, ymax = model_value + trend_resid),
                      width = 0, alpha = 0.5) +
        geom_point(aes(color = outl_label), alpha = 0.6) +
        scale_x_continuous(breaks = seq(min(ex$year_id), max(ex$year_id), 1)) +
        scale_linetype_manual(
            values = c("initial trend" = "dashed",
                       "final trend" = "solid")
        ) +
        scale_color_manual(
            values = c("non-outlier" = "black",
                       "country outlier" = "red",
                       "regional outlier" = "blue",
                       "hand flagged" = "orange")
        ) +
        labs(title = paste(ex[, c(location_id[1], ihme_loc_id[1], location_name[1])],
                           collapse = " - "),
             subtitle = ex$region_name[1],
             x = "Year",
             y = "model_value",
             color = "", linetype = "") +
        theme_linedraw() +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "top"
        )
    return(p)
}



#
# MAIN ========================================================================
#
message("")
message("Outlier Detection and Time-Trend Variance Calculation")
message("")

# load data, merge on location info
alldata <- fread(file.path(PATHS$dirs$working, "stgpr_all_data.csv"))

locs <- load_locations(cache_path = file.path(PATHS$dirs$working,
                                              "location_cache.feather"),
                       location_set_id = stgpr_config_base$location_set_id,
                       release_id = stgpr_config_base$release_id)
alldata <- merge(alldata,
                 locs[, .(location_id, location_name,
                          region_name, super_region_name)],
                 by = "location_id",
                 all.x = TRUE)

# load hand flagged outliers
.hand_outlier_list <- jsonlite::read_json(here::here(
    "analysis", "1_retro", "domestic", "stgpr_outliers.json"
))
hand_outliers <- construct_outlier_data(.hand_outlier_list)


#
# run outlier detection for each modeled variable
#

alldata[, row_id := .I]
alldata[, is_outlier := 0]
alldata[, trend_variance := 0]

for (vi in unique(alldata$value_id)) {
    vi_dir <- file.path(model_values_dir, vi)
    dir.create(vi_dir, showWarnings = FALSE)
    message("Running outlier detection for ", vi, "...")

    tmp <- alldata[value_id == vi,]
    tmp[, outl := 0]
    
    #
    # 1. Country-level outliers
    #
    tmp <- mark_hand_outliers(tmp, hand_outliers)
    for (x in unique(tmp$ihme_loc_id)) {
        tmp[ihme_loc_id == x & outl == 0,
            `:=`(
                resid = get_trend_resids(tmp[ihme_loc_id == x & outl == 0])
                # cooks = get_lintrend_cooksd(tmp[ihme_loc_id == x & outl == 0])
            )]
    }
    tmp[abs(resid) > 2, outl := 1]
    
    #
    # 3. Compute trend-based variance
    #
    for (x in unique(tmp$ihme_loc_id)) {
        tmp[ihme_loc_id == x & outl == 0,
            trend_resid := get_trend_resids(tmp[ihme_loc_id == x & outl == 0],
                                            scale = FALSE)]
    }
    tmp[, trend_resid := trend_resid^2]
    # compute regional level medians to impute for missing values 
    tmp[, region_trend_resid := median(trend_resid, na.rm = TRUE),
        by = region_name]
    tmp[, sr_trend_resid := median(trend_resid, na.rm = TRUE),
        by = super_region_name]
    tmp[is.na(trend_resid), trend_resid := region_trend_resid]
    tmp[is.na(trend_resid), trend_resid := sr_trend_resid]
    tmp[, c("region_trend_resid", "sr_trend_resid") := NULL]
    
    ## This China data comes from budget docs - no outliering allowed
    tmp[value_id == "public_dis2.3" & ihme_loc_id == "CHN" & data_src == "china",
        outl := 0]
    
    
    # merge results back into main data set
    ## each row-id is only processed once, so summing with 0 results in the
    ##  correct value
    alldata <- merge(alldata,
                     tmp[, .(row_id, outl, trend_resid)],
                     by = "row_id", all.x = TRUE)
    alldata[, is_outlier := rowSums(.SD, na.rm = TRUE),
            .SDcols = c("is_outlier", "outl")]
    alldata[, trend_variance := rowSums(.SD, na.rm = TRUE),
            .SDcols = c("trend_variance", "trend_resid")]
    alldata[, c("outl", "trend_resid") := NULL]
    
    plot_locs <- sort(unique( tmp[outl > 0, ihme_loc_id] ))
    message("    - Found ", sum(tmp$outl > 0), " outliers across ",
            length(plot_locs), " countries.")
    
    pdf(file.path(vi_dir, "outliers.pdf"), width = 8, height = 6)
    if (length(plot_locs) == 0) {
        p <- ggplot() + labs(title = paste("No outliers detected for", vi),
                             x = "", y = "")
        print(p)
    } else {
        for (x in plot_locs) {
            p <- plot_outliers(tmp, x)
            if (!is.null(p))
                suppressWarnings(print(p))
        }
    }
    dev.off()
}


alldata[is_outlier > 0, is_outlier := 1]
alldata[, c("location_name", "region_name",
            "super_region_name", "row_id") := NULL]




fwrite(alldata, file.path(PATHS$dirs$working, "stgpr_all_data.csv"))
