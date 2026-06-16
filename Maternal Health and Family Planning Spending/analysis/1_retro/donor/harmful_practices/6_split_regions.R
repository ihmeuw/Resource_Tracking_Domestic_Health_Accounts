#
# Handles the reallocation of regional project disbursements to member countries
#
# Note:
#   For now, am tabulating total disbursements by year-donor-region, calculating
#   the fraction for each region member, and applying to the regional
#   disbursements in the data.
#   Experimented with smoothing (like TT smooth).
#   If there are flows for which we don't have observed data, currently we used
#   an even split.


library(arrow)
library(data.table)

source(here::here("src", "r", "params.R"))
source(here::here("src", "r", "logging.R"))

CONFIG <- list(
    files = list(
        oda_regional = get_path("int", "data",
                                c("oda", "int", "oda_hp_regional.feather")),
        locs = get_path("int", "data", c("covars", "ihme_unfpa_locs.csv")),
        regions = get_path("int", "data",
                           c("oda", "int", "region_mappings.csv")),
        sr_to_region = get_path("int", "data",
                                c("oda", "int", "sr_to_region_mappings.csv")),
        output = get_path("int", "data",
                          c("oda", "int", "oda_region_split.feather"))
    ),
    hp = c("fgm", "gbv", "ecm", "other")
)


compute_region_fractions <- function(dat, regs, locs) {
    dat <- copy(dat)
    dat[, donor := paste0(income_sector, "_", income_type)]
    #
    # training data
    #
    train <- dat[is_regional == FALSE & recip_iso != "_QZA", ]
    train <- train[disb_total > 0,
                   .(disb_total = sum(disb_total)),
                   by = .(year, donor, recip_iso)]
    
    # need to repeat data for each regional category, since they overlap
    ## - e.g., Tuvalu (TUV) belongs to GBD region 'R17' but also special region
    ##   '_Polynesia'.
    ## - thus, must allow.cartesian so that country data is repeated for each
    ##   region it belongs to.
    ## - caution, because this limits what kind of modeling can be done - if we
    ##   tried to do some model (e.g., hierarchical) that included the full
    ##   data set, and hence the expanded sample, we'd be including repeat obs.
    train <- merge(train,
                   regs[, .(recip_iso = iso3, comp_id)],
                   by = "recip_iso",
                   all.x = TRUE,
                   allow.cartesian = TRUE)
    ## if comp_id is missing, it's because the region that this country is in
    ## is never specified as a regional recipient (this is only the case for the
    ## high-income regions anyways) - thus, no need to include this row in the
    ## training data. We could simply drop these rows, but...
    ## You might include them if you had covariates that you were
    ## going to try to use as predictors (e.g., to model the split based on
    ## shared covariates), since these are more examples of the region split
    ## process to train on (although these types of regions are fundamentally
    ## different).
    train <- merge(train,
                   locs[, .(recip_iso = iso3, region_name)],
                   by = "recip_iso", all.x = TRUE)
    train[is.na(comp_id), comp_id := paste0("_", region_name)]
    train[, region_name := NULL]
    
    ## for completeness, ensure that for each donor-region combination the same
    ## recipient countries always appear across time, even when they receive 0
    ## disbursements.
    grid <- regs[, .(year = seq(min(train$year), max(train$year))),
                 by = .(comp_id, recip_iso = iso3)]
    ## determine which donor-region pairs are needed
    grid <- merge(grid,
                  unique(train[, .(comp_id, donor)]),
                  by = "comp_id", all.x = TRUE,
                  allow.cartesian = TRUE)
    ## merge on disbursement data where exists
    grid <- merge(grid, train,
                  by = c("year", "comp_id", "donor", "recip_iso"),
                  all.x = TRUE)
    setnafill(grid, fill = 0, cols = "disb_total")
    
    grid[, region_total := sum(disb_total),
         by = .(year, donor, comp_id)]
    grid[, loc_reg_frac := disb_total / region_total]
    setnafill(grid, fill = 0, cols = "loc_reg_frac")
    
    
    # model fractions to recipients in each region
    fracs <- model_fractions(grid)
    fracs[, `:=`(
        loc_reg_frac = y_pr
    )]
    
    # If we have never seen a disbursement to a recipient in some donor-region
    # group, then their predicted fractions are all 0, which will cause loss of
    # disbursements if the donor-region flow exists in the complete data.
    # Thus we need to make sure all year,donor,comp_id combinations have
    # some fractions that sum to 1.

    # finally, if the donor-region-year still has a total of 0, use the fractions
    #  from the last observed year (e.g., Ex-Yugoslavia region received disbs
    #  before any of its member-states individually receive disbs)
    fracs[, tot := sum(loc_reg_frac), by = .(year, donor, comp_id)]
    fracs[, min_obs_yr := min(year[which(tot > 0)]),
          by = .(donor, comp_id)]
    fracs[, min_obs_yr_frac := loc_reg_frac[which(year == min_obs_yr)],
          by = .(donor, comp_id, recip_iso)]
    fracs[tot == 0, loc_reg_frac := min_obs_yr_frac]
    
    
    # final check
    fracs[, tot := sum(loc_reg_frac), by = .(year, donor, comp_id)]
    fracs[, warn := fifelse(abs(tot - 1) > 1e-6, TRUE, FALSE)]
    if (any(fracs$warn)) {
        warning(sum(fracs$warn), " fractions do not sum to 1")
    }
    fracs <- fracs[, .(year, donor, comp_id, recip_iso, loc_reg_frac)]
    return(fracs)
}



#
# For each donor-to-region flow, determine the likely split among region
#   members based on observed disbursement fractions.
#
# transform and then model
# - smithson and verkuilen transform just helps us avoid problems with exact
#   0s and exact 1s, and the additive log-ratio transform maps our compositional
#   problem from a D-dimensional simplex to a real vector, so we can use any
#   kind of unconstrained statistical techniques on the components.
#
# Casting wide - isos as columns - is an easier way to visualize what is
#   happening, but less efficient.
#   Essentially, each country's fractions becomes an independent "y" which
#   gets transformed, smoothed, and then transformed back.
#
model_fractions <- function(grid) {
    tr <- copy(grid)

    #
    # prepare sample to train on
    #
    ## drop years before the region starts receiving disbursements so that
    ##   predictions starts when observations start
    tr[, grp_disb := sum(disb_total),
       by = .(year, donor, comp_id)]
    tr[, min_disb_yr := min(year[which(grp_disb > 0)]),
       by = .(donor, comp_id, recip_iso)]
    drop_yrs <- tr[year < min_disb_yr]
    drop_yrs[, y_pr := 0]
    tr <- tr[year >= min_disb_yr, -c("min_disb_yr")]
    
    ## if an iso's frac is zero for all years in the group, drop it,
    ##  and then add back on later (its prediction is always 0)
    ## 
    tr[, max_zeros := length(seq(min(year), max(year))), # max possible 0 years
       by = .(donor, comp_id, recip_iso)]
    tr[, nzero := sum(loc_reg_frac < 1e-5),
       by = .(donor, comp_id, recip_iso)]
    zero_isos <- tr[nzero == max_zeros, ]
    zero_isos[, y_pr := 0]
    tr <- tr[nzero < max_zeros, -c("max_zeros", "nzero")]
    
    
    # calculate minimum year of any disbursement to the country
    # - for example, South Sudan comes into official existence in 2011, so we
    #   don't want them to have a non-zero fraction prior to 2011
    tr[, min_yr := min(year[which(disb_total > 0)]),
       by = .(recip_iso)]
    drop_iso_yrs <- tr[year < min_yr]
    drop_iso_yrs[, y_pr := 0]
    tr <- tr[year >= min_yr, -c("min_yr")]
    
    
    # calculate number of observations (years), and
    # number of components (countries) in each group (donor-region flow)
    #   (n_grp should be constant for each donor-region-country combo and
    #    d_grp should be constant for each year-donor-region combo)
    tr[, n_grp := length(loc_reg_frac), by = .(donor, comp_id, recip_iso)]
    tr[, d_grp := length(unique(recip_iso)), by = .(year, donor, comp_id)]
    
    
    ## drop groups where d_grp == 1 (only 1 component), which indicates other
    ## recipients are not receiving disbursements (so prediction will be 100%
    ## to the only historical recipient)
    ## or where n_grp == 1 (only 1 year), since not enough data to smooth
    drop_grp <- tr[d_grp == 1 | n_grp == 1]
    drop_grp[, y_pr := 1]
    tr <- tr[d_grp > 1 & n_grp > 1]
    
    
    ## add back dropped rows at end of estimation
    keep_cols <- c("year", "donor", "comp_id", "recip_iso", "loc_reg_frac", "y_pr")
    to_add <- zero_isos[, ..keep_cols]
    if (nrow(drop_yrs) > 0) to_add <- rbind(to_add, drop_yrs[, ..keep_cols])
    if (nrow(drop_grp) > 0) to_add <- rbind(to_add, drop_grp[, ..keep_cols])
    if (nrow(drop_iso_yrs) > 0) to_add <- rbind(to_add, drop_iso_yrs[, ..keep_cols])
    rm(zero_isos, drop_yrs, drop_grp, drop_iso_yrs)
    
    #
    # Generate estimated fractions ===
    #
    tr[, y_pr := NA_real_]
    #
    # Apply Smithson and Verkuilen (2006) transform to pull values away
    # from 0 (and 1) before other transforms
    #
    # \tilde{y} = (y * (n - 1) + 1/D) / N
    #     D = number of components, N = number of observations
    #
    # (e.g., see ?DirichletReg::DR_data section "trafo" and 
    #  https://www.jstatsoft.org/article/view/v034i02 page 3)
    # could also use: DirichletReg::DR_data(..., trafo = TRUE)
    #
    tr[, y := loc_reg_frac]
    # apply transform across rows
    # (within groups, since n_grp and d_grp are group-specific)
    tr[, y_tr := (y * (n_grp - 1) + (1/d_grp)) / n_grp]
    
    #
    # Apply additive log-ratio transformation
    #
    ## ALR:
    ## for a D-component system, we transform our composition vector from the
    ## D-dimensional simplex S^D --> real numbers R^{D-1}:
    ##      x = [x_1, x_2, ..., x_D],
    #  where sum(x) = 1, to
    ##      x_alr = [log(x_1 / x_D), log(x_2 / x_D), ..., log(x_{D-1} / x_D)]
    ##  where x_D is the reference component (the last one by convention,
    ##  though the choice is arbitrary).
    ## To invert the ALR, we go from R^{D-1} --> S^D:
    ##     x = C[exp(x_alr_1), exp(x_alr_2), ..., exp(x_alr_{D-1}), 1]
    ## where C[] is the closure operation (division by sum of components)
    ## 
    ## See Aitchinson, Concise Guide to Compositional Data Analysis.
    tr[, i_comp := 1:.N, by = .(year, donor, comp_id)]
    tr[, is_reference := i_comp == d_grp]
    tr <- merge(
        tr,
        tr[is_reference == TRUE, .(year, donor, comp_id, y_tr_holdout = y_tr)],
        by = c("year", "donor", "comp_id"), all.x = TRUE
    )
    tr[, y_tr_alr := log(y_tr / y_tr_holdout)]
    tr[is_reference == TRUE, y_tr_alr := NA_real_]
    
    #
    # predict - apply some method to the transformed values
    #
    ##
    ## loess: local polynomial regression to smooth transformed values
    ##   for each recipient country
    ##
    ## apply across the time dimension of each donor-region-country combo
    setorder(tr, year)
    tr[n_grp > 2 & is_reference == FALSE,
       y_tr_alr_sm := fitted(loess(y_tr_alr ~ year, span = 0.75)),
       by = .(donor, comp_id, recip_iso)]
    ## if only 2 years observed, just use the mean
    tr[n_grp == 2 & is_reference == FALSE,
       y_tr_alr_sm := mean(y_tr_alr),
       by = .(donor, comp_id, recip_iso)]
    ## if only 1 year, then the group was dropped in pre-processing
    
    #
    # inverse the ALR transformation
    #
    tr[, exp_y_tr_alr_sm := exp(y_tr_alr_sm)]
    tr[is_reference == TRUE, exp_y_tr_alr_sm := 1]
    tr[, inv_alr_denom := sum(exp_y_tr_alr_sm), by = .(year, donor, comp_id)]
    tr[, y_pr := exp_y_tr_alr_sm / inv_alr_denom]
    
    tr[, tmp := sum(y_pr), by = .(year, donor, comp_id)]
    if ( tr[abs(tmp - 1) > 1e-6, .N] != 0 )
        stop("Predicted fractions do not sum to 1, investigate!")
    
    #
    # finalize
    #
    fin <- tr[, .(year, donor, comp_id, recip_iso, loc_reg_frac, y_pr)]
    # add back in dropped rows
    if (nrow(to_add)) {
        fin <- rbind(fin, to_add)
    }
    setorder(fin, year)
    
    if (nrow(fin) != nrow(grid))
        stop("Rows mistakenly dropped during estimation, investigate!")
    
    fin[, tmp := sum(y_pr), by = .(year, donor, comp_id)]
    
    stopifnot(
        ## the final sum across components should be 1, or 0 in some cases...
        fin[tmp != 0 & abs(tmp - 1) > 1e-6, .N] == 0,
        ## should only be 0 if the whole region doesn't receive disbursements,
        ##  in which case loc_reg_frac is always 0
        fin[tmp == 0 & loc_reg_frac != 0, .N] == 0
    )
    fin[, tmp := NULL]
    
    
    if (interactive()) {
        ex = fin[donor == "USA_CENTRAL" & comp_id == "_Polynesia"]
        ex[, tot := sum(y_pr), by = .(year)]
        
        library(ggplot2)
        ggplot(ex, aes(x = year, color = recip_iso)) +
            geom_line(aes(y = y_pr)) +
            geom_point(aes(y = y_pr), size = 0.5) +
            geom_point(aes(y = loc_reg_frac, color = recip_iso), shape = 1) +
            # geom_line(aes(y = tot, color = "_TOTAL"), linetype = "dashed") +
            theme_minimal() +
            facet_wrap(~ recip_iso, scales = "free_y")
    }
    return(fin)
}



redistribute_disb <- function(dat) {
    ## for now, methodology assumes that the recipient fraction applies the same
    ## way across all harmful practices and total disbursements
    dat <- copy(dat)
    disb_cols <- grep("^disb_", names(dat), value = TRUE)
    
    dat[, region_total_disb := disb_total] ## save
    # recalculate disb columns
    dat[, (disb_cols) := lapply(.SD, function(x) x * loc_reg_frac),
        .SDcols = disb_cols]

    return(dat)
}


test_disb_redist <- function(dat) {
    dat <- copy(dat)
    dat[, test_disb_total := sum(disb_total), by = flow_id]
    dat[, diff_disb_total := abs(test_disb_total - region_total_disb)]
    
    if ( nrow(dat[diff_disb_total > 0.1]) ) {
        log_error("Redistribution of disbursements failed")
        stop("Redistribution of disbursements failed")
    }
}


test_final_locs <- function(dat, locs) {
    dat <- copy(dat)
    ## ensure we only have level 3 locs (ie, countries)
    dat <- unique(dat[, .(recip_iso)])
    
    dat <- merge(dat,
                 locs[, .(recip_iso = iso3, flg = 1)],
                 by = "recip_iso", all.x = TRUE)
    dat[recip_iso == "_QZA", flg := 1] ## special case
    
    if (any(is.na(dat$flg))) {
        log_error("Some locations in final data are not level 3 GBD countries")
        stop("Some locations in final data are not level 3 GBD countries")
    }
}


# new_oda = copy(oda_fin)
# old_oda = copy(oda)
test_final_disb <- function(new_oda, old_oda) {
    new <- new_oda[, .(new = sum(disb_total)), by = .(year)]
    old <- old_oda[, .(old = sum(disb_total)), by = .(year)]
    cmp <- merge(new, old, by = "year", all = TRUE)
    
    cmp[, diff := abs(new - old)]
    cmp[, warn := diff > 0.1]
    
    if (any(cmp$warn)) {
        log_error("{sum(cmp$warn)} disbursements in final data do not match original")
        stop("Disbursements in final data do not match original")
    }
}





main <- function() {
    log_level("INFO")
    log_info("Split ODA flows with regional recipients to country level")
    
    log_info("* Prepare inputs")
    #
    # data sources
    #
    
    # region-to-country mappings
    regions <- fread(CONFIG$files$regions)
    # region-to-super_region mappings
    super_regions <- fread(CONFIG$files$sr_to_region)
    # location metadata
    locs <- fread(CONFIG$files$locs)
    
    # processed flows of ODA with harmful practice categories
    oda <- as.data.table(arrow::read_feather(CONFIG$files$oda_regional))
    oda[, flow_id := .I]
    # identify regional recipients
    #   (if they merged to level 3 locations, they would be countries)
    oda <- merge(oda,
                 locs[, .(recip_iso = iso3, is_regional = FALSE)],
                 by = "recip_iso", all.x = TRUE)
    oda[recip_iso == "_QZA", is_regional := FALSE] # can't split "unallocated"
    oda[is.na(is_regional), is_regional := TRUE]
    
    # convert super-region recipients to collection of regions
    oda <- merge(oda,
                 super_regions,
                 by.x = "recip_iso", by.y = "super_region_code",
                 all.x = TRUE)
    oda[!is.na(region_code), recip_iso := region_code]
    oda[, region_code := NULL]
    
    
    log_info("* Split regional flows")
    log_info("** Explode regional projects")
    #
    # explode regions into country level
    #
    oda_reg <- oda[is_regional == TRUE]
    setnames(oda_reg, "recip_iso", "comp_id")
    ## allow.cartesian so each occurrence of a region will repeat for each
    ## member country (duplicating disbursement values)
    oda_reg <- merge(
        oda_reg,
        regions[, .(comp_id, recip_iso = iso3)],
        by = "comp_id",
        all.x = TRUE,
        allow.cartesian = TRUE 
    )
    
    log_info("** Compute recipient fractions")
    #
    # calculate recipient country fractions
    #
    fracs <- compute_region_fractions(oda, regions, locs)
    
    #
    # merge on fractions computed above by year-donor-region-recipient
    #
    oda_reg[, donor := paste0(income_sector, "_", income_type)]
    oda_reg <- merge(
        oda_reg,
        fracs,
        by = c("year", "donor", "comp_id", "recip_iso"),
        all.x = TRUE
    )
    
    if (oda_reg[is.na(loc_reg_frac), .N]) {
        log_error("Some regional flows do not have recipient fractions")
        stop("Some regional flows do not have recipient fractions")
    }
    oda_reg[, donor := NULL]
    
    log_info("** Redistribute disbursements")
    #
    # redistribute disbursements based on recipient fractions
    #
    oda_reg <- redistribute_disb(oda_reg)
    ## test no issues in redistribution 
    test_disb_redist(oda_reg)
    
    #
    # re-combine data
    #
    oda_noreg <- oda[is_regional == FALSE]
    oda_noreg[, `:=`(
        comp_id = recip_iso,
        loc_reg_frac = NA_real_,
        region_total_disb = NA_real_
    )]
    oda_fin <- rbind(oda_noreg, oda_reg)
    ## test
    test_final_locs(oda_fin, locs)
    test_final_disb(oda_fin, oda)
    
    # save
    log_info("* Writing output to {CONFIG$files$output}")
    arrow::write_feather(oda_fin, CONFIG$files$output)
    
    log_info("* Done")
}


main()
