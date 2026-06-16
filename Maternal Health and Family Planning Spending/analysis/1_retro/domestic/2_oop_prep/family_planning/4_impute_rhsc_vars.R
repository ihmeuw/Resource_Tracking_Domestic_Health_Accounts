#
# Need to impute costs per user for countries without RHSC estimates
#
library(data.table)
library(ggplot2)
library(arrow)
library(lme4)
source(here::here("src", "r", "params.R"))

CONFIG <- list(
    files = list(
        gbd_locs = get_path("int", "data", c("covars", "gbd_locations.csv")),
        ihme_unfpa_locs = get_path("int", "data", c("covars", "ihme_unfpa_locs.csv"))
    ),
    dirs = list(
        working = get_path("int", "data", c("domestic", "int", "oop_fp")),
        covars = get_path("int", "data", c("covars"))
    ),
    est_years = 2000:2024
)

logit <- function(x) log(x / (1 - x))
inv_logit <- function(x) exp(x) / (1 + exp(x))

load_prep_rhsc <- function() {
    rhsc <- fread(file.path(CONFIG$dirs$working, "rhsc_clean.csv"))
    # filter out public sector data and keep only total costs and total users
    rhsc <- rhsc[sector %like% "private" & indicator %in% c("cost", "user")]
    rhsc <- rhsc[, .(ihme_loc_id,
                     year_id,
                     indicator, # cost or user
                     sector,    # private_sub or private_non_sub
                     method,    # type of contraceptive method
                     value)]    # value of indicator, in level space
    # pivot wide
    rhsc <- dcast(rhsc, ihme_loc_id + year_id + method ~ sector + indicator,
                  value.var = "value")
    
    # calculate share of private users and private costs per user by sector-type
    #    (subsidized vs non-sub), for each method and country-year
    
    ## proportion of private users that are subsidized vs non-subsidized
    rhsc[, prop_priv_nonsub_user := private_non_sub_user /
             (private_non_sub_user + private_sub_user)]
    rhsc[, prop_priv_sub_user := private_sub_user /
             (private_non_sub_user + private_sub_user)]
    
    ## cost per non-subsidized user
    rhsc[, cost_per_user_priv_nonsub := private_non_sub_cost / private_non_sub_user]
    ## cost per subsidized user
    rhsc[, cost_per_user_priv_sub := private_sub_cost / private_sub_user]
    
    calc_cols <- c("prop_priv_nonsub_user", "prop_priv_sub_user",
                   "cost_per_user_priv_nonsub", "cost_per_user_priv_sub")
    setnafill(rhsc, fill = 0, cols = calc_cols)
    rhsc[, (calc_cols) := lapply(.SD, \(x) fifelse(is.infinite(x), 0, x)),
         .SDcols = calc_cols]
    return(rhsc)
}


rhsc <- load_prep_rhsc()
## in these cases, the estimates suggest that there are no private users
rhsc <- rhsc[prop_priv_sub_user + prop_priv_nonsub_user != 0]


gbd_locs <- fread(CONFIG$files$gbd_locs)[level == 3]
pops <- fread(file.path(CONFIG$dirs$covars, "population.csv"))
pops <- pops[
    level == 3 & age_group_name == "All Ages" & sex == "Both",
    .(year_id, ihme_loc_id, population)
]

grid <- data.table::CJ(
    ihme_loc_id = unique(gbd_locs[, ihme_loc_id]),
    year_id = unique(rhsc$year_id),
    method = unique(rhsc$method)
)

grid <- merge(grid, rhsc,
              by = c("ihme_loc_id", "year_id", "method"),
              all.x = TRUE)

grid <- merge(grid,
              unique(gbd_locs[, .(ihme_loc_id, region_name,
                                  super_region_name)]),
              by = "ihme_loc_id",
              all.x = TRUE)

grid <- merge(grid,
              pops,
              by = c("ihme_loc_id", "year_id"),
              all.x = TRUE)

## add some covariates
gdp <- fread(file.path(CONFIG$dirs$covars, "gdp_pc.csv"))
grid <- merge(grid,
              gdp[, .(ihme_loc_id, year_id, gdp_pc)],
              by = c("ihme_loc_id", "year_id"),
              all.x = TRUE)

oop <- fread(file.path(CONFIG$dirs$covars, "oop_totes.csv"))
grid <- merge(grid,
              oop[, .(ihme_loc_id, year_id, oop_totes)],
              by = c("ihme_loc_id", "year_id"),
              all.x = TRUE)
grid[, oop_pc := oop_totes / population]

tfr <- fread(file.path(CONFIG$dirs$covars, "tfr.csv"))
grid <- merge(grid,
              tfr[, .(ihme_loc_id, year_id, tfr)],
              by = c("ihme_loc_id", "year_id"),
              all.x = TRUE)



plots <- list()

#
# impute costs
#
# strategy: drop observations of 0-cost, then log-transform the cost variable,
# fit a mixed effects model to predict log-cost, then back-transform to get an
# imputed cost > 0.
#
cost_vars <- c("cost_per_user_priv_nonsub", "cost_per_user_priv_sub")
for (var in cost_vars) {
    grid[, `:=`(y = NA_real_, pred = NA_real_)]
    grid[get(var) > 0, y := log(get(var))]
    
    for (m in unique(grid$method)) {
        mod <- lmer(y ~ 1 +
                        log(gdp_pc) +
                        log(oop_pc) +
                        log(tfr) +
                        as.factor(year_id) +
                        (1 | super_region_name/region_name),
                    data = grid[!is.na(y) & method == m])
        grid[method == m,
             pred := exp(predict(mod,
                                 newdata = grid[method == m],
                                 allow.new.levels = TRUE))]
    }
    
    plot_dt <- copy(grid)
    plot_dt[, x := ifelse(is.na(get(var)), pred, get(var))]
    
    p <- ggplot(plot_dt, aes(x = x, y = pred)) +
        geom_point(aes(color = is.na(get(var)),
                       shape = as.factor(year_id)), alpha = 0.5) +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue") +
        facet_wrap(~method, scales = "free") +
        scale_color_manual(values = c("TRUE" = "red", "FALSE" = "grey50")) +
        labs(x = "Observed",
             y = "Predicted",
             shape = "Year",
             color = "Impute",
             title = var) +
        theme_minimal()
    plots[[var]] <- p
    
    setnames(grid, "pred", paste0(var, "_pred"))
}



#
# impute user proportions
#
# strategy: treat proportions as the p parameter of a Binomial distribution, and
# for the n parameter use the number of users.
# We just need to estimate the proportion of private users that are subsidized,
# and then the remaining users are non-subsidized (the sum of the proportions is 1).
# https://stats.stackexchange.com/questions/560996/which-glmm-to-use-for-proportional-data
#
for (m in unique(grid$method)) {
    train_dt <- grid[!is.na(prop_priv_sub_user) & method == m]
    train_dt[, N := as.integer(private_sub_user + private_non_sub_user)]
    if (m %in% c("implant", "iud")) {
        # if more complex model not converging 
        mod <- glmer(prop_priv_sub_user ~ 1 +
                         log(gdp_pc) +
                         (1 | super_region_name/region_name),
                    weights = train_dt$N, # provide prior weights to model proportions
                    family = binomial(link = "logit"),
                    data = train_dt)
    } else {
        mod <- glmer(prop_priv_sub_user ~ 1 +
                         log(gdp_pc) +
                         log(oop_pc) +
                         log(tfr) +
                         (1 | super_region_name/region_name),
                    weights = train_dt$N, # provide prior weights to model proportions
                    family = binomial(link = "logit"),
                    data = train_dt)
    }
    grid[method == m,
         pred := predict(mod,
                         newdata = grid[method == m],
                         allow.new.levels = TRUE,
                         type = "response")]
}

plot_dt <- copy(grid)
plot_dt[, x := ifelse(is.na(prop_priv_sub_user), pred, prop_priv_sub_user)]

p <- ggplot(plot_dt, aes(x = x, y = pred)) +
    geom_point(aes(color = is.na(prop_priv_sub_user),
                   shape = as.factor(year_id)), alpha = 0.5) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue") +
    facet_wrap(~method, scales = "free") +
    scale_color_manual(values = c("TRUE" = "red", "FALSE" = "grey50")) +
    labs(x = "Observed",
         y = "Predicted",
         shape = "Year",
         color = "Impute",
         title = "prop_priv_sub_user") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

plots[["prop_priv_sub_user"]] <- p
setnames(grid, "pred", "prop_priv_sub_user_pred")
    
## check that predicted values are valid proportions
stopifnot( grid[!between(prop_priv_sub_user_pred, 0, 1), .N] == 0 )



fin <- grid[, .(ihme_loc_id, year_id, method,
                prop_priv_sub_user,
                prop_priv_nonsub_user,
                cost_per_user_priv_sub,
                cost_per_user_priv_nonsub,
                prop_priv_sub_user_pred,
                cost_per_user_priv_sub_pred,
                cost_per_user_priv_nonsub_pred)]

fin <- melt(fin, id.vars = c("ihme_loc_id", "year_id", "method",
                             "prop_priv_sub_user_pred",
                             "cost_per_user_priv_sub_pred",
                             "cost_per_user_priv_nonsub_pred"),
            variable.name = "var", value.name = "value")

fin[, imputed := is.na(value)]
fin[is.na(value) & var == "prop_priv_sub_user",
    value := prop_priv_sub_user_pred]
fin[is.na(value) & var == "prop_priv_nonsub_user",
    value := 1 - prop_priv_sub_user_pred]
fin[is.na(value) & var == "cost_per_user_priv_sub",
    value := cost_per_user_priv_sub_pred]
fin[is.na(value) & var == "cost_per_user_priv_nonsub",
    value := cost_per_user_priv_nonsub_pred]


## confirm all values have been imputed
stopifnot( fin[is.na(value), .N] == 0 )
fin[, grep("_pred$", names(fin)) := NULL]


# save
fp <- file.path(CONFIG$dirs$working, "rhsc_imputed.csv")
message("Saving imputed data: ", fp)
fwrite(fin, fp)


# save plots
pdf(here::here("data", "outputs", "figures", "impute_rhsc_vars.pdf"),
    width = 12, height = 10)
for (p in plots) {
    print(p)
}
invisible(capture.output(dev.off()))

message("* Done.")
# ============================================================================ 







