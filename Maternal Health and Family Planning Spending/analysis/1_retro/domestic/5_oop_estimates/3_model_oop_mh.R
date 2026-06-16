#
# Use utilization estimates and OOP-MH spending data from literature to estimate
# OOP-MH spending for each country-year-service
#
set.seed(13009)

library(data.table)
library(lme4)
library(ggplot2)
library(patchwork)
source(here::here("src", "r", "params.R"))
source(here::here("src", "r", "internals.R"))

CONFIG <- list(
    files = list(
        figure_pdf = get_path("fin", "outputs", c("figures", "model_oop_mh.pdf"))
    ),
    dirs = list(
        covars = get_path("int", "data", c("covars")),
        working = get_path("int", "data", c("domestic", "int", "oop_mh")),
        output = get_path("int", "data", c("domestic", "int", "draws"))
    )
)


if (!interactive()) pdf(CONFIG$files$figure_pdf, width = 12, height = 10)

logit <- function(x) log(x / (1 - x))
inv_logit <- function(x) exp(x) / (1 + exp(x))

message("\n* Finalizing estimates of OOP costs for maternal health services...")


#
# load inputs =================================================================
#


# load extracted maternal health cost data
extract <- fread(file.path(CONFIG$dirs$working, "mh_costs_clean.csv"))
extract[, currency := "2023 USD"]


extract[, value_id := fifelse(
    care_type %in% c("delivery", "c_section"),
    paste0(facility, "_", care_type),
    care_type
)]


# load covariates

## location metadata
locs <- fread(get_path("int", "data", c("covars", "ihme_unfpa_locs.csv")))

## population
pops <- fread(file.path(CONFIG$dirs$covars, "population.csv"))
pops <- pops[level == 3 &
                 age_group_name == "All Ages" &
                 sex == "Both",
             .(year_id, iso3 = ihme_loc_id, population)]

## gdp
gdp <- fread(file.path(CONFIG$dirs$covars, "gdp_pc.csv"))
gdp <- gdp[level == 3,
           .(year_id, iso3 = ihme_loc_id, gdp_pc)]
## convert from per-capita
gdp <- merge(gdp, pops,
             by = c("year_id", "iso3"), all.x = TRUE)
gdp[, gdp := gdp_pc * population]


## total oop spending on health
oop <- fread(file.path(CONFIG$dirs$covars, "oop_totes.csv"))
oop <- oop[, .(year_id, iso3 = ihme_loc_id, oop_totes)]
oop <- merge(oop, pops,
             by = c("year_id", "iso3"), all.x = TRUE)
oop[, `:=` (
    oop_per_cap = oop_totes / population
)]


# load maternal health services utilization estimates
util <- arrow::read_feather(file.path(CONFIG$dirs$working, "mh_utilization_summ.feather"))
## summarize draws - use mean estimate as value
util <- util[, .(iso3 = ihme_loc_id, year_id, value_id, users = mean)]
##
## mixed facility delivery and c-section: use average across facility type
mixed_d <- util[value_id %in% c("public_facility_delivery", "private_facility_delivery"),
               .(users = mean(users),
                 value_id = "mixed_facility_delivery"),
               by = .(iso3, year_id)]
mixed_c <- util[value_id %in% c("public_facility_c_section", "private_facility_c_section"),
               .(users = mean(users),
                 value_id = "mixed_facility_c_section"),
               by = .(iso3, year_id)]
util <- rbind(util, mixed_d, mixed_c)
rm(mixed_d, mixed_c)

# value_id categories
vals <- unique(extract[, .(value_id)])
vals[, facility_type := fcase(
    grepl("public_facility", value_id), "public",
    grepl("private_facility", value_id), "private",
    grepl("mixed_facility", value_id), "mixed",
    grepl("home", value_id), "home",
    default = "mixed"
)]
vals[, care_type := gsub("public_facility_|private_facility_|mixed_facility_|home_",
                         "",
                         value_id)]


# finalize features
features <- merge(
    oop[, .(iso3, year_id, population, oop_totes, oop_per_cap)],
    gdp[, .(iso3, year_id, gdp_pc, gdp)],
    by = c("iso3", "year_id"), all.x = TRUE
)
features <- merge(
    util,
    features,
    by = c("iso3", "year_id"), all.x = TRUE
)
features <- merge(
    features,
    unique(locs[, .(iso3, super_region_name, is_unfpa)]),
    by = "iso3", all.x = TRUE
)
features <- merge(
    features,
    vals,
    by = "value_id", all.x = TRUE
)


features[iso3 %in% c("CHN", "IND", "BRA", "MEX", "ARG"),
         super_region_name := "High OOP Countries"]
features[iso3 %in% c("CHL", "URY"),
         super_region_name := "Latin America and Caribbean"]
setnames(features, "super_region_name", "region")


#
# prepare training and prediction data sets ===================================
#

# training data
mod_dt <- extract[, .(study,
                      year_id = as.integer(year),
                      iso3, country,
                      value_id,
                      unit_cost = cost)]
mod_dt <- mod_dt[year_id >= 2000, ]

mod_dt <- merge(mod_dt, features,
                by = c("iso3", "year_id", "value_id"),
                all.x = TRUE)

## convert model value to proportion of total oop spending
mod_dt[, total_oop_mh := unit_cost * users]
mod_dt[, `:=` (
    prop = total_oop_mh / oop_totes
)]


# prediction grid
pred_grid <- data.table::CJ(
    iso3 = unique(locs[is_gbd == TRUE, ihme_loc_id]),
    year_id = 2000:2026,
    value_id = unique(mod_dt$value_id)
)
pred_grid <- merge(pred_grid, features,
                   by = c("iso3", "year_id", "value_id"),
                   all.x = TRUE)





#
# fit model ===================================================================
#

mod <- lme4::lmer(logit(prop) ~ 1
                  + log(gdp_pc)
                  + log(users)
                  + as.factor(care_type)
                  + as.factor(facility_type)
                  + (1 | region)
                  ,data = mod_dt)


# predict
pred_grid[, prop := inv_logit(predict(mod,
                                      newdata = pred_grid,
                                      allow.new.levels = TRUE))]
pred_grid[, value := prop * oop_totes]

## calculate SE
mod_dt[, pred := inv_logit(predict(mod, newdata = mod_dt))]
mod_dt[, se := sqrt(sum( (prop - pred)^2 )) / (.N - 2), by = value_id]

pred_grid[, value_id := factor(value_id, levels = c(
    "antenatal",
    "public_facility_delivery",
    "public_facility_c_section",
    "private_facility_delivery",
    "private_facility_c_section",
    "mixed_facility_delivery",
    "mixed_facility_c_section",
    "home_delivery",
    "postnatal"
))]
pred_grid <- merge(
    pred_grid,
    unique(mod_dt[, .(value_id, se)]),
    by = "value_id", all.x = TRUE
)




#
# save results ================================================================
#
message("** Saving oop_mh_estimates.csv to ", CONFIG$dirs$working)

# aggregate across service types (value_id's)
#  Note, we included mixed_facility_delivery and mixed_facility_c_section for
#  estimation purposes (to take advantage of additional data), but they don't have
#  real utilization estimates (utilization is split between public, private, and
#  home) so we now drop them.
fin <- pred_grid[!value_id %like% "mixed",
                 .(year_id, iso3, is_unfpa, value_id, oop_mh = value, oop_totes, se)]
fin[, prop_oop_totes := oop_mh / oop_totes]

fin[, currency := "2023 USD"]

setnames(fin, "iso3", "ihme_loc_id")

fwrite(fin,
       file.path(CONFIG$dirs$working, "oop_mh_estimates.csv"))



message("** Saving oop_mh_observed.csv to ", CONFIG$dirs$working)
obs <- mod_dt[! value_id %like% "mixed",
              .(study, year_id, ihme_loc_id = iso3, is_unfpa, value_id,
                oop_mh = total_oop_mh, oop_totes, users)]

## merge on uncertainty from utilization data
util <- arrow::read_feather(file.path(CONFIG$dirs$working, "mh_utilization_summ.feather"))
obs <- merge(obs,
             util[, .(ihme_loc_id, year_id, value_id,
                      users_lower = lower,
                      users_upper = upper)],
             by = c("ihme_loc_id", "year_id", "value_id"),
             all.x = TRUE)

fwrite(obs,
       file.path(CONFIG$dirs$working, "oop_mh_observed.csv"))

message("** Done!")

#
# figures =====================================================================
#
message("Saving figures to: ", CONFIG$files$figure_pdf)
pred_grid[!value_id %like% "mixed" & is_unfpa == TRUE,
          .(val = median(value/oop_totes),
            lb = quantile(value/oop_totes, 0.25),
            ub = quantile(value/oop_totes, 0.75)),
    by = .(region, value_id)] |>
    ggplot(aes(x = value_id, y = val, fill = value_id)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = lb, ymax = ub),
                  color = "grey50",
                  width = 0.2, position = position_dodge()) +
    geom_text(aes(label = formatC(val, format = "f", digits = 3)),
              vjust = -1, size = 4) +
    facet_wrap(~region, scales = "fixed") +
    labs(title = "Median (and IQR) estimates of OOP Spending on Maternal Health",
         subtitle = "Proportion of total OOP",
         x = "", y = "", fill = "") +
    theme_bw() +
    theme(
        legend.position = "top",
        axis.text.x = element_blank()
    )


pred_grid[!value_id %like% "mixed" & is_unfpa == TRUE,
          .(val = median(value/users),
            lb = quantile(value/users, 0.25),
            ub = quantile(value/users, 0.75)),
    by = .(region, value_id)] |>
    ggplot(aes(x = value_id, y = val, fill = value_id)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = lb, ymax = ub),
                  color = "grey50",
                  width = 0.2, position = position_dodge()) +
    geom_text(aes(label = formatC(val, format = "f", digits = 1)),
              vjust = -1, size = 4) +
    facet_wrap(~region, scales = "fixed") +
    labs(title = "Median (and IQR) estimates of OOP Spending on Maternal Health",
         subtitle = "Spending per user",
         x = "", y = "", fill = "") +
    theme_bw() +
    theme(
        legend.position = "top",
        axis.text.x = element_blank()
    )


util_ex <- pred_grid[! value_id %like% "mixed" & is_unfpa == TRUE]
util_ex[, total_cases := sum(users), by = .(iso3, year_id)]
util_ex[,
          .(val = median(users/total_cases),
            lb = quantile(users/total_cases, 0.25),
            ub = quantile(users/total_cases, 0.75)),
    by = .(region, value_id)] |>
    ggplot(aes(x = value_id, y = val, fill = value_id)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = lb, ymax = ub),
                  color = "grey50",
                  width = 0.2, position = position_dodge()) +
    geom_text(aes(label = formatC(val, format = "f", digits = 2)),
              vjust = -1, size = 4) +
    facet_wrap(~region, scales = "fixed") +
    labs(title = "Median (and IQR) estimates of Maternal Health Utilization",
         subtitle = "Proportion of total cases",
         x = "", y = "", fill = "") +
    theme_bw() +
    theme(
        legend.position = "top",
        axis.text.x = element_blank()
    )

pred_grid[! value_id %like% "mixed" & is_unfpa == TRUE,
          .(val = median(value/1e6),
            lb = quantile(value/1e6, 0.25),
            ub = quantile(value/1e6, 0.75)),
          by = .(region, value_id)] |>
    ggplot(aes(x = value_id, y = val, fill = value_id)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = lb, ymax = ub),
                  color = "grey50",
                  width = 0.2, position = position_dodge()) +
    geom_text(aes(label = formatC(val, format = "f", digits = 1)),
              vjust = -1, size = 4) +
    facet_wrap(~region, scales = "free_y") +
    labs(title = "Median (and IQR) estimates of OOP Spending on Maternal Health",
         subtitle = "Millions of USD",
         x = "", y = "", fill = "") +
    theme_bw() +
    theme(
        legend.position = "top",
        axis.text.x = element_blank()
    )

pred_grid[! value_id %like% "mixed" & is_unfpa == TRUE,
          .(y = sum(value)/oop_totes),
          by = .(iso3, year_id, region)
          ][, .(val = median(y),
                lb = quantile(y, 0.25),
                ub = quantile(y, 0.75)),
            by = .(region)] |>
    ggplot(aes(x = region, y = val, fill = region)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = lb, ymax = ub),
                  color = "grey50",
                  width = 0.2, position = position_dodge()) +
    geom_text(aes(label = formatC(val, format = "f", digits = 3)),
              vjust = -1, size = 4) +
    labs(title = "OOP-MH proportion of total OOP",
         subtitle = "Median and IQR",
         x = "", y = "", fill = "") +
    theme_bw() +
    theme(
        legend.position = "top",
        axis.text.x = element_blank()
    )


pred_grid[! value_id %like% "mixed" & is_unfpa == TRUE,
          .(value = sum(value)/1e6),
          by = .(iso3, year_id, region)
          ][, .(val = median(value),
                lb = quantile(value, 0.25),
                ub = quantile(value, 0.75)),
            by = .(region)] |>
    ggplot(aes(x = region, y = val, fill = region)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = lb, ymax = ub),
                  color = "grey50",
                  width = 0.2, position = position_dodge()) +
    geom_text(aes(label = formatC(val, format = "f", digits = 2)),
              vjust = -1, size = 4) +
    labs(title = "OOP-MH - millions of 2023 USD",
         subtitle = "Median and IQR",
         x = "", y = "", fill = "") +
    theme_bw() +
    theme(
        legend.position = "top",
        axis.text.x = element_blank()
    )



