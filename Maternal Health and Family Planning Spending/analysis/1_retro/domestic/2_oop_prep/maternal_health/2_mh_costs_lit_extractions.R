library(data.table)
library(ggplot2)
library(patchwork)
source(here::here("src", "r", "params.R"))
source(here::here("src", "r", "internals.R"))

CONFIG <- list(
    dirs = list(
        working = get_path("int", "data", c("domestic", "int", "oop_mh"))
    )
)


#
# load auxiliary data
#
locs <- fread(get_path("int", "data", c("covars", "ihme_unfpa_locs.csv")))


#
# load lit review extracted data
#
extract <- fread(get_path("raw", "input",
                          c("mh_costs_lit_review",
                            "maternal_health_costs_extractions.csv")))
extract <- extract[drop != 1]

extract <- extract[, .(
    study = paste(`Author(s)`, `Year of publication`),
    country = `Country of study`,
    intervention = `Intervention(s) costed`,
    facility_owner = `Facility ownership`,
    facility_type = `Facility type`,
    visit = Visit,
    year = `Year of cost data`,
    cost = `Total costs reported`,
    currency = Currency,
    currency_year = `Currency Year`,
    oop_frac = `Out-of-pocket`,
    rurality = `Population covered`,
    rural_prop = `Rural Proportion`,
    national = `Nationally Representative`
)]

tmp <- extract[, .(study, country, intervention)]
tmp <- tmp[, .(
    country = paste(unique(country), collapse = "; "),
    intervention = paste(unique(intervention), collapse = "; ")
), by = study]
openxlsx::write.xlsx(tmp, "~/mh_oop_studies.xlsx")

#
# process data
#

# drop unneeded rows
extract <- extract[!is.na(cost) & cost != ""]

# clean up cost column
extract[, cost := as.numeric(gsub(",", "", cost))]

# clean up intervention type, drop unwanted interventions
extract[, care_type := fcase(
    # antenatal
    intervention == "Ante-natal care", "antenatal",
    # normal/vaginal delivery
    # - if delivery type not specified, group with normal delivery
    intervention %in% c("Vaginal delivery", "Normal delivery", "Delivery",
                        "Delivery (C/S not specifically stated)",
                        "Assisted vaginal delivery", "General delivery",
                        "Home delivery"),
    "delivery",
    # assume sepsis requires c-section 
    # assume complicated delivery requires c-section
    intervention %in% c("Caesarean delivery", "Sepsis", "Complicated delivery"),
    "c_section",
    intervention == "Post-natal care",
    "postnatal",
    # drop any other category
    rep_len(TRUE, .N), NA_character_
)]
extract <- extract[!is.na(care_type)]

# clean up facility 
extract[, facility := fcase(
    facility_owner %in% c("Government", "Public", "Mission"),
    "public_facility",
    facility_owner %in% c("Private", "Corporation"),
    "private_facility",
    facility_owner %in% c("Mixed: Public and private", "mixed"),
    "mixed_facility",
    facility_owner == "Home",
    "home",
    rep_len(TRUE, .N), NA_character_
)]

## drop the observations from the one study that reported
##   costs of antenatal and postnatal care at home
extract <- extract[!(facility == "home" &
                         care_type %in% c("antenatal", "postnatal"))]


# add iso codes
extract[, country := fcase(
    country == "Lao PDR", "Lao People's Democratic Republic",
    country == "Tanzania", "United Republic of Tanzania",
    rep_len(TRUE, .N), country
)]
extract <- merge(
    extract,
    locs[, .(country = ihme_location_name, iso3, super_region_name)],
    by = "country", all.x = TRUE
)
stopifnot(extract[is.na(iso3), .N] == 0)


#
# convert currency to 2023 USD
#
extract[, currency_orig := currency]  # currency will be consumed by currency_convert
extract[, cost_orig := cost]
extract[currency != "USD", currency := "LCU"]
extract[, curr_year := currency_year] # gets consumed by currency_convert

extract <- currency_convert(extract,
                            # what
                            col.loc = "iso3", col.value = "cost",
                            # from
                            col.currency = "currency", col.currency.year = "curr_year",
                            # to
                            base.unit = "USD", base.year = 2023)

extract[, currency := "2023 USD"]

#
# adjust costs
#

# adjust ante-natal care - if only reported cost was per-visit, multiply by 4
extract[care_type == "antenatal" & visit == "1", cost := cost * 4]

# apply oop_frac: in most cases 1, in some cases OOP was just a proportion of the
#  total reported cost
extract[, cost := cost * oop_frac]



#
# save
#
message("\n* Saving mh_costs_clean.csv to ", CONFIG$dirs$working)
fwrite(extract, file.path(CONFIG$dirs$working, "mh_costs_clean.csv"))



# generate some plots =========================================================
summ_type <- extract[, .(mean_cost = mean(cost),
                    median_cost = median(cost),
                    ncountry = length(unique(iso3)),
                    ncy = .N),
                by = .(care_type)]
summ_type[, lab := paste0("Countries = ", ncountry, "\nCountry-Years = ", ncy)]
summ_facility <- extract[, .(mean_cost = mean(cost),
                    median_cost = median(cost),
                    ncountry = length(unique(iso3)),
                    ncy = .N),
                by = .(facility)]
summ_facility[, lab := paste0("Countries = ", ncountry, "\nCountry-Years = ", ncy)]
summ_ft <- extract[, .(mean_cost = mean(cost),
                    median_cost = median(cost),
                    ncountry = length(unique(iso3)),
                    ncy = .N),
                by = .(care_type, facility)]
summ_ft[, lab := paste0("Countries = ", ncountry, "\nCountry-Years = ", ncy)]




pdf(here::here("data", "outputs", "figures", "mh_lit_extractions.pdf"),
    width = 12, height = 8)
ggplot(extract, aes(x = cost)) +
    geom_histogram(bins = 30) +
    geom_vline(data = summ_type, aes(xintercept = mean_cost, color = "mean")) +
    geom_vline(data = summ_type, aes(xintercept = median_cost, color = "median")) +
    geom_text(data = summ_type, aes(label = lab),
              x = Inf, y = Inf, hjust = 1, vjust = 1.1) +
    facet_wrap(~ care_type, scales = "free") +
    labs(title = "Distribution of OOP costs by type, 2023 USD")

ggplot(extract, aes(x = cost)) +
    geom_histogram(bins = 30) +
    geom_vline(data = summ_facility, aes(xintercept = mean_cost, color = "mean")) +
    geom_vline(data = summ_facility, aes(xintercept = median_cost, color = "median")) +
    geom_text(data = summ_facility, aes(label = lab),
              x = Inf, y = Inf, hjust = 1, vjust = 1.1) +
    facet_wrap(~ facility, scales = "free") +
    labs(title = "Distribution of OOP costs by facility, 2023 USD")


ggplot(extract, aes(x = cost)) +
    geom_histogram(bins = 30) +
    geom_vline(data = summ_ft, aes(xintercept = mean_cost, color = "mean")) +
    geom_vline(data = summ_ft, aes(xintercept = median_cost, color = "median")) +
    geom_text(data = summ_ft, aes(label = lab),
              x = Inf, y = Inf, hjust = 1, vjust = 1.1) +
    facet_wrap(~ care_type + facility, scales = "free") +
    labs(title = "Distribution of OOP costs by type and facility, 2023 USD")


ggplot(extract, aes(x = year, y = cost, color = iso3)) +
    geom_point() +
    facet_wrap(~care_type, scales = "free") +
    labs(title = "OOP costs by year, country, type")


ggplot(extract,
       aes(x = year, y = cost, color = country)) +
    geom_point() +
    facet_wrap(~facility, scales = "free") +
    labs(title = "OOP costs by year, country, facility")

invisible(capture.output(dev.off()))



# explore how costs vary by rurality ==========================================

extract[, value_id := fifelse(
    care_type %in% c("delivery", "c_section"),
    paste0(facility, "_", care_type),
    care_type
)]


# with nationally representative studies only
t1 <- extract[rurality %in% c("urban", "rural") | (rurality == "all" & national == "yes"),
              .(mean_cost = mean(cost),
                med_cost = median(cost),
                lb = quantile(cost, 0.25),
                ub = quantile(cost, 0.75),
                countries = paste(unique(country), collapse = ", "),
                N_obs = .N),
              by = .(value_id, population = rurality)]
setorder(t1, value_id, population)

t1_full <- data.table::CJ(value_id = unique(t1$value_id),
                          population = unique(t1$population))

t1 <- merge(t1_full, t1, all.x = TRUE)


# with all studies
t2 <- extract[,
              .(mean_cost = mean(cost),
                med_cost = median(cost),
                lb = quantile(cost, 0.25),
                ub = quantile(cost, 0.75),
                countries = paste(unique(country), collapse = ", "),
                N_obs = .N),
              by = .(value_id, population = rurality)]
setorder(t2, value_id, population)

t2_full <- data.table::CJ(value_id = unique(t2$value_id),
                          population = unique(t2$population))

t2 <- merge(t2_full, t2, all.x = TRUE)


sheets <- list(
    "nationally_representative" = t1,
    "all_studies" = t2
)
openxlsx::write.xlsx(sheets,
                     file = here::here("data", "outputs", "tables",
                                       "oop_costs_rurality.xlsx"))

# plots
t1[, value_id := factor(value_id, levels = c("antenatal",
                                             "public_facility_delivery",
                                             "public_facility_c_section",
                                             "private_facility_delivery",
                                             "private_facility_c_section",
                                             "mixed_facility_delivery",
                                             "mixed_facility_c_section",
                                             "home_delivery",
                                             "postnatal"))]


p1 <- ggplot(t1, aes(x = population, y = med_cost, fill = population)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = lb, ymax = ub, linetype = "IQR"), width = 0.2) +
    geom_point(aes(y = mean_cost, shape = "mean"),
               size = 2, color = "black") +
    facet_wrap(~value_id, scales = "free") +
    labs(title = "Median OOP costs by population",
         subtitle = "Nationally representative studies",
         x = "Population Covered", y = "2023 US$", shape = "", linetype = "") +
    theme(legend.position = "top") +
    guides(fill = "none")
    



t2[, value_id := factor(value_id, levels = c("antenatal",
                                             "public_facility_delivery",
                                             "public_facility_c_section",
                                             "private_facility_delivery",
                                             "private_facility_c_section",
                                             "mixed_facility_delivery",
                                             "mixed_facility_c_section",
                                             "home_delivery",
                                             "postnatal"))]

p2 <- ggplot(t2, aes(x = population, y = med_cost, fill = population)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = lb, ymax = ub, linetype = "IQR"), width = 0.2) +
    geom_point(aes(y = mean_cost, shape = "mean"),
               size = 2, color = "black") +
    facet_wrap(~value_id, scales = "free") +
    labs(title = "Median OOP costs by population",
         subtitle = "All studies",
         x = "Population Covered", y = "2023 US$", shape = "", linetype = "") +
    theme(legend.position = "top") +
    guides(fill = "none")
    

pdf(here::here("data", "outputs", "figures", "oop_costs_rurality.pdf"),
    width = 10, height = 10)
print(p1)
print(p2)
invisible(capture.output(dev.off()))

