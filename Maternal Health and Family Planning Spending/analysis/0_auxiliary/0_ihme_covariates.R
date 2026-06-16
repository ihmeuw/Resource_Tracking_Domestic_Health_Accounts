
library(data.table)
library(arrow)
source(here::here("src", "r", "params.R"))
source(here::here("src", "r", "internals.R"))
source_gbd_utils()


CONFIG <- list(
    dirs = list(
        ei_the = get_path("raw", "ei_the"),
        ei_gdp = get_path("raw", "ei_gdp"),
        out = get_path("int", "data", "covars")
    )
)

invisible(lapply(CONFIG$dirs, dir.create, recursive = TRUE, showWarnings = FALSE))


.release_id <- get_param("gbd_release_id") # GBD 2023
.location_set_id <- 35 # Model Results
.age_group_id <- "all"
.location_id <- "all"
.sex_id <- 3 # both
.year_id <- "all"
.tfr_covar_id <- 149 # TFR
.live_births_covar_id <- 60 # Live Births
.still_births_covar_id <- 2267 # Stillbirth (28 weeks) to live birth ratio
.anc4_covar_id <- 8 # Antenatal Care (4 visits) Coverage (proportion)
.pnc_covar_id <- 2503 # Coverage of Postnatal Care Visit (proportion)
.csection_covar_id <- 2381 # Caesarean Section Coverage (proportion)
.matcare_covar_id <- 2382 # Full Maternal Care Coverage (proportion)

age_groups <- get_ids("age_group")
sexes <- get_ids("sex")


#
# LOCATION METADATA ====
#
locs <- get_location_metadata(
    location_set_id = .location_set_id,
    release_id = .release_id
)
locs[, release_id := .release_id]

fwrite(locs, file.path(CONFIG$dirs$out, "gbd_locations.csv"))


# will be used below
locs_small <- locs[, .(location_id, level, ihme_loc_id, location_name)]


#
# POPULATION ESTIMATES ====
#

# retrospective estimates
popret <- get_population(
    age_group_id = .age_group_id,
    sex_id = c(1,2,3), # male, female, both
    location_id = .location_id,
    year_id = "all",
    release_id = .release_id
)

popret[, run_id := NULL]
popret[, forecast := FALSE]
# merge with locs
popret <- merge(popret, locs_small, by = "location_id", all.x = TRUE)
popret <- popret[level < 4] # drop subnat

# merge on age-group names
popret <- merge(popret, age_groups, by = "age_group_id", all.x = TRUE)

# merge on sex names
popret <- merge(popret, sexes, by = "sex_id", all.x = TRUE)

# finalize and save
setcolorder(popret, c("location_id", "location_name", "ihme_loc_id", "level",
                    "year_id", "age_group_id", "age_group_name", "sex_id", "sex",
                    "population", "forecast"))

fwrite(popret, file.path(CONFIG$dirs$out, "population.csv"))



# future estimates
pops <- get_population(
    age_group_id = .age_group_id,
    sex_id = c(1,2,3), # male, female, both
    location_id = .location_id,
    year_id = 1990:2050,
    forecasted_pop = TRUE,
    release_id = 16 # no forecast for release id 31, so use previous GBD release
)

pops[, run_id := NULL]
pops[, forecast := fifelse(year_id < 2025, FALSE, TRUE)]
# merge with locs
pops <- merge(pops, locs_small, by = "location_id", all.x = TRUE)
pops <- pops[level < 4] # drop subnat

# merge on age-group names
pops <- merge(pops, age_groups, by = "age_group_id", all.x = TRUE)

# merge on sex names
pops <- merge(pops, sexes, by = "sex_id", all.x = TRUE)

# finalize and save
setcolorder(pops, c("location_id", "location_name", "ihme_loc_id", "level",
                    "year_id", "age_group_id", "age_group_name", "sex_id", "sex",
                    "population", "forecast"))

fwrite(pops, file.path(CONFIG$dirs$out, "future_population.csv"))

#
# PREGNANT POPULATION ====
#
preg <- get_population(
    age_group_id = "all",
    sex_id = "all",
    location_id = "all",
    year_id = "all",
    release_id = .release_id,
    population_group_id = 16 # pregnant population
)

# merge with locs
preg <- merge(preg, locs_small, by = "location_id", all.x = TRUE)
preg <- preg[level == 3]

# add age group and sex names
preg <- merge(preg, age_groups, by = "age_group_id", all.x = TRUE)
preg <- merge(preg, sexes, by = "sex_id", all.x = TRUE)

fwrite(preg, file.path(CONFIG$dirs$out, "pregnant_population.csv"))



#
# TOTAL FERTILITY RATE ====
#

# retrospective estimates
tfrret <- get_covariate_estimates(
    covariate_id = .tfr_covar_id,
    age_group_id = .age_group_id,
    location_id = .location_id,
    sex_id = .sex_id,
    year_id = .year_id,
    release_id = .release_id
)
tfrret <- tfrret[, .(
    location_id,
    year_id,
    tfr = mean_value
)]


tfrret[, forecast := FALSE]
tfr <- tfrret

# merge with locs
tfr <- merge(tfr, locs_small, by = "location_id", all.x = TRUE)
tfr <- tfr[level < 4] # drop subnat

setcolorder(tfr, c("location_id", "location_name", "ihme_loc_id", "level",
                   "year_id", "tfr", "forecast"))

fwrite(tfr, file.path(CONFIG$dirs$out, "tfr.csv"))


#
# LIVE BIRTHS ====
#
lbirths <- get_covariate_estimates(
    covariate_id = .live_births_covar_id, # live births, in 1000s of births
    age_group_id = .age_group_id,
    location_id = .location_id,
    sex_id = .sex_id,
    year_id = .year_id,
    release_id = 31 ## need through 2026 for st-gpr modeling
)
lbirths <- merge(lbirths,
                 locs_small[, .(location_id, level, ihme_loc_id)],
                 by = "location_id", all.x = TRUE)
lbirths <- lbirths[level == 3] # keep countries only

fwrite(lbirths, file.path(CONFIG$dirs$out, "live_births.csv"))


#
# STILL BIRTH RATIO ====
#
sbr <- get_covariate_estimates(
    covariate_id = .still_births_covar_id, # still births / live births
    age_group_id = .age_group_id,
    location_id = .location_id,
    sex_id = .sex_id,
    year_id = .year_id,
    release_id = 31
)
sbr <- merge(sbr,
             locs_small[, .(location_id, level, ihme_loc_id)],
             by = "location_id", all.x = TRUE)
sbr <- sbr[level == 3] # keep countries only

fwrite(sbr, file.path(CONFIG$dirs$out, "still_birth_ratio.csv"))


#
# ANC4 COVERAGE ====
#
anc <- get_covariate_estimates(
    covariate_id = .anc4_covar_id, # ANC4 coverage
    age_group_id = .age_group_id,
    location_id = .location_id,
    sex_id = .sex_id,
    year_id = .year_id,
    release_id = 31
)
anc <- merge(anc,
             locs_small[, .(location_id, level, ihme_loc_id)],
             by = "location_id", all.x = TRUE)
anc <- anc[level == 3] # keep countries only

fwrite(anc, file.path(CONFIG$dirs$out, "anc4_coverage.csv"))


#
# PNC COVERAGE ====
#
pnc <- get_covariate_estimates(
    covariate_id = .pnc_covar_id, # PNC coverage
    age_group_id = .age_group_id,
    location_id = .location_id,
    sex_id = .sex_id,
    year_id = .year_id,
    release_id = 31
)
pnc <- merge(pnc,
             locs_small[, .(location_id, level, ihme_loc_id)],
             by = "location_id", all.x = TRUE)
pnc <- pnc[level == 3] # keep countries only

fwrite(pnc, file.path(CONFIG$dirs$out, "pnc_coverage.csv"))


#
# C-SECTION COVERAGE ====
#
csc <- get_covariate_estimates(
    covariate_id = .csection_covar_id, # c-section coverage
    age_group_id = .age_group_id,
    location_id = .location_id,
    sex_id = .sex_id,
    year_id = .year_id,
    release_id = 31
)
csc <- merge(csc,
             locs_small[, .(location_id, level, ihme_loc_id)],
             by = "location_id", all.x = TRUE)
csc <- csc[level == 3] # keep countries only

fwrite(csc, file.path(CONFIG$dirs$out, "csection_coverage.csv"))



#
# FULL MATERNAL CARE COVERAGE ====
#
# proportion of women receiving the full spectrum of maternal care
# (>4 antenatal care visits, skilled birth attendance, in-facility delivery,
#  and postnatal care)	
mat <- get_covariate_estimates(
    covariate_id = .matcare_covar_id,
    age_group_id = .age_group_id,
    location_id = .location_id,
    sex_id = .sex_id,
    year_id = .year_id,
    release_id = 31
)
mat <- merge(mat,
             locs_small[, .(location_id, level, ihme_loc_id)],
             by = "location_id", all.x = TRUE)
mat <- mat[level == 3] # keep countries only

fwrite(mat, file.path(CONFIG$dirs$out, "maternal_care_coverage.csv"))



#
# MEAN SHEEP DENSITY ====
#
# mean sheep density (proxy for rurality?)
.sheep_covar_id <- 2358
sheep <- get_covariate_estimates(
    covariate_id = .sheep_covar_id,
    age_group_id = .age_group_id,
    location_id = .location_id,
    sex_id = .sex_id,
    year_id = .year_id,
    release_id = .release_id
)
sheep <- merge(sheep,
               locs_small[, .(location_id, level, ihme_loc_id)],
               by = "location_id", all.x = TRUE)
sheep <- sheep[level == 3] # keep countries only

fwrite(sheep, file.path(CONFIG$dirs$out, "sheep_density.csv"))



#
# PROPORTION URBAN ====
#
# Proportion of the location that is urban, by population-weighting a binary
# urbanicity value (location is either urban or not). Values in covariate are
# between 0 and 1 (not binary), due to population weighting.	
.urban_covar_id <- 854
urb <- get_covariate_estimates(
    covariate_id = .urban_covar_id,
    age_group_id = .age_group_id,
    location_id = .location_id,
    sex_id = .sex_id,
    year_id = .year_id,
    release_id = .release_id
)
urb <- merge(urb,
             locs_small[, .(location_id, level, ihme_loc_id)],
             by = "location_id", all.x = TRUE)
urb <- urb[level == 3] # keep countries only

fwrite(urb, file.path(CONFIG$dirs$out, "urbanicity.csv"))




#
# HEALTH EXPENDITURE ====
#

load_he <- function(nm) {
    # retrospective estimates of domestic health spending
    # - last year of data is 2022
    dt <- fread(get_path("raw", "ei_the", c("stats", paste0(nm, "_totes.csv"))))
    dt <- dt[scenario == 0, -"scenario"]
    dt[, indicator := nm]
    dt <- dt[between(year, 1995, 2050)]
    return(dt)
}


ghe <- load_he("ghes")
oop <- load_he("oop")
ppp <- load_he("ppp")
he <- rbind(ghe, oop, ppp)
setnames(he, c("iso3", "year"), c("ihme_loc_id", "year_id"))


# load dah
dahr <- fread("/FILEPATH/dah_by_channel_hfa_recip_1990_2050.csv")
dahr <- dahr[, .(mean = sum(dah)), by = .(year, recip)]
setnames(dahr, c("recip", "year"), c("ihme_loc_id", "year_id"))
dahr <- dahr[between(year_id, 1995, 2050)]
setorder(dahr, year_id)

dahr[!ihme_loc_id %in% unique(he$ihme_loc_id),
     ihme_loc_id := "global_inkind_unalloc_dah"]
dahr <- dahr[, .(mean = sum(mean)), by = .(year_id, ihme_loc_id)]

dahr_all <- unique(he[, .(year_id, ihme_loc_id)])
dahr_all <- merge(dahr_all, dahr, by = c("year_id", "ihme_loc_id"), all.x = TRUE)
setnafill(dahr_all, fill = 0, cols = "mean")
dahr_all[, `:=`(
    indicator = "dah",
    lower = mean,
    upper = mean
)]


the <- rbind(he, dahr_all)
the[indicator == "ghe", indicator := "ghes"]
the[, indicator := paste0(indicator, "_totes")]

the <- merge(
    the,
    locs_small[, .(ihme_loc_id, location_id, location_name, level)],
    by = "ihme_loc_id", all.x = TRUE
)
the[, currency := "2023 USD"]


# make THE
the_totes <- the[, .(
    the_totes = sum(mean)
), by = .(ihme_loc_id, year_id, location_id, level, location_name, currency)]

fwrite(the_totes, file.path(CONFIG$dirs$out, "the_totes.csv"))

for (ind in unique(the$indicator)) {
    dt <- the[indicator == ind, -"indicator"]
    setnames(dt, "mean", ind)
    fwrite(dt, file.path(CONFIG$dirs$out, paste0(ind, ".csv")))
}





#
# GDP ====
#
gdp <- fread(
    file.path(CONFIG$dirs$ei_gdp, "total_gdppc_stats_scenarios_2023USD.csv")
)
gdp <- gdp[scenario == 4.5, -"scenario"]
setnames(gdp,
         c("iso3", "year", "mean"),
         c("ihme_loc_id", "year_id", "gdp_pc"))

# merge with locs
gdp <- merge(gdp, locs_small, by = "ihme_loc_id", all.x = TRUE)
gdp[, currency := "2023 USD"]

fwrite(gdp, file.path(CONFIG$dirs$out, "gdp_pc.csv"))
