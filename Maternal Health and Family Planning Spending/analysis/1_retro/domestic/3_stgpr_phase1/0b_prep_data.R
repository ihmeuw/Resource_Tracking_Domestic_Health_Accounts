#
# prep input data for st-gpr steps
#
library(data.table)
library(arrow)
source(here::here("src", "r", "params.R"))
source(here::here("src", "r", "stgpr_utils.R"))
source(here::here("analysis", "1_retro", "domestic", "stgpr_configs.R"))


CONFIG <- list(
    files = list(
        rh = get_path("int", "data", c("domestic", "int", "rh_extracted.csv")),
        chn_fp = get_path("int", "data", c("domestic", "int", "china_public_fp.csv")),
        cu = get_path("int", "data",
                      c("domestic", "int", "oop_fp", "contraceptive_method_use.csv")),
        mh = get_path("int", "data",
                      c("domestic", "int", "oop_mh", "dhs_mh_clean.csv"))
    ),
    dirs = list(
        covars = get_path("int", "data", "covars"),
        working = get_path("int", "data", c("domestic", "int", "stgpr"))
    )
)

invisible(lapply(CONFIG$dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

#
# aux data
#
locs <- load_locations(cache_path = file.path(CONFIG$dirs$working,
                                              "location_cache.feather"),
                       location_set_id = stgpr_config_base$location_set_id,
                       release_id = stgpr_config_base$release_id,
                       rebuild = FALSE)


#
#    DOMESTIC EXPENDITURES ON REPRODUCTIVE HEALTH ============================= 
#
rh <- fread(CONFIG$files$rh)
rh <- rh[year_id >= 2000]

## maternal health = dis2.1, family planning = dis2.3
rh <- rh[value_code %in% c(
    "fs_domestic_public_dis2.1",
    "fs_domestic_public_dis2.3",
    "fs_domestic_private_oop_dis2.1",
    "fs_domestic_private_oop_dis2.3"
)]
rh[, value_id := gsub("private_", "", gsub("fs_domestic_", "", value_code))]

rh <- rh[!is.na(value),
         .(ihme_loc_id = iso3, year_id, value_id, value, data_src = src)]

## add china's data
chn <- fread(CONFIG$files$chn_fp)
chn <- chn[year_id >= 2000]
rh <- rbind(
    rh,
    chn[, .(ihme_loc_id = "CHN", year_id,
            value_id = "public_dis2.3", value, data_src = src)]
)


#
# add health expenditure data
#
series <- c("oop_totes", "ghes_totes")
for (s in series) {
    if (s %in% names(rh))
        rh[, (s) := NULL]
    tmp <- arrow::read_feather(file.path(
        CONFIG$dirs$working, "denominators", paste0(s, ".feather")
    ))
    rh <- merge(rh,
                tmp[, .(ihme_loc_id, year_id, he = get(s))],
                by = c("ihme_loc_id", "year_id"),
                all.x = TRUE)
    setnames(rh, "he", s)
}



#
# convert values to proportion of health expenditure
#
rh[, denominator := fcase(
    value_id %like% "oop", oop_totes,
    value_id %like% "public", ghes_totes
)]
rh[, denom_name := fcase(
    value_id %like% "oop", "oop_totes",
    value_id %like% "public", "ghes_totes"
)]
rh[, proportion := value / denominator]

rh[, `:=`(
    oop_totes = NULL,
    ghes_totes = NULL
)]


#
# drop extreme outliers
#
# ASSUMPTION
# even though we have an outliering step, these values are very extreme
rh <- rh[proportion <= 0.5]



#
#    CONTRACEPTIVE USE ========================================================
#
cu <- fread(CONFIG$files$cu)
cu <- cu[year_id >= 2000]
cu <- cu[source == "private"] ## only interested in private use, paid for OOP
cu[, value_id := paste0(source, "_", method)]
cu <- cu[, .(ihme_loc_id,
             year_id,
             value_id,
             proportion = value,
             data_src = src)]

# don't use RHSC's own modeled data
cu <- cu[data_src != "rhsc"]

#
# add reproductive population denominator
#
repr_pops <- arrow::read_feather(file.path(CONFIG$dirs$working, "denominators",
                                           "repr_pops.feather"))

cu <- merge(cu,
            repr_pops[, .(ihme_loc_id, year_id, denominator = repr_pops)],
            by = c("ihme_loc_id", "year_id"), all.x = TRUE)
cu[, denom_name := "repr_pops"]
cu[, value := proportion * denominator]



#
#    MATERNAL HEALTH UTILIZATION ==============================================
#
mh <- fread(CONFIG$files$mh)
mh <- mh[year_id >= 2000]
# drop non-gbd countries - i.e., those for which we have no birth data
mh <- mh[!is.na(total_births)]
mh <- mh[, .(ihme_loc_id,
             year_id,
             value_id,
             value = value * total_births,
             data_src = "dhs")]

# add denominator
total_births <- arrow::read_feather(file.path(CONFIG$dirs$working, "denominators",
                                              "total_births.feather"))

mh <- merge(mh,
            total_births[, .(ihme_loc_id, year_id, denominator = total_births)],
            by = c("ihme_loc_id", "year_id"),
            all.x = TRUE)
mh[, denom_name := "total_births"]
mh[, proportion := value / denominator]


#
# COMBINE ALL DATA ============================================================
#

rh[, model_group := "rh_spending"]
cu[, model_group := "contraceptive_use"]
mh[, model_group := "maternal_health"]
alldata <- rbind(rh, cu, mh)


#
# FINALIZE DATA ===============================================================
#

# add location metadata
alldata <- merge(alldata,
                 unique(locs[, .(ihme_loc_id, location_id,
                                 region_name, super_region_name)]),
                 by = "ihme_loc_id",
                 all.x = TRUE)



# Drop countries with missing location data (i.e., Kosovo/any non-GBD countries)
alldata <- alldata[ihme_loc_id != "XKK"]
stopifnot(alldata[is.na(location_id), .N] == 0)
stopifnot(alldata[is.na(denominator), .N] == 0)


# Drop observations with missing outcome data
# ASSUMPTION
## Assume recorded 0s are missing data
alldata <- alldata[!is.na(proportion) & proportion > 0]


# Set sample size
# ASSUMPTION
alldata[, sample_size := 10]


# Finalize
## required columns and values for ST-GPR.
## some columns are created by further steps in this pipeline
fin <- alldata[, .(
    model_group,
    value_id,
    ihme_loc_id,
    data_src,
    nid = 560199,
    location_id,
    year_id,
    age_group_id = stgpr_config_base$age_group_id,
    sex_id = stgpr_config_base$sex_id,
    model_value = proportion,
    model_value_denom = denominator,
    model_value_descr = "proportion of denominator",
    denom_name,
    value,
    sample_size
)]

message("Saving stgpr_all_data.csv to ", CONFIG$dirs$working)
fwrite(fin, file.path(CONFIG$dirs$working, "stgpr_all_data.csv"))

message("Done.")
