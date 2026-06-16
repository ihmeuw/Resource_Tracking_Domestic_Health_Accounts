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
        oop_mh = get_path("int", "data",
                          c("domestic", "int", "oop_mh", "oop_mh_observed.csv"))
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

#
#    OOP SPENDING ON MATERNAL HEALTH ==========================================
#

oop_mh <- fread(CONFIG$files$oop_mh)

# merge on denominator
oop <- arrow::read_feather(file.path(CONFIG$dirs$working, "denominators",
                                     "oop_totes.feather"))
oop_mh <- merge(oop_mh,
                oop[, .(ihme_loc_id, year_id, oop_totes_denom = oop_totes)],
                by = c("ihme_loc_id", "year_id"),
                all.x = TRUE)

if (oop_mh[oop_totes != oop_totes_denom, .N] != 0)
    stop("We assume oop_mh estimates are based on oop_totes, but they don't match.")
oop_mh[, oop_totes_denom := NULL]

setnames(oop_mh,
         c("oop_totes",  "oop_mh"),
         c("denominator", "value"))
oop_mh[, proportion := value / denominator]

oop_mh[, `:=`(
    value_id = paste0("oop_", value_id),
    denom_name = "oop_totes",
    data_src = "modeled_lit_dhs"
)]

oop_mh[, variance := var(proportion), by = .(value_id)]
oop_mh[, loc_var := var(proportion), by = .(value_id, ihme_loc_id)]
oop_mh[!is.na(loc_var), variance := variance + loc_var]
oop_mh[, num_study := uniqueN(study), by = value_id]
oop_mh[, scale := (max(num_study)) + 2 - num_study]
oop_mh[, variance := variance * scale]





#
#    OOP SPENDING ON FAMILY PLANNING ==========================================
#
# ....


#
#    COMBINE ==================================================================
#
oop_mh[, model_group := "oop_mh"]
# oop_fp[, model_group := "oop_fp"]
alldata <- rbind(oop_mh) #, oop_fp)


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
    variance,
    denom_name,
    value,
    sample_size
)]


# SAVE
message("Updating stgpr_all_data.csv in ", CONFIG$dirs$working)

all <- fread(file.path(CONFIG$dirs$working, "stgpr_all_data.csv"))
all[, `:=`(variance = NA_real_)]
all <- all[! value_id %in% unique(fin$value_id)]

fin[, `:=`(is_outlier = 0, trend_variance = NA_real_)]

all <- rbind(all, fin)

fwrite(all, file.path(CONFIG$dirs$working, "stgpr_all_data.csv"))

message("Done.")

