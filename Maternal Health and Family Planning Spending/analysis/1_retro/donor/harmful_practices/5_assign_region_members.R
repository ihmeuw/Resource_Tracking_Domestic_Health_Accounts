#
# Determines which countries make up each region in the underlying data so that
#   regional level projects can be assigned to individual countries. 
#
# There are 4 categories of regions:
#   - GBD super-regions (which get broken into GBD regions)
#   - GBD regions
#   - Composite GBD regions
#   - Special regions
#
# GBD regions and super-regions can be directly mapped to countries using
# GBD location metadata (the assignments have already been done for us).
# Composite GBD regions are used when a project recipient is a group of GBD
# regions or super-regions, but not an individual one. Again, the GBD location
# metadata provides us with the member countries.
# Special regions have to be handled on a case-by-case basis.
#

library(arrow)
library(data.table)

source(here::here("src", "r", "params.R"))
source(here::here("src", "r", "logging.R"))

CONFIG <- list(
    files = list(
        oda_regional = get_path("int", "data",
                                c("oda", "int", "oda_hp_regional.feather")),
        locs = get_path("int", "data", c("covars", "ihme_unfpa_locs.csv")),
        gbd_locs = get_path("int", "data", c("covars", "gbd_locations.csv")),
        output_reg = get_path("int", "data",
                              c("oda", "int", "region_mappings.csv")),
        output_sr  = get_path("int", "data",
                              c("oda", "int", "sr_to_region_mappings.csv"))
    )
)
log_level("INFO")
log_info("Create region assigment data")


#
# create mappings for GBD regions =============================================
#
# location metadata
gbd_loc <- fread(CONFIG$files$gbd_locs)[level <= 3] # countries, regions, and super-regions

locs <- fread(CONFIG$files$locs) # IHME & UNFPA locations

# create mapping from country iso code to region & super-region code
## merge on region code
countries <- merge(
    locs[, .(iso3, region_name, super_region_name)],
    gbd_loc[level == 2, .(region_code = ihme_loc_id, location_name)],
    by.x = "region_name", by.y = "location_name",
    all.x = TRUE
)
## merge on super-region code
countries <- merge(
    countries,
    gbd_loc[level == 1, .(super_region_code = ihme_loc_id, location_name)],
    by.x = "super_region_name", by.y = "location_name",
    all.x = TRUE
)

# create mapping from super-region to region list
sr_to_region <- countries[, .(region_code = paste(unique(region_code),
                                                     collapse = ",")),
                             by = super_region_code]


#
# Load and prep oda data ======================================================
#
log_info("* Identifying regions in data")

# oda data from previous stage - contains regional level projects
oda <- as.data.table(arrow::read_feather(CONFIG$files$oda_regional))

#
# isolate regional projects and pull out list of region ids
#
oda <- merge(oda,
             locs[, .(recip_iso = iso3, is_region = FALSE)],
             by = "recip_iso", all.x = TRUE)
oda[is.na(is_region), is_region := TRUE]

regs <- unique(oda[is_region == TRUE, .(comp_id = recip_iso)])

## convert super-regions into regions
### if a list of SRs, expand into individual
sr_exp <- data.table(
    comp_id = unlist(strsplit(grep("^S", regs$comp_id, value = TRUE), ","))
)
regs <- regs[!grepl("^S", comp_id)]
regs <- rbind(regs, sr_exp)
### if comp_id is a SR, convert to the comma sep list of regions
regs <- merge(regs, sr_to_region,
              by.x = "comp_id", by.y = "super_region_code",
              all.x = TRUE)
regs[!is.na(region_code), comp_id := region_code]
regs[, region_code := NULL]

## explode composite regions into individual rows
## e.g., comp_id = "R7,R12", id = c("R7", "R12")
regs <- regs[,  .(id = unlist(tstrsplit(comp_id, ","))), by = comp_id]


#
# identify the member countries of each region ===============================
#
log_info("* Identifying region member countries")

#
# Start with gbd regions and use location hierarchy to get member countries
#
## merge on all countries within each region
## - must allow.cartesian since each region has multiple countries
reg_mems_gbdr <- merge(regs,
                       countries[, .(id = region_code, iso3)],
                       by = "id",
                       all.x = TRUE,
                       allow.cartesian = TRUE)
## drop special regions
reg_mems_gbdr <- reg_mems_gbdr[!grepl("^_", comp_id)]

## after dropping special regs, all regions should have at least one country
stopifnot( nrow(reg_mems_gbdr[is.na(iso3)]) == 0 )


#
# Manually allocate countries to the special regions
#

# prep: identify countries in some of the broad special regions programmatically
#       when possible
#
countries <- gbd_loc[level == 3, .(ihme_loc_id, location_name, region_name,
                                   super_region_name)]
# NORTH OF SAHARA
countries[, north_of_sahara := FALSE]
## "north africa and middle east" region countries in north africa
countries[location_name %in% c("Libya", "Morocco", "Algeria", "Egypt",
                               "Sudan", "Tunisia"),
          north_of_sahara := TRUE]

# AFRICA
countries[, africa := FALSE]
countries[super_region_name == "Sub-Saharan Africa" | north_of_sahara == TRUE,
          africa := TRUE]

# ASIA
countries[, asia := FALSE]
countries[region_name %in% c("Central Asia", "High-income Asia Pacific",
                             "South Asia", "East Asia", "Southeast Asia"),
          asia := TRUE]

#
# create mappings from special id to corresponding country ids
reg_special <- list(
    data.table(
        id = "_Africa",
        iso3 = countries[africa == TRUE, ihme_loc_id]
    ),
    data.table(
        id = "_NorthOfSahara",
        iso3 = countries[north_of_sahara == TRUE, ihme_loc_id]
    ),
    data.table(
        id = "_Asia",
        iso3 = countries[asia == TRUE, ihme_loc_id]
    ),
    data.table(
        # https://www.eac.int/eac-partner-states
        id = "_EAC", # East African Community region
        iso3 = c("COD", "BDI", "KEN", "RWA", "SOM", "SSD", "UGA", "TZA")
    ),
    data.table(
        id = "_ExYugoslavia",
        iso3 = c("BIH", "HRV", "MKD", "MNE", "SRB", "SVN")
    ),
    data.table(
        id = "_Melanesia",
        iso3 = c("FJI", "SLB", "VUT", "PNG")
    ),
    data.table(
        id = "_Micronesia",
        iso3 = c("FSM", "MHL", "PLW", "KIR", "MNP", "NRU", "GUM")
    ),
    data.table(
        id = "_Polynesia",
        iso3 = c("ASM", "COK", "NIU", "WSM", "TKL", "TON", "TUV")
    )
)

reg_mems_special <- rbindlist(reg_special)
reg_mems_special[, comp_id := id]


#
# combine all region assignments & save =======================================
#
reg_mems <- rbindlist(list(reg_mems_gbdr, reg_mems_special),
                      use.names = TRUE)


## test that all regions have been assigned out
log_info("* Testing all regions are covered...")
regs_test <- merge(
    unique(regs[, .(comp_id)]),
    reg_mems,
    by = "comp_id",
    all = TRUE
)
regs_test[comp_id == "_QZA", iso3 := ""] # ignore the "unallocated" reg

if ( any(is.na(regs_test$iso3)) ) {
    unassigned <- regs_test[is.na(iso3), comp_id]
    log_warn("Unassigned regions: ", paste(unassigned, collapse = "; "))
    stop("Some regions have not been matched to any countries")
}

sr_ids <- gbd_loc[level == 1, ihme_loc_id]
r_ids <- gbd_loc[level == 2, ihme_loc_id]

if ( any(reg_mems$iso3 %in% sr_ids) ||
     any(reg_mems$iso3 %in% r_ids) ||
     any(reg_mems$iso3 %in% unique(reg_mems_special$id))
){
    log_warn("Regions should only be assigned to countries")
    log_error("Some regions have been assigned to other regions")
    stop("Regions should only be assigned to countries")
}

log_info("** success!")


log_info("* Writing region-country mappings to {CONFIG$files$output_reg}")
fwrite(reg_mems, CONFIG$files$output_reg)

log_info("* Writing region-super_region mappings to {CONFIG$files$output_sr}")
fwrite(sr_to_region, CONFIG$files$output_sr)


