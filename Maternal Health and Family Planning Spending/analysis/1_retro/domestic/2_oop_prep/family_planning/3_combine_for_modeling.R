#
# combine RHSC data with the DHS data to finalize data for modeling
#
# we want to model the proportion of all women (of reproductive age) using
# a given contraceptive method (e.g., pill, IUD, etc.) from a given source
# (public or private).
#
library(data.table)
source(here::here("src", "r", "params.R"))


CONFIG <- list(
    files = list(
        locs = get_path("int", "data", c("covars", "gbd_locations.csv")),
        pops = get_path("int", "data", c("covars", "population.csv")) 
    ),
    dirs = list(
        input = get_path("int", "data", c("domestic", "int", "oop_fp"))
    )
)


#
# load population data to be used below
#
pops <- fread(CONFIG$files$pops)
## standard demographic reproductive population, and matches DHS population
repr_pops <- pops[age_group_name == "15-49 years" &
                      sex == "Female" &
                      level == 3 &
                      forecast == FALSE]




#
# DHS
#
dhs <- fread(file.path(CONFIG$dirs$input, "dhs_clean.csv"))
dhs <- dhs[, .(ihme_loc_id, year_id, method, private_source, public_source)]

#
# RHSC
#
# load rhsc data, filter to data that contains number of users per method-sector
rhsc <- fread(file.path(CONFIG$dirs$input, "rhsc_clean.csv"))
rhsc <- rhsc[indicator == "user", -"indicator"]

# merge on population data to calculate number of users as proportion of
# reproductive population, to match DHS data
rhsc <- merge(rhsc,
              repr_pops[, .(ihme_loc_id, year_id, repr_pop = population)],
              by = c("ihme_loc_id", "year_id"),
              all.x = TRUE)
rhsc[, value := value / repr_pop]

rhsc <- rhsc[, .(ihme_loc_id, year_id, method, sector, value)]
rhsc <- dcast(rhsc, ihme_loc_id + year_id + method ~ sector, value.var = "value")
rhsc[, `:=` (
    private_source = private_non_sub + private_sub,
    public_source = public,
    private_non_sub = NULL,
    private_sub = NULL,
    public = NULL
)]



#
# COMBINE DHS AND RHSC
#
dhs[, src := "dhs"]
rhsc[, src := "rhsc"]
alldata <- rbind(dhs, rhsc)
alldata <- melt(alldata,
                measure.vars= c("private_source", "public_source"),
                variable.name = "source")
alldata[, source := gsub("_source", "", source)]

# merge on reproductive population denominator
alldata <- merge(alldata,
                repr_pops[, .(ihme_loc_id, year_id, repr_pop = population)],
                by = c("ihme_loc_id", "year_id"),
                all.x = TRUE)

# save
alldata[, denominator := "Women of reproductive age (repr_pop)"]
fwrite(alldata, file.path(CONFIG$dirs$input,
                          "contraceptive_method_use.csv"))

