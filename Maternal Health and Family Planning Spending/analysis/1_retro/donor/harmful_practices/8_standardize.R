#
# Standardize final ODA harmful practice data into format required for analysis
#
library(data.table)

source(here::here("src", "r", "params.R"))
source(here::here("src", "r", "internals.R"))

CONFIG <- list(
    files = list(
        oda_hp = get_path("int", "data", c("oda", "fin", "oda_harmful_practices.csv"))
    ),
    dirs = list(
        input = get_path("raw", "input"),
        covars = get_path("int", "data", "covars"),
        output = get_path("int", "data", c("oda", "fin"))
    )
)


# Process ODA harmful practice data
#
#
obs_dt <- fread(CONFIG$files$oda_hp)
# get deflated disbursement columns
dfl_cols <- grep("dfl23_", names(obs_dt), value = TRUE)


#
# standardize source of funds
#
obs_dt[, source := income_sector]
obs_dt[, source := fcase(
    # multilaterals
    donor_name == "African Development Bank", "AfDB",
    donor_name == "African Development Fund", "AfDB",
    donor_name == "Asian Development Bank", "AsDB",
    donor_name == "Global Alliance for Vaccines and Immunization", "GAVI",
    donor_name == "Global Fund", "GFATM",
    donor_name == "Inter-American Development Bank", "IDB",
    donor_name == "International Development Association", "WB_IDA",
    donor_name == "UNAIDS", "UNAIDS",
    donor_name == "UNFPA", "UNFPA",
    donor_name == "UNICEF", "UNICEF",
    donor_name == "WHO-Strategic Preparedness and Response Plan", "WHO",
    donor_name == "World Health Organisation", "WHO",
    donor_name == "EU Institutions", "EC",
    # foundations
    donor_name == "Gates Foundation", "GATES",
    # other adjustments
    source == "OTHERDAC", "OTHERPUB",
    rep_len(TRUE, .N), source
)]


#
# create groupings for channel of funds
#
obs_dt[, channel_group := fcase(
    between(channel_code, 10000, 20000-1), "BILAT",
    between(channel_code, 20000, 30000-1), "NGO",
    between(channel_code, 30000, 40000-1), "PPP",
    between(channel_code, 40000, 51000-1), "MULTI",
    between(channel_code, 51000, 60000-1), "ACADEMIC",
    between(channel_code, 60000, 90000-1), "PRIVATE",
    channel_code >= 90000, "OTHER",
    default = NA
)]
obs_dt[is.na(channel_code) | channel_code == 0, channel_group := "OTHER"]
stopifnot(obs_dt[is.na(channel_group), .N] == 0)

## hand-label some specific channel types
channel_specs <- as.data.table(openxlsx::read.xlsx(
    file.path(CONFIG$dirs$input, "oecd_channels_map.xlsx")
))
obs_dt <- merge(obs_dt,
                channel_specs[, .(channel_name, channel_specific)],
                by = "channel_name", all.x = TRUE)
obs_dt[is.na(channel_specific), channel_specific := channel_group]
obs_dt[channel_name %like% "United Nation|UN" & channel_specific == "MULTI",
       channel_specific := "OTHER_UN"]
obs_dt[channel_specific == "UN", channel_specific := "OTHER_UN"]
obs_dt[channel_name %ilike% "development bank" & channel_specific == "MULTI",
       channel_specific := "OTHER_DEVBANK"]
obs_dt[channel_specific == "DEVBANK", channel_specific := "OTHER_DEVBANK"]


## adjust source-channel pairs
obs_dt[source == "GATES" & channel_specific == "BILAT",
       channel_specific := "GATES"]
obs_dt[source == "PRIVATE" & channel_specific == "BILAT",
       channel_specific := "PRIVATE"]
#### last disbursing entity is the donor, so use donor info to assign channel
obs_dt[source == "OTHER" & channel_specific == "BILAT",
       channel_specific := "src"]
obs_dt[channel_specific == "src" & (
    donor_name %ilike% "development bank" |
        donor_name == "Asian Infrastructure Investment Bank" |
        donor_name == "Arab Bank for Economic Development in Africa" |
        donor_name == "Central American Bank for Economic Integration"
    ),
    channel_specific := "OTHER_DEVBANK"]
obs_dt[channel_specific == "src" & donor_name %in% c(
    "IFAD", "UN Peacebuilding Fund", "UNDP", "Food and Agriculture Organization"
), channel_specific := "OTHER_UN"]
obs_dt[channel_specific == "src", # left overs - basically just multilat funds
       channel_specific := "MULTI"]

## finalize
obs_dt[channel_specific == "MULTI", channel_specific := "OTHER_MULTI"]


obs_dt <- obs_dt[year >= 2000,
                 lapply(.SD, sum, na.rm = TRUE),
                 .SDcols = dfl_cols,
                 by = .(year, source, channel = channel_specific, recip_iso)]
setorder(obs_dt, year)


#
# Reshape long
#
obs_dt <- melt(obs_dt,
               id.vars = c("year", "source", "channel", "recip_iso"),
               measure.vars = dfl_cols,
               variable.name = "focus",
               value.name = "value")
obs_dt[, focus := gsub("dfl23_disb_", "", focus)]
obs_dt <- obs_dt[! focus %in% c("total", "hp")] # drop aggregate categories
obs_dt <- obs_dt[value > 0]



#
# Disaggregate donors that should be channels within our framework
#
## load income shares for these channels from DAH data
dah <- fread(
    "/FILEPATH/fgh_figure_data.csv"
)
dah_income_shares <- dah[, .(dah = sum(dah)), by = .(year, source, channel)]
dah_income_shares[, source_frac := dah / sum(dah), by = .(year, channel)]
dah_income_shares <- dah_income_shares[channel %in% c(
    "UNICEF", "UNFPA", "WHO", "PAHO", "UNAIDS", "GAVI", "GFATM",
    "AsDB", "IDB", "WB_IDA", "EC"
)]


disagg_dt <- obs_dt[source %in% unique(dah_income_shares$channel)]
disagg_dt <- disagg_dt[, .(value = sum(value)),
                       by = .(year, channel = source, focus, recip_iso)]
disagg_dt <- merge(
    disagg_dt,
    dah_income_shares[, .(year, channel, source, source_frac)],
    by = c("year", "channel"),
    all.x = TRUE,
    allow.cartesian = TRUE
)
disagg_dt[, `:=`(
    value = value * source_frac,
    source_frac = NULL
)]

obs_dt <- rbind(
    obs_dt[! source %in% unique(dah_income_shares$channel)],
    disagg_dt
)



#
# Finalize
#
obs_dt <- obs_dt[, .(value = sum(value)),
                 by = .(year, source, channel, focus, recip_iso)]
obs_dt[, currency := "2023 USD"]
setorder(obs_dt, year)


fwrite(
    obs_dt,
    file.path(CONFIG$dirs$output, "oda_hp_standardized.csv")
)
