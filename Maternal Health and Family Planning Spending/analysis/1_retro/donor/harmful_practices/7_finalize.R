#
# Final processing of ODA flows to prepare output for analysis
#
library(data.table)
library(arrow)

source(here::here("src", "r", "params.R"))
source(here::here("src", "r", "logging.R"))
source(here::here("src", "r", "internals.R"))


CONFIG <- list(
    files = list(
        oda_split = get_path("int", "data",
                             c("oda", "int", "oda_region_split.feather")),
        output = get_path("int", "data",
                          c("oda", "fin", "oda_harmful_practices.csv"))
    ),
    dirs = list(
        backup = get_path("int", "data", c("oda", "backup"))
    )
)


log_info("Finalizing ODA for Harmful Practices data")

oda <- as.data.table(arrow::read_feather(CONFIG$files$oda_split))
disb_cols <- grep("^disb_", names(oda), value = TRUE)
## drop very small disbursements that are due to estimation
oda <- oda[disb_total >= 1, ]



log_info("* Adding deflated disbursements")
## deflate disbursements to 2022 usd

## if donor is not a country and we don't have iso, assume USA conversion rates 
oda[, curr_iso := fifelse(is.na(donor_iso), "USA", donor_iso)]

for (col in disb_cols) {
    log_info("** Converting '{col}' to 2023 USD")
    oda[, curr_year := year] # currency_coversion consumes this
    oda[, paste0("dfl23_", col) := get(col)]
    oda <- currency_convert(oda,
                            col.loc = "curr_iso",
                            col.value = paste0("dfl23_", col),
                            # from
                            currency = "usd", col.currency.year = "curr_year",
                            # to
                            base.unit = "usd", base.year = 2023)
}


# Finalize:
oda[, c("curr_iso", "flow_id", "comp_id", "region_total_disb", "loc_reg_frac") := NULL]

oda[, flow_id := .I]
setcolorder(oda,
            c("flow_id", "year",
              # donor_name + features
              "donor_name", "donor_code", "donor_iso", "donor_type",
              "income_sector", "income_type",
              # channel_name + features
              "channel_name", "channel_code",
              # recipient_name + features
              "recipient_name", "recip_iso", "is_regional",
              # additional flow features
              "sector_category"
              # "treat_hp_projs", "prev_hp_projs",
              # "treat_prev_hp_projs", "total_hp_projs"
              ))

# Level of data:
# flow_id = [year X donor_name X channel_name X recipient_name X sector_category]
#  disb_total: total ODA disbursed at this level
#  disb_hp: total ODA for harmful practices disbursed at this level
#  disb_hp_*: total ODA for harmful practices disaggregated by gbv, fgm, ecm, other
#  dfl23_*: disbursement variables, deflated wrt 2022
log_info("* Writing to {CONFIG$files$output}")
fwrite(oda, CONFIG$files$output)

# backup
date_str <- format(Sys.Date(), "%Y_%m_%d")
fp <- file.path(CONFIG$dirs$backup,
                paste0("oda_harmful_practices__", date_str, ".feather"))
log_info("** Backing up to {fp}")
arrow::write_feather(oda, fp)

log_info("* Done.")


if (interactive()) {
    library(ggplot2)
    
    oda[, .(disb_hp = sum(disb_hp),
            disb_hp_23 = sum(dfl23_disb_hp, na.rm = T)),
        by = year] |>
        ggplot(aes(x = year)) +
        geom_line(aes(y = disb_hp, color = "disb_hp")) +
        geom_line(aes(y = disb_hp_23, color = "disb_hp_23"))
    
    oda[, lapply(.SD, sum), .SDcols = grep("^disb_hp_", names(oda)), by = year] |>
        melt(id.vars = "year") |>
        ggplot(aes(x = year, y = value, fill = variable)) +
        geom_bar(stat = "identity")
    
    oda[, .(disb_hp = sum(disb_hp),
            disb_total = sum(disb_total)),
        by = year] |>
        ggplot(aes(x = year, y = disb_hp / disb_total)) +
        geom_line()
    
    old <- arrow::read_feather(file.path(CONFIG$dirs$backup,
                                         "oda_harmful_practices__2025_01_07.feather"))
    setDT(old)
    cmp <- merge(
        old[, .(disb_hp_old = sum(disb_hp), disb_tot_old = sum(disb_total)), by = .(recip_iso, year)],
        oda[, .(disb_hp = sum(disb_hp), disb_tot = sum(disb_total)), by = .(recip_iso, year)],
        by = c("recip_iso", "year"),
        all = TRUE
    )
    
    cmp[, diff := disb_tot - disb_tot_old]
    ex_iso <- cmp[order(-abs(diff))][1:20, recip_iso]
    ex_iso <- c(ex_iso, "SYC", "KNA", "MNP")
    
    ggplot(cmp[recip_iso %in% ex_iso], aes(x = year)) +
        geom_line(aes(y = disb_tot, color = "new")) +
        geom_line(aes(y = disb_tot_old, color = "old")) +
        facet_wrap(~recip_iso, scales = "free_y")
    
}
