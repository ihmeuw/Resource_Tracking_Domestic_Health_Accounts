library(readxl)
library(data.table)

source(here::here("src", "r", "params.R"))
source(here::here("src", "r", "logging.R"))
source(here::here("src", "r", "internals.R"))

CONFIG <- list(
    files = list(
        locs = get_path("int", "data", c("covars", "gbd_locations.csv"))
    ),
    dirs = list(
        input = get_path("raw", "input"),
        output = get_path("int", "data", c("domestic", "int"))
    )
)



DIS_HIER <- list(
    "the_dis2" = list(
        "the_dis2.1" = c(
            "fs_domestic_public_dis2.1",
            "fs_domestic_private_ppp_dis2.1",
            "fs_domestic_private_oop_dis2.1",
            "fs_dah_dis2.1",
            "fs_nsk_dis2.1"
        ),
        "the_dis2.2" = c(
            "fs_domestic_public_dis2.2",
            "fs_domestic_private_ppp_dis2.2",
            "fs_domestic_private_oop_dis2.2",
            "fs_dah_dis2.2",
            "fs_nsk_dis2.2"
        ),
        "the_dis2.3" = c(
            "fs_domestic_public_dis2.3",
            "fs_domestic_private_ppp_dis2.3",
            "fs_domestic_private_oop_dis2.3",
            "fs_dah_dis2.3",
            "fs_nsk_dis2.3"
        ),
        "the_dis2.nec" = c(
            "fs_domestic_public_dis2.nec",
            "fs_domestic_private_ppp_dis2.nec",
            "fs_domestic_private_oop_dis2.nec",
            "fs_dah_dis2.nec"
        )
    )
)

SRC_HIER <- list(
    "the_dis2" = list(
        "fs_domestic_public_dis2" = c(
            "fs_domestic_public_dis2.1",
            "fs_domestic_public_dis2.2",
            "fs_domestic_public_dis2.3",
            "fs_domestic_public_dis2.nec"
        ),
        "fs_domestic_private_ppp_dis2" = c(
            "fs_domestic_private_ppp_dis2.1",
            "fs_domestic_private_ppp_dis2.2",
            "fs_domestic_private_ppp_dis2.3",
            "fs_domestic_private_ppp_dis2.nec"
        ),
        "fs_domestic_private_oop_dis2" = c(
            "fs_domestic_private_oop_dis2.1",
            "fs_domestic_private_oop_dis2.2",
            "fs_domestic_private_oop_dis2.3",
            "fs_domestic_private_oop_dis2.nec"
        ),
        "fs_dah_dis2" = c(
            "fs_dah_dis2.1",
            "fs_dah_dis2.2",
            "fs_dah_dis2.3",
            "fs_dah_dis2.nec"
        ),
        "fs_nsk_dis2" = c(
            "fs_nsk_dis2.1",
            "fs_nsk_dis2.2",
            "fs_nsk_dis2.3"
        )
    )
)



#
# Standardize the recorded monetary values
#
# Currencies are in different scales (e.g., ones, millions, billions) and
# different units (e.g., USD, local currency). We need to standardize these
# before combining components from potentially different sources.
# They will be converted to 2023 USD by default.
standardize_monetary_values <- function(dt_cmb, base_year = 2023) {
    dt_cmb <- copy(dt_cmb)
    # re-scale spending values
    dt_cmb[, `:=`(
        value = as.numeric(gsub(",", "", value, fixed = TRUE)),
        mult_factor = as.numeric(gsub(",", "", mult_factor, fixed = TRUE))
    )]
    stopifnot( dt_cmb[is.na(value) | is.na(mult_factor), .N] == 0 )
    dt_cmb[, `:=`(
        value = value * mult_factor,
        mult_factor = NULL
    )]
    # Mauritania - in Dec 2017, currency was re-denominated such that 1 unit
    # of the new national currency is equal to 10 of the old national currency
    dt_cmb[iso3 == "MRT" & year_start < 2018 & units != "USD" & src == "nha",
           value := value / 10]
    # Sao Tome and Principe - in Jan 2018, currency was re-denominated such
    # that 1 unit of the new national currency is equal to 1000 of the old.
    # Values for 2018+ were converted to "old" currency units when extracted
    # (i.e. multiplied by 1000) and now all need to be converted to the new
    # currency units before the currency conversion.
    dt_cmb[iso3 == "STP" & units != "USD" & src == "nha",
           value := value / 1000]
    
    # standardize currency
    dt_cmb[, units := ifelse(units == "USD", "USD", "LCU")]
    dt_cmb[, curr_iso := iso3] ## iso3 gets consumed by function otherwise
    ## special case: Kosovo is not in currency_conversion's database. can use
    ##   any country that uses the Euro, since Kosovo uses Euro
    dt_cmb[curr_iso == "XKK",  curr_iso := "SVK"]
    
    dt_cmb <- currency_convert(
        dt_cmb,
        # convert {values}
        col.loc = "curr_iso", col.value = "value",
        # from {original_base_year} {currency}
        col.currency = "units", col.currency.year = "base_year",
        # to {base_year} USD
        base.unit = "USD", base.year = base_year
    )
    stopifnot( dt_cmb[is.na(value), .N] == 0 )
    dt_cmb[, curr_iso := NULL]
    
    return(dt_cmb)
}



#
# Standardize DIS & SRC hierarchy components
#
# If a value_code is not recorded for a given country-year, it may be because
# the other value_codes sum to the total, or it may be that the other value_codes
# do not sum to the total but not enough information was available to determine
# all the components.
# For example, if all the DIS x SRC info is missing, we want to keep it
# as missing so it can be imputed. But if some of the DIS x SRC info
# is present and some is missing, we need to determine if the missing info is
# not recorded because it is 0 or because it is truly unknown.
# So if the sum of the recorded values is very close to the total, then we can
# assume the missing elements should be 0. Otherwise, they need to be imputed.
# This can be tough because even if we theoretically have all the true info,
# the values are often rounded and thus don't sum exactly to the total.
# In the messages created by `expand_hier_level`, if the "prop_included" is
# close to the threshold (0.98), it should be investigated since this means that
# most of the total value is explained by the recorded components, and the
# remainder could be due to rounding.
#
# Note that if the_dis2 is missing, then we have only recorded a partial picture
# of reproductive health spending and all missing components are to be imputed
# or calculated by the sums of the other components, anyways.
# We can check the cases where this happens:
# cmb[, tst := "the_dis2" %in% value_code, by = .(location_name_id, year_id)]
# cmb[tst == FALSE]
# cmb[, tst := NULL]
expand_hier_level <- function(dt_cy, lvl_codes) {
    col_order <- names(dt_cy)
    dt_cy <- as.data.table(dt_cy)
    # checks
    valcodes <- dt_cy$value_code
    if (length(valcodes) != length(unique(valcodes))) {
        stop("value_code contains duplicates: ",
             dt_cy$location_name_id[1], " ", dt_cy$year_id[1])
    }
    if (! "the_dis2" %in% dt_cy$value_code) {
        # if total is not present, any data that is not present is considered missing
        return(dt_cy)
    }
    # determine if there are any components at this level
    id_cols <- setdiff(names(dt_cy), c("value_code", "value"))
    if (dt_cy[value_code %in% lvl_codes, .N] > 0) {
        tmp <- dcast(dt_cy, ... ~ value_code, value.var = "value")
        not_in_tmp <- setdiff(lvl_codes, names(tmp))
        in_tmp <- setdiff(lvl_codes, not_in_tmp)
        # if there are some missing components at this level...
        if (length(not_in_tmp)) {
            tmp[, prop_included := rowSums(.SD) / the_dis2, .SDcols = in_tmp]
            if (tmp[, prop_included] < 0.98) {
                ## Components do not add up close enough to total, so assume
                ## that the missing components need imputation. Message if it
                ## is close to the threshold and should be double-checked.
                if (tmp[, prop_included > 0.9])
                    log_warn("*** Added missing components to impute for: ",
                             dt_cy$location_name_id[1], " - ", dt_cy$year_id[1],
                             ". Double check (prop_included = ", tmp[, round(prop_included, 2)],
                             ")")
                tmp[, (not_in_tmp) := NA_real_]
            } else {
                ## No to small difference between total and components, assume
                ## missing components were 0 and not recorded for that reason
                ## (or if components add up to more than the_dis2, then the_dis2
                ##  will be adjusted to be the sum of components so same logic applies)
                tmp[, (not_in_tmp) := 0.0]
            }
            num_cols <- c("the_dis2", in_tmp, not_in_tmp)
            tmp[, (num_cols) := lapply(.SD, as.double), .SDcols = num_cols]
            dt_cy <- melt(tmp[, -"prop_included"],
                          id.vars = id_cols,
                          variable.name = "value_code",
                          variable.factor = FALSE,
                          value.name = "value")
            setcolorder(dt_cy, col_order)
        }
    }
    return(dt_cy)
}



#
# Remove capital formation expenditures
#
# ASSUMPTION
#
# capital_ratio = (Capital Spending for RH) / (Total RH Spending - OOP RH Spending) 
#               = rh_HK / (the_dis2 - fs_domestic_private_oop_dis2)
# (because OOP does not usually pay for capital formation) 
# If OOP data is missing, then the denominator is just the total RH spending
remove_hk <- function(dt_wide) {
    stopifnot(
        dt_wide[capital_formation_included == 1 & is.na(rh_HK), .N] == 0
    )
    
    public_vcs <- grep("fs_domestic_public", value_codes, value = T)
    for (vc in public_vcs) {
        ## calculate ratio of total spending that goes to this component
        dt_wide[, cr := get(vc) / (the_dis2 - fs_domestic_private_oop_dis2)]
        dt_wide[is.na(fs_domestic_private_oop_dis2),
                cr := get(vc) / the_dis2]
        ## apply ratio to rh_HK to get amount of capital formation in this component
        dt_wide[, capital := rh_HK * cr]
        setnafill(dt_wide, fill = 0, cols = "capital")
        ## remove capital formation from this component
        dt_wide[capital_formation_included == 1,
                (vc) := get(vc) - capital]
        dt_wide[, c("cr", "capital") := NULL]
    }
    
    dt_wide[, capital_formation_included := NULL]
    return(dt_wide)
}




#
# Recursively test that each level of the hierarchy's total is matched by the
# sum of its sub-components
#
# dt - the data.table conta
# total_col - the name of the column that contains the top-level total
# hier - the hierarchy list to reference for sub-components (DIS_HIER or SRC_HIER)
test_components <- function(dt_wide, total_col, hier) {
    components <- (if (is.list(hier[[total_col]]))
        names(hier[[total_col]])
        else hier[[total_col]])
    dt_wide[, tst := rowSums(.SD), .SDcols = components]
    dt_wide[, tot := get(total_col[length(total_col)])]
    n_bad <- dt_wide[abs(tst - tot) > 1, .N]
    if (n_bad > 0) {
        log_error(paste(total_col, collapse = ": "))
        log_error("  ", n_bad, " rows with misaligned components")
        log_error("  ", dt_wide[abs(tst - tot) > 1, paste0(iso3, "_", year_id)])
    }
    dt_wide[, `:=`(tst = NULL, tot = NULL)]
    if (is.list(hier[[total_col]])) {
        for (comp in components) {
            test_components(dt_wide, c(total_col, comp), hier)
        }
    }
}



load_nha_extractions <- function() {
    keep_cols <- c("location_name_id", "year_start", "month_start", "year_end", "month_end",
                   "value_code", "value", "mult_factor", "units", "base_year",
                   "capital_formation_included", "src_notes")
    
    #
    #       New SHA 2011 Extractions
    #
    new <- fread(file.path(CONFIG$dirs$input, "extractions",
                           "nha_extractions_sha2011_new.csv"))
    
    ## Drop rows where needed and subset columns
    setnafill(new, fill = 0, cols = c("to_delete", "capital_formation_included"))
    new <- new[to_delete != 1, ]
    new[, src_notes := ifelse(not_from_who == 0,
                              "sha2011-new-who", "sha2011-new-other")]
    
    # note - typically values are assumed to be nominal and so we set the base
    # year to be the year of the report. In new extractions those were coded as
    # equal to the start year, but we will assume end year instead:
    new[base_year == year_start, # (all rows currently)
        base_year := ifelse(is.na(year_end), year_start, year_end)]
    
    new <- new[, ..keep_cols]
    
    
    #
    #       Old SHA 2011 Extractions
    #
    old <- fread(file.path(CONFIG$dirs$input, "extractions",
                           "nha_extractions_sha2011_old.csv"))
    
    # remove private total category
    #
    # ASSUMPTION
    ## In old extractions, sometimes they just extracted a total "private" amount
    ## instead of separating private by PPP and OOP. Drop those cases.
    old <- old[! value_code %like% "fs_domestic_private_dis"]
    
    # ASSUMPTION
    ##
    ## deal with duplicated records
    ##
    ## In old extractions - MRT 2012 & 2013 were each recorded twice,
    ## once with capital and once without - just keep no-capital version
    old <- old[! (location_name_id == "Mauritania|MRT" & year_start %in% 2012:2013 &
                      capital_formation_included == 1)]
    ## Armenia 2014 & 2015: the_dis2 was recorded twice. Other entries match components
    old <- old[!( location_name_id == "Armenia|ARM" & year_start == 2014 &
                      value_code == "the_dis2" & value == "17,608.9000" )]
    old <- old[!( location_name_id == "Armenia|ARM" & year_start == 2015 &
               value_code == "the_dis2" & value == "18,671.0000")]
    ## Namibia 2012 - drop the_dis2 version with capital
    old <- old[!( location_name_id == "Namibia|NAM" & year_start == 2012 &
                      value_code == "the_dis2" & capital_formation_included == 1)]
    ## Mali 2015 - drop second the_dis2 entry
    old <- old[!( location_name_id == "Mali|MLI" & year_start == 2015 &
                      value_code == "the_dis2" & value == "35,834.5100")]
    
    ## Kenya 2012,2009 - drop these duplicate entries since other ones match components
    ## and are from newer report
    old <- old[!(location_name_id == "Kenya|KEN" & year_start %in% c(2009, 2012) &
                      value_code %in% c("the_dis2", "fs_domestic_public_dis2", "fs_dah_dis2") &
                     report_year == 2015)]
    
    ## Nigeria - drop these duplicated values since others match with components
    old <- old[!(location_name_id == "Nigeria|NGA" & year_start == 2010 &
                     notes == "different values that were also provided")]
    
    ## Laos 2010, 2011 - drop duplicated entries which are rounded more than other entries
    old <- old[!(location_name_id == "Laos|LAO" & year_start %in% 2010:2011 &
                      value_code == "the_dis2.1" & value %in% c("35,575.0000", "45,701.0000"))]
    old <- old[!(location_name_id == "Laos|LAO" & year_start %in% 2010:2011 &
                      value_code == "the_dis2.3" & value %in% c("36,646.0000", "49,750.0000"))]
    
    ## Comoros 2011 - values were revised and in the new SHA2011 extractions are
    ##  quite different, so just use the new values
    old <- old[!(location_name_id == "Comoros|COM" & year_start == 2011)]
    
    
    # Drop rows where needed and subset columns
    setnafill(old, fill = 0, cols = c("flag", "to_delete", "capital_formation_included"))
    # ASSUMPTION
    old <- old[flag != 1 & to_delete != 1] ## based on old code, these should be dropped
    
    old[, src_notes := "sha2011-old"]
    old <- old[, ..keep_cols]
    
    
    
    #
    #       Old SHA 2000 Extractions
    #
    sha00 <- fread(file.path(CONFIG$dirs$input, "extractions",
                             "nha_extractions_sha2000_old.csv"))
    
    # fix some of the hand-entered data
    sha00[mult_factor == "1,000,000,001", mult_factor := "1,000,000,000"]
    sha00 <- sha00[value_code != ""]
    # ASSUMPTION
    sha00[location_name_id == "Ethiopia|ETH" & year_start == 2004 &
              is.na(includes_HCR_values),
          includes_HCR_values := 1] ## these values do include capital formation
    
    setnames(sha00, "includes_HCR_values", "capital_formation_included")
    setnafill(sha00, fill = 0, cols = c("flag", "to_delete", "capital_formation_included"))
    sha00 <- sha00[to_delete != 1]
    
    
    # ASSUMPTION
    # reclassify to treat national health expenditure and total health expenditure
    #   as the same thing
    sha00[, value_code := gsub("nhe", "the", value_code)]
    
    # map the old value_codes to the new equivalents
    vc_remap <- c(
        # 'pattern' = 'replacement'
        "_the_rh$" = "_dis2",
        "^the_rh$" = "the_dis2",
        "_the_mh$" = "_dis2.1",
        "^the_mh$" = "the_dis2.1",
        "_the_fp$" = "_dis2.3",
        "^the_fp$" = "the_dis2.3",
        "_the_dv$" = "_dis2.nec",
        "^the_dv$" = "the_dis2.nec",
        "^hcr_dis2$" = "rh_HK" # sha 2000 equivalent of capital formation 
    )
    
    for (i in seq_along(vc_remap)) {
        sha00[, value_code := gsub(names(vc_remap)[i], vc_remap[i], value_code)]
    }
    
    
    # ASSUMPTION
    # Remove records that are for value_codes that we are not using
    ## (note - if wanted to include even more layers in the hierarchy, then
    ##  aggregate "domestic" and "domestic-private" categories *could* be used).
    sha00 <- sha00[!value_code %like% "fs_domestic_dis2" &
                       !value_code %like% "fs_domestic_private_dis2"]
    
    ## deal with duplicated records
    ##
    ## Rwanda 2006 - drop the flagged records. review of document indicates that
    ## the non-flagged records are the ones most aligned with our current methodology
    sha00 <- sha00[!(location_name_id == "Rwanda|RWA" & year_start == 2006 &
                         flag == 1)]
    
    # subset columns
    sha00[, src_notes := "sha2000-old"]
    sha00 <- sha00[, ..keep_cols]
    
    
    
    #
    # Test that there are no duplicate records
    #
    stopifnot(
        new[, .N, by = .(location_name_id, year_start, value_code)][N > 1, .N] == 0,
        old[, .N, by = .(location_name_id, year_start, value_code)][N > 1, .N] == 0,
        sha00[, .N, by = .(location_name_id, year_start, value_code)][N > 1, .N] == 0
    )
    
    #
    # Merge all extractions
    #
    ## extract iso code from names which are like "country name|ISO"
    new[, iso3 := unlist(lapply(strsplit(location_name_id, "|", fixed = TRUE),
                                `[[`, 2))]
    old[, iso3 := unlist(lapply(strsplit(location_name_id, "|", fixed = TRUE),
                                `[[`, 2))]
    sha00[, iso3 := unlist(lapply(strsplit(location_name_id, "|", fixed = TRUE),
                                  `[[`, 2))]
    
    ## drop country-years from old extractions that are already in the new
    ## extractions (i.e., they were re-extracted)
    old <- merge(
        old,
        unique(new[, .(iso3, year_start, value_code, drop_flag = 1)]),
        by = c("iso3", "year_start", "value_code"),
        all.x = TRUE
    )
    old <- old[is.na(drop_flag), -"drop_flag"] ## drops rows that are in "new"
    
    ## repeat for sha 2000 extractions but drop any country-year that was re-extracted
    ## using SHA 2011 documents, since methodology may have differed during extraction
    sha00 <- merge(
        sha00,
        rbind(
            unique(new[, .(iso3, year_start, drop_flag = 1)]),
            unique(old[, .(iso3, year_start, drop_flag = 1)])
        ),
        by = c("iso3", "year_start"),
        all.x = TRUE
    )
    sha00 <- sha00[is.na(drop_flag), -"drop_flag"] ## drops rows that are in "new"
    
    cmb <- rbind(new, old, sha00)
    
    ## test that there are no duplicate records
    stopifnot( cmb[, .N, by = c(keep_cols[keep_cols!="value"])][N > 1, .N] == 0 )
    return(cmb)
}


load_fpsa_extractions <- function() {
    #
    # load manual extractions (hand extracted from FPSA documents)
    #
    man <- fread(file.path(CONFIG$dirs$input, "extractions", "fpsa_extractions.csv"))
    man <- man[, .(location_name_id,
                   year_start, month_start, year_end, month_end,
                   value_code, value, mult_factor, units, base_year)]
    ## get iso code from location_name_id which is like "country name|ISO"
    man[, iso3 := unlist(lapply(strsplit(location_name_id, "|", fixed = TRUE),
                                `[[`, 2))]
    # ASSUMPTION
    ## for consistency with published extractions, if the values are for a
    ## fiscal year, we assume they are for the start year.
    man[!is.na(year_end), year_end := year_start]
    man <- man[year_start < 2022] ## these are now included in published data
    man[, value := as.numeric(gsub(",", "", value))]
    
    # Remove capital expenditures
    ## If we only have total capital spending for RH, we calculate the fraction
    ## of THE-RH that is capital, but first exclude OOP if we can
    #
    # calculate ratio (Public / (THE - OOP))
    #
    man[, rh_HK := value[value_code == "rh_HK"], by = .(iso3, year_start)]
    man[, rh_oop := value[value_code == "fs_domestic_private_oop_dis2.3"],
        by = .(iso3, year_start)]
    man[, rh_the := value[value_code == "the_dis2.3"],
        by = .(iso3, year_start)]
    setnafill(man, fill = 0, cols = "rh_oop") ## if missing, assume 0 for calculation
    man[, capital_ratio := value / (rh_the - rh_oop)]
    #
    # apply capital ratio - capital for gov = total capital * capital ratio
    #
    man[!is.na(capital_ratio) & value_code == "fs_domestic_public_dis2.3",
        capital := capital_ratio * rh_HK]
    man[!is.na(capital),
        value := value - capital]
    man[, c("rh_HK", "rh_oop", "rh_the", "capital_ratio", "capital") := NULL]
    #
    # if we directly observe capital spending for government, just subtract
    #
    man[, public_rh_HK := value[value_code == "public_rh_HK"], by = .(iso3, year_start)]
    man[!is.na(public_rh_HK) & value_code == "fs_domestic_public_dis2.3",
        value := value - public_rh_HK]
    man[, public_rh_HK := NULL]
    
    
    #
    # load published extractions (published online)
    #
    ## domestic government expenditure on family planning - i.e., fs_domestic_public_dis2.3
    pub <- readxl::read_excel(file.path(CONFIG$dirs$input, "intake", "FPSA_Database Oct 23 2024.xlsx"),
                              sheet = "Data",
                              skip = 1,
                              .name_repair = "unique_quiet")
    setDT(pub)
    
    pub <- pub[, c("Country", "Reported donor expenditures, not just government",
                   "Year", "Capital Costs", "Total Government")]
    setnames(pub, c("country", "donor_info", "year_start", "capital", "value"))
    setnafill(pub, fill = 0, cols = c("capital", "value"))
    ## remove capital expenditure
    pub[, `:=`(
        value = value - capital,
        capital = NULL
    )]
    ## make adjustments based on donor spending notes
    pub[, value := fcase(
        country == "Lao PDR" & year_start == 2020, value - 439498,
        country == "Philippines" & year_start == 2020, value * (1 - 0.020151499),
        rep_len(TRUE, .N), value
    )]
    pub[, donor_info := NULL]
    
    pub[, `:=`(
        value_code = "fs_domestic_public_dis2.3",
        mult_factor = 1,
        units = "USD",
        base_year = year_start,
        month_start = NA_integer_, year_end = NA_integer_, month_end = NA_integer_
    )]
    pub <- pub[!is.na(value)]
    
    ## merge on iso code
    pub[, location_name_id := fcase(
        country == "Bolivia", "Bolivia (Plurinational State of)",
        country %in% c("Côte d’Ivoire", "Cote d'Ivoire"), "Côte d'Ivoire",
        country == "Democratic Republic of Congo (DRC)", "Democratic Republic of the Congo",
        country == "DR Congo", "Democratic Republic of the Congo",
        country == "Kyrgyz Rep.", "Kyrgyzstan",
        country %in% c("Laos", "Lao PDR"), "Lao People's Democratic Republic",
        country == "Sao Tome & Principe", "Sao Tome and Principe",
        country == "State of Palestine", "Palestine",
        country == "Tanzania", "United Republic of Tanzania",
        country == "Vietnam", "Viet Nam",
        rep_len(TRUE, .N), country
    )]
    
    locs <- fread(CONFIG$files$locs)[level == 3] # countries
    pub <- merge(pub, locs[, .(location_name, iso3 = ihme_loc_id)],
                 by.x = "location_name_id", by.y = "location_name",
                 all.x = TRUE)
    pub[, country := NULL]
    
    
    #
    # combine
    #
    ## When combining the manual extractions with the published extractions,
    ## years before 2020 should be dropped from the published extractions since
    ## these were all hand extracted.
    ## Then for each country-year that appears in both sets, only the manually
    ## extracted version should be kept (because it would have been cross-checked
    ## with the published extraction value and then manually extracted only if we
    ## deemed the published extraction to be incorrect)
    pub <- pub[year_start >= 2020]
    pub <- merge(pub,
                 unique(man[, .(iso3, year_start, drop_flag = 1)]),
                 by = c("iso3", "year_start"),
                 all.x = TRUE)
    pub <- pub[is.na(drop_flag), -"drop_flag"]
    
    ## bind
    man[, src_notes := "fpsa-manual"]
    pub[, src_notes := "fpsa-published"]
    cmb <- rbind(man, pub)
    
    ## test that there are no duplicate records
    stopifnot( cmb[, .N, by = .(iso3, year_start, value_code)][N > 1] == 0 )
    return(cmb)
}


load_nidi_extractions <- function() {
    #
    # load old manual extractions
    #
    ## public expenditures
    pub_old <- fread(file.path(CONFIG$dirs$input, "extractions",
                               "nidi_public_old.csv"))
    pub_old <- pub_old[, c("location_name_id", "year_end",
                           "fs_domestic_public_the_fp", "fs_domestic_public_the_mh",
                           "mult_factor", "recurrent_expenditures",
                           "base_year", "units")]
    ### convert values to char and pivot long
    num_cols <- c("fs_domestic_public_the_fp", "fs_domestic_public_the_mh")
    pub_old[, (num_cols) := lapply(.SD, as.character), .SDcols = num_cols]
    pub_old <- melt(pub_old,
                    id.vars = setdiff(names(pub_old), num_cols),
                    variable.name = "value_code",
                    variable.factor = FALSE)
    #
    ## ppp expenditures
    ppp_old <- fread(file.path(CONFIG$dirs$input, "extractions",
                               "nidi_ppp_old.csv"))
    ppp_old <- ppp_old[, c("location_name_id", "year_end",
                           "fs_domestic_private_ppp_the_fp",
                           "mult_factor", "recurrent_expenditures",
                           "base_year", "units", "to_delete")]
    setnafill(ppp_old, fill = 0, cols = c("to_delete"))
    ppp_old <- ppp_old[to_delete == 0, -"to_delete"]
    ### convert values to char and pivot long
    num_cols <- c("fs_domestic_private_ppp_the_fp")
    ppp_old[, (num_cols) := lapply(.SD, as.character), .SDcols = num_cols]
    ppp_old <- melt(ppp_old,
                    id.vars = setdiff(names(ppp_old), num_cols),
                    variable.name = "value_code",
                    variable.factor = FALSE)
    
    #
    # load new manual extractions
    #
    new <- fread(file.path(CONFIG$dirs$input, "extractions",
                           "nidi_extractions_new.csv"))
    new[, `:=`(
        value = gsub("$", "", value, fixed = TRUE),
        recurrent_expenditures = gsub("%", "", recurrent_expenditures, fixed = TRUE)
    )]
    new <- new[, -c("source_id", "note", "flag")]
    
    #
    # combine extractions
    #
    all <- rbind(pub_old, ppp_old, new)
    
    # clean numeric values so they can be scaled by recurrent expenditure
    all[, `:=`(
        value = as.numeric(gsub(",", "", value, fixed = TRUE)),
        mult_factor = as.numeric(mult_factor),
        recurrent_expenditures = as.numeric(recurrent_expenditures)
    )]
    all[, `:=`(
        value = value * mult_factor,
        mult_factor = 1
    )]
    all <- all[!is.na(value)]
    
    # remove capital expenditures (keep recurrent only)
    # ASSUMPTION
    ## If the percent recurrent expenditures was reported as 0, we assume the
    ## true percent is missing and don't multiply
    setnafill(all, fill = 0, cols = c("recurrent_expenditures"))
    all[recurrent_expenditures > 0, value := value * (recurrent_expenditures/100)]
    all[, recurrent_expenditures := NULL]
    
    #
    # finalize
    ## convert value_codes to the nha encoding for consistency
    all[, value_code := fcase(
        value_code == "fs_domestic_public_the_fp", "fs_domestic_public_dis2.3",
        value_code == "fs_domestic_public_the_mh", "fs_domestic_public_dis2.1",
        value_code == "fs_domestic_private_ppp_the_fp", "fs_domestic_private_ppp_dis2.3"
    )]
    all[, iso3 := unlist(lapply(strsplit(location_name_id, "|", fixed = TRUE),
                                `[[`, 2))]
    all[location_name_id == "Kosovo|RKS", iso3 := "XKK"]
    
    ## for consistency with other sources, assume year_start = year_end
    ## (year_end is the only date information for nidi data)
    all[, year_start := year_end]
    
    # ASSUMPTION
    ## If a value is 0 for this source, assume it is actually missing
    all <- all[value != 0]
    
    all[, src_notes := "nidi"]
    return(all)
}



load_rhsc_data <- function() {
    # load OOP spending on family planning data from RHSC's reports
    
    #
    # 2021 leap report - contraceptive costs for 2019
    #
    leap21_fp <- fread(file.path(CONFIG$dirs$input, "rhsc", "custom_fp_158.csv"),
                       encoding = "UTF-8")
    leap21_fp <- leap21_fp[sector %in% c("Private non-subsidized", "Private subsidized") &
                               year == 2019 &
                               indicator == "Costs"]
    ## collapse costs across all contraceptive methods
    leap21_fp <- leap21_fp[, .(value = sum(value)),
                           by = .(location_name_id = region)]
    leap21_fp[, `:=`(
        value_code = "fs_domestic_private_oop_dis2.3",
        year_start = 2019,
        base_year = 2019,
        units = "USD",
        mult_factor = 1
    )]
    leap21_fp[, src_notes := "leap2021"]
    
    #
    # 2019 commodity gap analysis - contraceptive costs for 2018
    #
    cga19_fp <- list()
    for (ig in c("lic", "lmic", "umic")) {
        # read in file for income group and drop rows after last country
        # (which just contain file info/notes)
        x <- fread(file.path(CONFIG$dirs$input, "rhsc",
                             paste0("rhsc_cga2019_", ig, ".csv")),
                   fill = TRUE, encoding = "UTF-8")
        drop_ixs <- seq(which(x$Category == "")[1], nrow(x))
        cga19_fp[[ig]] <- x[!drop_ixs]
    }
    cga19_fp <- rbindlist(cga19_fp)
    setnames(cga19_fp, c("Category", "Costs"), c("location_name_id", "value"))
    cga19_fp[, value_code := "fs_domestic_private_oop_dis2.3"]
    cga19_fp[, `:=`(
        value_code = "fs_domestic_private_oop_dis2.3",
        year_start = 2018,
        base_year = 2018,
        units = "USD",
        mult_factor = 1
    )]
    cga19_fp[, src_notes := "cga2019"]
    
    
    #
    # Combine all
    #
    rhsc <- rbind(leap21_fp, cga19_fp)
    
    #
    # Merge on country iso codes
    #
    locs <- fread(file.path(CONFIG$files$locs))[level == 3, ] ## countries
    ## modify names so they merge
    rhsc[, location_name_id := fcase(
        location_name_id == "Bolivia", "Bolivia (Plurinational State of)",
        location_name_id == "Bosnia & Herzegovina", "Bosnia and Herzegovina",
        location_name_id == "CAR", "Central African Republic",
        location_name_id == "Congo, DR", "Democratic Republic of the Congo",
        location_name_id == "C\xf4te d'Ivoire", "Côte d'Ivoire",
        location_name_id == "Iran", "Iran (Islamic Republic of)",
        location_name_id == "Korea DPR", "Democratic People's Republic of Korea",
        location_name_id == "Lao PDR", "Lao People's Democratic Republic",
        location_name_id == "Macedonia", "North Macedonia",
        location_name_id == "Moldova", "Republic of Moldova",
        location_name_id == "St. Lucia", "Saint Lucia",
        location_name_id == "St. Vincent & Gren.", "Saint Vincent and the Grenadines",
        location_name_id == "State of Palestine", "Palestine",
        location_name_id == "Swaziland", "Eswatini",
        location_name_id == "Syria", "Syrian Arab Republic",
        location_name_id == "S\xe3o Tom\xe9 and Pr\xedncipe", "Sao Tome and Principe",
        location_name_id == "Tanzania", "United Republic of Tanzania",
        location_name_id == "Turkey", "Türkiye",
        location_name_id == "Vietnam", "Viet Nam",
        rep_len(TRUE, .N), location_name_id
    )]
    rhsc <- merge(rhsc,
                  locs[, .(location_name_id = location_name,
                           iso3 = ihme_loc_id)],
                  by = "location_name_id",
                  all.x = TRUE)
    return(rhsc)
}



#
# MAIN ========================================================================
#
log_info("Processing All Extractions")

#
# Load and prep extractions ====
#
log_info("* Loading data")
nha <- load_nha_extractions()

fpsa <- load_fpsa_extractions()
fpsa[, capital_formation_included := 0] ## already removed

nidi <- load_nidi_extractions()
nidi[, `:=`(month_start = NA_real_,
            month_end = NA_real_,
            capital_formation_included = 0)] ## already removed

rhsc <- load_rhsc_data()
rhsc[, `:=`(month_start = NA_real_,
            year_end = NA_real_,
            month_end = NA_real_,
            capital_formation_included = 0)] ## not relevant


## combine all
nha[, src := "nha"]
fpsa[, src := "fpsa"]
nidi[, src := "nidi"]
rhsc[, src := "rhsc"]
cmb <- rbind(nha, fpsa, nidi, rhsc)

# ASSUMPTION
## - for fiscal years, assume the end year is the year_id
## - if no year_end recorded, then values are for year specified by year_start
cmb[, year_id := ifelse(is.na(year_end) | year_end == 0,
                        year_start,
                        year_end)]


#
# Standardize spending values
#
log_info("* Standardizing monetary values")

cmb <- standardize_monetary_values(cmb, base_year = 2023)



#
# Standardize DIS & SRC hierarchy components
#
log_info("* Standardizing DIS & SRC hierarchy components")
log_info("**")
test_tot <- cmb[, sum(value)]

# repeat for each country-year and concatenate...
hier_lvls <- list(
    "DIS x SRC" = unname(unlist(SRC_HIER)),  ## DIS x SRC amounts
    "SRC" = names(SRC_HIER$the_dis2),  ## SRC totals
    "DIS" = names(DIS_HIER$the_dis2)   ## DIS totals
)

for (i in 1:3) {
    log_info("** Expanding hierarchy level ", names(hier_lvls)[i], "...")
    cmb <- cmb[, expand_hier_level(c(.BY, .SD), hier_lvls[[i]]),
               by = .(src, iso3, year_id)] 
}


stopifnot(
    # test that total value is maintained
    cmb[, sum(value, na.rm = TRUE)] - test_tot < 0.1,
    # test that no duplicates were created
    cmb[, .N, by = .(src, location_name_id, year_id, value_code)][N > 1] == 0
)
log_info("**")


#
# Pivot expenditure variables from long to wide
#
value_codes <- unique(cmb$value_code) ## for later
full <- dcast(cmb, ... ~ value_code, value.var = "value")



#
# Remove capital formation expenditures where it is included in health spending
#
log_info("* Removing capital formation expenditures")
full <- remove_hk(full)


#
# Re-compute spending values based on hierarchy
#
log_info("* Finalizing spending values")

## sums of components may not exactly equate to aggregates due to rounding in
## the documents - re-adjust aggregates based on lowest-level components, add
## checks to make sure differences aren't too large, since those could be due
## to extraction errors or data quality issues

# Note:
# We want data like this (DIS x SRC):
# ----------------------------------------------------
#          Public | OOP | PPP | DAH | (row totals...)
# Dis2.1
# Dis2.2
# Dis2.3
# Dis2.nec
#          (column totals...)       | (total RH)
# ----------------------------------------------------
# Often we could only extract something from the margins, either:
#
# (SRC):
# ----------------------------------------------------
#          Public | OOP | PPP | DAH | (row totals...)
# Dis2     (column totals...)       | (total RH)
# ----------------------------------------------------
#
# or (DIS):
# ----------------------------------------------------
#           Dis2 (row totals...)
# Dis2.1
# Dis2.2
# Dis2.3
# Dis2.nec 
#          (total RH)
# ----------------------------------------------------
#
# In some cases all we found was total RH:
# ----------------------------------------------------
# Total RH
# ----------------------------------------------------
#
# The goal is to ensure that inner cells sum to DIS and SRC totals where we
# have them, and that DIS and SRC totals sum to total RH where we have that.



# 0: convert DAH and NSK components to NA if they are not recorded
#   DAH and NSK are special cases of the hierarchy.
#   We did not always extract DIS x SRC values for DAH or NSK and usually
#   just total DAH (fs_dah_dis2) or total NSK (fs_nsk_dis2) for the sake of
#   computing the_dis2.
#   But in some cases the above procedure produces 0 values for the components
#   (instead of NA) because no DAH (NSK) sub-components were recorded but the
#   total DAH (NSK) is such a small fraction of the_dis2 that the algorithm
#   assumes DAH (NSK) is 0.
#   To correct this, we can simply reset the sub-components to be missing when
#   they are all 0 but the recorded total is >0.
dah_comps <- SRC_HIER$the_dis2$fs_dah_dis2
nsk_comps <- SRC_HIER$the_dis2$fs_nsk_dis2
full[, dah_d2 := rowSums(.SD), .SDcols = dah_comps]
full[dah_d2 == 0 & fs_dah_dis2 != 0, (dah_comps) := NA]
full[, nsk_d2 := rowSums(.SD), .SDcols = nsk_comps]
full[nsk_d2 == 0 & fs_nsk_dis2 != 0, (nsk_comps) := NA]



# I: try to aggregate from level (DIS x SRC) to level (SRC)
full[, pub_d2 := fs_domestic_public_dis2.1 + fs_domestic_public_dis2.2 +
                 fs_domestic_public_dis2.3 + fs_domestic_public_dis2.nec]

full[, ppp_d2 := fs_domestic_private_ppp_dis2.1 + fs_domestic_private_ppp_dis2.2 +
                 fs_domestic_private_ppp_dis2.3 + fs_domestic_private_ppp_dis2.nec]

full[, oop_d2 := fs_domestic_private_oop_dis2.1 + fs_domestic_private_oop_dis2.2 +
                 fs_domestic_private_oop_dis2.3 + fs_domestic_private_oop_dis2.nec]

full[, dah_d2 := fs_dah_dis2.1 + fs_dah_dis2.2 + fs_dah_dis2.3 + fs_dah_dis2.nec]

full[, nsk_d2 := fs_nsk_dis2.1 + fs_nsk_dis2.2 + fs_nsk_dis2.3]


# II: try to aggregate up from (DIS x SRC) to (DIS)
# - note: if an NSK value is missing, we assume zero because this is usually
#     not recorded unless explicitly stated in a document
full[, d2.1 := fs_domestic_public_dis2.1 + fs_domestic_private_ppp_dis2.1 +
               fs_domestic_private_oop_dis2.1 + fs_dah_dis2.1 +
               nafill(fs_nsk_dis2.1, fill = 0)]

full[, d2.2 := fs_domestic_public_dis2.2 + fs_domestic_private_ppp_dis2.2 +
               fs_domestic_private_oop_dis2.2 + fs_dah_dis2.2 +
               nafill(fs_nsk_dis2.2, fill = 0)]

full[, d2.3 := fs_domestic_public_dis2.3 + fs_domestic_private_ppp_dis2.3 +
               fs_domestic_private_oop_dis2.3 + fs_dah_dis2.3 +
               nafill(fs_nsk_dis2.3, fill = 0)]

full[, d2.nec := fs_domestic_public_dis2.nec + fs_domestic_private_ppp_dis2.nec +
                 fs_domestic_private_oop_dis2.nec + fs_dah_dis2.nec]



# III: try to aggregate to total RH spending from DIS and SRC totals
margins_map <- c(
    "pub_d2" = "fs_domestic_public_dis2",
    "ppp_d2" = "fs_domestic_private_ppp_dis2",
    "oop_d2" = "fs_domestic_private_oop_dis2",
    "dah_d2" = "fs_dah_dis2",
    "nsk_d2" = "fs_nsk_dis2",
    "d2.1"   = "the_dis2.1",
    "d2.2"   = "the_dis2.2",
    "d2.3"   = "the_dis2.3",
    "d2.nec" = "the_dis2.nec"
)
rev_margins_map <- setNames(names(margins_map), margins_map)


full[, d2_v1 := pub_d2 + ppp_d2 + oop_d2 + dah_d2 + nsk_d2]
full[, d2_v2 := d2.1 + d2.2 + d2.3 + d2.nec]

## where both methods are complete, they should agree (DIS sum == SRC sum)
stopifnot( full[abs(d2_v1 - d2_v2) > 1, .N] == 0 )

## now we can fill in the_dis2 as the sum of the components
full[, d2 := ifelse(is.na(d2_v1), d2_v2, d2_v1)]


# IV: where we don't have the (DIS x SRC) information, we use the information
#     we do have from the margins to fill in the higher levels of aggregation.
#     (If "calcd" is missing or 0, it's because the components are missing or
#      have been filled in with 0, in which case we use the recorded value
#      instead)
for (i in seq_along(margins_map)) {
    recorded <- unname(margins_map[i])
    calcd <- names(margins_map)[i]
    full[is.na(get(calcd)) | 
             (get(calcd) == 0 & get(recorded) != 0),
         (calcd) := get(recorded)]
}

# V: compute RH totals from the margin values
#
## In this case, if there are differences in the total RH computed by summing
## across DIS vs. SRC, this is due to the reporting of the components
## in the NHA doucments (the differences are minor - at least on the original
## scale - and due to the reported components being rounded, typically).
## Thus, we need to pick which level to keep (i.e., keep the SRC totals or the
## DIS totals?) and adjust the other level.
## We will keep the disease totals and adjust the source totals.
full[, d2_fromdis := d2.1 + d2.2 + d2.3 + d2.nec]
full[, d2_fromsrc := pub_d2 + ppp_d2 + oop_d2 + dah_d2 + nsk_d2]


# adjust source totals where we have both DIS and SRC totals
## adjust total source amounts by computing fraction of SRC out of total SRC
## summation and multiplying by total DIS summation
full[!is.na(d2_fromdis) & !is.na(d2_fromsrc), `:=`(
    pub_d2_fin = d2_fromdis * (pub_d2/d2_fromsrc),
    ppp_d2_fin = d2_fromdis * (ppp_d2/d2_fromsrc),
    oop_d2_fin = d2_fromdis * (oop_d2/d2_fromsrc),
    dah_d2_fin = d2_fromdis * (dah_d2/d2_fromsrc),
    nsk_d2_fin = d2_fromdis * (nsk_d2/d2_fromsrc)
)]
## adjust sub-component source amounts
for (nm in names(SRC_HIER$the_dis2)) {
    calcd_total <- unname(rev_margins_map[nm])
    calcd_total_adj <- paste0(calcd_total, "_fin")
    childs <- unname(unlist(SRC_HIER$the_dis2[[nm]]))
    full[!is.na(d2_fromdis) & !is.na(d2_fromsrc),
         (childs) := lapply(.SD,
                            \(x) get(calcd_total_adj) * x / get(calcd_total)),
         .SDcols = childs]
}

# add "fin"al versions of source totals where we only have source totals
src_codes <- c("pub_d2", "ppp_d2", "oop_d2", "dah_d2", "nsk_d2")
full[is.na(d2_fromdis), paste0(src_codes, "_fin") := lapply(.SD, identity),
     .SDcols = src_codes]

# add "fin"al versions of the disease totals
dis_codes <- c("d2.1", "d2.2", "d2.3", "d2.nec")
full[, paste0(dis_codes, "_fin") := lapply(.SD, identity),
     .SDcols = dis_codes]

# drop the non-final versions of the margins
full[, (rev_margins_map) := NULL]

# use newly calculated DIS and SRC totals to compute the_dis2
full[, d2_v3 := d2.1_fin + d2.2_fin + d2.3_fin + d2.nec_fin]
full[, d2_v4 := pub_d2_fin + ppp_d2_fin + oop_d2_fin + dah_d2_fin + nsk_d2_fin]
## sum of DIS and sum of SRC should agree
stopifnot( full[abs(d2_v3 - d2_v4) > 1, .N] == 0 )

full[is.na(d2), d2 := ifelse(is.na(d2_v3), d2_v4, d2_v3)]


## At this point if 'd2' is missing, it is because we don't have consistent
## recording of sub-components, so they can't be used to calculate total RH,
## and so our final value is just the recorded total RH
## (or 'NA' if that is missing too)
full[is.na(d2), d2 := the_dis2]


#
# VI. final double-checks and cleanup
#
## good idea to manually review the differences to ensure no errors in the manual
## extractions - hard to put a test here because differences may be large but just
## due to rounding (e.g., if extracted total is '3.6 million' but extracted components
## sum to 3.5 million)
##
tmp <- full[abs(d2 - the_dis2) > 1,
            .(src, iso3, year_id, d2, the_dis2, diff = abs(d2 - the_dis2))]
tmp[, pct_diff := diff / the_dis2]
if (tmp[pct_diff > 0.1, .N] > 0) {
    stop("There are signficant differences between calculated and recorded total RH spending.")
}
rm(tmp)

## cleanup
full[, c("d2_v1", "d2_v2", "d2_v3", "d2_v4", "d2_fromdis", "d2_fromsrc") := NULL]
full[, c(margins_map, "the_dis2") := NULL]
## set re-computed columns to original value_code names
setnames(full,
         old = paste0(names(margins_map), "_fin"),
         new = unname(margins_map))
setnames(full, old = "d2", new = "the_dis2")




#
# Test, finalize, & save
#

# Test that the hierarchy is consistent
test_components(full, "the_dis2", DIS_HIER)
test_components(full, "the_dis2", SRC_HIER)


# Pivot value_codes from wide to long
fin <- melt(full,
            id.vars = setdiff(names(full), value_codes),
            variable.name = "value_code",
            value.name = "value")


# Test that processed data has not lost any extracted values
## (it may have gained some values when sub-components were summed to calculate
##  totals, but should not have lost any)
src_counts <- fin[!is.na(value), .N, by = src_notes]
test_counts <- rbind(
    fpsa[, .N, by = src_notes],
    nha[, .N, by = src_notes],
    nidi[, .N, by = src_notes],
    rhsc[, .N, by = src_notes]
)
src_counts <- merge(src_counts, test_counts,
                    by = "src_notes", all = TRUE,
                    suffixes = c("_fin", "_orig"))

if ( src_counts[N_orig > N_fin, .N] != 0 )
    stop("Data processing had resulted in loss of values.")



# Finalize and save
fin[, `:=`(
    units = "2023 USD",
    location_name_id = NULL
)]

fwrite(fin, file.path(CONFIG$dirs$output, "rh_extracted.csv"), row.names = FALSE)
