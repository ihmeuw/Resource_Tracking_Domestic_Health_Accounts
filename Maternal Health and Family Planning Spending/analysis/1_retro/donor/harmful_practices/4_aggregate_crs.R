#
# Handles the integration and aggregation of all ODA data sources together
#
# Development notes:
# - Source assignments should match those used by EI for their forecasting, see
#   comments below.
# - Recipient assignments are ideally to GBD countries. If to a non-GBD country,
#   assign to the "unallocated" category "_QZA".
#   * If a project is regional, try to assign it to a GBD region or super-region.
#   * If it appears to be a combination of multiple GBD regions, assign it to the
#     comma-seperated list of GBD regions (e.g., "R7,R12").
#     * Pipeline can't currently handle a list of multiple super-regions!!
#   * If a data region cannot be assigned to any GBD regions, give it a special
#     region name that starts with an underscore (e.g., "_Melanesia").
#     ** All special regions will have to be handled on a case-by-case basis in
#        the next script!
#
# NOTE:
# For now, using the CRS as the only source of ODA since the UNDP data and
# UNWomen data are harder to integrate or less complete in various ways

library(data.table)
library(arrow)

source(here::here("src", "r", "params.R"))
source(here::here("src", "r", "logging.R"))

CONFIG <- list(
    dirs = list(
        input = get_path("raw", "input"),
        covars = get_path("int", "data", "covars"),
        working = get_path("int", "data", c("oda", "int"))
    )
)


categorize_oecd_sectors <- function(dt) {
    sector_map1 <- openxlsx::read.xlsx(
        file.path(CONFIG$dirs$input, "oecd_sectors_categorized.xlsx"),
        sheet = "map1"
    )
    setDT(sector_map1)
    sector_map2 <- openxlsx::read.xlsx(
        file.path(CONFIG$dirs$input, "oecd_sectors_categorized.xlsx"),
        sheet = "map2"
    )
    setDT(sector_map2)
    
    dt <- merge(dt, sector_map1,
                by = "sector_name", all.x = TRUE)
    dt <- merge(dt, sector_map2,
                by = c("sector_name", "purpose_name"), all.x = TRUE)
    dt[, sector_category := fifelse(is.na(sector_category1),
                                    sector_category2,
                                    sector_category1)]
    dt[, c("sector_category1", "sector_category2") := NULL]
    stopifnot(dt[is.na(sector_category), .N] == 0)
    
    return(dt)
}


tag_treat_prevent <- function(hp) {
    # run treatment/prevention search
    hp[, is_treatment := fifelse(
        srchstr %like% "TREATM|RESPON|VICTIM", TRUE, FALSE
    )]
    hp[, is_prevention := fifelse(
        # elimination, empowerment, 
        srchstr %like% "PREVEN|ELIM|EMPOWER", TRUE, FALSE
    )]
    return(hp)
}


get_locs <- function() {
    locs <- fread(file.path(CONFIG$dirs$covars, "ihme_unfpa_locs.csv"),
                  select = c("iso3", "unfpa_country", "ihme_location_name"))
    locs[ihme_location_name == "Unallocated", ihme_location_name := unfpa_country]
    locs[unfpa_country == "Kosovo", ihme_location_name := "Kosovo"]
    locs[, unfpa_country := NULL]
    setnames(locs, c("ihme_location_name") , c("location_name"))

    locs[, crs_name := fcase(
        location_name == "Bolivia (Plurinational State of)", "Bolivia",
        location_name == "China", "China (People's Republic of)",
        location_name == "Iran (Islamic Republic of)", "Iran",
        location_name == "Micronesia (Federated States of)", "Micronesia",
        location_name == "Republic of Moldova", "Moldova",
        location_name == "United Republic of Tanzania", "Tanzania",
        location_name == "Venezuela (Bolivarian Republic of)", "Venezuela",
        location_name == "Taiwan (Province of China)", "Chinese Taipei",
        location_name == "Republic of Korea", "Korea",
        location_name == "United States of America", "United States",
        location_name == "Republic of Korea", "Korea",
        location_name == "Slovakia", "Slovak Republic",
        location_name == "Russian Federation", "Russia",
        rep_len(TRUE, .N), location_name
    )]
    locs[, location_name := NULL]
    #
    # Special locations where different country/special regions are mapped to
    #   a country that is included in the GBD location hierarchy
    locs <- rbind(locs, data.table(crs_name = "West Bank and Gaza Strip",
                                   iso3 = "PSE")) # palestine
    locs <- rbind(locs, data.table(crs_name = c("Hong Kong (China)", "Macau (China)"),
                                   iso3 = "CHN")) # china
    ## Kosovo intentionally retained (UNFPA priority country)
    return(locs)
}


standardize_recipient <- function(crs_agg, locs) {

    dt <- merge(crs_agg, locs,
                by.x = "recipient_name", by.y = "crs_name",
                all.x = TRUE)

    ## assign regional projects to GBD region or super-region ihme_loc_id(s)
    ## - for special regions, give a unique id that can be used later during
    ##   region splitting
    dt[, iso3 := fcase(
        recipient_name == "Bilateral, unspecified", "_QZA",
        recipient_name == "Africa, regional", "_Africa",
        recipient_name == "Asia, regional", "_Asia",
        recipient_name == "North of Sahara, regional", "_NorthOfSahara",
        recipient_name == "Melanesia, regional", "_Melanesia",
        recipient_name == "Polynesia, regional", "_Polynesia",
        recipient_name == "Micronesia, regional", "_Micronesia",
        recipient_name == "East African Community", "_EAC",
        recipient_name == "States Ex-Yugoslavia unspecified", "_ExYugoslavia",
        recipient_name == "Central Asia, regional", "R2",
        recipient_name == "Far East Asia, regional", "R3",
        recipient_name == "Oceania, regional", "R17",
        recipient_name == "South Asia, regional", "R4",
        recipient_name == "South & Central Asia, regional", "R4,R2",
        recipient_name == "Caribbean, regional", "R7",
        recipient_name == "Middle East, regional", "R15",
        recipient_name == "South of Sahara, regional", "S3",
        recipient_name == "America, regional", "S7",
        recipient_name == "Eastern Africa, regional", "R19",
        recipient_name == "Europe, regional", "R8,R9",
        recipient_name == "Southern Africa, regional", "R20",
        recipient_name == "Caribbean & Central America, regional", "R7,R12",
        recipient_name == "South America, regional", "R11,R13,R14",
        recipient_name == "Western Africa, regional", "R21",
        recipient_name == "Central America, regional", "R12",
        recipient_name == "Middle Africa, regional", "R18",
        rep_len(TRUE, .N), iso3
    )]
    
    # still missing countries are not gbd or unfpa locations - assign to QZA
    ## dt[is.na(iso3), unique(recipient_name)]
    dt[is.na(iso3), iso3 := "_QZA"]
    return(dt)
}


standardize_donor <- function(dt, locs) {
    crs_agg <- copy(dt)
    crs_agg[, `:=`(income_sector = "", income_type = "")]
    ## first try to merge on country names to identify country donors
    crs_agg <- merge(crs_agg,
                     locs[, .(donor_name = crs_name,
                              donor_iso = iso3)],
                     by = "donor_name", all.x = TRUE)
    # main dac donors
    dac_isos <- c("USA", "GBR", "DEU", "FRA", "CAN", "AUS", "JPN", "NOR", "ESP","NLD", "AUT", 
                  "BEL", "DNK", "FIN", "GRC", "IRL", "ITA", "KOR", "LUX", "NZL","PRT", "SWE", 
                  "CHE", "CHN")
    for (iso in dac_isos)
        crs_agg[!is.na(donor_iso) & donor_iso == iso,
                `:=`(income_sector = iso, income_type = "CENTRAL")]
    # other dac donors
    crs_agg[donor_iso %in% c("CZE", "HUN", "ISL", "POL", "SVK", "SVN", "EST", "LTU"),
            `:=`(income_sector = "OTHERDAC", income_type = "CENTRAL")]
    crs_agg[donor_name %in% c("Liechtenstein", "Chinese Taipei"), ## not a gbd loc so no iso
            `:=`(income_sector = "OTHERDAC", income_type = "CENTRAL")] 
    crs_agg[donor_name == "EU Institutions",
            `:=`(income_sector = "OTHERDAC", income_type = "EC")]
    
    # other public donors (donor is country but not in DAC)
    crs_agg[!is.na(donor_iso) & income_sector == "",
            `:=`(income_sector = "OTHERPUB", income_type = "CENTRAL")]
    
    # dev banks should be OTHER
    crs_agg[grepl("Development Bank", donor_name),
            `:=`(income_sector = "OTHER", income_type = "DEVBANK")]
    crs_agg[donor_name %in% c(
        "African Development Fund", # part of African DB
        "International Development Association", # part of World Bank
        "Arab Bank for Economic Development in Africa",
        "Asian Infrastructure Investment Bank",
        "Central American Bank for Economic Integration"
    ), `:=`(income_sector = "OTHER", income_type = "DEVBANK")]
     
    # UN agencies should be OTHER
    crs_agg[donor_name %in% c(
        "UN Capital Development Fund", "UN Institute for Disarmament Research",
        "UN Peacebuilding Fund", "UN Women", "UNAIDS", "UNDP", "UNECE", "UNEP",
        "UNFPA", "UNHCR", "UNICEF", "UNRWA",
        "United Nations Conference on Trade and Development",
        "United Nations Industrial Development Organization",
        "World Health Organisation", "WHO-Strategic Preparedness and Response Plan",
        "World Trade Organisation", "WTO - International Trade Centre",
        "World Tourism Organisation", "Food and Agriculture Organisation",
        "Green Climate Fund", "IFAD", "IMF (Concessional Trust Funds)",
        "International Atomic Energy Agency",
        "International Centre for Genetic Engineering and Biotechnology",
        "International Labour Organisation",
        "WFP",
        "COVID-19 Response and Recovery Multi-Partner Trust Fund",
        "Central Emergency Response Fund",
        "Joint Sustainable Development Goals Fund",
        "UN Development Coordination Office",
        "UN Economic and Social Commission for Western Asia"
    ), `:=`(income_sector = "OTHER", income_type = "UN")]
    
    # puplic-private partnerships should be OTHER
    crs_agg[donor_name %in% c(
        "Global Fund",
        "Global Alliance for Vaccines and Immunization"
    ), `:=`(income_sector = "OTHER", income_type = "PPP")]
    
    # private donors should be PRIVATE - examining, they are all foundations
    priv_d <- crs_agg[donor_type == "private", unique(donor_name)]
    crs_agg[donor_name %in% priv_d,
            `:=`(income_sector = "PRIVATE", income_type = "FOUND")]
    
    # foundations = PRIVATE FOUND
    crs_agg[grepl("foundation", donor_name, ignore.case = TRUE),
            `:=`(income_sector = "PRIVATE", income_type = "FOUND")]
    crs_agg[donor_name %in% c("Wellcome Trust"),
            `:=`(income_sector = "PRIVATE", income_type = "FOUND")]
    
    # BMGF is its own category
    crs_agg[donor_name == "Bill & Melinda Gates Foundation",
            `:=`(income_sector = "BMGF", income_type = "BMGF")]
    
    # multilateral/inter-gov orgs
    crs_agg[donor_name %in% c(
        "Nordic Development Fund", "OPEC Fund for International Development",
        "Montreal Protocol", "OSCE", "Global Green Growth Institute",
        "International Commission on Missing Persons",
        "Global Environment Facility",
        "Enhanced Integrated Framework (EIF)",
        "Climate Investment Funds",
        "World Organisation for Animal Health",
        "Asian Forest Cooperation Organisation",
        "Private Infrastructure Development Group"
    ), `:=`(income_sector = "OTHER", income_type = "OTHER")]
     
    # these donors know to be OTHER
    crs_agg[donor_name %in% c(
        "Arab Fund (AFESD)", "Center of Excellence in Finance",
        "Adaptation Fund", "CGIAR"
    ), `:=`(income_sector = "OTHER", income_type = "OTHER")]

    ## check and assign as needed...
    ## unique(crs_agg[income_sector == "", donor_name])
    stopifnot( nrow(crs_agg[income_sector == "" | income_type == ""]) == 0 )
    
    # Eventually EI combines all non-main DAC donors and non-BMGF into OTHER
    # (so they include the main DAC donor countries, BMGF, DEBT, and OTHER;
    #  and DEBT is not applicable here). That's what we want for modeling.
    #
    crs_agg[, donor_code := income_sector]
    crs_agg[donor_code %like% "OTHER|PRIV", donor_code := "OTHER"]
    
    return(crs_agg)
}





redist_neg_disbs <- function(dt, disb_col, proj_id_cols) {
    if ("year" %in% proj_id_cols) {
        warning("'year' should not be included in proj_id_cols, removing")
        proj_id_cols <- proj_id_cols[proj_id_cols != "year"]
    }
    dt <- copy(dt)
    
    dt[, flow_id := .I]
    dt[, proj_id := .GRP, by = proj_id_cols]
    
    dt[, neg_proj := sum(get(disb_col) < 0, na.rm = TRUE),
       by = proj_id]
    
    # determine each flow-id's proportion of actual (i.e., positive) disbursements
    #   from the project
    dt[, pos_disb_only := fifelse(get(disb_col) < 0, 0, get(disb_col))]
    dt[, proj_disb_total := sum(pos_disb_only, na.rm = TRUE), by = .(year, proj_id)]
    dt[, pos_flow_frac := pos_disb_only / proj_disb_total]
    dt[is.infinite(pos_flow_frac) | is.na(pos_flow_frac), pos_flow_frac := 0]
    
    dt[, neg_disb_only := fifelse(get(disb_col) > 0, 0, get(disb_col))]
    dt[, proj_disb_total := sum(neg_disb_only, na.rm = TRUE), by = .(year, proj_id)]
    dt[, neg_flow_frac := neg_disb_only / proj_disb_total]
    dt[is.infinite(neg_flow_frac) | is.na(neg_flow_frac), neg_flow_frac := 0]
    
    dt[, tmp := sum(pos_flow_frac), by = .(year, proj_id)]
    stopifnot( all(abs(dt[tmp != 0, unique(tmp)] - 1) < 1e-6) )
    dt[, tmp := sum(neg_flow_frac), by = .(year, proj_id)]
    stopifnot( all(abs(dt[tmp != 0, unique(tmp)] - 1) < 1e-6) )
    stopifnot( dt[pos_flow_frac > 0 & neg_flow_frac > 0, .N] == 0 )
    dt[, tmp := NULL]
    
    pid_agg <- dt[, .(disb = sum(usd_disbursement, na.rm = TRUE)),
                  by = .(year, proj_id)]
    
    setorder(pid_agg, year)
    pid_agg[order(year), proj_n := 1:.N, by = proj_id]
    pid_agg[order(year), proj_N := .N, by = proj_id]
    pid_agg[, orig_total := sum(disb, na.rm = TRUE), by = proj_id]
    
    max_N <- max(pid_agg$proj_N)
    for (i in seq(1, max_N)) {
        # get the value of the next disbursement
        pid_agg[order(year),
               next_disb := shift(disb, type = "lead"),
               by = .(proj_id)]
        
        # update the annual total by adding the next disbursement (<0) to the current
        pid_agg[proj_n == proj_N - i & next_disb < 0,
            disb := disb + next_disb]
        
        # set the next disbursement to 0 if it's been removed from the current
        pid_agg[proj_n == proj_N - i + 1 & disb < 0 &      ## if next project disbursement is negative,
                proj_N - i != 0,                           ## and this is not the earliest appearance of this project,
            disb := 0]                                     ## set this next disb to 0, since it's been subtracted from the current disb
    }
    
    
    # test results:
    
    # negative disbursements should only remain if subtracting backwards made it
    # all the way to the first disbursement
    if (pid_agg[disb < 0  & proj_n != 1, .N] > 0)
        stop("Negative disbursements were not removed correctly")
    
    # the total sum of disbursements for each project should match the original total,
    #   since the original total included negative disbursements and the final total
    #   is the sum of the remaining disbursements after negative removals
    pid_agg[, final_total := sum(disb, na.rm = TRUE), by = proj_id]
    if (pid_agg[abs(final_total - orig_total) > 1e-6, .N])
        stop("Negative disbursement removal resulted in a loss of funds")
    
    
    # merge the updated proj disbursements onto the flows
    dt <- merge(
        dt, pid_agg[, .(year, proj_id, proj_yr_disb = disb)],
        by = c("year", "proj_id"),
        all.x = TRUE
    )
    # If we've eliminated the negative disbursement for the project,
    # then we distribute the disb back to the positive disbursing flows.
    # Otherwise, redisitribute the negatives so they can be dealt with by
    # the next iteration of the function.
    dt[, (disb_col) := fifelse(
        proj_yr_disb >= 0,
        pos_flow_frac * proj_yr_disb,
        neg_flow_frac * proj_yr_disb
    )]
    
    # cleanup
    dt[, c("flow_id", "proj_id", "neg_proj",
           "pos_disb_only", "neg_disb_only", "proj_disb_total",
           "proj_yr_disb", "pos_flow_frac", "neg_flow_frac") := NULL]
    return(dt)
}



distribute_hp_funds <- function(dt, value_col) {
    hp_keys <- c("fgm", "gbv", "ecm", "other")
    dt[, harmful_count := rowSums(.SD, na.rm = TRUE),
       .SDcols = paste0(hp_keys, "_count")]

    # compute fractions of harmful_count
    dt[, paste0(hp_keys, "_frac") := lapply(.SD, function(x) x / harmful_count),
       .SDcols = paste0(hp_keys, "_count")]
    setnafill(dt, fill = 0., cols = paste0(hp_keys, "_frac"))
    
    # apply fractions to total disbursed funds to distribute across categories
    dt[, paste0("disb_hp_", hp_keys) := lapply(.SD, function(x) x * get(value_col)),
       .SDcols = paste0(hp_keys, "_frac")]
    # compute totals
    dt[, disb_hp := rowSums(.SD, na.rm = TRUE),
       .SDcols = paste0("disb_hp_", hp_keys)]
    return(dt)
}


agg_crs <- function() {
    log_info("* Aggregating CRS data")
    log_info("** Load post-KWS data")
    crs <- arrow::read_feather(file.path(CONFIG$dirs$working, "crs_post_kws.feather"))

    log_info("** Filter the database")
    #
    # filter flowtype and donor ===============================================
    #
    # - we want to track ODA primarily but also track the private donors using
    #   the Private Developmet Finance flowtype.
    #   But make sure to drop flows from official donors that are classified as
    #   private flows since we want to be able to track the private flows separately.
    #
    crs <- crs[flow_name %in% c("ODA Loans", "ODA Grants", "Equity Investment", # "ODA"
                               "Private Development Finance")] # non-"ODA"
    
    official_d <- unique(crs[flow_name %in% c("ODA Loans", "ODA Grants", "Equity Investment"),
                             donor_name])
    priv_d <- unique(crs[flow_name == "Private Development Finance", donor_name])
    non_official_d <- setdiff(priv_d, official_d)
    
    crs[donor_name %in% official_d, donor_type := "official"]
    crs[donor_name %in% non_official_d, donor_type := "private"]
    stopifnot( nrow(crs[is.na(donor_type)]) == 0 )
    
    # drop country-donor private flows
    crs <- crs[!(flow_name == "Private Development Finance" & donor_type == "official")]
    
    # drop flows to administrative costs
    crs <- crs[sector_name != "Administrative Costs of Donors"]
    
    
    #
    # filter projects =========================================================
    #
    
    #
    # the DAH pipeline uses the below sectors, and any project tagged by fp or mh
    # is already included in the overall rmh_fp or rmh_mh envelope. Thus we don't
    # want to count these funds towards harmful practices, else double-counting
    #
    crs[, dah_overlap := FALSE]
    # CRS sectors used by the DAH pipeline (overlap with rmh_fp / rmh_mh envelope)
    crs[(sector_code %in% c(121, 122, 130) & (fp_count + mh_count) > 0),
        dah_overlap := TRUE]
 
    #
    # Flag other observations that should not be included in the final estimates
    # of harmful practices funding.
    #
    crs[, drop := FALSE]
    
    # These acronyms match a lot of irrelevant activities. If it is the only
    # keyword match (i.e., no other HP related keyword was detected), drop
    # the project
    crs[all_matches_kws == " FGC ", drop := TRUE]
    crs[all_matches_kws == " CEM ", drop := TRUE]
    
    # Very large disbursements here - the descr mentions GBV and FGM in Somalia
    # but only as some of many motivating factors for the UNMPTrustFund.
    crs[donor_name == "Sweden" & recipient_name == "Somalia" &
            project_title == "UNMPTF Phase II",
        drop := TRUE]
    
    # Some keywords tag projects that are not related to harmful practices
    crs[, srchstr := toupper(paste0(
        " ", project_title,
        " ; ", short_description,
        " ; ", long_description
    ))]
    crs[, row_id := .I]
    
    crs_fgm <- crs[fgm_count > 0]
    crs_fgm[srchstr %like% "GENITAL WART", drop := TRUE]
    crs_fgm[srchstr %like% "GENITAL HEART", drop := TRUE] # con-genital heart disease
    crs_fgm[srchstr %like% "MYCOPLASMA GENITALIUM", drop := TRUE]
    crs[row_id %in% crs_fgm[drop == TRUE, row_id], drop := TRUE]
    rm(crs_fgm)
    
    crs[dah_overlap == TRUE, drop := TRUE]

    
    #
    # redistribute negative disbursements ====================================
    #
    log_info("** Redistribute negative disbursements")
    crs[, orig_disb := usd_disbursement]
    
    log_info("*** level 1")
    crs <- redist_neg_disbs(
        crs,
        proj_id_cols = c("crs_id",
                         "donor_code", "agency_code",
                         "channel_code",
                         "sector_code", "purpose_code",
                         "recipient_code"),
        disb_col = "usd_disbursement"
    )
    crs <- crs[usd_disbursement != 0]
    
    log_info("*** level 2")
    crs <- redist_neg_disbs(
        crs,
        proj_id_cols = c("crs_id",
                         "donor_code", "agency_code",
                         "channel_code",
                         "recipient_code"),
        disb_col = "usd_disbursement"
    )
    crs <- crs[usd_disbursement != 0]
    
    log_info("*** level 3")
    crs <- redist_neg_disbs(
        crs,
        proj_id_cols = c("donor_code", "agency_code",
                         "channel_code",
                         "recipient_code"),
        disb_col = "usd_disbursement"
    )
    crs <- crs[usd_disbursement != 0]
    
    log_info("*** level 4")
    crs <- redist_neg_disbs(
        crs,
        proj_id_cols = c("donor_code", "recipient_code"),
        disb_col = "usd_disbursement"
    )
    crs <- crs[usd_disbursement != 0]
    
    log_info("*** level 5")
    crs <- redist_neg_disbs(
        crs,
        proj_id_cols = c("donor_code"),
        disb_col = "usd_disbursement"
    )
    crs <- crs[usd_disbursement != 0]
    
    if (crs[usd_disbursement < 0, .N] != 0) {
        # if this happens in the future, it implies you've found a donor for
        #   which negative disbursements outweigh positive disbursements, which
        #   is unlikely
        stop("Negative disbursements remain. Resolve.")
    }
    log_info("*** done.")
    
    #
    # aggregate flows =========================================================
    #
    # Need total ODA by flow and ODA for HP by flow.
    #
    # Group total disbursements by year-donor-channel-recipient-sector
    # so that we can calculate total ODA disbursements for each group. 
    #
    # Then calculate total of HP disbursements and merge onto the total ODA.
    #
    # The result gives us flows to harmful practices and total flows of ODA for
    # each of these groups.
    #
    log_info("** Aggregate flows")
    
    crs <- categorize_oecd_sectors(crs)
    
    grp_cols <- c("year",
                  "donor_name", "donor_type",
                  "channel_name", "channel_code",
                  "recipient_name", "sector_category")
    crs[, group_id := .GRP, by = grp_cols]
    grp_cols <- c("group_id", grp_cols)
    
    #
    # aggregate total ODA by group
    #
    crs_agg <- crs[, .(disb_total = sum(usd_disbursement, na.rm = TRUE)),
                   by = grp_cols]
    
    #
    # handle HP projects:
    #
    hp <- crs[drop == FALSE & harmful_count > 0]
    
    hp <- distribute_hp_funds(hp, value_col = "usd_disbursement")
    hp_agg <- hp[,
                 lapply(.SD, sum, na.rm = TRUE),
                 by = grp_cols,
                 .SDcols = grep("^disb_hp", names(hp), value = TRUE)]
    
    # merge total ODA and HP ODA
    crs_agg <- merge(
        crs_agg,
        hp_agg,
        by = grp_cols,
        all.x = TRUE
    )
    setnafill(crs_agg,
              fill = 0.,
              cols = grep("^disb_hp", names(hp_agg), value = TRUE))
    
    ## if disb_hp > disb_total error (also check for case where they are just
    ##   floating point errors apart)
    if ( crs_agg[disb_hp > disb_total & abs(disb_hp - disb_total) > 1e-6, .N] > 0 )
        stop("Total ODA disbursements and HP ODA disbursements do not match")
    
    
    #
    # standardize donors and recipients =======================================
    #
    log_info("** Standardize donors and recipients")
    locs <- get_locs()
    
    # merge on recipient country codes
    crs_agg <- standardize_recipient(crs_agg, locs)
    
    qza_recips <- crs_agg[iso3 == "_QZA", unique(recipient_name)]
    log_info("*** the following recipients are assigned to _QZA: ",
             paste(qza_recips, collapse = "; "))
    
    
    # determine source assignments (based on DAH & EI methodology) 
    # - called income_sector in the DAH pipeline
    crs_agg <- standardize_donor(crs_agg, locs)
    
    #
    # finalize ===============================================================
    #
    outfile <- file.path(CONFIG$dirs$working, "oda_hp_regional.feather")
    log_info("* Writing to {outfile}")
    # Data at level:
    # [year X donor X channel X recipient X sector-category]
    #   Contains: 
    #   ODA disbursements for harmful practices and its components
    #       hp: harmful practices
    #       hp_gbv: gender-based violence
    #       hp_ecm: early and child marriage
    #       hp_fgm: female genital mutilation
    #       hp_other: other harmful practice related projects
    #
    crs_agg[, c("group_id") := NULL]
    setnames(crs_agg, "iso3", "recip_iso")
    setcolorder(crs_agg,
                c("year",
                  "donor_name", "donor_code", "donor_iso",
                  "income_sector", "income_type", "donor_type",
                  "channel_name", "channel_code",
                  "recipient_name", "recip_iso",
                  "sector_category",
                  grep("^disb_", names(crs_agg), value = TRUE)))

    arrow::write_feather(crs_agg, outfile)
}

main <- function() {
    log_level("INFO")
    log_info("Combine sources of ODA data for harmful practices")

    agg_crs()
    
    log_info("Done.")
}

main()

