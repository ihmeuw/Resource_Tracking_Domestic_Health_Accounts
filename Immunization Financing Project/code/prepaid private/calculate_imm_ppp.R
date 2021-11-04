## ==========================================================
#' @Purpose: Calculate estimates of Immunization PPP Spending
#' ----------------------------------------------------------
#' @Inputs: Health Sector Data 
#'          Extracted immunization NHA data 
#'          Final immunization estimates
#' ----------------------------------------------------------
#' @Outputs: Vetting figures
#'           Data
#' ----------------------------------------------------------
#' @Authors:
#' @Date: 11/03/2021
## ==========================================================

rm(list = ls())
pacman::p_load(data.table, tidyverse, scales, plyr, feather)

## ------------------------------------------------------------------------------------------------------------
## !!! set parameters: 
## - Dates 
## - Out directories for plots and data
## - Filepaths
## ------------------------------------------------------------------------------------------------------------

DATE <- format(Sys.time(), "%Y.%m.%d")

plot_out_dir <- "FILEPATH"
data_out_dir <- "FILEPATH"

imm_nha_extractions_filepath <- "FILEPATH/extractions_07092020.csv"
imm_dah_filepath <- "FILEPATH/Imfinance_dah_total_country_year.csv"
imm_ghes_filepath <- "FILEPATH/07_post_stgpr_draws/final_data_draws.feather"
imm_oop_filepath <- "FILEPATH/draws_data_component_OOP_by_country_20200807_best.csv"

## ------------------------------------------------------------------------------------------------------------
##  Get location ids
## ------------------------------------------------------------------------------------------------------------
source("FILEPATH/get_location_metadata.R")
location <- get_location_metadata(location_set_id = 22, gbd_round_id = 5)
locs <- location[level == 3, .(location_id, ihme_loc_id)]

## ------------------------------------------------------------------------------------------------------------
##  Get health sector data 
##  And calculate ratio PPP/Non-PPP
## ------------------------------------------------------------------------------------------------------------

## function to read and clean the health sector data
read_hs <- function(filename){
  temp <- data.table(read_feather(paste0("/ihme/resource_tracking/forecasting/the/best/draws/", filename, ".feather")))
  temp <- temp[scenario == 0 & year %in% 2000:2017,
               c("iso3",
                 "year",
                 "draw",
                 filename), 
               with = F]
  setnames(temp, c("iso3", "year"), c("ihme_loc_id", "year_id"))
  
  ## convert draws to 0:999 instead of 1:1000 so we can easily merge later
  temp[, draw_num := as.numeric(str_remove(draw, "draw_"))]
  temp[, draw_num := draw_num - 1]
  temp[, draw := paste0("draw_", draw_num)]
  temp[, draw_num := NULL]
  
  ## return
  return(temp)
}

## get THE and PPP merged
hs_the <- read_hs("the_totes")
hs_ppp <- read_hs("ppp_totes")
hs <- merge(hs_the, hs_ppp)

## calculate ratio PPP/Non-PPP and remove other columns
hs[,
   `:=`(hs_ppp_ratio = ppp_totes/(the_totes - ppp_totes), 
        the_totes = NULL, 
        ppp_totes = NULL)]

## get means of the ratio PPP/Non-PPP
hs_mean <- hs[,
              .(hs_ppp_ratio = mean(hs_ppp_ratio)), 
              by = .(ihme_loc_id, year_id)]

## ------------------------------------------------------------------------------------------------------------
##  Get raw NHA immunization data 
##  And calculate ratio PPP/Non-PPP
## ------------------------------------------------------------------------------------------------------------

## read data
imm_nha <- fread(imm_nha_extractions_filepath)

## fill NAs with available other data
imm_nha[is.na(dis_immunization_ppp),
        dis_immunization_ppp := dis_vpd_ppp]
imm_nha[is.na(dis_immunization_total),
        dis_immunization_total := dis_vpd_total]

## simplify data
imm_nha <- imm_nha[!is.na(dis_immunization_ppp),
                   .(ihme_loc_id = str_extract(location_name_id, "[:upper:]{3}"), 
                     year_id = year_start, 
                     imm_ppp = dis_immunization_ppp, 
                     imm_the = dis_immunization_total)]

## calculate ratio PPP/Non-PPP and remove other columns
imm_nha[,
        `:=`(imm_ppp_ratio = imm_ppp / (imm_the - imm_ppp), 
             imm_ppp = NULL, 
             imm_the = NULL)]

## ------------------------------------------------------------------------------------------------------------
##  Calculate the ratio of the two ratios and take the median (arbitrarily named "rho")
## 
##  rho = median ( imm_ratio / hs_ratio )
## ------------------------------------------------------------------------------------------------------------

## merge
## we use the HS means here
imm_hs <- merge(hs_mean, imm_nha)

## calculate rho
rho <- imm_hs[, median(imm_ppp_ratio / hs_ppp_ratio)]

## ------------------------------------------------------------------------------------------------------------
##  Get final estimates of non-PPP spending on Immunization
## ------------------------------------------------------------------------------------------------------------

## get Imm. DAH
imm_dah <- fread(imm_dah_filepath)
imm_dah <- imm_dah[ihme_loc_id %chin% locs$ihme_loc_id, 
                   .(ihme_loc_id, 
                     year_id, 
                     imm_dah = total)]

## get Imm. GHES and convert location_id to ihme_loc_id
imm_ghe <- data.table(read_feather(imm_ghes_filepath))
imm_ghe <- merge(imm_ghe, locs)
imm_ghe <- imm_ghe[, .(ihme_loc_id, 
                       year_id, 
                       draw, 
                       imm_ghe = immunization)]

## get Imm. OOP
imm_oop <- fread(imm_oop_filepath)
imm_oop <- imm_oop[, .(ihme_loc_id, 
                       year_id, 
                       draw = paste0("draw_", draw), 
                       imm_oop = oop_spending)]

## merge GHE, DAH, & OOP
imm <- merge(imm_ghe, imm_dah, by = c("ihme_loc_id", "year_id"), all = T)
imm <- merge(imm, imm_oop, by = c("ihme_loc_id", "year_id", "draw"))

## fill DAH NAs
imm[is.na(imm_dah), imm_dah := 0]

## sum to non-PPP and remove the other columns
imm[,
    `:=`(imm_non_ppp = imm_ghe + imm_dah + imm_oop, 
         imm_ghe = NULL, 
         imm_dah = NULL, 
         imm_oop = NULL)]

## ------------------------------------------------------------------------------------------------------------
##  Do final calculation for each country-year:
##  
##  Estimated Imm. PPP = rho * Health Sector PPP ratio * Imm. Non-PPP
## ------------------------------------------------------------------------------------------------------------

## merge in hs_ratio
## use the HS draws here for uncertainty calculations
imm_ppp <- merge(imm, hs, by = c("ihme_loc_id", "year_id", "draw"))

## add in "rho" as a column
imm_ppp[, rho := rho]

## calculate estimated Imm. PPP 
imm_ppp[, imm_ppp := rho * hs_ppp_ratio * imm_non_ppp]

## remove unneeded columns
imm_ppp[, c("rho", "hs_ppp_ratio") := NULL]

## ------------------------------------------------------------------------------------------------------------
##  Create smoothed versions of the draws
##
##  Smoothing is done in logit fraction space
## ------------------------------------------------------------------------------------------------------------

## merge in health sector PPP
imm_ppp <- merge(imm_ppp, hs_ppp, by = c("ihme_loc_id", "year_id", "draw"))

## create logit fraction
imm_ppp[, logit_share := boot::logit(imm_ppp / ppp_totes)]

## check for NAs
imm_ppp[is.infinite(logit_share) | is.na(logit_share)]

## order by year within each country and draw
imm_ppp <- imm_ppp[order(ihme_loc_id, draw, year_id)]

## create function to smooth values using splines
smooth_spline <- function(data, 
                          smooth_level){ # degree of smoothness runs from 0-1
  smoothed <- smooth.spline(x = data, spar = smooth_level)
  out <- smoothed$y
  return(out)
}

## for each year and draw, create smoothed estimates
imm_ppp[, 
        logit_share_sm := smooth_spline(logit_share, smooth_level = 0.45), 
        by = .(ihme_loc_id, draw)]

## convert smoothed logit fraction to level space
imm_ppp[, imm_ppp_sm := boot::inv.logit(logit_share_sm) * ppp_totes]

## remove unneeded columns
imm_ppp[, c("logit_share", "logit_share_sm") := NULL]


## ------------------------------------------------------------------------------------------------------------
## Save data results
## ------------------------------------------------------------------------------------------------------------

## simplify data 
OUT <- imm_ppp[, .(ihme_loc_id, 
                   year_id, 
                   draw, 
                   immunization_ppp = imm_ppp_sm)]

## write data
setwd(data_out_dir)
fwrite(OUT, paste0("imm_ppp_draws_", DATE, ".csv"))
