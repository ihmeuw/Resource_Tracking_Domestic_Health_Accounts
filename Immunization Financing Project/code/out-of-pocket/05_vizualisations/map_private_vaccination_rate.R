##############################################################################################################
# Author: Emilie Maddison
# Last edited: 2/6/2020
# Description: creates maps based on a given variable
##############################################################################################################

rm(list = ls())

## Set filepaths
if (Sys.info()[1] == "Linux") {
  j <- FILEPATH 
  h <- FILEPATH 
  k <- FILEPATH 
} else if (Sys.info()[1] == "Windows") {
  j <- FILEPATH
  h <- FILEPATH
  k <- FILEPATH
}

## load required packages
require(data.table)
require(classInt, lib.loc = FILEPATH)
library(readxl)

## source mapping function
source(paste0(FILEPATH, "GBD_WITH_INSETS_MAPPING_FUNCTION.R"))
source(paste0(FILEPATH, "get_location_metadata.R"))  

outdir <- FILEPATH
#outfilename <- "private_utilization_map_20200422_average.pdf"
#outfilename <- "private_vaccination-rate_map_20200429.pdf"
outfilename <- "oop_scalar/private_imm_map_20200501.pdf"

#load dataset
#mapping.data <- dhs_proportions
# mapping.data <- data.table(
#   read_excel(paste0(FILEPATH, "percent_private_vaccines_from_lit_20200429.xlsx")))
mapping.data <- fread(paste0(out.dir, "oop_scalar/private_immunization_utilization_rate_20200501.csv"))

#load locations dataset for adding on iso3 column
# locations <- get_location_metadata(location_set_id = 1, gbd_round_id = 5)
# mapping.data <- merge(mapping.data, locations[, .(location_id, ihme_loc_id)], by = "ihme_loc_id", all.x = TRUE)

#rename ratio column to mapvar because that is the name the mapping function will accept
# setnames(mapping.data, "private_facility_use_prop", "mapvar")
#setnames(mapping.data, "pct_private", "mapvar")
setnames(mapping.data, "pct_private_imm", "mapvar")

#make dataframes
mapping.data$mapvar <- as.numeric(mapping.data$mapvar)
mapping.data$mapvar <- mapping.data$mapvar * 100
mapping.data <- mapping.data[, .(mapvar = mean(mapvar)), .(ihme_loc_id)]

length(unique(mapping.data$ihme_loc_id))

#map and output using the mapping function
gbd_map(data = mapping.data,
        # limits = c(0, 10, 20, 30, 40, 50, 60, 100),
        # labels = c("0 to 10%", "10 to 20%", "20 to 30%", "30 to 40%", "40 to 50%", "50 to 60%", "60 to 100%"),
        limits = c(0, 20, 40, 50, 80, 100),
        labels = c("0 to 20%", "20 to 40%", "40 to 60%", "60 to 80%", "80 to 100%"),
        col = "RdYlBu", col.reverse = TRUE,
        na.color = "#999999",
        #title = "Private facility utilization rate",
        title = "% vaccinations provided by private facilities",
        legend.title = "Percent",
        legend.cex = 1,
        legend.shift = c(-5, -10),
        fname = paste0(outdir,outfilename))
