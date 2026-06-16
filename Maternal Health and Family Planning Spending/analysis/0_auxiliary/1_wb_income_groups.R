#
# World Bank Income Group data
# Cleans and preps data from the world bank on historical country income groups
#
library(data.table)

source(here::here("src", "r", "params.R"))

CONFIG <- list(
    dirs = list(
        covars = get_path("int", "data", "covars")
    ),
    files = list(
        locs = get_path("int", "data", c("covars", "gbd_locations.csv")) 
    )
)

date_str <- format(Sys.Date(), "%Y_%m_%d")

#
# read in WB data
#
# data files can be found here:
# https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups
# we can obtain the direct download link from this site. make sure you download
# the "historical" file
dest_path <- file.path(CONFIG$dirs$covars,
                       paste0("raw__wb_historical_incgrps_",date_str,".xlsx"))
dataURL <- "https://datacatalogapi.worldbank.org/ddhxext/ResourceDownload?resource_unique_id=DR0090754"
download.file(dataURL, destfile=dest_path, mode="wb")
wb_raw <- as.data.table(readxl::read_excel(dest_path,
                                           sheet = "Country Analytical History"))



## cleaning wb excel sheet
wb <- wb_raw[!is.na(wb_raw[[1]]) | wb_raw[[2]] %like% "Data for calendar year "]
colnames(wb) <- as.character(wb[1, ])
setnames(wb, names(wb)[1:2], c("iso", "name"))
wb <- wb[!is.na(iso)]
wb <- melt(wb, id.vars = c("iso", "name"),
           variable.name = "year",
           variable.factor = FALSE,
           value.name = "income_group")
wb[income_group == "..", income_group := NA]

wb <- wb[, .(iso3 = iso, year, income_group)]
fwrite(wb,
       file.path(CONFIG$dirs$covars, "all_wb_ig.csv"))

