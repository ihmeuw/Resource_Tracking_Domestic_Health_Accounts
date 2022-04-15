
#cleaning the environment
rm(list = ls())


# Set directories 
if (Sys.info()["sysname"] =="Linux") {
  root <-"ADDRESS"
} else {
  root <- "ADDRESS"
}

# loading libraries
library(data.table)

# loading fac_tot
fac_tot <- fread(paste0(root,'FILEPATH'))

# adding locations so we can only keep 195 locations
location <- fread(paste0(root, 'FILEPATH'))
locs <- location[level == 3, c('location_id', 'location_name')]

# cleaning data
fac_tot1 <- fac_tot[year_id == 2000 | year_id == 2019, ]
fac_tot_l <- merge(fac_tot1, locs)
setnames(fac_tot_l, old = "variable", new = "draw")
setnames(fac_tot_l, old = "total", new = "data_var")
head(fac_tot_l)


# calculating AROCS

source("ADDRESS")

# calculating aroc between 2000 and 2019 for fac_tot
fac_tot_aroc <- create_metric(fac_tot_l,
              id_var = c('year_id', 'location_id', 'location_name'), 
              aroc_years = c(2000, 2019))

head(fac_tot_aroc)

fwrite(fac_tot_aroc, paste0(root, 'FILEPATH'), row.names = F)