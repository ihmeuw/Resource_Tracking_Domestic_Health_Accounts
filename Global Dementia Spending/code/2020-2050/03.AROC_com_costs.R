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

# loading com_tot
com_tot <- fread(paste0(root, 'FILEPATH'))

# adding locations so we can only keep 195 locations
location <- fread(paste0(root, 'FILEPATH'))
locs <- location[level == 3, c('location_id', 'location_name')]

# cleaning data
com_tot1 <- com_tot[year_id == 2000 | year_id == 2019, ]
com_tot_l <- merge(com_tot1, locs)
setnames(com_tot_l, old = "variable", new = "draw")
setnames(com_tot_l, old = "total", new = "data_var")
head(com_tot_l)


# calculating AROCS


source("ADDRESS")

# calculating aroc between 2000 and 2019 for com_tot
com_tot_aroc <- create_metric(com_tot_l,
              id_var = c('year_id', 'location_id', 'location_name'), 
              aroc_years = c(2000, 2019))

head(com_tot_aroc)

fwrite(com_tot_aroc, paste0(root, 'FILEPATH'), row.names = F)