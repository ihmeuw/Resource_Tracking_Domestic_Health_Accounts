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

# loading gamma
gamma <- fread("FILEPATH")
head(gamma)
# adding locations so we can only keep 195 locations
location <- fread(paste0(root, 'FILEPATH'))
locs <- location[level == 3, c('location_id', 'location_name')]

# cleaning data
gamma1 <- gamma[year_id == 2000 | year_id == 2019, ]
gamma_l <- merge(gamma1, locs)
setnames(gamma_l, old = "variable", new = "draw")
setnames(gamma_l, old = "value", new = "data_var")
head(gamma_l)

# calculating AROCS

source("ADDRESS")

# calculating aroc between 2000 and 2019 for gamma
gamma_aroc <- create_metric(gamma_l,
              id_var = c('year_id', 'location_id', 'location_name'), 
              aroc_years = c(2000, 2019))

head(gamma_aroc)

fwrite(gamma_aroc, paste0(root, 'FILEPATH'), row.names = F)