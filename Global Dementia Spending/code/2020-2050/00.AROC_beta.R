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

# loading beta
beta <- fread(paste0(root, 'FILEPATH'))

# adding locations so we can only keep 195 locations
location <- fread(paste0(root, 'FILEPATH'))
locs <- location[level == 3, c('location_id', 'location_name')]

# cleaning data
beta1 <- beta[year_id == 2000 | year_id == 2019, ]
beta_l <- merge(beta1, locs)
setnames(beta_l, old = "variable", new = "draw")
setnames(beta_l, old = "value", new = "data_var")
head(beta_l)

# calculating AROCS

source("ADDRESS")

# calculating aroc between 2000 and 2019 for beta
beta_aroc <- create_metric(beta_l,
              id_var = c('year_id', 'location_id', 'location_name'), 
              aroc_years = c(2000, 2019))

fwrite(beta_aroc, paste0(root, 'FILEPATH'), row.names = F)