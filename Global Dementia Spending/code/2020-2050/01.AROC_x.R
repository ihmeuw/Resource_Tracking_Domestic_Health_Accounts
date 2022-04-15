######################################
#
# This file calculates AROCs for X. 
# With the inputs from this file I will 
# calculate x from 2020 to 2030. 
#
######################################


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

# loading x
x <- fread('FILEPATH')
head(x)

# adding locations so we can only keep 195 locations
location <- fread(paste0(root, 'FILEPATH'))
locs <- location[level == 3, c('location_id', 'location_name')]

# cleaning data
x1 <- x[year_id == 2000 | year_id == 2019, ]
x_l <- merge(x1, locs)
setnames(x_l, old = "variable", new = "draw")
setnames(x_l, old = "value", new = "data_var")
head(x_l)

# calculating AROCS

source("ADDRESS")

# calculating aroc between 2000 and 2019 for x
x_aroc <- create_metric(x_l,
              id_var = c('year_id', 'location_id', 'location_name'), 
              aroc_years = c(2000, 2019))

head(x_aroc)

fwrite(x_aroc, 
       paste0(root, 'FILEPATH'), 
       row.names = F)