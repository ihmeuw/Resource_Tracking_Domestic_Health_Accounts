######
# This script calculates 2030 x reference scenario
# aroc^(2030-2019) * x(2019)
# Take the mean of the above
# I calculated the aroc with the output from ST-GPR
# These files are named theta, but they are producing x
########
rm(list = ls())

# Set directories 
if (Sys.info()["sysname"] =="Linux") {
  root <- "ADDRESS"
} else {
  root <- "ADDRESS"
}

# loading libraries
library(data.table)

# loading data

# theta AROCS
b_aroc <- fread(paste0(root, 'FILEPATH'))
b_aroc_s <- b_aroc[, c('location_id', 'location_name', 'draw', 'aroc_output')]
setnames(b_aroc_s, old = 'aroc_output', new = 'aroc')
setnames(b_aroc_s, old = 'draw', new = 'variable')

###########################################################################
# Fix for Equatorial Guine and Myanmar
###########################################################################

# loading locations
locations <- fread(paste0(root, 'FILEPATH'))
loca <- locations[level == 3, c('location_id', 'parent_id')]

# merge arocs and location
fix <- merge(b_aroc_s, loca, by = 'location_id')

# estimating median by draw and parent id
fix[, median_aroc := median(aroc), by = c('variable', 'parent_id')]

##
## replace values in trouble countries with the median aroc
##

# Myanmar
fix[location_id == 15, aroc := median_aroc]

# Equatorial Guinea
fix[location_id == 172, aroc := median_aroc]

# selecting only columns of interest and saving needed df 
b_aroc_f <- fix[, c('location_id', 'location_name', 'variable', 'aroc')]


fwrite(b_aroc_f, 
       paste0(root, 'FILEPATH'), 
       row.names = F)

############################################################################

# thetas from ST-GPR
x <- fread("ADDRESS")

head(x)

x_19 <- x[year_id == 2019, ]
setnames(x_19, old = 'value', new = 'x')

# merges with arocs and keeps only 195 locs
x_30 <- merge(x_19, b_aroc_f, by = c('location_id', 'variable'))

# # Raises aroc^13
x_30[, aroc_30_19 := (1 + aroc)^(2030-2019)]

# Loop to calculate all the years. 
year_end <- (2020:2050)

for (year in year_end){
  x_30[, paste0('aroc_', year, '_19') := (1 + aroc)^(year-2019) ]
}


# calculating the reference x for each year between 208 and 2030

# # Multiply aroc^13 by x_19 

x_30[, x_50_r := aroc_2050_19 * x]
x_30[, x_49_r := aroc_2049_19 * x]
x_30[, x_48_r := aroc_2048_19 * x]
x_30[, x_47_r := aroc_2047_19 * x]
x_30[, x_46_r := aroc_2046_19 * x]
x_30[, x_45_r := aroc_2045_19 * x]
x_30[, x_44_r := aroc_2044_19 * x]
x_30[, x_43_r := aroc_2043_19 * x]
x_30[, x_42_r := aroc_2042_19 * x]
x_30[, x_41_r := aroc_2041_19 * x]
x_30[, x_40_r := aroc_2040_19 * x]

x_30[, x_39_r := aroc_2039_19 * x]
x_30[, x_38_r := aroc_2038_19 * x]
x_30[, x_37_r := aroc_2037_19 * x]
x_30[, x_36_r := aroc_2036_19 * x]
x_30[, x_35_r := aroc_2035_19 * x]
x_30[, x_34_r := aroc_2034_19 * x]
x_30[, x_33_r := aroc_2033_19 * x]
x_30[, x_32_r := aroc_2032_19 * x]
x_30[, x_31_r := aroc_2031_19 * x]


x_30[, x_30_r := aroc_2030_19 * x]
x_30[, x_29_r := aroc_2029_19 * x]
x_30[, x_28_r := aroc_2028_19 * x]
x_30[, x_27_r := aroc_2027_19 * x]
x_30[, x_26_r := aroc_2026_19 * x]
x_30[, x_25_r := aroc_2025_19 * x]
x_30[, x_24_r := aroc_2024_19 * x]
x_30[, x_23_r := aroc_2023_19 * x]
x_30[, x_22_r := aroc_2022_19 * x]
x_30[, x_21_r := aroc_2021_19 * x]
x_30[, x_20_r := aroc_2020_19 * x]

head(x_30)


# saving by location to speed up the process
locs <- unique(x_30$location_id)
setnames(x_30, old = 'variable', new = 'draw')
bl_path <- "ADDRESS"


for (loc in locs){
  print(loc) 
  
  beta_stan <- fread(paste0(bl_path, 'FILEPATH', loc, 'FILEPATH'))
  
  x_1 <- x_30[location_id == loc, c('location_id', 'draw', 'location_name', 
                                          'x_50_r', 'x_49_r', 'x_48_r', 'x_47_r', 
                                          'x_46_r', 'x_45_r', 'x_44_r', 'x_43_r', 
                                          'x_42_r', 'x_41_r', 'x_40_r', 'x_39_r', 
                                          'x_38_r', 'x_37_r', 'x_36_r', 'x_35_r', 
                                          'x_34_r', 'x_33_r', 'x_32_r', 'x_31_r',
                                          'x_30_r', 'x_29_r', 'x_28_r', 'x_27_r',
                                          'x_26_r', 'x_25_r', 'x_24_r', 'x_23_r',
                                          'x_22_r', 'x_21_r', 'x_20_r')]

  
  x_1l <- as.data.table(melt(x_1, 
                                id.vars = c('location_id', 'draw', 'location_name')))
  
  setnames(x_1l, old = 'value', new = 'x')
  x_1l[, year := tstrsplit(variable, "_", keep = 2)]
  x_1l[, year_id := as.numeric(paste0(20, year))]
  x_1l[, year := NULL]  
  x_1l[, variable := NULL]
  
  data <- merge(beta_stan, x_1l)
  
  # Adjusting x so it makes sense with the model
  data[x < beta, x := beta ] # x needs be to >= beta
  data[x > .9, x := .9] # using the same approach as beta
  
  data_f <- data[, c('location_id', 'draw', 'location_name', 'year_id', 'x')]

  fwrite(data_f,
         paste0(bl_path, 'FILEPATH', loc, 'FILEPATH'),
         row.names = F)
}
