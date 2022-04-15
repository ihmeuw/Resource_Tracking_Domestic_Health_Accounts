######
# This script calculates 2030 beta reference scenario
# aroc^(2030-2019) * beta(2019)
# Take the mean of the above
########

# Set directories 
if (Sys.info()["sysname"] =="Linux") {
  root <- "ADDRESS"
} else {
  root <- "ADDRESS"
}

# loading libraries
library(data.table)

# loading data

# beta AROCS
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

# saving so I can use for scenarios

fwrite(b_aroc_f, 
       paste0(root, 'FILEPATH'), 
       row.names = F)


############################################################################

# betas from ST-GPR
beta <- fread(paste0(root, 'FILEPATH')) 

beta <- beta[year_id > 1999, ]
beta_19 <- beta[year_id == 2019, ]

# merges with arocs and keeps only 195 locs
beta_30 <- merge(beta_19, b_aroc_f, by = c('location_id', 'variable'))

# # Raises aroc^13
# this is the equation we used
# beta_30[, aroc_30_19 := (1 + aroc)^(2030-2019)]

# Loop to calculate all the years. 
year_end <- (2020:2050) # updated to 2050

for (year in year_end){
  beta_30[, paste0('aroc_', year, '_19') := (1 + aroc)^(year-2019) ]
}


# calculating the reference value for each year between 208 and 2030

# # Multiply aroc^13 by beta_19 

beta_30[, beta_50_r := aroc_2050_19 * value]
beta_30[, beta_49_r := aroc_2049_19 * value]
beta_30[, beta_48_r := aroc_2048_19 * value]
beta_30[, beta_47_r := aroc_2047_19 * value]
beta_30[, beta_46_r := aroc_2046_19 * value]
beta_30[, beta_45_r := aroc_2045_19 * value]
beta_30[, beta_44_r := aroc_2044_19 * value]
beta_30[, beta_43_r := aroc_2043_19 * value]
beta_30[, beta_42_r := aroc_2042_19 * value]
beta_30[, beta_41_r := aroc_2041_19 * value]
beta_30[, beta_40_r := aroc_2040_19 * value]

beta_30[, beta_39_r := aroc_2039_19 * value]
beta_30[, beta_38_r := aroc_2038_19 * value]
beta_30[, beta_37_r := aroc_2037_19 * value]
beta_30[, beta_36_r := aroc_2036_19 * value]
beta_30[, beta_35_r := aroc_2035_19 * value]
beta_30[, beta_34_r := aroc_2034_19 * value]
beta_30[, beta_33_r := aroc_2033_19 * value]
beta_30[, beta_32_r := aroc_2032_19 * value]
beta_30[, beta_31_r := aroc_2031_19 * value]

beta_30[, beta_30_r := aroc_2030_19 * value]
beta_30[, beta_29_r := aroc_2029_19 * value]
beta_30[, beta_28_r := aroc_2028_19 * value]
beta_30[, beta_27_r := aroc_2027_19 * value]
beta_30[, beta_26_r := aroc_2026_19 * value]
beta_30[, beta_25_r := aroc_2025_19 * value]
beta_30[, beta_24_r := aroc_2024_19 * value]
beta_30[, beta_23_r := aroc_2023_19 * value]
beta_30[, beta_22_r := aroc_2022_19 * value]
beta_30[, beta_21_r := aroc_2021_19 * value]
beta_30[, beta_20_r := aroc_2020_19 * value]

head(beta_30)


# saving by location to speed up the process
locs <- unique(beta_30$location_id)
setnames(beta_30, old = 'variable', new = 'draw')
bl_path <- "ADDRESS"


for (loc in locs){
  print(loc) 
  
  beta_1 <- beta_30[location_id == loc, c('location_id', 'draw', 'location_name', 
                     'beta_50_r', 'beta_49_r', 'beta_48_r', 'beta_47_r', 
                     'beta_46_r', 'beta_45_r', 'beta_44_r', 'beta_43_r', 
                     'beta_42_r', 'beta_41_r', 'beta_40_r', 'beta_39_r', 
                     'beta_38_r', 'beta_37_r', 'beta_36_r', 'beta_35_r', 
                     'beta_34_r', 'beta_33_r', 'beta_32_r', 'beta_31_r',
                     'beta_30_r', 'beta_29_r', 'beta_28_r', 'beta_27_r',
                     'beta_26_r', 'beta_25_r', 'beta_24_r', 'beta_23_r',
                     'beta_22_r', 'beta_21_r', 'beta_20_r')]
  
  beta_1l <- as.data.table(melt(beta_1, 
                                id.vars = c('location_id', 'draw', 'location_name')))
  
  setnames(beta_1l, old = 'value', new = 'beta')
  beta_1l[, year := tstrsplit(variable, "_", keep = 2)]
  beta_1l[, year_id := as.numeric(paste0(20, year))]
  beta_1l[, year := NULL]  
  beta_1l[, variable := NULL]
  
  ## setting limit of .90 for when beta is greater than .90
  beta_1l[beta > .9, beta := .9]
  
  
  fwrite(beta_1l,
         paste0(bl_path, 'FILEPATH', loc, 'FILEPATH'),
         row.names = F)
}


