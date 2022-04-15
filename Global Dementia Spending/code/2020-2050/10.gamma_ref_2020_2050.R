######
# This script calculates 2030 gamma reference scenario
# 1 + aroc^(2030-2019) * gamma(2019)
# Take the mean of the above
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

# gamma AROCS
g_aroc <- fread(paste0(root, 'FILEPATH'))
g_aroc_s <- g_aroc[, c('location_id', 'location_name', 'aroc_output', 'draw')]
setnames(g_aroc_s, old = 'aroc_output', new = 'aroc')

###########################################################################
# Fix for Equatorial Guine and Myanmar
###########################################################################

# loading locations
locations <- fread(paste0(root, 'FILEPATH'))
loca <- locations[level == 3, c('location_id', 'parent_id')]

# merge arocs and location
fix <- merge(g_aroc_s, loca, by = 'location_id')

# estimating median by draw and parent id
fix[, median_aroc := median(aroc), by = c('draw', 'parent_id')]

##
## replace values in trouble countries with the median aroc
##

# Myanmar
fix[location_id == 15, aroc := median_aroc]

# Equatorial Guinea
fix[location_id == 172, aroc := median_aroc]

# selecting only columns of interest and saving needed df 
g_aroc_f <- fix[, c('location_id', 'location_name', 'draw', 'aroc')]

fwrite(g_aroc_f, 
       paste0(root, 'FILEPATH'), 
       row.names = F)

############################################################################


# gammas from ST-GPR
gamma <- fread(paste0(root, "FILEPATH"))
head(gamma)
gamma_19 <- gamma[year_id == 2019, ]

setnames(gamma_19, old = "variable", new = "draw")
head(gamma_19)

# merges with arocs and keeps only 195 locs
gamma_30 <- merge(gamma_19, g_aroc_f, by = c('draw','location_id'))
head(gamma_30)

# Raises aroc^13
gamma_30[, aroc_30_19 := (1 + aroc)^(2030-2019)]

# Loop to calculate all the years. 
year_end <- (2020:2050)

for (year in year_end){
  gamma_30[, paste0('aroc_', year, '_19') := (1 + aroc)^(year-2019) ]
}

# Multiply aroc^13 by gamma_19 
gamma_30[, gamma_30_r := aroc_30_19 * value]

# calculating the reference value for each year between 2020 and 2030

# # Multiply aroc^13 by beta_19 
gamma_30[, gamma_50_r := aroc_2050_19 * value]
gamma_30[, gamma_49_r := aroc_2049_19 * value]
gamma_30[, gamma_48_r := aroc_2048_19 * value]
gamma_30[, gamma_47_r := aroc_2047_19 * value]
gamma_30[, gamma_46_r := aroc_2046_19 * value]
gamma_30[, gamma_45_r := aroc_2045_19 * value]
gamma_30[, gamma_44_r := aroc_2044_19 * value]
gamma_30[, gamma_43_r := aroc_2043_19 * value]
gamma_30[, gamma_42_r := aroc_2042_19 * value]
gamma_30[, gamma_41_r := aroc_2041_19 * value]
gamma_30[, gamma_40_r := aroc_2040_19 * value]

gamma_30[, gamma_39_r := aroc_2039_19 * value]
gamma_30[, gamma_38_r := aroc_2038_19 * value]
gamma_30[, gamma_37_r := aroc_2037_19 * value]
gamma_30[, gamma_36_r := aroc_2036_19 * value]
gamma_30[, gamma_35_r := aroc_2035_19 * value]
gamma_30[, gamma_34_r := aroc_2034_19 * value]
gamma_30[, gamma_33_r := aroc_2033_19 * value]
gamma_30[, gamma_32_r := aroc_2032_19 * value]
gamma_30[, gamma_31_r := aroc_2031_19 * value]



gamma_30[, gamma_30_r := aroc_2030_19 * value]
gamma_30[, gamma_29_r := aroc_2029_19 * value]
gamma_30[, gamma_28_r := aroc_2028_19 * value]
gamma_30[, gamma_27_r := aroc_2027_19 * value]
gamma_30[, gamma_26_r := aroc_2026_19 * value]
gamma_30[, gamma_25_r := aroc_2025_19 * value]
gamma_30[, gamma_24_r := aroc_2024_19 * value]
gamma_30[, gamma_23_r := aroc_2023_19 * value]
gamma_30[, gamma_22_r := aroc_2022_19 * value]
gamma_30[, gamma_21_r := aroc_2021_19 * value]
gamma_30[, gamma_20_r := aroc_2020_19 * value]

head(gamma_30)

### saving by location to save time

locs <- unique(gamma_30$location_id)
bl_path <- "ADDRESS"

for (loc in locs){
  
  inst_m <- gamma_30[location_id == loc, 
                     c('location_id', 'location_name', 'draw',
                     'gamma_50_r', 'gamma_49_r', 'gamma_48_r', 'gamma_47_r', 
                     'gamma_46_r', 'gamma_45_r', 'gamma_44_r', 'gamma_43_r', 
                     'gamma_42_r', 'gamma_41_r', 'gamma_40_r', 'gamma_39_r', 
                     'gamma_38_r', 'gamma_37_r', 'gamma_36_r', 'gamma_35_r', 
                     'gamma_34_r', 'gamma_33_r', 'gamma_32_r', 'gamma_31_r',
                     'gamma_30_r', 'gamma_29_r', 'gamma_28_r', 'gamma_27_r', 
                     'gamma_26_r', 'gamma_25_r', 'gamma_24_r', 'gamma_23_r', 
                     'gamma_22_r', 'gamma_21_r', 'gamma_20_r')]
  
  
  gammal <- as.data.table(melt(inst_m, 
                              id.vars = c('location_id', 'location_name', 'draw')))
  
  setnames(gammal, old = 'value', new = 'gamma')
  gammal[, year := tstrsplit(variable, "_", keep = 2)]
  gammal[, year_id := as.numeric(paste0(20, year))]
  gammal[, year := NULL]  
  gammal[, variable := NULL]
  
  gammal[gamma > .8, gamma := .8]
  
  fwrite(gammal,
         paste0(bl_path, 'FILEPATH', loc, 'FILEPATH'),
         row.names = F)
}

