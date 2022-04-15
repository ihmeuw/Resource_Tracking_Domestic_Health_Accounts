######
# This script calculates 2030 com reference scenario
# 1 + aroc^(2030-2019) * com(2019)
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

# com AROCS
com_aroc <- fread(paste0(root, 'FILEPATH'))
com_aroc_s <- com_aroc[, c('location_id', 'location_name', 'draw', 'aroc_output')]

setnames(com_aroc_s, old = 'aroc_output', new = 'aroc')
setnames(com_aroc_s, old = 'draw', new = 'variable')

###########################################################################
# Fix for Equatorial Guine and Myanmar
###########################################################################

# loading locations
locations <- fread(paste0(root, 'FILEPATH'))
loca <- locations[level == 3, c('location_id', 'parent_id')]

# merge arocs and location
fix <- merge(com_aroc_s, loca, by = 'location_id')

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
com_aroc_f <- fix[, c('location_id', 'location_name', 'variable', 'aroc')]


# saving so I can use for scenarios

fwrite(com_aroc_f, 
       paste0(root, 'FILEPATH'), 
       row.names = F)
############################################################################



# coms from ST-GPR
com <- fread(paste0(root, 'FILEPATH'))
head(com)

com_19 <- com[year_id == 2019, ]

# merges with arocs and keeps only 195 locs
com_30 <- merge(com_19, com_aroc_f, by = c('location_id', 'variable'))

# Raises aroc^13
com_30[, aroc_30_19 := (1 + aroc)^(2030-2019)]


# Loop to calculate all the years. 
year_end <- (2020:2050)

for (year in year_end){
  com_30[, paste0('aroc_', year, '_19') := (1 + aroc)^(year-2019) ]
}

head(com_30)

##
# Attribution
## 

# # Multiply aroc^13 by beta_19 
com_30[, com_50_r_attri := aroc_2050_19 * attributable]
com_30[, com_49_r_attri := aroc_2049_19 * attributable]
com_30[, com_48_r_attri := aroc_2048_19 * attributable]
com_30[, com_47_r_attri := aroc_2047_19 * attributable]
com_30[, com_46_r_attri := aroc_2046_19 * attributable]
com_30[, com_45_r_attri := aroc_2045_19 * attributable]
com_30[, com_44_r_attri := aroc_2044_19 * attributable]
com_30[, com_43_r_attri := aroc_2043_19 * attributable]
com_30[, com_42_r_attri := aroc_2042_19 * attributable]
com_30[, com_41_r_attri := aroc_2041_19 * attributable]
com_30[, com_40_r_attri := aroc_2040_19 * attributable]

com_30[, com_39_r_attri := aroc_2039_19 * attributable]
com_30[, com_38_r_attri := aroc_2038_19 * attributable]
com_30[, com_37_r_attri := aroc_2037_19 * attributable]
com_30[, com_36_r_attri := aroc_2036_19 * attributable]
com_30[, com_35_r_attri := aroc_2035_19 * attributable]
com_30[, com_34_r_attri := aroc_2034_19 * attributable]
com_30[, com_33_r_attri := aroc_2033_19 * attributable]
com_30[, com_32_r_attri := aroc_2032_19 * attributable]
com_30[, com_31_r_attri := aroc_2031_19 * attributable]

com_30[, com_30_r_attri := aroc_2030_19 * attributable]
com_30[, com_29_r_attri := aroc_2029_19 * attributable]
com_30[, com_28_r_attri := aroc_2028_19 * attributable]
com_30[, com_27_r_attri := aroc_2027_19 * attributable]
com_30[, com_26_r_attri := aroc_2026_19 * attributable]
com_30[, com_25_r_attri := aroc_2025_19 * attributable]
com_30[, com_24_r_attri := aroc_2024_19 * attributable]
com_30[, com_23_r_attri := aroc_2023_19 * attributable]
com_30[, com_22_r_attri := aroc_2022_19 * attributable]
com_30[, com_21_r_attri := aroc_2021_19 * attributable]
com_30[, com_20_r_attri := aroc_2020_19 * attributable]

head(com_30)

##
# Total
## 
com_30[, com_50_r_total := aroc_2050_19 * total]
com_30[, com_49_r_total := aroc_2049_19 * total]
com_30[, com_48_r_total := aroc_2048_19 * total]
com_30[, com_47_r_total := aroc_2047_19 * total]
com_30[, com_46_r_total := aroc_2046_19 * total]
com_30[, com_45_r_total := aroc_2045_19 * total]
com_30[, com_44_r_total := aroc_2044_19 * total]
com_30[, com_43_r_total := aroc_2043_19 * total]
com_30[, com_42_r_total := aroc_2042_19 * total]
com_30[, com_41_r_total := aroc_2041_19 * total]
com_30[, com_40_r_total := aroc_2040_19 * total]

com_30[, com_39_r_total := aroc_2039_19 * total]
com_30[, com_38_r_total := aroc_2038_19 * total]
com_30[, com_37_r_total := aroc_2037_19 * total]
com_30[, com_36_r_total := aroc_2036_19 * total]
com_30[, com_35_r_total := aroc_2035_19 * total]
com_30[, com_34_r_total := aroc_2034_19 * total]
com_30[, com_33_r_total := aroc_2033_19 * total]
com_30[, com_32_r_total := aroc_2032_19 * total]
com_30[, com_31_r_total := aroc_2031_19 * total]

com_30[, com_30_r_total := aroc_2030_19 * total]
com_30[, com_29_r_total := aroc_2029_19 * total]
com_30[, com_28_r_total := aroc_2028_19 * total]
com_30[, com_27_r_total := aroc_2027_19 * total]
com_30[, com_26_r_total := aroc_2026_19 * total]
com_30[, com_25_r_total := aroc_2025_19 * total]
com_30[, com_24_r_total := aroc_2024_19 * total]
com_30[, com_23_r_total := aroc_2023_19 * total]
com_30[, com_22_r_total := aroc_2022_19 * total]
com_30[, com_21_r_total := aroc_2021_19 * total]
com_30[, com_20_r_total := aroc_2020_19 * total]

head(com_30)

### Saving bby locations

locs <- unique(com_30$location_id)

bl_path <- "ADDRESS"


for (loc in locs){
  print(loc)

  # subsetting by country
  com_s <- com_30[location_id == loc, c('location_id', 'variable', 
                                        'com_50_r_total', 'com_50_r_attri', 
                                        'com_49_r_total', 'com_49_r_attri',  'com_48_r_total', 'com_48_r_attri',
                                        'com_47_r_total', 'com_47_r_attri',  'com_46_r_total', 'com_46_r_attri', 
                                        'com_45_r_total', 'com_45_r_attri',  'com_44_r_total', 'com_44_r_attri', 
                                        'com_43_r_total', 'com_43_r_attri',  'com_42_r_total', 'com_42_r_attri', 
                                        'com_41_r_total', 'com_41_r_attri',  'com_40_r_total', 'com_40_r_attri', 
                                        
                                        'com_39_r_total', 'com_39_r_attri',  'com_38_r_total', 'com_38_r_attri',
                                        'com_37_r_total', 'com_37_r_attri',  'com_36_r_total', 'com_36_r_attri', 
                                        'com_35_r_total', 'com_35_r_attri',  'com_34_r_total', 'com_34_r_attri', 
                                        'com_33_r_total', 'com_33_r_attri',  'com_32_r_total', 'com_32_r_attri', 
                                        'com_31_r_total', 'com_31_r_attri', 'com_30_r_total', 'com_30_r_attri', 
                                        
                                       'com_29_r_total', 'com_29_r_attri',  'com_28_r_total', 'com_28_r_attri',
                                       'com_27_r_total', 'com_27_r_attri',  'com_26_r_total', 'com_26_r_attri', 
                                       'com_25_r_total', 'com_25_r_attri',  'com_24_r_total', 'com_24_r_attri', 
                                       'com_23_r_total', 'com_23_r_attri',  'com_22_r_total', 'com_22_r_attri', 
                                       'com_21_r_total', 'com_21_r_attri',  'com_20_r_total', 'com_20_r_attri')]
  
  setnames(com_s, old = 'variable', new = 'draw')
  
  
  com_l <- as.data.table(melt(com_s, 
                              id.vars = c('location_id', 'draw')))
  
  setnames(com_l, old = 'value', new = 'com_cost')
  com_l[, year := tstrsplit(variable, "_", keep = 2)]
  com_l[, year_id := as.numeric(paste0(20, year))]
  com_l[, year := NULL]  
  com_l[, cost_type := tstrsplit(variable, "_", keep = 4)]
  
  com_l[, variable := NULL]
  
  fwrite(com_l, 
         paste0(bl_path, 'FILEPATH', loc, 'FILEPATH'),
         row.names = F)
}


