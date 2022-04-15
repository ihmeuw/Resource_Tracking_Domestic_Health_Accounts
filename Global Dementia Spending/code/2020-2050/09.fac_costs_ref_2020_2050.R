######
# This script calculates 2030 fac reference scenario
# 1 + aroc^(2030-2019) * fac(2019)
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

# fac AROCS
fac_aroc <- fread(paste0(root, 'FILEPATH'))
fac_aroc_s <- fac_aroc[, c('location_id', 'location_name', 'draw', 'aroc_output')]
setnames(fac_aroc_s, old = 'aroc_output', new = 'aroc')
setnames(fac_aroc_s, old = 'draw', new = 'variable')

###########################################################################
# Fix for Equatorial Guine and Myanmar
###########################################################################

# loading locations
locations <- fread(paste0(root, 'FILEPATH'))
loca <- locations[level == 3, c('location_id', 'parent_id')]

# merge arocs and location
fix <- merge(fac_aroc_s, loca, by = 'location_id')

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
fac_aroc_f <- fix[, c('location_id', 'location_name', 'variable', 'aroc')]

# saving so I can use for scenarios

fwrite(fac_aroc_f, 
       paste0(root, 'FILEPATH'), 
       row.names = F)

############################################################################



# facs from ST-GPR
fac <- fread(paste0(root, 'FILEPATH'))
fac_19 <- fac[year_id == 2019, ]

# merges with arocs and keeps only 195 locs
fac_30 <- merge(fac_19, fac_aroc_f, by = c('location_id', 'variable'))

# Raises aroc^13
fac_30[, aroc_30_19 := (1 + aroc)^(2030-2019)]

# Loop to calculate all the years. 
year_end <- (2020:2050)

for (year in year_end){
  fac_30[, paste0('aroc_', year, '_19') := (1 + aroc)^(year-2019) ]
}

head(fac_30)



##
# Attribution with dx
## 

# # Multiply aroc^13 by beta_19 

fac_30[, fac_50_r_attri := aroc_2050_19 * attributable_dx]
fac_30[, fac_49_r_attri := aroc_2049_19 * attributable_dx]
fac_30[, fac_48_r_attri := aroc_2048_19 * attributable_dx]
fac_30[, fac_47_r_attri := aroc_2047_19 * attributable_dx]
fac_30[, fac_46_r_attri := aroc_2046_19 * attributable_dx]
fac_30[, fac_45_r_attri := aroc_2045_19 * attributable_dx]
fac_30[, fac_44_r_attri := aroc_2044_19 * attributable_dx]
fac_30[, fac_43_r_attri := aroc_2043_19 * attributable_dx]
fac_30[, fac_42_r_attri := aroc_2042_19 * attributable_dx]
fac_30[, fac_41_r_attri := aroc_2041_19 * attributable_dx]
fac_30[, fac_40_r_attri := aroc_2040_19 * attributable_dx]

fac_30[, fac_39_r_attri := aroc_2039_19 * attributable_dx]
fac_30[, fac_38_r_attri := aroc_2038_19 * attributable_dx]
fac_30[, fac_37_r_attri := aroc_2037_19 * attributable_dx]
fac_30[, fac_36_r_attri := aroc_2036_19 * attributable_dx]
fac_30[, fac_35_r_attri := aroc_2035_19 * attributable_dx]
fac_30[, fac_34_r_attri := aroc_2034_19 * attributable_dx]
fac_30[, fac_33_r_attri := aroc_2033_19 * attributable_dx]
fac_30[, fac_32_r_attri := aroc_2032_19 * attributable_dx]
fac_30[, fac_31_r_attri := aroc_2031_19 * attributable_dx]

fac_30[, fac_30_r_attri := aroc_2030_19 * attributable_dx]
fac_30[, fac_29_r_attri := aroc_2029_19 * attributable_dx]
fac_30[, fac_28_r_attri := aroc_2028_19 * attributable_dx]
fac_30[, fac_27_r_attri := aroc_2027_19 * attributable_dx]
fac_30[, fac_26_r_attri := aroc_2026_19 * attributable_dx]
fac_30[, fac_25_r_attri := aroc_2025_19 * attributable_dx]
fac_30[, fac_24_r_attri := aroc_2024_19 * attributable_dx]
fac_30[, fac_23_r_attri := aroc_2023_19 * attributable_dx]
fac_30[, fac_22_r_attri := aroc_2022_19 * attributable_dx]
fac_30[, fac_21_r_attri := aroc_2021_19 * attributable_dx]
fac_30[, fac_20_r_attri := aroc_2020_19 * attributable_dx]

head(fac_30)


##
# Attribution without dx
## 

# # Multiply aroc^13 by beta_19 

fac_30[, fac_50_r_attri_no_dx := aroc_2050_19 * attributable_no_dx]
fac_30[, fac_49_r_attri_no_dx := aroc_2049_19 * attributable_no_dx]
fac_30[, fac_48_r_attri_no_dx := aroc_2048_19 * attributable_no_dx]
fac_30[, fac_47_r_attri_no_dx := aroc_2047_19 * attributable_no_dx]
fac_30[, fac_46_r_attri_no_dx := aroc_2046_19 * attributable_no_dx]
fac_30[, fac_45_r_attri_no_dx := aroc_2045_19 * attributable_no_dx]
fac_30[, fac_44_r_attri_no_dx := aroc_2044_19 * attributable_no_dx]
fac_30[, fac_43_r_attri_no_dx := aroc_2043_19 * attributable_no_dx]
fac_30[, fac_42_r_attri_no_dx := aroc_2042_19 * attributable_no_dx]
fac_30[, fac_41_r_attri_no_dx := aroc_2041_19 * attributable_no_dx]
fac_30[, fac_40_r_attri_no_dx := aroc_2040_19 * attributable_no_dx]

fac_30[, fac_39_r_attri_no_dx := aroc_2039_19 * attributable_no_dx]
fac_30[, fac_38_r_attri_no_dx := aroc_2038_19 * attributable_no_dx]
fac_30[, fac_37_r_attri_no_dx := aroc_2037_19 * attributable_no_dx]
fac_30[, fac_36_r_attri_no_dx := aroc_2036_19 * attributable_no_dx]
fac_30[, fac_35_r_attri_no_dx := aroc_2035_19 * attributable_no_dx]
fac_30[, fac_34_r_attri_no_dx := aroc_2034_19 * attributable_no_dx]
fac_30[, fac_33_r_attri_no_dx := aroc_2033_19 * attributable_no_dx]
fac_30[, fac_32_r_attri_no_dx := aroc_2032_19 * attributable_no_dx]
fac_30[, fac_31_r_attri_no_dx := aroc_2031_19 * attributable_no_dx]

fac_30[, fac_30_r_attri_no_dx  := aroc_2030_19 * attributable_no_dx]
fac_30[, fac_29_r_attri_no_dx  := aroc_2029_19 * attributable_no_dx]
fac_30[, fac_28_r_attri_no_dx  := aroc_2028_19 * attributable_no_dx]
fac_30[, fac_27_r_attri_no_dx  := aroc_2027_19 * attributable_no_dx]
fac_30[, fac_26_r_attri_no_dx  := aroc_2026_19 * attributable_no_dx]
fac_30[, fac_25_r_attri_no_dx  := aroc_2025_19 * attributable_no_dx]
fac_30[, fac_24_r_attri_no_dx  := aroc_2024_19 * attributable_no_dx]
fac_30[, fac_23_r_attri_no_dx  := aroc_2023_19 * attributable_no_dx]
fac_30[, fac_22_r_attri_no_dx  := aroc_2022_19 * attributable_no_dx]
fac_30[, fac_21_r_attri_no_dx  := aroc_2021_19 * attributable_no_dx]
fac_30[, fac_20_r_attri_no_dx  := aroc_2020_19 * attributable_no_dx]

head(fac_30)


##
# Total
## 
fac_30[, fac_50_r_total := aroc_2050_19 * total]
fac_30[, fac_49_r_total := aroc_2049_19 * total]
fac_30[, fac_48_r_total := aroc_2048_19 * total]
fac_30[, fac_47_r_total := aroc_2047_19 * total]
fac_30[, fac_46_r_total := aroc_2046_19 * total]
fac_30[, fac_45_r_total := aroc_2045_19 * total]
fac_30[, fac_44_r_total := aroc_2044_19 * total]
fac_30[, fac_43_r_total := aroc_2043_19 * total]
fac_30[, fac_42_r_total := aroc_2042_19 * total]
fac_30[, fac_41_r_total := aroc_2041_19 * total]
fac_30[, fac_40_r_total := aroc_2040_19 * total]

fac_30[, fac_39_r_total := aroc_2039_19 * total]
fac_30[, fac_38_r_total := aroc_2038_19 * total]
fac_30[, fac_37_r_total := aroc_2037_19 * total]
fac_30[, fac_36_r_total := aroc_2036_19 * total]
fac_30[, fac_35_r_total := aroc_2035_19 * total]
fac_30[, fac_34_r_total := aroc_2034_19 * total]
fac_30[, fac_33_r_total := aroc_2033_19 * total]
fac_30[, fac_32_r_total := aroc_2032_19 * total]
fac_30[, fac_31_r_total := aroc_2031_19 * total]

fac_30[, fac_30_r_total := aroc_2030_19 * total]
fac_30[, fac_29_r_total := aroc_2029_19 * total]
fac_30[, fac_28_r_total := aroc_2028_19 * total]
fac_30[, fac_27_r_total := aroc_2027_19 * total]
fac_30[, fac_26_r_total := aroc_2026_19 * total]
fac_30[, fac_25_r_total := aroc_2025_19 * total]
fac_30[, fac_24_r_total := aroc_2024_19 * total]
fac_30[, fac_23_r_total := aroc_2023_19 * total]
fac_30[, fac_22_r_total := aroc_2022_19 * total]
fac_30[, fac_21_r_total := aroc_2021_19 * total]
fac_30[, fac_20_r_total := aroc_2020_19 * total]

head(fac_30)


## Saving by location

locations <- unique(fac_30$location_id)


bl_path <- "ADDRESS"

for (loc in locations){
  print(loc)
  
  fac_m <- fac_30[location_id == loc, c('location_id','location_name', 'variable', 
                                        
                                        'fac_50_r_total', 'fac_50_r_attri', 'fac_50_r_attri_no_dx', 
                                        'fac_49_r_total', 'fac_49_r_attri', 'fac_49_r_attri_no_dx', 'fac_48_r_total', 'fac_48_r_attri', 'fac_48_r_attri_no_dx', 
                                        'fac_47_r_total', 'fac_47_r_attri', 'fac_47_r_attri_no_dx', 'fac_46_r_total', 'fac_46_r_attri', 'fac_46_r_attri_no_dx', 
                                        'fac_45_r_total', 'fac_45_r_attri', 'fac_45_r_attri_no_dx', 'fac_44_r_total', 'fac_44_r_attri', 'fac_44_r_attri_no_dx', 
                                        'fac_43_r_total', 'fac_43_r_attri', 'fac_43_r_attri_no_dx', 'fac_42_r_total', 'fac_42_r_attri', 'fac_42_r_attri_no_dx', 
                                        'fac_41_r_total', 'fac_41_r_attri', 'fac_41_r_attri_no_dx', 'fac_40_r_total', 'fac_40_r_attri', 'fac_40_r_attri_no_dx', 
                                        
                                        
                                        'fac_39_r_total', 'fac_39_r_attri', 'fac_39_r_attri_no_dx', 'fac_38_r_total', 'fac_38_r_attri', 'fac_38_r_attri_no_dx', 
                                        'fac_37_r_total', 'fac_37_r_attri', 'fac_37_r_attri_no_dx', 'fac_36_r_total', 'fac_36_r_attri', 'fac_36_r_attri_no_dx', 
                                        'fac_35_r_total', 'fac_35_r_attri', 'fac_35_r_attri_no_dx', 'fac_34_r_total', 'fac_34_r_attri', 'fac_34_r_attri_no_dx', 
                                        'fac_33_r_total', 'fac_33_r_attri', 'fac_33_r_attri_no_dx', 'fac_32_r_total', 'fac_32_r_attri', 'fac_32_r_attri_no_dx', 
                                        'fac_31_r_total', 'fac_31_r_attri', 'fac_31_r_attri_no_dx', 'fac_30_r_total', 'fac_30_r_attri', 'fac_30_r_attri_no_dx', 
                                        
                                       'fac_29_r_total', 'fac_29_r_attri', 'fac_29_r_attri_no_dx', 'fac_28_r_total', 'fac_28_r_attri', 'fac_28_r_attri_no_dx', 
                                       'fac_27_r_total', 'fac_27_r_attri', 'fac_27_r_attri_no_dx', 'fac_26_r_total', 'fac_26_r_attri', 'fac_26_r_attri_no_dx', 
                                       'fac_25_r_total', 'fac_25_r_attri', 'fac_25_r_attri_no_dx', 'fac_24_r_total', 'fac_24_r_attri', 'fac_24_r_attri_no_dx', 
                                       'fac_23_r_total', 'fac_23_r_attri', 'fac_23_r_attri_no_dx', 'fac_22_r_total', 'fac_22_r_attri', 'fac_22_r_attri_no_dx', 
                                       'fac_21_r_total', 'fac_21_r_attri', 'fac_21_r_attri_no_dx', 'fac_20_r_total', 'fac_20_r_attri', 'fac_20_r_attri_no_dx')]
  
  setnames(fac_m, old = 'variable', new = 'draw')

  fac_l <- as.data.table(melt(fac_m, 
                              id.vars = c('location_id', 'location_name','draw')))
  

  setnames(fac_l, old = 'value', new = 'fac_cost')
  fac_l[, year := tstrsplit(variable, "_", keep = 2)]
  fac_l[, year_id := as.numeric(paste0(20, year))]
  fac_l[, year := NULL]  
  fac_l[, cost_type := tstrsplit(variable, "_", keep = 4)]
  fac_l[, dx_status := tstrsplit(variable, "_", keep = 5)]
  fac_l[, variable := NULL]
  
  fac_l[!is.na(dx_status), dx_status := 'no_dx']
  fac_l[is.na(dx_status), dx_status := 'dx']
  
  
  head(fac_l)
  
  
  fwrite(fac_l,
         paste0(bl_path, 'FILEPATH', loc, 'FILEPATH'),
         row.names = F)
  # 
}
