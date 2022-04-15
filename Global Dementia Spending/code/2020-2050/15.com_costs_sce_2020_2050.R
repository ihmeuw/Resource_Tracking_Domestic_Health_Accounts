######
# This script calculates 2022 com reference scenario
# 1 + aroc^(2022-2019) * com(2019)
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
com_aroc_s <- com_aroc[, c('location_id', 'location_name','variable', 'aroc')]
setnames(com_aroc_s, old = 'variable', new = 'draw')

# picking the 85th percentile

draws <- paste0('draw_', (0:999))

for (draw_t in draws){
    # looing over draws
    a <- com_aroc_s[ draw == draw_t, ]
    b <- setorder(a, 'aroc')
    loc_id <- b[166, unique(location_id)]
    print(loc_id)
    c <- b[location_id == loc_id, aroc]
    print(paste0('85th AROC: ', c))
    com_aroc_s[draw == draw_t, aroc_sce := c]
    
}

# scenario AROCS
aroc_sce <- copy(com_aroc_s)
aroc_sce[aroc < aroc_sce, aroc := aroc_sce]
head(aroc_sce)

# coms from ST-GPR
com <- fread(paste0(root, 'FILEPATH'))
com_19 <- com[year_id == 2019, ]

setnames(com_19, old = 'variable', new = 'draw')

# merges with arocs and keeps only 195 locs
com_sce <- merge(com_19, aroc_sce)

# Raises aroc^13
years <- (2020:2050)

# calculating arocs from 2020 to 2050
for (year in years){
  com_sce[, paste0('aroc_', year, '_19') := (1 + aroc)^(year-2019) ]
}




# Multiply aroc^13 by com_19 

com_sce[, com_sce_total_50 := aroc_2050_19 * total]
com_sce[, com_sce_attri_50 := aroc_2050_19 * attributable]

com_sce[, com_sce_total_49 := aroc_2049_19 * total]
com_sce[, com_sce_attri_49 := aroc_2049_19 * attributable]

com_sce[, com_sce_total_48 := aroc_2048_19 * total]
com_sce[, com_sce_attri_48 := aroc_2048_19 * attributable]

com_sce[, com_sce_total_47 := aroc_2047_19 * total]
com_sce[, com_sce_attri_47 := aroc_2047_19 * attributable]

com_sce[, com_sce_total_46 := aroc_2046_19 * total]
com_sce[, com_sce_attri_46 := aroc_2046_19 * attributable]

com_sce[, com_sce_total_45 := aroc_2045_19 * total]
com_sce[, com_sce_attri_45 := aroc_2045_19 * attributable]

com_sce[, com_sce_total_44 := aroc_2044_19 * total]
com_sce[, com_sce_attri_44 := aroc_2044_19 * attributable]

com_sce[, com_sce_total_43 := aroc_2043_19 * total]
com_sce[, com_sce_attri_43 := aroc_2043_19 * attributable]

com_sce[, com_sce_total_42 := aroc_2042_19 * total]
com_sce[, com_sce_attri_42 := aroc_2042_19 * attributable]

com_sce[, com_sce_total_41 := aroc_2041_19 * total]
com_sce[, com_sce_attri_41 := aroc_2041_19 * attributable]

com_sce[, com_sce_total_40 := aroc_2040_19 * total]
com_sce[, com_sce_attri_40 := aroc_2040_19 * attributable]


com_sce[, com_sce_total_39 := aroc_2039_19 * total]
com_sce[, com_sce_attri_39 := aroc_2039_19 * attributable]

com_sce[, com_sce_total_38 := aroc_2038_19 * total]
com_sce[, com_sce_attri_38 := aroc_2038_19 * attributable]

com_sce[, com_sce_total_37 := aroc_2037_19 * total]
com_sce[, com_sce_attri_37 := aroc_2037_19 * attributable]

com_sce[, com_sce_total_36 := aroc_2036_19 * total]
com_sce[, com_sce_attri_36 := aroc_2036_19 * attributable]

com_sce[, com_sce_total_35 := aroc_2035_19 * total]
com_sce[, com_sce_attri_35 := aroc_2035_19 * attributable]

com_sce[, com_sce_total_34 := aroc_2034_19 * total]
com_sce[, com_sce_attri_34 := aroc_2034_19 * attributable]

com_sce[, com_sce_total_33 := aroc_2033_19 * total]
com_sce[, com_sce_attri_33 := aroc_2033_19 * attributable]

com_sce[, com_sce_total_32 := aroc_2032_19 * total]
com_sce[, com_sce_attri_32 := aroc_2032_19 * attributable]

com_sce[, com_sce_total_31 := aroc_2031_19 * total]
com_sce[, com_sce_attri_31 := aroc_2031_19 * attributable]

com_sce[, com_sce_total_30 := aroc_2030_19 * total]
com_sce[, com_sce_attri_30 := aroc_2030_19 * attributable]


com_sce[, com_sce_total_29 := aroc_2029_19 * total]
com_sce[, com_sce_attri_29 := aroc_2029_19 * attributable]

com_sce[, com_sce_total_28 := aroc_2028_19 * total]
com_sce[, com_sce_attri_28 := aroc_2028_19 * attributable]

com_sce[, com_sce_total_27 := aroc_2027_19 * total]
com_sce[, com_sce_attri_27 := aroc_2027_19 * attributable]

com_sce[, com_sce_total_26 := aroc_2026_19 * total]
com_sce[, com_sce_attri_26 := aroc_2026_19 * attributable]

com_sce[, com_sce_total_25 := aroc_2025_19 * total]
com_sce[, com_sce_attri_25 := aroc_2025_19 * attributable]

com_sce[, com_sce_total_24 := aroc_2024_19 * total]
com_sce[, com_sce_attri_24 := aroc_2024_19 * attributable]

com_sce[, com_sce_total_23 := aroc_2023_19 * total]
com_sce[, com_sce_attri_23 := aroc_2023_19 * attributable]

com_sce[, com_sce_total_22 := aroc_2022_19 * total]
com_sce[, com_sce_attri_22 := aroc_2022_19 * attributable]

com_sce[, com_sce_total_21 := aroc_2021_19 * total]
com_sce[, com_sce_attri_21 := aroc_2021_19 * attributable]

com_sce[, com_sce_total_20 := aroc_2020_19 * total]
com_sce[, com_sce_attri_20 := aroc_2020_19 * attributable]

head(com_sce)

### saving by location
locations <- unique(com_sce$location_id)

out_root <- "ADDRESS"
for (loc in locations){
  print(loc)
  
  fc <- com_sce[location_id == loc, c('location_id', 'draw', 
                                      'com_sce_total_50', 'com_sce_attri_50', 'com_sce_total_49', 'com_sce_attri_49', 
                                      'com_sce_total_48', 'com_sce_attri_48', 'com_sce_total_47', 'com_sce_attri_47',
                                      'com_sce_total_46', 'com_sce_attri_46', 'com_sce_total_45', 'com_sce_attri_45', 
                                      'com_sce_total_44', 'com_sce_attri_44', 'com_sce_total_43', 'com_sce_attri_43', 
                                      'com_sce_total_42', 'com_sce_attri_42', 'com_sce_total_41', 'com_sce_attri_41', 
                                      'com_sce_total_40', 'com_sce_attri_40', 'com_sce_total_39', 'com_sce_attri_39', 
                                      'com_sce_total_38', 'com_sce_attri_38', 'com_sce_total_37', 'com_sce_attri_37',
                                      'com_sce_total_36', 'com_sce_attri_36', 'com_sce_total_35', 'com_sce_attri_35', 
                                      'com_sce_total_34', 'com_sce_attri_34', 'com_sce_total_33', 'com_sce_attri_33', 
                                      'com_sce_total_32', 'com_sce_attri_32', 'com_sce_total_31', 'com_sce_attri_31',
                                      'com_sce_total_30', 'com_sce_attri_30', 'com_sce_total_29', 'com_sce_attri_29', 
                                      'com_sce_total_28', 'com_sce_attri_28', 'com_sce_total_27', 'com_sce_attri_27',
                                      'com_sce_total_26', 'com_sce_attri_26', 'com_sce_total_25', 'com_sce_attri_25', 
                                      'com_sce_total_24', 'com_sce_attri_24', 'com_sce_total_23', 'com_sce_attri_23', 
                                      'com_sce_total_22', 'com_sce_attri_22', 'com_sce_total_21', 'com_sce_attri_21', 
                                      'com_sce_total_20', 'com_sce_attri_20')]
  
  
  com_l <- as.data.table(melt(fc, 
                              id.vars = c('location_id', 'draw')))
  
  setnames(com_l, old = 'value', new = 'com_cost')
  
  com_l[, year := tstrsplit(variable, "_", keep = 4)]
  com_l[, year_id := as.numeric(paste0(20, year))]
  com_l[, year := NULL]  
  com_l[, cost_type := tstrsplit(variable, "_", keep = 3)]
  
  com_l[, variable := NULL]

  
  fwrite(com_l, paste0(out_root, loc, 'FILEPATH'),
         row.names = F)

}