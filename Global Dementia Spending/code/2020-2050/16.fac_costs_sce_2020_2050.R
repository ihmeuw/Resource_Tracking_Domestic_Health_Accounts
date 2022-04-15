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
fac_aroc_s <- fac_aroc[, c('location_id', 'location_name', 'variable', 'aroc')]
setnames(fac_aroc_s, old = 'variable', new = 'draw')




# picking the 85th percentile

draws <- paste0('draw_', (0:999))

for (draw_t in draws){
    # looing over draws
    a <- fac_aroc_s[ draw == draw_t, ]
    b <- setorder(a, 'aroc')
    loc_id <- b[166, unique(location_id)]
    print(loc_id)
    c <- b[location_id == loc_id, aroc]
    print(paste0('85th AROC: ', c))
    fac_aroc_s[draw == draw_t, aroc_sce := c]
    
}

# scenario AROCS
aroc_sce <- copy(fac_aroc_s)
aroc_sce[aroc < aroc_sce, aroc := aroc_sce]
head(aroc_sce)

# facs from ST-GPR
fac <- fread(paste0(root, 'FILEPATH'))
fac_19 <- fac[year_id == 2019, ]
setnames(fac_19, old = 'variable', new = 'draw')


# merges with arocs and keeps only 195 locs
fac_sce <- merge(fac_19, aroc_sce)
head(fac_sce)

# Raises aroc^13
years <- (2020:2050)

# calculating arocs from 2040 to 2050
for (year in years){
  fac_sce[, paste0('aroc_', year, '_19') := (1 + aroc)^(year-2019) ]
}



# Multiply aroc^13 by fac_19 
fac_sce[, fac_sce_total_50 := aroc_2050_19 * total]
fac_sce[, fac_sce_attri_dx_50 := aroc_2050_19 * attributable_dx]
fac_sce[, fac_sce_attri_no_dx_50 := aroc_2050_19 * attributable_no_dx]

fac_sce[, fac_sce_total_49 := aroc_2049_19 * total]
fac_sce[, fac_sce_attri_dx_49 := aroc_2049_19 * attributable_dx]
fac_sce[, fac_sce_attri_no_dx_49 := aroc_2049_19 * attributable_no_dx]

fac_sce[, fac_sce_total_48 := aroc_2048_19 * total]
fac_sce[, fac_sce_attri_dx_48 := aroc_2048_19 * attributable_dx]
fac_sce[, fac_sce_attri_no_dx_48 := aroc_2048_19 * attributable_no_dx]

fac_sce[, fac_sce_total_47 := aroc_2047_19 * total]
fac_sce[, fac_sce_attri_dx_47 := aroc_2047_19 * attributable_dx]
fac_sce[, fac_sce_attri_no_dx_47 := aroc_2047_19 * attributable_no_dx]

fac_sce[, fac_sce_total_46 := aroc_2046_19 * total]
fac_sce[, fac_sce_attri_dx_46 := aroc_2046_19 * attributable_dx]
fac_sce[, fac_sce_attri_no_dx_46 := aroc_2046_19 * attributable_no_dx]

fac_sce[, fac_sce_total_45 := aroc_2045_19 * total]
fac_sce[, fac_sce_attri_dx_45 := aroc_2045_19 * attributable_dx]
fac_sce[, fac_sce_attri_no_dx_45 := aroc_2045_19 * attributable_no_dx]

fac_sce[, fac_sce_total_44 := aroc_2044_19 * total]
fac_sce[, fac_sce_attri_dx_44 := aroc_2044_19 * attributable_dx]
fac_sce[, fac_sce_attri_no_dx_44 := aroc_2044_19 * attributable_no_dx]

fac_sce[, fac_sce_total_43 := aroc_2043_19 * total]
fac_sce[, fac_sce_attri_dx_43 := aroc_2043_19 * attributable_dx]
fac_sce[, fac_sce_attri_no_dx_43 := aroc_2043_19 * attributable_no_dx]

fac_sce[, fac_sce_total_42 := aroc_2042_19 * total]
fac_sce[, fac_sce_attri_dx_42 := aroc_2042_19 * attributable_dx]
fac_sce[, fac_sce_attri_no_dx_42 := aroc_2042_19 * attributable_no_dx]

fac_sce[, fac_sce_total_41 := aroc_2041_19 * total]
fac_sce[, fac_sce_attri_dx_41 := aroc_2041_19 * attributable_dx]
fac_sce[, fac_sce_attri_no_dx_41 := aroc_2041_19 * attributable_no_dx]

fac_sce[, fac_sce_total_40 := aroc_2040_19 * total]
fac_sce[, fac_sce_attri_dx_40 := aroc_2040_19 * attributable_dx]
fac_sce[, fac_sce_attri_no_dx_40 := aroc_2040_19 * attributable_no_dx]



fac_sce[, fac_sce_total_39 := aroc_2039_19 * total]
fac_sce[, fac_sce_attri_dx_39 := aroc_2039_19 * attributable_dx]
fac_sce[, fac_sce_attri_no_dx_39 := aroc_2039_19 * attributable_no_dx]

fac_sce[, fac_sce_total_38 := aroc_2038_19 * total]
fac_sce[, fac_sce_attri_dx_38 := aroc_2038_19 * attributable_dx]
fac_sce[, fac_sce_attri_no_dx_38 := aroc_2038_19 * attributable_no_dx]

fac_sce[, fac_sce_total_37 := aroc_2037_19 * total]
fac_sce[, fac_sce_attri_dx_37 := aroc_2037_19 * attributable_dx]
fac_sce[, fac_sce_attri_no_dx_37 := aroc_2037_19 * attributable_no_dx]

fac_sce[, fac_sce_total_36 := aroc_2036_19 * total]
fac_sce[, fac_sce_attri_dx_36 := aroc_2036_19 * attributable_dx]
fac_sce[, fac_sce_attri_no_dx_36 := aroc_2036_19 * attributable_no_dx]

fac_sce[, fac_sce_total_35 := aroc_2035_19 * total]
fac_sce[, fac_sce_attri_dx_35 := aroc_2035_19 * attributable_dx]
fac_sce[, fac_sce_attri_no_dx_35 := aroc_2035_19 * attributable_no_dx]

fac_sce[, fac_sce_total_34 := aroc_2034_19 * total]
fac_sce[, fac_sce_attri_dx_34 := aroc_2034_19 * attributable_dx]
fac_sce[, fac_sce_attri_no_dx_34 := aroc_2034_19 * attributable_no_dx]

fac_sce[, fac_sce_total_33 := aroc_2033_19 * total]
fac_sce[, fac_sce_attri_dx_33 := aroc_2033_19 * attributable_dx]
fac_sce[, fac_sce_attri_no_dx_33 := aroc_2033_19 * attributable_no_dx]

fac_sce[, fac_sce_total_32 := aroc_2032_19 * total]
fac_sce[, fac_sce_attri_dx_32 := aroc_2032_19 * attributable_dx]
fac_sce[, fac_sce_attri_no_dx_32 := aroc_2032_19 * attributable_no_dx]

fac_sce[, fac_sce_total_31 := aroc_2031_19 * total]
fac_sce[, fac_sce_attri_dx_31 := aroc_2031_19 * attributable_dx]
fac_sce[, fac_sce_attri_no_dx_31 := aroc_2031_19 * attributable_no_dx]



fac_sce[, fac_sce_total_30 := aroc_2030_19 * total]
fac_sce[, fac_sce_attri_dx_30 := aroc_2030_19 * attributable_dx]
fac_sce[, fac_sce_attri_no_dx_30 := aroc_2030_19 * attributable_no_dx]

fac_sce[, fac_sce_total_29 := aroc_2029_19 * total]
fac_sce[, fac_sce_attri_dx_29 := aroc_2029_19 * attributable_dx]
fac_sce[, fac_sce_attri_no_dx_29 := aroc_2029_19 * attributable_no_dx]

fac_sce[, fac_sce_total_28 := aroc_2028_19 * total]
fac_sce[, fac_sce_attri_dx_28 := aroc_2028_19 * attributable_dx]
fac_sce[, fac_sce_attri_no_dx_28 := aroc_2028_19 * attributable_no_dx]

fac_sce[, fac_sce_total_27 := aroc_2027_19 * total]
fac_sce[, fac_sce_attri_dx_27 := aroc_2027_19 * attributable_dx]
fac_sce[, fac_sce_attri_no_dx_27 := aroc_2027_19 * attributable_no_dx]

fac_sce[, fac_sce_total_26 := aroc_2026_19 * total]
fac_sce[, fac_sce_attri_dx_26 := aroc_2026_19 * attributable_dx]
fac_sce[, fac_sce_attri_no_dx_26 := aroc_2026_19 * attributable_no_dx]

fac_sce[, fac_sce_total_25 := aroc_2025_19 * total]
fac_sce[, fac_sce_attri_dx_25 := aroc_2025_19 * attributable_dx]
fac_sce[, fac_sce_attri_no_dx_25 := aroc_2025_19 * attributable_no_dx]

fac_sce[, fac_sce_total_24 := aroc_2024_19 * total]
fac_sce[, fac_sce_attri_dx_24 := aroc_2024_19 * attributable_dx]
fac_sce[, fac_sce_attri_no_dx_24 := aroc_2024_19 * attributable_no_dx]

fac_sce[, fac_sce_total_23 := aroc_2023_19 * total]
fac_sce[, fac_sce_attri_dx_23 := aroc_2023_19 * attributable_dx]
fac_sce[, fac_sce_attri_no_dx_23 := aroc_2023_19 * attributable_no_dx]

fac_sce[, fac_sce_total_22 := aroc_2022_19 * total]
fac_sce[, fac_sce_attri_dx_22 := aroc_2022_19 * attributable_dx]
fac_sce[, fac_sce_attri_no_dx_22 := aroc_2022_19 * attributable_no_dx]

fac_sce[, fac_sce_total_21 := aroc_2021_19 * total]
fac_sce[, fac_sce_attri_dx_21 := aroc_2021_19 * attributable_dx]
fac_sce[, fac_sce_attri_no_dx_21 := aroc_2021_19 * attributable_no_dx]

fac_sce[, fac_sce_total_20 := aroc_2020_19 * total]
fac_sce[, fac_sce_attri_dx_20 := aroc_2020_19 * attributable_dx]
fac_sce[, fac_sce_attri_no_dx_20 := aroc_2020_19 * attributable_no_dx]

head(fac_sce)

##### Saving by location
locations <- unique(fac_sce$location_id)

out_root <- "ADDRESS"
for (loc in locations){
  print(loc)
  
  fc <- fac_sce[location_id == loc, c('location_id','location_name', 'draw', 
                                    'fac_sce_total_50', 'fac_sce_attri_dx_50', 'fac_sce_attri_no_dx_50',
                                    'fac_sce_total_49', 'fac_sce_attri_dx_49', 'fac_sce_attri_no_dx_49',
                                    'fac_sce_total_48', 'fac_sce_attri_dx_48', 'fac_sce_attri_no_dx_48',
                                    'fac_sce_total_47', 'fac_sce_attri_dx_47', 'fac_sce_attri_no_dx_47',
                                    'fac_sce_total_46', 'fac_sce_attri_dx_46', 'fac_sce_attri_no_dx_46', 
                                    'fac_sce_total_45', 'fac_sce_attri_dx_45', 'fac_sce_attri_no_dx_45',
                                    'fac_sce_total_44', 'fac_sce_attri_dx_44', 'fac_sce_attri_no_dx_44',
                                    'fac_sce_total_43', 'fac_sce_attri_dx_43', 'fac_sce_attri_no_dx_43',
                                    'fac_sce_total_42', 'fac_sce_attri_dx_42', 'fac_sce_attri_no_dx_42',
                                    'fac_sce_total_41', 'fac_sce_attri_dx_41', 'fac_sce_attri_no_dx_41', 
                                    'fac_sce_total_40', 'fac_sce_attri_dx_40', 'fac_sce_attri_no_dx_40', 
                                    
                                    'fac_sce_total_39', 'fac_sce_attri_dx_39', 'fac_sce_attri_no_dx_39',
                                    'fac_sce_total_38', 'fac_sce_attri_dx_38', 'fac_sce_attri_no_dx_38',
                                    'fac_sce_total_37', 'fac_sce_attri_dx_37', 'fac_sce_attri_no_dx_37',
                                    'fac_sce_total_36', 'fac_sce_attri_dx_36', 'fac_sce_attri_no_dx_36', 
                                    'fac_sce_total_35', 'fac_sce_attri_dx_35', 'fac_sce_attri_no_dx_35',
                                    'fac_sce_total_34', 'fac_sce_attri_dx_34', 'fac_sce_attri_no_dx_34',
                                    'fac_sce_total_33', 'fac_sce_attri_dx_33', 'fac_sce_attri_no_dx_33',
                                    'fac_sce_total_32', 'fac_sce_attri_dx_32', 'fac_sce_attri_no_dx_32',
                                    'fac_sce_total_31', 'fac_sce_attri_dx_31', 'fac_sce_attri_no_dx_31', 
                                    
                                    'fac_sce_total_30', 'fac_sce_attri_dx_30', 'fac_sce_attri_no_dx_30',
                                    'fac_sce_total_29', 'fac_sce_attri_dx_29', 'fac_sce_attri_no_dx_29',
                                    'fac_sce_total_28', 'fac_sce_attri_dx_28', 'fac_sce_attri_no_dx_28',
                                    'fac_sce_total_27', 'fac_sce_attri_dx_27', 'fac_sce_attri_no_dx_27',
                                    'fac_sce_total_26', 'fac_sce_attri_dx_26', 'fac_sce_attri_no_dx_26', 
                                    'fac_sce_total_25', 'fac_sce_attri_dx_25', 'fac_sce_attri_no_dx_25',
                                    'fac_sce_total_24', 'fac_sce_attri_dx_24', 'fac_sce_attri_no_dx_24',
                                    'fac_sce_total_23', 'fac_sce_attri_dx_23', 'fac_sce_attri_no_dx_23',
                                    'fac_sce_total_22', 'fac_sce_attri_dx_22', 'fac_sce_attri_no_dx_22',
                                    'fac_sce_total_21', 'fac_sce_attri_dx_21', 'fac_sce_attri_no_dx_21', 
                                    'fac_sce_total_20', 'fac_sce_attri_dx_20', 'fac_sce_attri_no_dx_20')]
  
  
  
  fac_l <- as.data.table(melt(fc, 
                              id.vars = c('location_id', 'location_name','draw')))
  
  setnames(fac_l, old = 'value', new = 'fac_cost')
  fac_l[, year := tstrsplit(variable, "_", keep = 4)]
  fac_l[year == 'dx', year := tstrsplit(variable, "_", keep = 5)]
  fac_l[year == 'no', year := tstrsplit(variable, "_", keep = 6)]
  
  fac_l[, year_id := as.numeric(paste0(20, year))]
  
  fac_l[, year := NULL]  
  fac_l[, cost_type := tstrsplit(variable, "_", keep = 3)]
  
  fac_l[cost_type == 'attri', dx_status := tstrsplit(variable, "_", keep = 4)]
  fac_l[, variable := NULL]
  
  fac_l[is.na(dx_status), dx_status := 'dx']
  fac_l[dx_status == 'no', dx_status := 'no_dx']
  
  
  print(paste0(out_root, loc, 'FILEPATH'))
  
  fwrite(fac_l, paste0(out_root, loc, 'FILEPATH'),
         row.names = F)
  # 
}
