######
# This script calculates 2030 beta reference scenario
# 1 + aroc^(2030-2019) * beta(2019)
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

# beta AROCS
b_aroc <- fread(paste0(root, 'FILEPATH'))
b_aroc_s <- b_aroc[, c('location_id', 'location_name', 'variable','aroc')]
setnames(b_aroc_s, old = 'variable', new = 'draw')

# sorting data to select the 85th percentile


draws <- paste0('draw_', (0:999))

for (draw_t in draws){
    # looing over draws
    a <- b_aroc_s[ draw == draw_t, ]
    b <- setorder(a, 'aroc')
    loc_id <- b[166, unique(location_id)]
    print(loc_id)
    c <- b[location_id == loc_id, aroc]
    print(paste0('85th AROC: ', c))
    b_aroc_s[draw == draw_t, aroc_sce := c]
    
}


# scenario AROCS
aroc_sce <- copy(b_aroc_s)
aroc_sce[aroc_sce > aroc, aroc := aroc_sce] # original aroc is greater than the 85th 
# percentile use original aroc, if it's less use the alternative aroc

# betas from ST-GPR
beta <- fread(paste0(root, 'FILEPATH'))
beta_19 <- beta[year_id == 2019, ]
setnames(beta_19, old = 'variable', new = 'draw')
head(beta_19)

# merges with arocs and keeps only 195 locs
beta_sce <- merge(beta_19, aroc_sce)

# Raises aroc^13
years <- (2020:2050)

# calculating arocs from 2020 to 2050
for (year in years){
  beta_sce[, paste0('aroc_', year, '_19') := (1 + aroc)^(year-2019) ]
}


# Multiply aroc^13 by beta_19 
beta_sce[, beta_sce_50 := aroc_2050_19 * value]
beta_sce[, beta_sce_49 := aroc_2049_19 * value]
beta_sce[, beta_sce_48 := aroc_2048_19 * value]
beta_sce[, beta_sce_47 := aroc_2047_19 * value]
beta_sce[, beta_sce_46 := aroc_2046_19 * value]
beta_sce[, beta_sce_45 := aroc_2045_19 * value]
beta_sce[, beta_sce_44 := aroc_2044_19 * value]
beta_sce[, beta_sce_43 := aroc_2043_19 * value]
beta_sce[, beta_sce_42 := aroc_2042_19 * value]
beta_sce[, beta_sce_41 := aroc_2041_19 * value]
beta_sce[, beta_sce_40 := aroc_2040_19 * value]

beta_sce[, beta_sce_39 := aroc_2039_19 * value]
beta_sce[, beta_sce_38 := aroc_2038_19 * value]
beta_sce[, beta_sce_37 := aroc_2037_19 * value]
beta_sce[, beta_sce_36 := aroc_2036_19 * value]
beta_sce[, beta_sce_35 := aroc_2035_19 * value]
beta_sce[, beta_sce_34 := aroc_2034_19 * value]
beta_sce[, beta_sce_33 := aroc_2033_19 * value]
beta_sce[, beta_sce_32 := aroc_2032_19 * value]
beta_sce[, beta_sce_31 := aroc_2031_19 * value]

# Multiply aroc^13 by beta_19 
beta_sce[, beta_sce_30 := aroc_2030_19 * value]
beta_sce[, beta_sce_29 := aroc_2029_19 * value]
beta_sce[, beta_sce_28 := aroc_2028_19 * value]
beta_sce[, beta_sce_27 := aroc_2027_19 * value]
beta_sce[, beta_sce_26 := aroc_2026_19 * value]
beta_sce[, beta_sce_25 := aroc_2025_19 * value]
beta_sce[, beta_sce_24 := aroc_2024_19 * value]
beta_sce[, beta_sce_23 := aroc_2023_19 * value]
beta_sce[, beta_sce_22 := aroc_2022_19 * value]
beta_sce[, beta_sce_21 := aroc_2021_19 * value]
beta_sce[, beta_sce_20 := aroc_2020_19 * value]

head(beta_sce)

# saving locations

locs <- unique(beta_sce$location_id)


out_root <- "ADDRESS" 

for (loc in locs){
  print(loc)
  
  beta_1 <- beta_sce[location_id == loc, c('location_id', 'location_name', 'draw', 
                                         "beta_sce_50" ,  "beta_sce_49",   "beta_sce_48",   "beta_sce_47",   "beta_sce_46",   "beta_sce_45", 
                                         "beta_sce_44",   "beta_sce_43",   "beta_sce_42",   "beta_sce_41",   "beta_sce_40",   "beta_sce_39", 
                                         "beta_sce_38",   "beta_sce_37",   "beta_sce_36",   "beta_sce_35",   "beta_sce_34",   "beta_sce_33",  
                                         "beta_sce_32",   "beta_sce_31",  'beta_sce_30', 'beta_sce_29', 'beta_sce_28', 'beta_sce_27', 
                                         'beta_sce_26', 'beta_sce_25', 'beta_sce_24', 'beta_sce_23', 'beta_sce_22', 'beta_sce_21', 'beta_sce_20')]
  
  beta_1l <- as.data.table(melt(beta_1, 
                                id.vars = c('location_id', 'draw', 'location_name')))
  
  setnames(beta_1l, old = 'value', new = 'beta')
  beta_1l[, year := tstrsplit(variable, "_", keep = 3)]
  beta_1l[, year_id := as.numeric(paste0(20, year))]
  beta_1l[, year := NULL]  
  beta_1l[, variable := NULL]
  
  ## setting limit of .90 for when beta is greater than .90
  beta_1l[beta > .9, beta := .9]
  
  # 
  fwrite(beta_1l,
         paste0(out_root, loc, 'FILEPATH'),
         row.names = F)
  # 
}



