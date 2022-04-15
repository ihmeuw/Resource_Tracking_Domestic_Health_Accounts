######
# This script calculates 2030 gamma reference scenario
# 1 + aroc^(2030-2019) * gamma(2019)
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

# gamma AROCS
g_aroc <- fread(paste0(root, 'FILEPATH'))
g_aroc_s <- g_aroc[, c('location_id', 'location_name', 'draw','aroc')]


# sorting data to select the 85th percentile


draws <- paste0('draw_', (0:999))

for (draw_t in draws){
    # looing over draws
    a <- g_aroc_s[ draw == draw_t, ]
    b <- setorder(a, 'aroc')
    loc_id <- b[166, unique(location_id)]
    print(loc_id)
    c <- b[location_id == loc_id, aroc]
    print(paste0('85th AROC: ', c))
    g_aroc_s[draw == draw_t, aroc_sce := c]
    
}

# scenario AROCS
aroc_sce <- copy(g_aroc_s)
head(aroc_sce)

# if aroc_sce is less than aroc then  use original aroc
aroc_sce[aroc < aroc_sce, aroc := aroc_sce]


# gammas from ST-GPR
gamma <- fread(paste0(root, "FILEPATH"))
gamma_19 <- gamma[year_id == 2019, ]

setnames(gamma_19, old = "variable", new = "draw")
head(gamma_19)


# merges with arocs and keeps only 195 locs
gamma_sce <- merge(gamma_19, aroc_sce, c('location_id', 'draw'))

# Raises aroc^13
years <- (2020:2050)

# calculating arocs from 2020 to 2050
for (year in years){
  gamma_sce[, paste0('aroc_', year, '_19') := (1 + aroc)^(year-2019) ]
}


# Multiply aroc^13 by gamma_19 
gamma_sce[, gamma_sce_50 := aroc_2050_19 * value]
gamma_sce[, gamma_sce_49 := aroc_2049_19 * value]
gamma_sce[, gamma_sce_48 := aroc_2048_19 * value]
gamma_sce[, gamma_sce_47 := aroc_2047_19 * value]
gamma_sce[, gamma_sce_46 := aroc_2046_19 * value]
gamma_sce[, gamma_sce_45 := aroc_2045_19 * value]
gamma_sce[, gamma_sce_44 := aroc_2044_19 * value]
gamma_sce[, gamma_sce_43 := aroc_2043_19 * value]
gamma_sce[, gamma_sce_42 := aroc_2042_19 * value]
gamma_sce[, gamma_sce_41 := aroc_2041_19 * value]
gamma_sce[, gamma_sce_40 := aroc_2040_19 * value]

gamma_sce[, gamma_sce_39 := aroc_2039_19 * value]
gamma_sce[, gamma_sce_38 := aroc_2038_19 * value]
gamma_sce[, gamma_sce_37 := aroc_2037_19 * value]
gamma_sce[, gamma_sce_36 := aroc_2036_19 * value]
gamma_sce[, gamma_sce_35 := aroc_2035_19 * value]
gamma_sce[, gamma_sce_34 := aroc_2034_19 * value]
gamma_sce[, gamma_sce_33 := aroc_2033_19 * value]
gamma_sce[, gamma_sce_32 := aroc_2032_19 * value]
gamma_sce[, gamma_sce_31 := aroc_2031_19 * value]

# Multiply aroc^13 by gamma_19 
gamma_sce[, gamma_sce_30 := aroc_2030_19 * value]
gamma_sce[, gamma_sce_29 := aroc_2029_19 * value]
gamma_sce[, gamma_sce_28 := aroc_2028_19 * value]
gamma_sce[, gamma_sce_27 := aroc_2027_19 * value]
gamma_sce[, gamma_sce_26 := aroc_2026_19 * value]
gamma_sce[, gamma_sce_25 := aroc_2025_19 * value]
gamma_sce[, gamma_sce_24 := aroc_2024_19 * value]
gamma_sce[, gamma_sce_23 := aroc_2023_19 * value]
gamma_sce[, gamma_sce_22 := aroc_2022_19 * value]
gamma_sce[, gamma_sce_21 := aroc_2021_19 * value]
gamma_sce[, gamma_sce_20 := aroc_2020_19 * value]

head(gamma_sce)


## saving draws


out_root <- "ADDRESS" 

locs <- unique(gamma_sce$location_id)

for (loc in locs){
  print(loc)
  
  gamma_1 <- gamma_sce[location_id == loc, c('location_id', 'location_name', 'draw', 
                                           "gamma_sce_50" ,  "gamma_sce_49",   "gamma_sce_48",   "gamma_sce_47",   "gamma_sce_46",   "gamma_sce_45", 
                                           "gamma_sce_44",   "gamma_sce_43",   "gamma_sce_42",   "gamma_sce_41",   "gamma_sce_40",   "gamma_sce_39", 
                                           "gamma_sce_38",   "gamma_sce_37",   "gamma_sce_36",   "gamma_sce_35",   "gamma_sce_34",   "gamma_sce_33",  
                                           "gamma_sce_32",   "gamma_sce_31",  'gamma_sce_30', 'gamma_sce_29', 'gamma_sce_28', 'gamma_sce_27', 
                                           'gamma_sce_26', 'gamma_sce_25', 'gamma_sce_24', 'gamma_sce_23', 'gamma_sce_22', 'gamma_sce_21', 'gamma_sce_20')]
  
  gamma_s <- as.data.table(melt(gamma_1, 
                              id.vars = c('location_id', 'location_name', 'draw')))
  
  setnames(gamma_s, old = 'value', new = 'gamma')
  gamma_s[, year := tstrsplit(variable, "_", keep = 3)]
  gamma_s[, year_id := as.numeric(paste0(20, year))]
  gamma_s[, year := NULL]  
  gamma_s[, variable := NULL]
  
  
  # Capping gamma at .8 to be consitent with gamma baseline
  gamma_s[gamma > .8, gamma := .8]
  # 
  # 
  fwrite(gamma_s,
         paste0(out_root, loc, 'FILEPATH'),
         row.names = F)
}

