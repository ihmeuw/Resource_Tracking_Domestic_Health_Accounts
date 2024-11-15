####################################################################################
## Creator: USERNAME
## 
## Description: Flag clear erroneous data points as outliers
####################################################################################

## read data
ghes_data <- fread("FILEPATH")

## fill and clear
ghes_data[is.na(flag_cd) | flag_cd == 2, 
          flag_cd := 0]

## government
ghes_data[flag_cd == 0 & location_name == "South Africa" & data > 120*10^6,
          flag_cd := 2]

ghes_data[flag_cd == 0 & location_name == "Venezuela" & data > 50*10^6,
          flag_cd := 2]

ghes_data[flag_cd == 0 & location_name == "Zimbabwe" & data > 15*10^6,
          flag_cd := 2]

ghes_data[flag_cd == 0 & location_name == "Papua New Guinea" & data > 6*10^6,
flag_cd := 2]

ghes_data[flag_cd == 0 & y_frac > 0.6,
          flag_cd := 2]

## write data
fwrite(ghes_data, "FILEPATH")

## End of Script ##