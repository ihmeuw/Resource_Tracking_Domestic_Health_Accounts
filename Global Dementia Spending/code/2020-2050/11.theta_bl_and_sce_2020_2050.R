######################################
#
# This file calculates AROCs for X.
# With the inputs from this file I will
# calculate x from 2020 to 2030.
#
######################################
rm(list = ls())

# Set directories
if (Sys.info()["sysname"] =="Linux") {
  root <- "ADDRESS"
} else {
  root <- "ADDRESS"
}

library(data.table)

# locations are we intested on
locations <- fread(paste0(root, 'FILEPATH'))
loc <- locations[level == 3, c('location_id')]
locs <- unique(loc$location_id)


#########################################
#
# theta for baseline scenario
#
#########################################

for (l in locs){


  print(l)
  # beta
  beta <- fread(paste0(bl_path, 'FILEPATH', l, 'FILEPATH')) # baseline

  # gamma
  gamma <- fread(paste0(bl_path, 'FILEPATH', l, 'FILEPATH')) # baseline

  # x
  x <- fread(paste0(bl_path, 'FILEPATH', l, 'FILEPATH')) # baseline


  ### merge
  b_l <- merge(beta, gamma)
  b_l1 <- merge(b_l, x)

  # Equation we worked out 
  b_l1[, ratio := ((beta * (1 - x))/((1-beta)*x))]
  b_l1[, theta := gamma * ratio]

  # removing unnecesary columns

  b_s <- b_l1[, c('location_id', 'draw', 'year_id', 'theta')]

  ### save to scratch
  # fwrite(b_s, paste0(bl_path, 'FILEPATH', l, 'FILEPATH'),
  #        row.names = F)
  #
}

#########################################
#
# theta for beta scenario
#
#########################################

for (l in locs){


  print(l)
  # beta
  beta <- fread(paste0(sc_path, 'FILEPATH', l, 'FILEPATH')) # scenario

  # gamma
  gamma <- fread(paste0(bl_path, 'FILEPATH', l, 'FILEPATH')) # baseline

  # x
  x <- fread(paste0(sc_path, 'FILEPATH', l, 'FILEPATH')) # scenario


  ### merge
  t <- merge(beta, gamma)
  t1 <- merge(t, x)

  # Equation we worked out 
  t1[, ratio := ((beta * (1 - x))/((1-beta)*x))]
  t1[, theta_beta_sce := gamma * ratio]


  t_s <- t1[, c('location_id', 'draw', 'year_id', 'theta_beta_sce')]

  ### save to scratch
  fwrite(t_s, paste0(sc_path, 'FILEPATH', l, 'FILEPATH'),
         row.names = F)
}



#########################################
#
# theta for gamma scenario
#
#########################################


for (l in locs){
  
  
  print(l)
  # beta
  beta <- fread(paste0(bl_path, 'FILEPATH', l, 'FILEPATH')) # baseline
  
  # gamma
  gamma <- fread(paste0(sc_path, 'FILEPATH', l, 'FILEPATH')) # scenario
  
  # x
  x <- fread(paste0(bl_path, 'FILEPATH', l, 'FILEPATH')) # baseline
  
  
  ### merge
  t <- merge(beta, gamma)
  t1 <- merge(t, x)
  
  # Equation we worked out 
  t1[, ratio := ((beta * (1 - x))/((1-beta)*x))]
  t1[, theta_gamma_sce := gamma * ratio]
  
  t_s1 <- t1[, c('location_id', 'draw', 'year_id', 'theta_gamma_sce')]
  
  ### save to scratch
  fwrite(t_s1, paste0(sc_path, 'FILEPATH', l, 'FILEPATH'),
         row.names = F)
}

