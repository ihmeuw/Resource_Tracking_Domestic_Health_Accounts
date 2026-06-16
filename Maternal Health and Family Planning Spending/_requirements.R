if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  # misc
  here,
  tictoc,
  
  # data
  yaml,
  data.table,
  arrow,
  jsonlite,
  readxl,
  openxlsx,
  rhdf5,
  
  # modeling/stats
  mgcv,
  lme4,
  MASS,
  laGP,
  brms,
  rstan,
  
  # plotting
  ggplot2,
  patchwork,
  scales
)
