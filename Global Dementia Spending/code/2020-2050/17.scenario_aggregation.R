rm(list = ls())

library(data.table)
library(dplyr)

bl_path <- "ADDRESS"

sc_path <- "ADDRESS"


loc <- commandArgs()[8]


print(loc)
print(Sys.time())
##
# Reading prevalence
##

prev <- fread(paste0(bl_path, "FILEPATH", loc, "FILEPATH"))

##
# reading beta
##

beta <- fread(paste0(bl_path, "FILEPATH", loc, "FILEPATH"))

###
# Merging data
###

data <- merge(prev, beta, 
              by = c('location_id', 'location_name', 'year_id', 'draw'), 
              all.x = T)

###########################################################################
# Calculating dx and treated count
###########################################################################

data[, pre_dx_counts := num * beta]
data[, pre_no_dx_counts := num * (1 - beta)] 

  
  ##
  # Reading gamma
  ## 
 
  gamma_sce <- fread(paste0(sc_path, "FILEPATH", loc, "FILEPATH"))
  

  # Merging together
  data <- merge(data, gamma_sce, by = c("location_id", 'location_name','year_id',"draw"), all.x = T)
  
  ##
  # Reading theta
  ##
  
  theta <- fread(paste0(sc_path, "FILEPATH", loc, "FILEPATH"))
  
  ##
  # Bringing everything together
  ##
  
  data <- merge(data, theta, by = c('location_id', 'year_id','draw'), all.x = T)
  
  #########################################################################################
  ##
  # calculating people with dementia & diagnosis living in the community 
  ##
  #########################################################################################
  
  data[, pre_dx_com_counts := pre_dx_counts * (1 - gamma)] # Community
  
  ##
  # Calculating people with dementin who live in nursing homes
  ##
  
  data[, pre_dx_ins_counts := pre_dx_counts * gamma] # Institution with dx
  
  data[, pre_no_dx_ins_counts := pre_no_dx_counts * theta_gamma_sce] # Institution with no dx
  
  ##
  # Creating dfs with community and facility estimates only
  ## 
  
  #### Community
  com_counts <- copy(data[, c('location_id', 'year_id', 'draw',  'age_group_id', 'sex_id', 'pre_dx_com_counts')])
  
  #### Facility 
  fac_counts <- copy(data[, c('location_id', 'year_id', 'draw', 'age_group_id', 'sex_id', 'pre_dx_ins_counts', 'pre_no_dx_ins_counts')])
  
  
  
  ##
  # Reading costs
  ##
  
  comm <- fread(paste0(bl_path, "FILEPATH", loc, "FILEPATH"))
  
  # merge
  
  data_1 <- merge(com_counts, comm, 
                  by = c('location_id', 'year_id', 'draw'), 
                  allow.cartesian = T)
  
  ##############################################################################################
  # calculating community total costs
  ##############################################################################################
  
  data_1[, final_community_costs := pre_dx_com_counts * com_cost]
  
  
  ##
  # making a df with only the columns that matter
  ##
  
  c_costs <- data_1[, c('location_id', 'year_id', 'draw', 'age_group_id', 'sex_id', 'cost_type', 'final_community_costs')]
  c_costs[, dx_status := 'dx']
  
  ##############################################################################################
  # calculating facility total costs
  ##############################################################################################
  
  ##
  # making a df with facility dweller counts 
  ##
 
  
  fac_l <- as.data.table(melt(fac_counts, 
                              id.vars = c('location_id', 'year_id', 'draw', 'age_group_id', 'sex_id')))
  
  fac_l[, dx_status := tstrsplit(variable, "_", keep = 2)]
  fac_l[dx_status == 'no', dx_status := 'no_dx']
  
  
  ##
  # Loading facility costs
  ##
  
  inst_c <- fread(paste0(bl_path, "FILEPATH", loc, "FILEPATH"))
  
  
  #### Merging together
  
  f_costs <- merge(fac_l, inst_c, by = c('location_id', 'year_id', 'dx_status','draw'), 
                   allow.cartesian = T)
  
  # estimating facility costs
  # calculating facility total costs
  f_costs[, final_fac_costs := value * fac_cost]
  
  
  ###################################################################################################
  # Bringing cost together so we can estimate dementia spending
  ###################################################################################################
  
  spend <- merge(f_costs, c_costs,
                by = c('location_id', 'year_id', 'draw', 'age_group_id', 'sex_id', 'dx_status', 'cost_type'), 
                all.x = T)
  
  spend[is.na(final_community_costs), final_community_costs := 0]
  
  
  ##
  # calculating dementi spending!! 
  ##
  
  spend[, final_dem_costs := final_fac_costs  + final_community_costs]
  
  
  # add up so you are only left with 1000 draws, for each country and year
  
  spend[, agg_final_dem_costs := sum(final_dem_costs), 
       by = c('location_id', 'year_id', 'draw', 'cost_type')]
  
  # removing duplicates
  spend_s <- unique(spend[dx_status == 'dx', c('location_id', 'year_id', 'draw', 'cost_type', 'agg_final_dem_costs')])
  
  ## save by country
  fwrite(spend_s, 
         paste0(sc_path, "FILEPATH", loc, "FILEPATH"), 
         row.names = F)
  
  print(Sys.time())
  # 


