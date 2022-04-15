# Clean the environment 
rm(list=ls()) 

library(data.table)

root <- "FILEPATH"

# Loading data and cleaning unnecessary variables

data <- fread("FILEPATH")
head(data)

# Make a new df with unit costs and multiply with each scenario

#
## Community 
# 
com_total <- fread(paste0(root, "FILEPATH"))
head(com_total)

setnames(com_total, old = 'attributable', new = 'com_attri')
setnames(com_total, old = 'total', new = 'com_total')

com_total[, moderator := NULL]
com_total[, attri_fraction := NULL]
com_total[, delta := NULL]
head(com_total)

# 
## Facility
# 

fac_total <- fread(paste0(root, "FILEPATH"))
head(fac_total)

setnames(fac_total, old = 'attributable_dx', new = 'fac_attri_dx')
setnames(fac_total, old = 'total', new = 'fac_total')

fac_total[, moderator := NULL]
fac_total[, af_fac := NULL]
fac_total[, af_fac_not_dx := NULL]
fac_total[, delta := NULL]
head(fac_total)

# Merging it all together 

## 
# Unit cost
##

uc <- merge(com_total, fac_total)
head(uc)

# loading locations to get rid of all subnationals

locs <- fread(paste0(root, 'FILEPATH'))
head(locs)

locs1 <- locs[level == 3, c('location_id', 'location_name')]

ucl <- merge(locs1, uc, by = 'location_id')
head(ucl)


##
# Bringing everything together
data <- merge(data, ucl,
              by = c('location_id', 'year_id', 'variable'))
head(data)

# Calculating total costs

### Community 

# Attribution cost of caring for someone with AD that is dx and lives in the community
data[, final_dx_com_attri_uc := dx_comm * com_attri]

# Total cost of caring for someone with AD that is dx and lives in the community
data[, final_dx_com_total_uc := dx_comm * com_total]

### Institutional

# Attribution cost of caring for someone with AD that is dx and lives in the facility
data[, final_dx_fac_attri_uc := dx_inst * fac_attri_dx]

# Total cost of caring for someone with AD that is dx and lives in the facility
data[, final_dx_fac_total_uc := dx_inst * fac_total]

# No dx
# Attribution cost of caring for someone with AD that is no_dx and lives in the facility
data[, final_no_dx_fac_attri_uc := no_dx_inst * fac_attri_dx]

# Total cost of caring for someone with AD that is no_dx and lives in the facility
data[, final_no_dx_fac_total_uc := no_dx_inst * fac_total]

head(data)


# Latest addition 

data[, attribution_final_all := final_dx_com_attri_uc + final_dx_fac_attri_uc + final_no_dx_fac_attri_uc]
data[, total_final_all := final_dx_com_total_uc + final_dx_fac_total_uc + final_no_dx_fac_total_uc]

# only for those with a diagnosise

data[, attribution_final_dx_only := final_dx_com_attri_uc + final_dx_fac_attri_uc]
data[, total_final_dx_only := final_dx_com_total_uc + final_dx_fac_total_uc]
head(data)

# saving draws here for number plugging
fwrite(data, 
       "FILEPATH", 
       row.names = F)
##
