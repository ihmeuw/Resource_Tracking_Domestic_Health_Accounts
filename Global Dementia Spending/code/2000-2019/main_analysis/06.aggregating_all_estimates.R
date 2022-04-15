# Clean the environment 
rm(list=ls()) 

library(data.table)

# Loading data and cleaning unnecessary variables
root <- "FILEPATH"
pre <- fread(paste0(root, "FILEPATH"))

pre[, model_version_id    := NULL]
pre[, modelable_entity_id    := NULL]

# wide to long (melt)
id.vars <- c('location_id', 'year_id', 'age_group_id', 'sex_id')

# Melting

pre_l <- melt(pre, 
              id.vars = id.vars)

# Renaming value column to prevalence
setnames(pre_l, old = 'value', new = 'prevalence')
pre_l_sen <- pre_l[prevalence != 0 & (age_group_id != 33 & age_group_id != 27),]


# loading population
pop <- fread(paste0(root, "FILEPATH"))
pop[, run_id    := NULL]

# merging with prevalence
pre_lm <- merge(pre_l_sen, pop)
pre_lm[, prev_counts := prevalence * population]

# Loading betas
  
beta <- fread(paste0(root, "FILEPATH"))
setnames(beta, old = "value", new = "beta")
head(beta)

# Creating dataset
data <- merge(pre_lm, beta,
              by = c('location_id', "year_id", "variable"))

# Loading probability of living in an institution/comunity given that you have a diagnois
gamma <- fread(paste0(root, "FILEPATH")) # These are from ST-GPR
setnames(gamma, old = "value", new = 'gamma')


# Mergin with the rest of the data
data <- merge(data, gamma, by = c('location_id', "year_id", "variable"))
head(data)

# loading theta. no diagnosed with dementia over dementia residents in nursing homes
theta <- fread(paste0(root, "FILEPATH"))
head(theta)

theta_1 <- theta[, c('year_id', 'location_id', 'variable', 'theta')]
head(theta_1)

# Adding it to the rest of the data
data <- merge(data, theta_1, by = c('year_id', 'location_id', 'variable'))
head(data)

# Diagnosed
data[, dx := prev_counts * beta]
# not diagnosed
data[, no_dx := prev_counts * (1-beta)]

# living conditions of those with dx
data[, dx_comm := dx * (1-gamma)]
data[, dx_inst := dx * gamma]

# No diagnosed
data[, no_dx_inst := no_dx * theta]

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
setnames(fac_total, old = 'attributable_no_dx', new = 'fac_attri_no_dx')
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

locs <- fread(paste0(root, "FILEPATH"))
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
data[, final_no_dx_fac_attri_uc := no_dx_inst * fac_attri_no_dx]

# Total cost of caring for someone with AD that is no_dx and lives in the facility
data[, final_no_dx_fac_total_uc := no_dx_inst * fac_total]

head(data)


# Latest addition 

data[, attribution_final_all := final_dx_com_attri_uc + final_dx_fac_attri_uc + final_no_dx_fac_attri_uc]
data[, total_final_all := final_dx_com_total_uc + final_dx_fac_total_uc + final_no_dx_fac_total_uc]

# saving draws here 
fwrite(data, 
       "FILEPATH", 
       row.names = F)


