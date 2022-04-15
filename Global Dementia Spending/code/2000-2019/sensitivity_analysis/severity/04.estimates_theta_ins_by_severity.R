# Clean working directory
rm(list = ls())
library(data.table)

beta_ins <-  fread("FILEPATH")
gamma <- fread("FILEPATH")
beta <- fread("FILEPATH")

locs <- fread("FILEPATH")

loc <- locs[level == 3, c('location_id', 'location_name', 'ihme_loc_id')]

# dropping locations that we don't need
# beta inst
beta_ins.l <- merge(beta_ins, loc, by = 'location_id')
setnames(beta_ins.l, old = 'variable', new = 'draw')

# melt
bi <- melt(beta_ins.l, id.vars = c('location_id', 'year_id', 'draw', 'location_name', 'ihme_loc_id'),
         measure.vars = c('bi_mild', 'bi_mode', 'bi_seve'))

bi[variable == 'bi_mild', severity := 'mild']
bi[variable == 'bi_mode', severity := 'moderate']
bi[variable == 'bi_seve', severity := 'severe']

bi[, variable := NULL]
setnames(bi, old = 'value', new = 'bi')


# gamma
gamma.l <- merge(gamma, loc, by = 'location_id')

gamma.l <- gamma.l[year_id > 1999, c('location_id', 'year_id', 'variable', 
                                     'ins_mild', 'ins_mode', 'ins_seve', 
                                     'location_name', 'ihme_loc_id')]

setnames(gamma.l, old = 'variable', new = 'draw')

# melt
gs <- melt(gamma.l, id.vars = c('location_id', 'year_id', 'draw', 'location_name', 'ihme_loc_id'),
           measure.vars = c('ins_mild', 'ins_mode', 'ins_seve'))

gs[variable == 'ins_mild', severity := 'mild']
gs[variable == 'ins_mode', severity := 'moderate']
gs[variable == 'ins_seve', severity := 'severe']

gs[, variable := NULL]
setnames(gs, old = 'value', new = 'gamma')


# beta 
beta.l <- merge(beta, loc, by = 'location_id')
beta.l <- beta.l[year_id > 1999, c('location_id', 'year_id', 'variable', 
                                   'dx_mild', 'dx_mode', 'dx_seve', 
                                   'location_name', 'ihme_loc_id')]

setnames(beta.l, old = 'variable', new = 'draw')

# melt
dx <- melt(beta.l, id.vars = c('location_id', 'year_id', 'draw', 'location_name', 'ihme_loc_id'),
           measure.vars = c('dx_mild', 'dx_mode', 'dx_seve'))

dx[variable == 'dx_mild', severity := 'mild']
dx[variable == 'dx_mode', severity := 'moderate']
dx[variable == 'dx_seve', severity := 'severe']

dx[, variable := NULL]
setnames(dx, old = 'value', new = 'beta')

# bringing everything together
t <- merge(dx, gs, by = c('location_id', 'year_id', 'draw', 'location_name', 'ihme_loc_id', 'severity'))

data <- merge(t, bi, by = c('location_id', 'year_id', 'draw', 'location_name', 'ihme_loc_id', 'severity'))

# calculating beta
data[, ratio := ((beta * (1 - bi))/((1-beta)*bi))]

data[, theta := gamma * ratio]

fwrite(data, "FILEPATH")
