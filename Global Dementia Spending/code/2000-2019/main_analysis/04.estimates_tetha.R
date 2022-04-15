rm(list = ls())
library(data.table)
library(ggplot2)

root <- "FILEPATH"

gamma <- fread(paste0(root, "FILEPATH"))
setnames(gamma, old = 'value', new = 'gamma')
gammas <- gamma[, c('location_id', 'year_id', 'variable','gamma')]
head(gammas)


# loading beta
beta <- fread(paste0(root,"FILEPATH"))
setnames(beta, old = 'value', new = 'beta')
betas <- beta[, c('location_id', 'year_id', 'variable','beta')]
head(betas)

# loading prevalence

################################################################################
#
# Bringing everything together
#
################################################################################

data <- merge(betas, gammas)

################################################################################
#
# Estimates theta for data points where we have x 
#
################################################################################
x <- fread(paste0(root,"FILEPATH"))
head(x)

setnames(x, old = 'value', new = 'beta_institution')
beta_inst <- x[, c('location_id', 'year_id', 'variable','beta_institution')]
head(beta_inst)


data1 <- merge(data, beta_inst, by = c('year_id', 'location_id', 'variable'))
head(data1)

data1[, ratio := ((beta * (1 - beta_institution))/((1-beta)*beta_institution))]

data1[, theta := gamma * ratio]


# calculating delta
# beta_inst should always be higher than beta_community
data1[, delta_beta := beta_institution - beta]
summary(data1$delta_beta)
data1[delta_beta <0, .N, by = c('year_id', 'location_id')]

# gamma should always be higher than theta
data1[, delta_gamma_theta := gamma - theta]
summary(data1$delta_gamma_theta)
data1[delta_gamma_theta <0, .N, by = c('year_id', 'location_id')]

fwrite(data1,
       paste0(root,"FILEPATH"), 
       row.names = F)
