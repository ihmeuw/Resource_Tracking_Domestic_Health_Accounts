# Clean the environment 
rm(list=ls()) 

library(data.table)

# changint code so I can run it both in the cluster and locally. 

if (Sys.info()["sysname"] =="Linux") {
    root <-"ADDRESS"
} else {
    root <- "ADDRESS"
}


pre <- fread(paste0(root, "FILEPATH"))


# Making a list of age group ids that have zero prevalence
pre[draw_0 == 0, .N, by = age_group_id]

# Removing draws that have zeros
pre <- pre[age_group_id > 12 & year_id == 2019, ] # This should remove all the zeros

# Removing unnecessary columns
pre[, measure_id := NULL]
pre[, metric_id  := NULL]

#pre[, age_group_id  := NULL]
pre[, V1 := NULL]

# adding a severity column

pre[sequela_id == 452, severity := 'mild']
pre[sequela_id == 453, severity := 'moderate']
pre[sequela_id == 454, severity := 'severe']

pre[, sequela_id := NULL]

# wide to long (melt)
id.vars <- c('location_id', 'year_id', 'severity', 'age_group_id', "sex_id")

# Melting

pre_l <- melt(pre, 
              id.vars = id.vars)
head(pre_l)

# Renaming value column to prevalence
setnames(pre_l, old = 'value', new = 'prevalence')

# loading population
# check that this has all the columns that you need 
pop <- fread(paste0(root, "FILEPATH")) 
head(pop)

pop_m <- pop[year_id == 2019, 
             c('location_id', 'year_id', 'population', 'age_group_id', 'sex_id')]

# merging with prevalence
pre_lm <- merge(pre_l, pop_m, by = c('location_id', 'year_id', 'age_group_id', 'sex_id'))
head(pre_lm)
pre_lm[, prev_counts := prevalence * population]

pre_lm <- pre_lm[, c('location_id', 'year_id', 'variable',
                     'severity', 'prev_counts', 'age_group_id', 'sex_id')]

setnames(pre_lm, old = 'variable', new = 'draw')
# Loading betas
  
beta <- fread(paste0(root, "FILEPATH"))
head(beta)

beta <- beta[year_id == 2019, ]
beta[, prob_dx := NULL]
setnames(beta, old = 'variable', new = 'draw')

id.vars1 <- c('location_id', 'year_id', 'draw')

beta_l <- melt(beta, 
              id.vars = id.vars1)
setnames(beta_l, old = "value", new = "beta")

# create severity column
beta_l[variable == 'dx_mild' | variable == 'no_dx_mild', severity := 'mild']
beta_l[variable == 'dx_mode' | variable == 'no_dx_mode', severity := 'moderate']
beta_l[variable == 'dx_seve' | variable == 'no_dx_seve', severity := 'severe']

# create dx status column
beta_l[variable == 'dx_mild' | variable == 'dx_mode' | variable == 'dx_seve', dx_status := 'dx']
beta_l[variable == 'no_dx_mild' | variable == 'no_dx_mode'  | variable == 'no_dx_seve', dx_status := 'no_dx']

beta_l <- beta_l[year_id > 1999, ]
beta_l[, variable := NULL]
head(beta_l)

# Creating dataset
data <- merge(pre_lm, beta_l, 
              by = c('location_id', "year_id", "draw", "severity"), 
              allow.cartesian = T)

# Loading probability of living in an institution/comunity given that you have a diagnois
gamma <- fread(paste0(root, "FILEPATH")) 

head(gamma)

gammas <- gamma[year_id == 2019, ]
#############################################################################################
setnames(gammas, old = "variable", new = "draw")

# melt
gs <- melt(gammas, id.vars = c('location_id', 'year_id', 'draw'),
           measure.vars = c('ins_mild', 'ins_mode', 'ins_seve'))

gs[variable == 'ins_mild', severity := 'mild']
gs[variable == 'ins_mode', severity := 'moderate']
gs[variable == 'ins_seve', severity := 'severe']

gs[, variable := NULL]
setnames(gs, old = 'value', new = 'gamma')

# bringing gamma to data
data <- merge(data, gs, by = c('location_id', 'year_id', 'draw', 'severity'))
head(data)

data <- data[dx_status == 'dx', ]
head(data)

#################################################################################################
# theta

theta <- fread(paste0(root, "FILEPATH"))
head(theta)

t <- theta[year_id == 2019, c('location_id', 'year_id', 'draw', 'severity', 'theta')]
head(t)

# adding theta to data
data_t <- merge(data, t, by = c('location_id', 'year_id', 'draw', 'severity'))
head(data_t)


# Diagnosed

data_t[, dx := prev_counts * beta]
data_t[, dx_com := dx * (1-gamma)]
data_t[, dx_ins := dx * (gamma)]

# Not diagnosed
data_t[, no_dx := prev_counts * (1 -beta)]
data_t[, no_dx_ins := no_dx * theta]

data_t[, dx_status := NULL]
# Visual check
head(data_t)

data_ts <- data_t[, c('location_id', 'year_id', 'draw', 'severity', 'age_group_id', 'sex_id', 'dx', 'dx_com', 'dx_ins', 'no_dx', 'no_dx_ins')]
head(data_ts)

# saving results
fwrite(data_ts, paste0(root, "FILEPATH"))
