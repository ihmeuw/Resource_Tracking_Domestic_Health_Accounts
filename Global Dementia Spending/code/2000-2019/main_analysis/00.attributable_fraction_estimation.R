# Clear memory of all objects
rm(list=ls())

#Loading libraries
library("metafor")
library("dplyr")
library(data.table)
library(esc)
library(multcomp)

#Load data
dat <- fread("FILEPATH", 
       encoding = "Latin-1")

# calculating mean and median by care setting
# mean
dat[, mean := mean(At_Frac_total), by = .(care_setting)]
dat[, .N, by = .(care_setting, mean)]

# median
dat[, median := median(At_Frac_total), by = .(care_setting)]
dat[, .N, by = .(care_setting, median)]

###
# Data prep to run the meta-analysis
###


dat$study_id <- dat$study_name
dat <-dat %>% select(study_id, Author:control_group)

## 
# We need to convert our R values into Z-values
##

dat$care_Setting <- as.factor(dat$care_setting)

# We need to calculate the variance of the effect size
dat <- as.data.table(dat)
dat[, sample_size_AD := as.numeric(sample_size_AD)]
dat[, sample_size_control := as.numeric(sample_size_control)]

dat[, var_2 := (1/sample_size_AD) + (1/sample_size_control)]

ma_model_cs <- rma(At_Frac_total,
                   var_2, 
                   mods = ~  care_Setting,
                   method="REML" ,
                   data = dat, 
                   slab = paste(dat$Author, dat$year_published, dat$ihme_loc_id, dat$care_setting, sep="  "), )

summary(ma_model_cs)

# Forest plot
forest.rma(ma_model_cs, order = order(dat$year_published, dat$ihme_loc_id, dat$care_setting), 
           slab = paste(dat$Author, dat$year_published, dat$ihme_loc_id, dat$care_setting, sep="  "), 
           col = "blue")

# Simulating 1000 draws
sims <- as.data.table(simulate.rma(ma_model_cs, nsim = 1000, seed = 10),
                      keep.rownames = T)

################################################################################

#making a column with moderators
sims[, moderator := tstrsplit(rn, " ", fixed = TRUE, keep = 9)] 


sims[moderator == "Mixed.1", moderator := "Mixed"]
sims[moderator == "Mixed.2", moderator := "Mixed"]

sims[, .N, by = moderator]

# removing unnecesary columns
sims[, rn := NULL]

# Calculating the mean by moderator 
means <- as.data.table(aggregate(.~moderator, sims, mean))
View(means)

means[moderator == "Fac", moderator := "facility" ]
means[moderator == "Com", moderator := "community" ]

# creating a fifth moderator for people in facility, but without a dx
test <- melt(means, id.vars = 'moderator')
test_1 <- dcast(test, variable ~ moderator)
test_1[, facility_no_dx := facility/2] 
head(test_1)

com <- test_1[, c('variable', 'community')]
com[, moderator := 'community']
setnames(com, old = 'community', new = 'value')
head(com)

fac <- test_1[, c('variable', 'facility')]
fac[, moderator := 'facility']
setnames(fac, old = 'facility', new = 'value')
head(fac)

fac_no_dx <- test_1[, c('variable', 'facility_no_dx')]
fac_no_dx[, moderator := 'facility_no_dx']
setnames(fac_no_dx, old = 'facility_no_dx', new = 'value')

af <- rbind(com, fac, fac_no_dx)
head(af)

af_t <- dcast(af, moderator ~ variable)
View(head(af_t))

fwrite(af_t, 
       "FILEPATH")


###
#
# Calculating p-value and confidence interval for facility care setting
#
###

# Confidence interval estimation
mod.f <- glht(ma_model_cs, linfct = c("intrcpt  + care_SettingFac = 1"))
confint(mod.f)

mod.m <- glht(ma_model_cs, linfct = c("intrcpt  + care_SettingMixed = 1"))
confint(mod.m)

mod.n <- glht(ma_model_cs, linfct = c("intrcpt  + care_SettingNR = 1"))
confint(mod.n)

# Estimating p-value for facility
summary(glht(ma_model_cs, linfct = c("intrcpt + care_SettingFac  = 1 ")))