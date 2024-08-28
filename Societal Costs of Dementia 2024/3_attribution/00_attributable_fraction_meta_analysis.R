# Clear memory of all objects
rm(list=ls())


#Loading libraries
library("metafor")
library("dplyr")
library(data.table)
library(esc)
library(multcomp)

library(reticulate)
reticulate::use_python("FILEPATH")
mr <- import("mrtool")
cw <- import("crosswalk")

logit <- function(x){
  # We use the logit on models that are for percentage models, to keep their bounds
  # between 0 and 1.
  return(log(x/(1-x)))
}

today <- gsub('-','_',Sys.Date())

#Load data
prior_dat <- fread("FILEPATH/Attribution_05_26_20_pp.csv", encoding = "Latin-1")

new_dat <- fread("FILEPATH/attribution_extraction_2023_07_20.csv")

dat <- rbind(prior_dat, new_dat)

dat[care_setting == "mixed", care_setting := "Mixed"]

RTR <- 1

# if RTR flag is 1, cut papers with unadjusted estimates
if (RTR == 1) {
  dat <- dat[!(Author %in% c("Ayyagari 2007", "Caravau et al", "Abdin et al")),]
}

# there are two copies of the Menzin study, but the one with source id 172 is incorrect. 
dat <- dat[!(Author == "Menzin et al" & source_id == 172),]

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
dat <- dat %>% dplyr::select(study_id, Author:control_group)

## 
# We need to convert our R values into Z-values
##

dat$care_Setting <- as.factor(dat$care_setting)

# We need to calculate the variance of the effect size
dat <- as.data.table(dat)
dat[, sample_size_AD := as.numeric(sample_size_AD)]
dat[, sample_size_control := as.numeric(sample_size_control)]

dat[, var_2 := (1/sample_size_AD) + (1/sample_size_control)]
dat[, se := sqrt(var_2)]

dat[, logit_at_frac := logit(At_Frac_total)]

# use delta_transform function to get standard error of logit-transformed data
dat[, c("logit_mean", "logit_se")] <- cw$utils$linear_to_logit(
  mean = array(dat$At_Frac_total), 
  sd = array(dat$se))
  


ma_model_cs <- rma(logit_mean, 
                   sei = logit_se,  
                   mods = ~  care_Setting,
                   method="REML" ,
                   data = dat, 
                   slab = paste(dat$Author, dat$year_published, dat$ihme_loc_id, dat$care_setting, sep="  "), )

summary(ma_model_cs)

# Forest plot
forest.rma(ma_model_cs, atransf = transf.ilogit,order = order(dat$year_published, dat$ihme_loc_id, dat$care_setting), 
           slab = paste(dat$Author, dat$year_published, dat$ihme_loc_id, dat$care_setting, sep="  "), 
           col = "blue")

# Simulating 1000 draws
sims <- as.data.table(simulate.rma(ma_model_cs, nsim = 1000, seed = 10),
                      keep.rownames = T)

################################################################################

#making a column with moderators
# tstrsplit is a string split command. The column "rn" is filled with data like:
# Geldmacher et al  2013  USA  Mixed
sims[, moderator := tstrsplit(rn, "  ", fixed = TRUE, keep = 4)] 

# when sims is created, there are some rows with the same ID because the same paper might have two entries
# for example, Ayyagari 2007 has two rows because it has an estimate from two different years
# this is why there are .1 and .2 suffixes on some of the moderators
sims[moderator == "Mixed.1", moderator := "Mixed"]
sims[moderator == "Mixed.2", moderator := "Mixed"]

sims[, .N, by = moderator]

# removing unnecesary columns
sims[, rn := NULL]

# now, take the inverse logit of every column of sim data
# (except for moderator column; then tack that column back on afterwards)
sims2 <- sims[, lapply(.SD, transf.ilogit), .SDcols = -"moderator"]
sims <- sims[, .(moderator)]
sims2 <- cbind(sims, sims2)

# Calculating the mean by moderator 
means <- as.data.table(aggregate(.~moderator, sims2, mean))
#View(means)

means[moderator == "Fac", moderator := "facility" ]
means[moderator == "Com", moderator := "community" ]



# means is in wide form - one row per care setting, with 1000 draw columns
# test is long - one row per draw and per care setting (moderator)
# test_1 is in between means and test. It has 1 row per draw but 1 column per care setting
test <- melt(means, id.vars = 'moderator')
test_1 <- dcast(test, variable ~ moderator)

# creating a fifth moderator for people in facility, but without a dx
test_1[, facility_no_dx := facility/2] 
head(test_1)

# add 25% and 75% variables so that we can do sensitivity analyses with the attributable fraction 
# for those who are undiagnosed in nursing homes
test_1[, facility_no_dx_25 := facility/4] 
test_1[, facility_no_dx_75 := (3*facility)/4] 


# save out test_1 for use in the final forest plot figure - this one includes the "mixed" 
#      and "not reported" categories which we want to show in the figure
test_all <- copy(test_1)
setnames(test_all,"variable","draw")
af_all <- melt(test_all, id.vars = 'draw')
setnames(af_all, "variable", "moderator")


# look at the means - this is what will show up as the diamonds on the plot
comm_mean <- af_all[moderator=="community", mean(value)]
comm_lower <- af_all[moderator=="community", quantile(value, 0.025)]
comm_upper <- af_all[moderator=="community", quantile(value, 0.975)]

fac_mean <- af_all[moderator=="facility", mean(value)]
fac_lower <- af_all[moderator=="facility", quantile(value, 0.025)]
fac_upper <- af_all[moderator=="facility", quantile(value, 0.975)]

mixed_mean <- af_all[moderator=="Mixed", mean(value)]
mixed_lower <- af_all[moderator=="Mixed", quantile(value, 0.025)]
mixed_upper <- af_all[moderator=="Mixed", quantile(value, 0.975)]

nr_mean <- af_all[moderator=="NR", mean(value)]
nr_lower <- af_all[moderator=="NR", quantile(value, 0.025)]
nr_upper <- af_all[moderator=="NR", quantile(value, 0.975)]

if(RTR==1){
  fwrite(af_all, paste0(j_root,"Project/IRH/Global_dementia_2023/data/attribution/attributable_fraction_all_categories_RTR_",today,"_logit.csv"))
}  else{
  fwrite(af_all, paste0(j_root,"Project/IRH/Global_dementia_2023/data/attribution/attributable_fraction_all_categories_",today,".csv"))
}
# 
test_2 <- test_1[,.(draw = variable, community, facility, facility_no_dx,facility_no_dx_25,facility_no_dx_75)]

af <- melt(test_2, id.vars = 'draw')
setnames(af, "variable", "moderator")



if(RTR==1){
  fwrite(af, paste0(j_root,"Project/IRH/Global_dementia_2023/data/attribution/attributable_fraction_draws_RTR_",today,"_logit.csv"))
}else{
  fwrite(af, paste0(j_root,"Project/IRH/Global_dementia_2023/data/attribution/attributable_fraction_draws_",today,".csv"))
}


