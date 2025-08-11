##########################################################################
### Author: USERNAME
### Date: 07/26/24
### Project: Health Spending Effectiveness 
### Purpose: Pull all covariates in draw space (where available)
###          (1) Generate spend covariates (spending per THE by source)
###          (2) Generate draws for health worker density and primary care spending using standard error
##########################################################################

rm(list=ls())

# set seed for reproducible results for the beta draws
set.seed(123)


source("FILEPATH/currency_conversion.R")
library(data.table)
library(truncnorm)

# set threads for faster data.table
setDTthreads(threads = 2)

GBD_path = 'FILEPATH'
FGH_path = 'FILEPATH'
WB_path = 'FILEPATH'

##############################################
### spend covariates
##############################################

# create draws of OOP/THE, GHE/THE, (GHE+PPP)/THE, and DAH/THE
the = fread(paste0(FGH_path, "THE_ppp23_1995_2022.csv"))
the <- the[, .(location_id, year_id, the, the_pc, draw)]

# read in OOP per capita
oop <- fread(paste0(FGH_path, "oop_ppp23_1995_2022.csv"))
oop <- oop[, .(location_id, year_id, oop, oop_pc, draw)]

# read in GHE per capita 
ghe <- fread(paste0(FGH_path,"ghe_ppp23_1995_2022.csv"))
ghe <- ghe[, .(location_id, year_id, ghe, ghe_pc, draw)]

# read in PRIV per capita 
ppp <- fread(paste0(FGH_path, "PPP_ppp23_1995_2022.csv"))
ppp <- ppp[, .(location_id, year_id, ppp_totes, ppp_pc, draw)]

# read in DAH per capita
dah <- fread(paste0(FGH_path, "dah_ppp23_1995_2022.csv"))

# merge oop, ghe, ppp, dah, and the
all_covs <- merge(oop, the, by = c("year_id", "location_id","draw"))
all_covs <- merge(all_covs, ghe, by = c("year_id", "location_id","draw"))
all_covs <- merge(all_covs, ppp, by = c("year_id", "location_id","draw"))
all_covs <- merge(all_covs, dah, by = c("year_id", "location_id","draw"))
all_covs[, oop_per_the := oop_pc/the_pc]
all_covs[, ghe_per_the := ghe_pc/the_pc]
all_covs[, ppp_per_the := ppp_pc/the_pc]
all_covs[, dah_per_the := dah_pc/the_pc]

# change draw format to match rest of variables
all_covs[, draw := as.numeric(gsub("draw_", "", draw))]

##############################################
### health_worker_density
##############################################
health_worker_density = fread(paste0(GBD_path, "/health_worker_density_1995_2023.csv"))

# create draws of health_worker density based on the std. error 
health_worker_density <- health_worker_density[, .(year_id, location_id, HWD, HWD_lower,HWD_upper)]
health_worker_density <- health_worker_density[, hwd_std := sqrt(((HWD_upper - HWD_lower)/3.92)^2)]

# column 3 is the mean, column 6 is the std. error
tempDT <- data.table(t(apply(health_worker_density[,c(3,6)], 1, FUN = function(x) rnorm(500, mean = x[1], sd = x[2]))))
health_worker_density <- cbind(health_worker_density, tempDT)

# remove unnecessary columns
health_worker_density <- health_worker_density[, c("HWD", "HWD_lower", "HWD_upper","hwd_std") := NULL]

# melt health_worker_density to long
health_worker_density_long <- melt(health_worker_density, 
                                   id.vars = c("year_id", "location_id"),
                                   variable.name = "draw")
# remove "V" from the draw column
health_worker_density_long[, draw := as.numeric(gsub("V", "", draw))]
# rename "value" to "HWD"
setnames(health_worker_density_long, "value", "HWD")

all_covs <- merge(all_covs, health_worker_density_long, 
                  by = c("year_id", "location_id","draw"), all.x = TRUE)

##############################################
### primary_care_spending (as a percentage of THE)
##############################################

# read in primary care spending data
data_path = 'FILEPATH'
primary_care = fread(paste0(data_path, "IHME_LMIC_PHC_SPENDING_2000_2017_DATA_Y2021M08D12.CSV"))

#only keep rows where measurement_unit = "PHC expenditures per capita in 2017 USD"
primary_care <- primary_care[measurement_unit == "PHC expenditures per capita in 2017 USD"]

# read in location data
loc_dir = "FILEPATH"
locs = fread(paste0(loc_dir, "location_set35_release16.csv"))
locs <- locs[, .(location_id, ihme_loc_id)]

# merge primary_care with location data
primary_care <- merge(primary_care, locs, by = "location_id")

# convert primary_care to draw space
primary_care <- primary_care[, .(year_id=year, location_id,ihme_loc_id, PCS=val, PCS_lower = lower,PCS_upper = upper)]
primary_care <- primary_care[, PCS_std := ((PCS_upper - PCS_lower)/3.92)]

# argument a for rtruncnorm is the lower bound
# https://cran.r-project.org/web/packages/truncnorm/truncnorm.pdf
tempDT <- data.table(t(apply(primary_care[,c("PCS","PCS_std")], 1, FUN = function(x) rtruncnorm(500,a=0, mean = x[1], sd = x[2]))))
primary_care <- cbind(primary_care, tempDT)

# remove unnecessary columns
primary_care <- primary_care[, c("PCS", "PCS_lower", "PCS_upper","PCS_std") := NULL]

# melt primary_care to long
primary_care_long <- melt(primary_care, 
                                   id.vars = c("year_id", "location_id","ihme_loc_id"),
                                   variable.name = "draw")
# remove "V" from the draw column
primary_care_long[, draw := as.numeric(gsub("V", "", draw))]
# rename "value" to "PCS"
setnames(primary_care_long, "value", "PCS")


temp <- primary_care_long[location_id==168 & year_id==2011,]
temp[, PCS_lower := quantile(PCS, 0.025)]
temp[, PCS_upper := quantile(PCS, 0.975)]  
head(temp)


temp <- primary_care_long[location_id==178 & year_id==2005,]
temp[, PCS_lower := quantile(PCS, 0.025)]
temp[, PCS_upper := quantile(PCS, 0.975)]  
head(temp)


# read in THE data to match the year that the primary care spending 
# data were created - this file is in 2017 USD
the_old <- fread("FILEPATH/the_pc_previous_estimates_1990_2017.csv")

# merge primary_care_long_ppp and the, and then create PCS/THE
primary_care_long <- merge(primary_care_long, the_old, by = c("year_id", "ihme_loc_id","draw"))
primary_care_long[, PCS_per_THE := PCS/the_pc]


summary(primary_care_long$PCS_per_THE)

# PCS_per_THE is greater than 1 for some draws - check the mean of PCS_per_THE
# create a dataframe where we have PCS_per_THE in mean space
PCS_per_THE_mean <- primary_care_long[, .(PCS_per_THE = mean(PCS_per_THE),
                                          PCS = mean(PCS)), by = c("year_id", "location_id")]
summary(PCS_per_THE_mean$PCS_per_THE)

primary_care_long[, the_pc := NULL]

all_covs <- merge(all_covs, primary_care_long, 
                  by = c("year_id", "location_id","location_name","ihme_loc_id","draw"), 
                  all.x = TRUE)

##############################################
### primary_care_spending - ambulatory care only (as a percentage of THE)
##############################################

# read in primary care spending data
data_path = 'FILEPATH'
primary_care = fread(paste0(data_path, "IHME_LMIC_PHC_SPENDING_2000_2017_DATA_Y2021M08D12.CSV"))

primary_care <- primary_care[measurement_unit == "PHC ambulatory expenditures per capita in 2017 USD"]

# merge primary_care with location data
primary_care <- merge(primary_care, locs, by = "location_id")

# convert primary_care to draw space (PHCA = primary health care ambulatory)
primary_care <- primary_care[, .(year_id=year, location_id,ihme_loc_id, PHCA=val, PHCA_lower = lower,PHCA_upper = upper)]
primary_care <- primary_care[, PHCA_std := ((PHCA_upper - PHCA_lower)/3.92)]

tempDT <- data.table(t(apply(primary_care[,c("PHCA","PHCA_std")], 1, FUN = function(x) rtruncnorm(500,a=0, mean = x[1], sd = x[2]))))
primary_care <- cbind(primary_care, tempDT)

# remove unnecessary columns - draws are now columns named V1 through V500, which will be reshaped to long
primary_care <- primary_care[, c("PHCA", "PHCA_lower", "PHCA_upper","PHCA_std") := NULL]

# melt primary_care to long
PHCA_care_long <- melt(primary_care, 
                          id.vars = c("year_id", "location_id","ihme_loc_id"),
                          variable.name = "draw")

# remove "V" from the draw column
PHCA_care_long[, draw := as.numeric(gsub("V", "", draw))]
setnames(PHCA_care_long, "value", "PHCA")

# merge primary_care_long and the, and then create PHCA/THE
PHCA_care_long <- merge(PHCA_care_long, the_old, by = c("year_id", "ihme_loc_id","draw"))
PHCA_care_long[, PHCA_per_THE := PHCA/the_pc]

summary(PHCA_care_long$PHCA_per_THE)

# check the means
PHCA_per_THE_mean <- PHCA_care_long[, .(PHCA_per_THE = mean(PHCA_per_THE),
                                           PHCA = mean(PHCA)), by = c("year_id", "location_id")]
summary(PHCA_per_THE_mean$PHCA_per_THE)

PHCA_care_long[, the_pc := NULL]

all_covs <- merge(all_covs, PHCA_care_long, 
                  by = c("year_id", "location_id","location_name","ihme_loc_id","draw"), 
                  all.x=T)


##############################################
### corruption data from World Bank (Worldwide Governance Indicators)
##############################################
# read in corruption data
data_path = 'FILEPATH'
corr_df = fread(paste0(data_path, "WB_control_of_corruption.csv"))

# split into two DFs - value and std. error
corruption <- corr_df[`Series Name` == "Control of Corruption: Estimate"]
corruption_se <- corr_df[`Series Name` == "Control of Corruption: Standard Error"]

corruption[, `Series Name` := NULL]
corruption[, `Series Code` := NULL]
corruption_se[, `Series Name` := NULL]
corruption_se[, `Series Code` := NULL]

# convert columns named "1996 [YR1996]" to "1996"
year_list <- colnames(corruption)[3:length(colnames(corruption))]
year_list <- gsub(" \\[YR[0-9]{4}\\]", "", year_list)
colnames(corruption)[3:length(colnames(corruption))] <- year_list
colnames(corruption_se)[3:length(colnames(corruption_se))] <- year_list

# melt corruption to long based on year
corruption_long <- melt(corruption, 
                          id.vars = c("Country Name", "Country Code"),
                          variable.name = "year_id",
                          value.name = "corruption")

# melt corruption_se to long based on year
corruption_se_long <- melt(corruption_se, 
                          id.vars = c("Country Name", "Country Code"),
                          variable.name = "year_id",
                          value.name = "corruption_se")

# merge corruption and corruption_se
corruption_long <- merge(corruption_long, corruption_se_long, by = c("Country Name", "Country Code", "year_id"))
setnames(corruption_long, c("Country Name","Country Code"), c("location_name","ihme_loc_id"))

# update year from factor to numeric
corruption_long[, year_id := as.character(year_id)]
corruption_long[, year_id := as.numeric(year_id)]

# convert corruption_long to draw space
# first need to convert corruption and corruption_se to numeric
corruption_long[, corruption := as.numeric(corruption)]
corruption_long[, corruption_se := as.numeric(corruption_se)]
# remove all NA values
corruption_long <- corruption_long[!is.na(corruption) & !is.na(corruption_se)]
tempDT <- data.table(t(apply(corruption_long[,c("corruption","corruption_se")], 1, FUN = function(x) rnorm(500, mean = x[1], sd = x[2]))))
corruption_long <- cbind(corruption_long, tempDT)

# remove unnecessary columns - draws are now columns named V1 through V500, which will be reshaped to long
corruption_long <- corruption_long[, c("corruption","corruption_se") := NULL]

# melt draws to long
corruption_draws <- melt(corruption_long, 
                       id.vars = c("year_id", "location_name","ihme_loc_id"),
                       variable.name = "draw")

# remove "V" from the draw column
corruption_draws[, draw := as.numeric(gsub("V", "", draw))]
setnames(corruption_draws, "value", "WB_corruption")

# check the means for one year 
corr_mean <- corruption_draws[, .(WB_corruption = mean(WB_corruption)),
                                        , by = c("year_id", "location_name")]
corr_mean[year_id==2022,summary(WB_corruption)]

# convert corruption `2022` column to numeric
corruption[, `2022` := as.numeric(`2022`)]
summary(corruption$`2022`)


corruption_draws[, year_id := as.numeric(year_id)]

all_covs <- merge(all_covs, corruption_draws, 
                  by = c("year_id", "location_name","ihme_loc_id","draw"), all.x=T)

# check countries misaligning
temp <- merge(corruption,locs, by.x='Country Code', by.y = 'ihme_loc_id', all.x = T)
temp[is.na(location_id), .(`Country Name`,`Country Code`)]


#######################################################
##### additional governance variables from WB ######
###################################################

# read in data
effectiveness <- fread(paste0(WB_path, "gov_effectiveness.csv"))
stability <- fread(paste0(WB_path, "gov_stability.csv"))
regulatory_quality <- fread(paste0(WB_path, "gov_regulation_quality.csv"))
rule_of_law <- fread(paste0(WB_path, "gov_rule_of_law.csv"))
voice <- fread(paste0(WB_path, "gov_voice.csv"))

# merge each of these with all_covs
all_covs <- merge(all_covs, effectiveness, by = c("year_id", "location_name", "ihme_loc_id", "draw"), all.x = T)
all_covs <- merge(all_covs, stability, by = c("year_id", "location_name", "ihme_loc_id", "draw"), all.x = T)
all_covs <- merge(all_covs, regulatory_quality, by = c("year_id", "location_name", "ihme_loc_id", "draw"), all.x = T)
all_covs <- merge(all_covs, rule_of_law, by = c("year_id", "location_name", "ihme_loc_id", "draw"), all.x = T)
all_covs <- merge(all_covs, voice, by = c("year_id", "location_name", "ihme_loc_id", "draw"), all.x = T)
                       

#####################################################
##### Merge in government corruption data from Transparency.org: 
######################################################
corrupt_dir = "FILEPATH"
cpi = fread(file.path(corrupt_dir, 'CPI_scores_draws_2000_2023.csv'))

# remove duplicate rows
cpi = cpi[!duplicated(cpi[,.(location_id, year_id, draw)])]

all_covs = merge(all_covs, cpi, all.x = T, 
                 by = c("year_id", "location_id", "ihme_loc_id", "location_name", "draw"))

#####################################################
##### Merge in NHA inpatient, outpatient, and pharmaceutical expenditure fractions: 
######################################################
# NHA data
covar_dir = 'FILEPATH'
nha = fread(file.path(covar_dir, "NHA_expenditure_fraction_draws.csv"))

all_covs = merge(all_covs, nha, all.x = T, 
                 by = c("year_id", "location_id", "ihme_loc_id", 
                        "location_name", "draw"))

#####################################################
##### Merge in Hib3 & Measles (MCV) vaccination coverage: 
######################################################
# Vaccine coverage data
Hib3 = fread(file.path(GBD_path, "Hib3_vaccine_coverage_draws_2023.csv"))
MCV = fread(file.path(GBD_path, "MCV_vaccine_coverage_draws_2023.csv"))

# remove covariate ID from dataframes
Hib3[, covariate_id := NULL]
MCV[, covariate_id := NULL]

all_covs = merge(all_covs, Hib3, all.x = T, 
                 by = c("year_id", "location_id", "location_name", "draw"))
all_covs = merge(all_covs, MCV, all.x = T, 
                 by = c("year_id", "location_id", "location_name", "draw"))

#####################################################
##### Merge in skilled birth attendance coverage: 
######################################################
# SBA coverage data
SBA = fread(file.path(GBD_path, "skilled_birth_attendance_draws_2023.csv"))

# remove covariate ID from dataframe
SBA[, covariate_id := NULL]

all_covs = merge(all_covs, SBA, all.x = T, 
                 by = c("year_id", "location_id", "location_name", "draw"))

#####################################################
##### Merge in Antenatal Care (ANC) coverage: 
######################################################
# ANC coverage data
ANC1 = fread(file.path(GBD_path, "ANC1_coverage_draws_2023.csv"))
ANC4 = fread(file.path(GBD_path, "ANC4_coverage_draws_2023.csv"))

# remove covariate ID from dataframes
ANC1[, covariate_id := NULL]
ANC4[, covariate_id := NULL]

all_covs = merge(all_covs, ANC1, all.x = T, 
                 by = c("year_id", "location_id", "location_name", "draw"))
all_covs = merge(all_covs, ANC4, all.x = T, 
                 by = c("year_id", "location_id", "location_name", "draw"))


#####################################################
##### Save out file that has all the covariates in draw space 
######################################################
# check number of rows
nrow(all_covs) == nrow(the)
fwrite(all_covs, "FILEPATH/all_covs_draw_space_RTR_FGH24.csv")
