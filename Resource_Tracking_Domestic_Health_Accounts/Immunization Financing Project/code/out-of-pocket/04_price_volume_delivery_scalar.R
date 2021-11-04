########################################################################################
## Create OOP estimates
## Formula (vaccine price + delivery cost) * (volume of doses) * (OOP scalar)
## Author: Emilie Maddison
### Date: 20 May 2020
########################################################################################
## -------------------------------------------------------------------------------------
## 1. Set up environment
## -------------------------------------------------------------------------------------

## Clear environment
rm(list = ls())

## Set filepaths to directories
if (Sys.info()[1] == "Linux") {  
        j <- FILEPATH
        h <- FILEPATH
        k <- FILEPATH
} else if (Sys.info()[1] == "Windows") {  
        j <- FILEPATH
        h <- FILEPATH 
        k <- FILEPATH
}  

## Set up directories
code.dir <- FILEPATH
out.dir <- FILEPATH

## Load libraries
library(data.table)
library(ggplot2)
library(readxl)
library(classInt, lib.loc = FILEPATH)

source(paste0(FILEPATH, "helper_functions.R"))
source(paste0(FILEPATH, "get_population.R"))
source(paste0(FILEPATH, 'get_covariate_estimates.R'))
source(paste0(FILEPATH, 'get_outputs.R'))
## source mapping function
source(paste0(FILEPATH, "GBD_WITH_INSETS_MAPPING_FUNCTION.R"))

## Today's date
date1 <- format(Sys.time(), "%Y%m%d")

## Get list of 135 locations of interest (LMICs)
locs <- get_ig(locs)
locs_lm <- locs[income_group != 'H',]

## -------------------------------------------------------------------------------------
## 2. Read in datasets
## -------------------------------------------------------------------------------------

## Vaccine Volume (modeled in ST-GPR from DOVE dataset)
volume1 <- fread(paste0(out.dir, "volume/135_total_volume_vaccines_", '20200608', ".csv"))
volume <- volume1

## -------------------------------------------------------------------------------------

## Vaccine Price (linear model from the MI4A dataset)
price1 <- fread(paste0(out.dir, "price/filled_price_9_vaccines.csv"))
price <- copy(price1)

## -------------------------------------------------------------------------------------

## Delivery Cost (linear model from IDCC dataset)
delivery1 <- data.table(read_excel(paste0(out.dir, "delivery/modeled_delivery_costs_20200520.xlsx")))
head(delivery1)
delivery <- delivery1[, .(super_region_name, Vaccine, delivery_cost)]

## -------------------------------------------------------------------------------------

## OOP Scalar (modeled in ST-GPR from literature dataset)
scalar1 <- fread(paste0(out.dir, "oop_scalar/oop_scalar_stgpr_", '20200526', ".csv"))
scalar <- merge(scalar1, locs[, .(location_id, ihme_loc_id)],
                by = 'location_id', all.x = T)
scalar <- scalar[, .(ihme_loc_id, location_id, year_id, 
                     gpr_mean, gpr_lower, gpr_upper, data, variance)]
scalar <- scalar[ihme_loc_id == 'ARG', gpr_mean := 0.218]
scalar <- scalar[ihme_loc_id == 'ARG', gpr_lower := 0.208]
scalar <- scalar[ihme_loc_id == 'ARG', gpr_upper := 0.228]

## -------------------------------------------------------------------------------------
## 3. Adjustments
## -------------------------------------------------------------------------------------

## Clean the data up a bit
price[Vaccine == 'Rota', Vaccine := "RVV"]
volume[Vaccine %in% c('Measles 1st', 'Measles 2nd', 'Measles', 'Rubella'), Vaccine := 'MR']
unique(price$Vaccine)
unique(volume$Vaccine)

## Vaccine price - adjustments for Middle East (From LNCT Report)
naame_dt <- data.table(read_excel(paste0(out.dir, 
      "oop_scalar/middle_east_gov_private_immunization_adjustment_20200520.xlsx")))
naame_dt <- naame_dt[, .(ihme_loc_id, private_vac_allowed, gov_provides_private, 
                         private_NIP_only, urbanicity_type)]

price <- merge(price, naame_dt, by = 'ihme_loc_id', all.x = T)
mean(price$price)
price[private_vac_allowed == 0, price := 0]
price[gov_provides_private == 1, price := 0]
mean(price$price)
price <- price[, .(ihme_loc_id, year_id, super_region_name, Vaccine, price)]

## Vaccine volume - adjustments for China (from literature)
chn_dt <- data.table(read_excel(paste0(out.dir, 
                                 "oop_scalar/china_gov_private_imm_adj_20200521.xlsx")))

volume <- merge(volume, chn_dt, by = c('Vaccine', 'ihme_loc_id'), all.x = T)
mean(volume$volume)
volume[private_vac_allowed == 0, `:=`(volume = 0, lower = 0, upper = 0)]
mean(volume$volume)
volume <- volume[, .(ihme_loc_id, location_id, year_id, Vaccine, volume, lower, upper)]

## -------------------------------------------------------------------------------------
## 3. Calculate OOP Spending
## -------------------------------------------------------------------------------------

## Merge Price and volume
price_volume <- merge(price, volume, by = c('ihme_loc_id', 'year_id', 'Vaccine'),
                      all = T)
## Fix super-region name for Argentina (classified by GBD as "High-income")
price_volume <- price_volume[ihme_loc_id == 'ARG', 
                             super_region_name := 'Latin America and Caribbean']
price_volume_delivery <- merge(price_volume, delivery, 
                               by = c('super_region_name', 'Vaccine'))

## Calculate total spending on vaccines
price_volume_delivery[, `:=`(spending = ((price + delivery_cost) * volume),
                             spending_lower = ((price + delivery_cost) * lower),
                             spending_upper = ((price + delivery_cost) * upper))]
price_volume_delivery[, `:=`(del_spending = ((delivery_cost) * volume),
                             del_spending_lower = ((delivery_cost) * lower),
                             del_spending_upper = ((delivery_cost) * upper))]
price_volume_delivery[, `:=`(vac_spending = ((price) * volume),
                             vac_spending_lower = ((price) * lower),
                             vac_spending_upper = ((price) * upper))]

## Merge on OOP scalar
price_volume_scalar <- merge(price_volume_delivery, scalar, 
                             by = c('ihme_loc_id', 'location_id', 'year_id'), all.x = T)

## Scale down total price to Out-of-pocket spending, 
## for total, delivery, and vaccine spending
price_volume_scalar[, `:=`(oop_spending = spending * gpr_mean,
                           oop_spending_lower = spending_lower * gpr_mean,
                           oop_spending_upper = spending_upper * gpr_mean)]
price_volume_scalar[, `:=`(oop_del_spending = del_spending * gpr_mean,
                           oop_del_spending_lower = del_spending_lower * gpr_mean,
                           oop_del_spending_upper = del_spending_upper * gpr_mean)]
price_volume_scalar[, `:=`(oop_vac_spending = vac_spending * gpr_mean,
                           oop_vac_spending_lower = vac_spending_lower * gpr_mean,
                           oop_vac_spending_upper = vac_spending_upper * gpr_mean)]
price_volume_scalar2 <- price_volume_scalar[!is.na(oop_spending), 
                                            .(oop_spending = sum(oop_spending),
                                              oop_spending_lower = sum(oop_spending_lower),
                                              oop_spending_upper = sum(oop_spending_upper),
                                              
                                              oop_del_spending = sum(oop_del_spending),
                                              oop_del_spending_lower = sum(oop_del_spending_lower),
                                              oop_del_spending_upper = sum(oop_del_spending_upper),
                                              
                                              oop_vac_spending = sum(oop_vac_spending),
                                              oop_vac_spending_lower = sum(oop_vac_spending_lower),
                                              oop_vac_spending_upper = sum(oop_vac_spending_upper)),
                                            .(ihme_loc_id, location_id, year_id)]

## Read in list of Gavi countries
gavi <- fread("/home/j/Project/IRH/ImFinance/Data sources/gavi_recipient_by_country_year.csv")
gavi <- unique(gavi[, .(ihme_loc_id, gavi_country)])

## Merge list of Gavi countries onto results dataframe
price_volume_scalar2 <- merge(price_volume_scalar2, gavi, by = 'ihme_loc_id', all.x = T)
price_volume_scalar2[is.na(gavi_country), gavi_country := 0]

## -------------------------------------------------------------------------------------
## Calculate population of surviving infants

## Pull infant mortality rate per infant
infant_mortality <- get_outputs("cause",
                                gbd_round_id = 6,
                                decomp_step = 'step4',
                                version = 'latest',
                                location_id = locs_lm$location_id,
                                age_group_id = 28,
                                measure_id = 1,
                                year_id = 2000:2017,
                                metric_id = 3,
                                sex_id = 3)

## Pull surviving infant population (surviving infant = births * (1 - infant mortality rate))
live_births_thousands <- get_covariate_estimates(gbd_round_id = 6,
                                                 covariate_id = 60,
                                                 age_group_id = 22,
                                                 sex_id = 3,
                                                 year_id = 200:2017,
                                                 location_id = locs_lm$location_id,
                                                 decomp_step = "iterative")

live_births <- live_births_thousands[, live_births := mean_value * 1000]
custom_covariates <- merge(live_births, 
                           infant_mortality[, .(location_id, year_id, cv_infant_mortality = val)], 
                           by = c("location_id", "year_id"))
custom_covariates[, cv_surviving_infant_pop := live_births * (1 - cv_infant_mortality)]                         
custom_covariates <- custom_covariates[, .(location_id, year_id, cv_surviving_infant_pop)]

## -------------------------------------------------------------------------------------
## Calculate results per surviving infant

price_volume_scalar2 <- merge(price_volume_scalar2, 
                              custom_covariates, 
                              by = c('location_id', 'year_id'), all.x = T)
price_volume_scalar2[, `:=`(oop_spending_per_infant = oop_spending / cv_surviving_infant_pop,
                            oop_spending_per_infant_lower = oop_spending_lower / cv_surviving_infant_pop,
                            oop_spending_per_infant_upper = oop_spending_upper / cv_surviving_infant_pop,
                            
                            oop_del_spending_per_infant = oop_del_spending / cv_surviving_infant_pop,
                            oop_del_spending_per_infant_lower = oop_del_spending_lower / cv_surviving_infant_pop,
                            oop_del_spending_per_infant_upper = oop_del_spending_upper / cv_surviving_infant_pop,
                            
                            oop_vac_spending_per_infant = oop_vac_spending / cv_surviving_infant_pop,
                            oop_vac_spending_per_infant_lower = oop_vac_spending_lower / cv_surviving_infant_pop,
                            oop_vac_spending_per_infant_upper = oop_vac_spending_upper / cv_surviving_infant_pop)]

## Output results
fwrite(price_volume_scalar2, 
       paste0("FILEPATH/data_OOP_by_country_", 
              date1, ".csv"))

## -------------------------------------------------------------------------------------
## Calculate Global and Gavi results
price_volume_scalar_gbl <- price_volume_scalar2[, .(oop_spending = sum(oop_spending),
                                                    oop_spending_lower = sum(oop_spending_lower),
                                                    oop_spending_upper = sum(oop_spending_upper),
                                                    
                                                   oop_del_spending = sum(oop_del_spending),
                                                   oop_del_spending_lower = sum(oop_del_spending_lower),
                                                   oop_del_spending_upper = sum(oop_del_spending_upper),
                                                   
                                                   oop_vac_spending = sum(oop_vac_spending),
                                                   oop_vac_spending_lower = sum(oop_vac_spending_lower),
                                                   oop_vac_spending_upper = sum(oop_vac_spending_upper),
                                                   
                                                   cv_surviving_infant_pop = sum(cv_surviving_infant_pop)),
                     .(year_id)]

price_volume_scalar_gbl[, `:=`(oop_spending_per_infant = oop_spending / cv_surviving_infant_pop,
                               oop_spending_per_infant_lower = oop_spending_lower / cv_surviving_infant_pop,
                               oop_spending_per_infant_upper = oop_spending_upper / cv_surviving_infant_pop,
                               
                            oop_del_spending_per_infant = oop_del_spending / cv_surviving_infant_pop,
                            oop_del_spending_per_infant_lower = oop_del_spending_lower / cv_surviving_infant_pop,
                            oop_del_spending_per_infant_upper = oop_del_spending_upper / cv_surviving_infant_pop,
                            
                            oop_vac_spending_per_infant = oop_vac_spending / cv_surviving_infant_pop,
                            oop_vac_spending_per_infant_lower = oop_vac_spending_lower / cv_surviving_infant_pop,
                            oop_vac_spending_per_infant_upper = oop_vac_spending_upper / cv_surviving_infant_pop)]

## Output results
fwrite(price_volume_scalar_gbl, 
       paste0("FILEPATH/gbl_OOP_", 
              date1, ".csv"))

price_volume_scalar_gavi <- price_volume_scalar2[, .(oop_spending = sum(oop_spending),
                                                     oop_spending_lower = sum(oop_spending_lower),
                                                     oop_spending_upper = sum(oop_spending_upper),
                                                     
                                                     oop_del_spending = sum(oop_del_spending),
                                                     oop_del_spending_lower = sum(oop_del_spending_lower),
                                                     oop_del_spending_upper = sum(oop_del_spending_upper),
                                                     
                                                     oop_vac_spending = sum(oop_vac_spending),
                                                     oop_vac_spending_lower = sum(oop_vac_spending_lower),
                                                     oop_vac_spending_upper = sum(oop_vac_spending_upper),
                                                     
                                                     cv_surviving_infant_pop = sum(cv_surviving_infant_pop)),
                                                .(year_id, gavi_country)]

price_volume_scalar_gavi[,`:=`(oop_spending_per_infant = oop_spending / cv_surviving_infant_pop,
                               oop_spending_per_infant_lower = oop_spending_lower / cv_surviving_infant_pop,
                               oop_spending_per_infant_upper = oop_spending_upper / cv_surviving_infant_pop,
                               
                               oop_del_spending_per_infant = oop_del_spending / cv_surviving_infant_pop,
                               oop_del_spending_per_infant_lower = oop_del_spending_lower / cv_surviving_infant_pop,
                               oop_del_spending_per_infant_upper = oop_del_spending_upper / cv_surviving_infant_pop,
                               
                               oop_vac_spending_per_infant = oop_vac_spending / cv_surviving_infant_pop,
                               oop_vac_spending_per_infant_lower = oop_vac_spending_lower / cv_surviving_infant_pop,
                               oop_vac_spending_per_infant_upper = oop_vac_spending_upper / cv_surviving_infant_pop)]

price_volume_scalar_gbl[year_id == 2017, .(round(oop_spending_per_infant, 2),
                                           round(oop_del_spending_per_infant, 2),
                                           round(oop_vac_spending_per_infant, 2))]
price_volume_scalar_gavi[year_id == 2017, .(gavi_country,
                                            round(oop_spending_per_infant, 2),
                                            round(oop_del_spending_per_infant, 2),
                                            round(oop_vac_spending_per_infant, 2))]
## Output results
fwrite(price_volume_scalar_gavi, 
       paste0("FILEPATH/gavi_non-gavi_OOP_", 
              date1, ".csv"))

## -------------------------------------------------------------------------------------
## 4. Map and visualize results
## -------------------------------------------------------------------------------------
mapping.data1 <- copy(price_volume_scalar2)
setnames(mapping.data1, "oop_spending", "mapvar")
# mapping.data1 <- mapping.data1[super_region_name == "North Africa and Middle East", ]

#make dataframes
mapping.data1$mapvar <- as.numeric(mapping.data1$mapvar)
length(unique(mapping.data1$ihme_loc_id))
mapping.data1 <- merge(mapping.data1, locs_lm[, .(ihme_loc_id, location_id)], 
                      by = c('ihme_loc_id', 'location_id'), all.y = T)

mapping.data <- copy(mapping.data1[(!is.na(mapvar) & year_id == 2017), ])
# mapping.data <- mapping.data[!(ihme_loc_id %in% c('KAZ', 'MNG', 'TKM', 'UZB')), ]

out.dir <- FILEPATH
out.filename <- paste0("oop_scalar/2017 OOP spending on Immunizations ", date1, "arg.pdf")

max(mapping.data$mapvar/1e6)

#Find deciles for bucket cutoffs
#Uncomment the following line to create deciles based on your data
quantiles <- c(classIntervals(mapping.data$mapvar, 5, style = 'kmeans'))

get_labels_rounded <- function(in_list){
    labels_list <- c()
    if (length(in_list) < 2) {
        labels_list <- c("All values")
    } else {
        for (i in 2:length(in_list)) {
            this_label <- paste0("$", toString(round(in_list[i - 1] / 1e6, digits = 0)), " million",
                                 " to $",
                                 toString(round(in_list[i] / 1e6, digits = 0)), " million")
            labels_list <- append(labels_list,this_label)
        }
    }
    return(labels_list)
}

labels <- get_labels_rounded(quantiles$brks)

## Map Total Spending by location, 2017 ##
#map and output using the mapping function
gbd_map(data = mapping.data,
        limits = quantiles$brks,
        labels = labels,
        col = "RdYlBu", col.reverse = TRUE,
        na.color = "#999999",
        title = "2017 OOP spending",
        legend.title = "Dollars",
        legend.cex = 1,
        legend.shift = c(-5, -10),
        fname = paste0(out.dir,out.filename))

## -------------------------------------------------------------------------------------
## Map Spending per capita under 15
# pops <- get_population(location_id = locs_lm$location_id, 
#                        year_id = 2016, 
#                        age_group_id = c(1, 6, 7), 
#                        gbd_round_id = 6, 
#                        decomp_step = 'step4')
# 
# pops <- pops[, .(population = sum(population)), .(location_id)]
# 
# mapping.data1 <- merge(mapping.data1, pops[, .(location_id, population)],
#                       by = 'location_id', all.x = T)
# 
# mapping.data1[, mapvar := mapvar / population]
# 
# mapping.data <- copy(mapping.data1[year_id == 2017, ])
# mapping.data <- mapping.data[!(ihme_loc_id %in% c('KAZ', 'MNG', 'TKM', 'UZB')), ]

setnames(mapping.data, "mapvar", "oop_spending")
setnames(mapping.data, "oop_spending_per_infant", "mapvar")
max(mapping.data$mapvar)

#Find deciles for bucket cutoffs
#Uncomment the following line to create deciles based on your data
quantiles <- c(classIntervals(mapping.data$mapvar, 5, style = 'kmeans'))

get_labels_rounded <- function(in_list){
    labels_list <- c()
    if (length(in_list) < 2) {
        labels_list <- c("All values")
    } else {
        for (i in 2:length(in_list)) {
            this_label <- paste0("$", toString(round(in_list[i - 1], digits = 2)),
                                 " to $",
                                 toString(round(in_list[i], digits = 2)))
            labels_list <- append(labels_list,this_label)
        }
    }
    return(labels_list)
}

labels <- get_labels_rounded(quantiles$brks)

out.filename <- paste0("oop_scalar/2017 OOP spending on Immunizations per surviving infant ", date1, "arg.pdf")
#map and output using the mapping function
gbd_map(data = mapping.data,
        limits = quantiles$brks,
        labels = labels,
        col = "RdYlBu", 
        col.reverse = TRUE,
        na.color = "#999999",
        title = "2017 OOP spending per surviving infant",
        legend.title = "Dollars",
        legend.cex = 1,
        legend.shift = c(-5, -10),
        fname = paste0(out.dir,out.filename))

## -------------------------------------------------------------------------------------
## Produce location-specific time series of results

df <- get_region(mapping.data1)
df <- df[ihme_loc_id == 'ARG', 
                             super_region_name := 'Latin America and Caribbean']

setnames(df, "mapvar", "oop_spending")
setnames(df, "oop_spending_per_infant", "mapvar")

sr_locs <- unique(df$super_region_name)
sr_locs
pdf(paste0("FILEPATH/OOP_per_infant_by_country_2000-2017_", date1, "arg.pdf"))
for (i in c(1:7)) {
    a <- ggplot(df[super_region_name == sr_locs[i],], 
                aes(x = year_id, y = mapvar, color = super_region_name)) +
        geom_line() +
        ggtitle("Spending per surviving infant") +
        ylab("$USD") +
        theme_bw() +
        theme(legend.position = "top") +
        facet_wrap(~ihme_loc_id)
        print(a)
}
dev.off()

## END OF SCRIPT ##
