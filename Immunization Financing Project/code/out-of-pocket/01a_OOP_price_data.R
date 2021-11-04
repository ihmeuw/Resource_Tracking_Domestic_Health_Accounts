########################################################################################
## MI4A: Vaccine Purchase data from the WHO - MI4A
## Author: Emilie Maddison
## Date: Mar 26, 2020
## Description: 
########################################################################################

## Set up environment 
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

## load required packages
require(data.table)
require(classInt, lib.loc = paste0(h, 'R/'))
library(readxl)
library(ggplot2)
library(MASS)
library(lme4)
library(merTools)

## source functions 
## mapping function, which will be called with "gbd_map"
source(paste0(FILEPATH, "GBD_WITH_INSETS_MAPPING_FUNCTION.R"))
source(paste0(FILEPATH, "get_location_metadata.R"))
source(paste0(FILEPATH, "currency_conversion.R"))
source(paste0(FILEPATH, "helper_functions.R"))

## Today's date
date1 <- format(Sys.time(), "%Y%m%d")

## Set up directories
in.dir <- paste0(FILEPATH)
out.dir <- paste0(FILEPATH)
# out.filename <- "Total spending on Immunizations.pdf"

locs_lm <- get_ig(locs)
locs_lm <- locs_lm[income_group != 'H', ]

## -------------------------------------------------------------------------------------
excel_sheets(paste0(
  in.dir, 
  FILEPATH,
  "MI4A_public_database (3).xlsx"
))

## Read in data
price <- data.table(read_excel(paste0(
           in.dir, 
           FILEPATH,
           "MI4A_public_database (3).xlsx"),
           sheet = "Simplified view unfiltered"
           ))

setnames(price, "Vaccine", "vaccine")

## Get location ids so that your future is easier.
# locs <- get_location_metadata(location_set_id = 1, gbd_round_id = 5)
# locs <- locs[level == 3, .(ihme_loc_id, location_name, super_region_id, super_region_name)]
price <- merge(price, locs[, .(ihme_loc_id, location_name)], by.x = 'Country', by.y = 'location_name', all.x = T)

## Stop if these are not equal

stop(length(unique(price[is.na(ihme_loc_id), ]$Country)) == 
       length(c('BIH', 'BRN', 'CAF', 'COK', 'CIV', 'CZE', 'COD',
                'DOM', 'IRN', 'LAO',  'MKD', 'MDA', 'PRK', 
                'RUS', 'SMR', 'STP', 'KOR', 'SWZ', 'SYR', 'TZA', 
                'TTO', 'USA')))

fix_locs <- data.table(x = unique(price[is.na(ihme_loc_id), ]$Country),
                       ihme_loc_id2 = c('BIH', 'BRN', 'CAF', 'COK', 'CIV', 'CZE', 'COD',
                                        'DOM', 'IRN', 'LAO',  'MKD', 'MDA', 'PRK', 
                                        'RUS', 'SMR', 'STP', 'KOR', 'SWZ', 'SYR', 'TZA', 
                                        'TTO', 'USA'))
print("Visually check that these are correct:")
fix_locs <- merge(fix_locs, locs[,.(ihme_loc_id, location_name)], 
                  by.x = 'ihme_loc_id2', by.y = 'ihme_loc_id', all.x = T)
## Cook Islands and San Marino are acceptable because they aren't in the GBD2019 hierarchy
print(fix_locs[is.na(location_name)])


price <- merge(price, fix_locs[, .(x, ihme_loc_id2)], by.x = 'Country', by.y = 'x', all.x = T)
price[is.na(ihme_loc_id), ihme_loc_id := ihme_loc_id2]
price[, ihme_loc_id2 := NULL]

stop(nrow(price[is.na(ihme_loc_id), ]) == 0)

price <- merge(price, locs[, .(ihme_loc_id, super_region_name)],
               by = 'ihme_loc_id', all.x = T)
names(price)

## Currency conversion - to 2019 USD
price[, year := Year]
price <- currency_conversion(price,
                                col.loc = 'ihme_loc_id',
                                col.value = c('PricePerDoseInUSD'),
                                currency = 'USD',
                                col.currency.year = 'year',
                                base.year = 2019,
                                base.unit = 'USD')

## Take a look at the vaccine names
a <- data.table(unique(price[, .(vaccine, `Vaccine Sub-type`, 
                                 `Vaccine Sub-type`, CommercialName)]))
unique(price$vaccine)

## These are the vaccines we are interested in.
nine_vaccines <- c("HPV", 
                   "IPV",
                   "JE", 
                   "MenA", "MenAC", "MenACYW-135", "MenACYW-135 Ps", "MenA Ps",
                   "Measles", 
                   "MR",
                   "PCV",
                   "DTaP-HepB-Hib", "DTaP-HepB-IPV", "DTaP-Hib-IPV",
                   "Rota",
                   "YF")

price_9 <- price[vaccine %in% nine_vaccines, ]

menA_vaccines  <- c("MenA", "MenAC", "MenACYW-135", "MenACYW-135 Ps", "MenA Ps")

## Dropping the hexavalent vaccine because it is considerably more expensive than the
## pentavalent version
penta_vaccines <- c("DTaP-HepB-Hib", "DTaP-HepB-IPV", "DTaP-Hib-IPV")

##--------------------------------------------------------------------------------------
## Measles / Rubella investigation

mr_vaccines_dt <- price[vaccine %in% c('Measles', 'MM', 'MR', 'MMRV', 'MMR', 'Mumps', 'Rubella')]
boxplot(PricePerDoseInUSD ~ vaccine, mr_vaccines_dt)
## ProcurementMechanism investigation

boxplot(PricePerDoseInUSD ~ ProcurementMechanism, price_9)
summary(lm(PricePerDoseInUSD ~ ProcurementMechanism, price_9))
summary(lm(PricePerDoseInUSD ~ ProcurementMechanism + Country, price_9))

table(price_9$ProcurementMechanism, price_9$super_region_name)
##--------------------------------------------------------------------------------------
## Rename vaccines to 9 vaccines of interest
price_9[vaccine %in% menA_vaccines, vaccine := "MenA"]
price_9[vaccine %in% penta_vaccines, vaccine := "Penta"]

table(price_9$vaccine)
names(price)

price_9 <- price_9[!is.na(super_region_name)]

##
trues <- data.table(table(price_9[, .(ihme_loc_id, Year)]) >= 1)
sapply(trues, table)
##

a <- (lm(log(PricePerDoseInUSD) ~ vaccine + super_region_name, price_9))
a$coefficients
summary(a)

new <- CJ(
          Year = c(2000:2017),
          super_region_name = unique(price_9$super_region_name),
          vaccine = unique(price_9$vaccine))

new2 <- exp(predict(a, new, interval = 'confidence'))
new3 <- cbind(new, new2)

# fwrite(price_9[, .(ihme_loc_id, super_region_name, Country, Region, 
#                    year_id = Year, vaccine, INCOTerm, VAT, PricePerDoseInUSD)], 
#        paste0(out.dir, "price/price_9_vaccines_MRseparate.csv"))
# fwrite(new3, paste0(out.dir, "price/regional_price_9_vaccines_", date1, ".csv"))

##--------------------------------------------------------------------------------------
# ## Make 1000-draw predictions from linear model
b <- (lmer(log(PricePerDoseInUSD) ~ vaccine + super_region_name +
             (1|Year), price_9))
summary.lm(aov(log(PricePerDoseInUSD) ~ vaccine + super_region_name +
                 (1|Year), price_9))
summary(b)

new_pred <- CJ(
  Year = c(2000:2017),
  super_region_name = unique(price_9$super_region_name),
  vaccine = unique(price_9$vaccine))

## Use only fixed effects, and remove residual variance, which is exaggerated because of
## heterogeneity in data coverage
test_results <- predictInterval(b, new_pred, which = "fixed", 
                         level = 0.95, n.sims = 1000, stat = "mean",
                         include.resid.var = FALSE, returnSims = TRUE, seed = NULL,
                         .parallel = FALSE, .paropts = NULL, fix.intercept.variance = FALSE,
                         ignore.fixed.terms = NULL)

new_pred2 <- cbind(new_pred, attr(test_results, 'sim.results'))
cols <- paste0(1:1000)
new_pred2[ , (cols) := lapply(.SD, exp), .SDcols = cols]

new_pred3 <- melt(new_pred2, id.vars = c('Year', 'super_region_name', 'vaccine'),
                  variable.name = 'draw',
                  value.name = 'price')
## check that this is doing what we anticipate
# new_pred3$draw <- substring(new_pred3$draw, 2)
setnames(new_pred3, "Year", "year_id")

# fwrite(new_pred3, paste0(out.dir, "price/regional_price_10_vaccines_draws_", date1, ".csv"))
## -------------------------------------------------------------------------------------
## Examine results 

test_results2 <- cbind(new_pred, test_results)

## Summary table 
test_results2017 <- test_results2[Year == 2017, ]
test_results2017[, value := paste0(round(exp(fit), 2))]
test_results2017 <- dcast(test_results2017, super_region_name ~ vaccine,
                          value.var = 'value')
fwrite(test_results2017, paste0(out.dir, "price/2017_regional_price_table.csv"))

table(test_results2[Year == 2017, ])
## Plot mean results, alongside raw data and lm() linear model
ggplot(test_results2, aes(x = Year, y = exp(fit), color = super_region_name)) +
  geom_line() +
  geom_line(data = new3, aes(x = Year, y = fit, color = super_region_name), 
            linetype = 'dashed') +
  geom_ribbon(aes(ymin = exp(lwr), ymax = exp(upr), fill = super_region_name),
              alpha = 0.1, color = NA) +
  geom_point(data = price_9, aes(x = Year, y = PricePerDoseInUSD)) +
  theme_classic() +
  facet_wrap(~ vaccine)

## Plot first draw of prediction frame, to make sure data is ordered correctly
ggplot(new_pred2, aes(x = Year, y = 1, color = super_region_name)) +
  geom_point() +
  theme_classic() +
  facet_wrap(~ vaccine)

mean_results <- new_pred3[, .(mean = mean(price),
                              lower = quantile(price, 0.025),
                              upper = quantile(price, 0.975)), 
                           .(super_region_name, vaccine, year_id)]

## End of Script ##