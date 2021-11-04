########################################################################################
## MI4A: Vaccine Purchase data from the WHO
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

## source functions 
## mapping function, which will be called with "gbd_map"
source(paste0(("FILEPATH/GBD_WITH_INSETS_MAPPING_FUNCTION.R"))
source(paste0(("FILEPATH/get_location_metadata.R"))

## Today's date
date1 <- format(Sys.time(), "%Y%m%d")

## Set up directories
in.dir <- paste0(FILEPATH)
out.dir <- paste0(FILEPATH)
out.filename <- "Total spending on Immunizations.pdf"

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

## Get location ids so that your future is easier.
locs <- get_location_metadata(location_set_id = 1, gbd_round_id = 5)
locs <- locs[level == 3, .(ihme_loc_id, location_name, super_region_id, super_region_name)]
price <- merge(price, locs[, .(ihme_loc_id, location_name)], by.x = 'Country', by.y = 'location_name', all.x = T)

## some Country names are different than the IHME names, manually fix these.
## gbd_round_id = 6
# fix_locs <- data.table(x = unique(price[is.na(ihme_loc_id), ]$Country),
#                        ihme_loc_id2 = c('BIH', 'BRN', 'CAF', 'CIV', 'CZE',
#                                          'COD', 'DOM', 'IRN', 'LAO', 'MKD',
#                                          'MDA', 'PRK', 'RUS', 'STP', 'KOR', 
#                                          'SWZ', 'SYR', 'TZA', 'TTO', 'USA'))
## gbd_round_id = 5
fix_locs <- data.table(x = unique(price[is.na(ihme_loc_id), ]$Country),
                       ihme_loc_id2 = c('BIH', 'CAF', 'CPV', 'COK', 'CZE', 'COD',
                                        'DOM', 'GMB', 'RUS', 'SMR', 'STP', 
                                        'TTO', 'USA', 'VNM'))
print("Check that these are correct:")
print(fix_locs)


price <- merge(price, fix_locs, by.x = 'Country', by.y = 'x', all.x = T)
price[is.na(ihme_loc_id), ihme_loc_id := ihme_loc_id2]
price[, ihme_loc_id2 := NULL]

price <- merge(price, locs[, .(ihme_loc_id, super_region_id, super_region_name)],
               by = 'ihme_loc_id', all.x = T)
names(price)
## Take a look at the vaccine names
a <- data.table(unique(price[, .(Vaccine, `Vaccine Sub-type`, 
                                 `Vaccine Sub-type`, CommercialName)]))

unique(price$Vaccine)

## These are the vaccines we are interested in.
nine_vaccines <- c("HPV", 
                   "IPV",
                   "JE", 
                   "MenA", "MenAC", "MenACYW-135", "MenACYW-135 Ps", "MenA Ps",
                   "Measles", "Rubella", "MR",
                   "PCV",
                   "DTaP-HepB-Hib", "DTaP-HepB-IPV", "DTaP-Hib-IPV",
                   "Rota",
                   "YF")
price_9 <- price[Vaccine %in% nine_vaccines, ]

menA_vaccines  <- c("MenA", "MenAC", "MenACYW-135", "MenACYW-135 Ps", "MenA Ps")
## Dropping the hexavalent vaccine because it is considerably more expensive than the
## pentavalent version
#penta_vaccines <- c("DTaP-HepB-Hib", "DTaP-HepB-Hib-IPV", "DTaP-HepB-IPV", "DTaP-Hib-IPV")
penta_vaccines <- c("DTaP-HepB-Hib", "DTaP-HepB-IPV", "DTaP-Hib-IPV")
mr_vaccines    <- c("Measles", "Rubella", "MR")
## I'm not renaming Measles, Rubella, and MR yet, since they are separate in the volume 
## data as well.

## Explore MenA data
## Decided that since these are the costs reported by the country, they do not need to
## be adjusted. Where more than one MenA vaccine was purchased by the same country
## in the same year, I will attempt to take an average price, weighted by the units
## reported in this dataset.
price_menA <- price[Vaccine %in% menA_vaccines, ]
price_menA_vaccine <- price_menA[, .(PricePerDoseInUSD = mean(PricePerDoseInUSD)),
                         .(Vaccine, Region)]
ggplot(price_menA_vaccine, aes(x = Vaccine, y = PricePerDoseInUSD, color = Region)) +
  geom_point() +
  theme_minimal()

## Explore the penta data
price_penta <- price[Vaccine %in% penta_vaccines, ]
price_penta_vaccine <- price_penta[, .(PricePerDoseInUSD = mean(PricePerDoseInUSD)),
                                 .(Vaccine, Country)]
ggplot(price_penta_vaccine, aes(x = Vaccine, y = PricePerDoseInUSD, color = Country)) +
  geom_point() +
  theme_minimal()

##--------------------------------------------------------------------------------------
## Rename vaccines to 9 vaccines of interest
price_9$Vaccine <- ifelse(price_9$Vaccine %in% menA_vaccines, "MenA", price_9$Vaccine)
price_9$Vaccine <- ifelse(price_9$Vaccine %in% penta_vaccines, "Penta", price_9$Vaccine)
price_9$Vaccine <- ifelse(price_9$Vaccine %in% mr_vaccines, "MR", price_9$Vaccine)

table(price_9$Vaccine)

table(price$INCOTerm)
table(price_9$INCOTerm)

price_inco <- price_9[, .(PricePerDoseInUSD = mean(PricePerDoseInUSD)),
                        .(INCOTerm)]
price_vaccine <- price_9[, .(PricePerDoseInUSD = mean(PricePerDoseInUSD)),
                    .(Vaccine)]


## Country-specific data only available for 2005-2016. Regional data available for 2005-2017.
price_country <- price_9[, .(PricePerDoseInUSD = mean(PricePerDoseInUSD)),
                       .(Country)]

length(unique(price_9[Year == 2013, ]$Country))
unique(price_9$Year)

price_region <- price_9[, .(PricePerDoseInUSD = mean(PricePerDoseInUSD)),
                       .(super_region_name)]
price_vat <- price_9[, .(PricePerDoseInUSD = mean(PricePerDoseInUSD)),
                      .(VATName)]
## -----------------------------------------------------------------------------
## Plotting price

# ggplot(price_inco, aes(x=INCOTerm, y=PricePerDoseInUSD, color=INCOTerm)) +
#   geom_point() + 
#   theme_minimal()
# 
# ggplot(price_vaccine, aes(x=Vaccine, y=PricePerDoseInUSD)) +
#   geom_point() +
#   theme_minimal()
# 
# ggplot(price_country, aes(x=Country, y=PricePerDoseInUSD)) +
#   geom_point() + 
#   theme_minimal()
# 
# ggplot(price_region, aes(x=super_region_name, y=PricePerDoseInUSD)) +
#   geom_point() +
#   theme_minimal()
# 
# ggplot(price_vat, aes(x=VATName, y=PricePerDoseInUSD)) +
#   geom_point() +
#   theme_minimal()

## ------------------------------------------------------------------------------------
names(price)
#price_9_summary <- price_9[, .(PricePerDoseInUSD = mean(PricePerDoseInUSD)), .(Year, Country, Vaccine)]
#boxplot(as.numeric(PricePerDoseInUSD) ~ Vaccine, data = price_9_summary)

# boxplot(as.numeric(PricePerDoseInUSD) ~ Vaccine, data = price_9)
# 
# summary(lm(PricePerDoseInUSD ~ Vaccine, price_9))
# summary(lm(log(PricePerDoseInUSD) ~ Vaccine, price_9))
# 
# summary(lm(log(PricePerDoseInUSD) ~ Vaccine + super_region_name, price_9))
price_9 <- price_9[!is.na(super_region_name)]
a <- (lm(log(PricePerDoseInUSD) ~ Vaccine + super_region_name, price_9))
a$coefficients
summary(a)
# a <- summary(lm(log(PricePerDoseInUSD) ~ Vaccine + super_region_name * Year, price_9))
# a <- lm(log(PricePerDoseInUSD) ~ Vaccine + super_region_name * Year, price_9)
# a$coefficients
# b <- predict(a)
# 
new <- CJ(
  #Year = c(2000:2017),
          super_region_name = unique(price_9$super_region_name),
          Vaccine = unique(price_9$Vaccine))
#new$price <- exp(predict(a, new))
new2 <- exp(predict(a, new, interval = 'confidence'))
new3 <- cbind(new, new2)
# 
# 
# summary(lm(log(PricePerDoseInUSD) ~ Vaccine + super_region_name + Year, price_9))
# 
# summary(lm(PricePerDoseInUSD ~ Vaccine + Country, price_9))
# summary(lm(PricePerDoseInUSD ~ Vaccine + Country + Year, price_9))
# 
# summary(lm(PricePerDoseInUSD ~ Vaccine + INCOTerm, price_9))
# summary(lm(PricePerDoseInUSD ~ INCOTerm, price_9))
# summary(lm(PricePerDoseInUSD ~ Vaccine + VAT, price_9))
# summary(lm(PricePerDoseInUSD ~ Country + VAT, price_9))
# 
# table(price_9$Country, price_9$VAT)
# table(price_9$Region, price_9$VAT)
# 
# boxplot(as.numeric(PricePerDoseInUSD) ~ VAT, data = price_9)

fwrite(price_9[, .(ihme_loc_id, super_region_id, super_region_name, Country, Region, Year, Vaccine, INCOTerm, VAT, PricePerDoseInUSD)], paste0(out.dir, "price/price_9_vaccines.csv"))
fwrite(new3, paste0(out.dir, "price/regional_price_9_vaccines", date1, ".csv"))

## -------------------------------------------------------------------------------------
## Mapping price, coverage 

price_2 <- price[, .(Year, Country, Vaccine, AnnualNumberOfDoses, PricePerDoseInUSD)]
price_2[, AnnualExpense := AnnualNumberOfDoses * PricePerDoseInUSD]
price_sum <- price_2[, .(TotalAnnualExpense = sum(AnnualExpense)), .(Country)]

#add iso3 column
locs <- get_location_metadata(location_set_id = 22, gbd_round_id = 6)
locs2 <- locs[level == 3, .(location_name, ihme_loc_id, location_id)]
mapping.data <- merge(price_sum, locs2, 
                      by.x = 'Country', by.y = 'location_name', all = TRUE)
#mapping.data <- mapping.data[Year == 2016,]

#rename ratio column to mapvar because that is the name the mapping function will accept
setnames(mapping.data, "TotalAnnualExpense", "mapvar")

#Find deciles for bucket cutoffs
#Uncomment the following line to create deciles based on your data
quantiles <- c(classIntervals(mapping.data$mapvar, 5, style = 'kmeans'))

get_labels_rounded <- function(in_list){
  labels_list <- c()
  if (length(in_list) < 2) {
    labels_list <- c("All values")
  } else {
    for (i in 2:length(in_list)) {
      this_label <- paste0("$", toString(round(in_list[i - 1],digits = 0)), "",
                           " to ",
                           " $", toString(round(in_list[i],digits = 0)), "")
      labels_list <- append(labels_list,this_label)
    }
  }
  return(labels_list)
}

labels <- get_labels_rounded(quantiles$brks)

#map and output using the mapping function
#National
gbd_map(data = mapping.data,
        limits = quantiles$brks,
        labels = labels,
        col = "RdYlBu", col.reverse = TRUE,
        na.color = "#999999",
        title = "Total spending on immunizations, 2016",
        legend.title = "US dollars",
        legend.cex = 1,
        legend.shift = c(-5, -10)),
        fname = paste0(out.dir, "all_years_", out.filename))

## -----------------------------------------------------------------------------
# ## MI4A Volume data
# volume_vaccine <- price[, .(AnnualNumberOfDoses = mean(AnnualNumberOfDoses)),
#                         .(Vaccine, Year)]
# 
# unique(volume_vaccine$Vaccine)
# 
# vac_names<- c("DTaP-HepB-Hib", "DTaP-HepB-IPV", "DTaP-Hib", "DTaP-Hib-IPV",
#   "Tdap-IPV", "DTaP-IPV", "DTaP-HepB", "DTaP-HepB-Hib-IPV",
#   "Measles", "MMR", "MR", "MMRV",
#   "PCV", 
#   "IPV",  
#   "YF", 
#   "MenAC", "MenACYW-135 Ps", "MenACYW-135", "MenA Ps",            
#   "HPV", 
#   "Rota", 
#   "JE")
# 
# volume_vaccine <- volume_vaccine[Vaccine %in% vac_names, ]
# volume_vaccine$Vaccine <- ifelse(volume_vaccine$Vaccine %in% 
#                                    c("Measles", "MMR", "MR", "MMRV"),
#                                  "MR",
#                                  volume_vaccine$Vaccine
#                                  )
# volume_vaccine$Vaccine <- ifelse(volume_vaccine$Vaccine %in% 
#                                    c("DTaP-HepB-Hib", "DTaP-HepB-IPV", "DTaP-Hib", 
#                                      "DTaP-Hib-IPV", "Tdap-IPV", "DTaP-IPV", 
#                                      "DTaP-HepB", "DTaP-HepB-Hib-IPV"),
#                                  "Penta",
#                                  volume_vaccine$Vaccine)
# 
# volume_vaccine$Vaccine <- ifelse(volume_vaccine$Vaccine %in% 
#                                    c("MenAC", "MenACYW-135 Ps", 
#                                      "MenACYW-135", "MenA Ps"),
#                                  "MenA",
#                                  volume_vaccine$Vaccine)
# volume_vaccine$Vaccine <- ifelse(volume_vaccine$Vaccine %in% 
#                                    c("Rota"),
#                                  "RVV",
#                                  volume_vaccine$Vaccine)
# 
# unique(volume_vaccine$Vaccine)
# 
# names(volume_vaccine)
# names(volume9_data)
# volume9_data$Vaccine <- ifelse(volume9_data$Vaccine %like% 
#                                c("Measles"),
#                                "MR",
#                                volume9_data$Vaccine)
# 
# volume9_data$Vaccine <- ifelse(volume9_data$Vaccine %like% 
#                                  c("Rubella"),
#                                "MR",
#                                volume9_data$Vaccine)
# 
# volume_vaccine$Year <- as.factor(volume_vaccine$Year)
# compare_volumes <- merge(volume_vaccine, volume9_data,
#                          by.x=c('Vaccine', 'Year'),
#                          by.y=c('Vaccine', 'year_id'),
#                          all = T)
# unique(compare_volumes$Vaccine)
# unique(volume9_data$Vaccine)
# 
# #plot <- 
#   ggplot(compare_volumes, 
#                aes(x=AnnualNumberOfDoses/1E6, y=volume/1E6)) +
#           geom_abline() +
#           geom_point() +
#           ggtitle("Comparing M14A and DOVE volume data\nfor 2005-2018, by vaccine") +
#           xlab("M14A Doses (in millions)") +
#           ylab("\n\n\n\n\nDOVE\nDoses\n(in millions)") +
#           theme_minimal() +
#           theme(
#             panel.grid.minor = element_blank(),
#             axis.title.y = element_text(angle = 0)) +
#           facet_wrap(~ Vaccine, scales='free')
# 
# 
# # ggsave(plot = plot, 
# #        filename = paste0(out.dir, "comparing_MI4A_and_DOVE_volume_figures.pdf"),
# #        height = 7,
# #        width = 7)
# 
# 
