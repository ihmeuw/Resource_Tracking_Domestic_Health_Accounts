########################################################################################
## Prepare volume data from raw data files
## Author: Emilie Maddison
## Date: Last edited 24 June 2020
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
require(classInt, lib.loc = paste0(FILEPATH))
library(readxl)
library(ggplot2)

code.dir <- paste0(FILEPATH)
source(paste0(FILEPATH, "/helper_functions.R"))

in.dir <- paste0(FILEPATH)
out.dir <- paste0(FILEPATH)
# out.filename <- "Total volume of Immunizations.pdf"

locs_lm <- get_ig(locs)
locs_lm <- locs_lm[income_group != 'H',]

## -------------------------------------------------------------------------------------
## 1. Read in raw data

volume_dt <- paste0(in.dir, FILEPATH,
                    "Copy of DOVE_doses_19NOV2019 2000 to 2018.xlsx")
ipv_volume_dt <- paste0(in.dir, FILEPATH,
                        "HepBBD and IPV Vaccine Volumes_DOVE_data_2.10.2020_ermadd.xlsx")

## Prepare all the vaccine data except for IPV
sheets <- excel_sheets(volume_dt)
x <- lapply(sheets, function(X) readxl::read_excel(volume_dt, sheet = X))
names(x) <- sheets
volume_data <- rbindlist(x, use.names = T)

volume_data <- melt(volume_data, 
    id.vars = c("Index", "Country", "WHO Region", "Vaccine", "Strategy"),
    measure.vars = c('2000', '2001', '2002', '2003', '2004', '2005', '2006', 
                     '2007', '2008', '2009', '2010', '2011', '2012', '2013',
                     '2014', '2015', '2016', '2017'),
    variable.name = 'year_id',
    value.name = 'volume'
    )

length(unique(volume_data$Index))
length(unique(volume_data$Country))
length(unique(volume_data$`WHO Region`))
length(unique(volume_data$Vaccine))
length(unique(volume_data$Strategy))
length(unique(volume_data$year_id))
length(volume_data[volume > 0, volume])

## Based on sheet name and DOVE ROI documentation, this is the Measles-Rubella vaccine
volume_data[Vaccine == 'Rubella', Vaccine := "MR"]

## Prepare IPV data
excel_sheets(ipv_volume_dt)
ipv_data  <- read_excel(ipv_volume_dt, sheet = 'IPV')
names(ipv_data)
ipv_data2 <- data.table(ipv_data[, c("Index", "Country", "WHO Region", 
                                     '2011', '2012', '2013', '2014', 
                                     '2015', '2016', '2017')])
ipv_data2[, Vaccine := 'IPV']
ipv_data2[, Strategy := 'IPV']
ipv_data2 <- melt(ipv_data2,
    id.vars = c("Index", "Country", "WHO Region", "Vaccine", "Strategy"),
    measure.vars = c('2011', '2012', '2013', '2014', '2015', '2016', '2017'),
    variable.name = 'year_id',
    value.name = 'volume')

## -------------------------------------------------------------------------------------
## 3. Prepare final dataset

##Bind the two datasets together
volume9_data <- rbind(volume_data, ipv_data2)
names(volume9_data)

## Clean up data
setnames(volume9_data, 'Index', 'ihme_loc_id')
setnames(volume9_data, 'Vaccine', 'vaccine')

## Reassign Kosovo to Serbia (reevaluate if Kosovo is ever added to the GBD hierarchy)
volume9_data[ihme_loc_id == 'XK', ihme_loc_id := 'SRB']
volume9_data[is.na(Strategy), Strategy := "."]
volume9_data <- get_location_id(volume9_data)

## Assign Measles 1st and 2nd to just 'Measles'
volume9_data[vaccine %in% c("Measles 1st", "Measles 2nd"), vaccine := "Measles"]

## We are only interested in Routine vaccines
sum(volume9_data$volume)
## Drop volume which is part of an SIA campaign (Supplementary Immunization activity)
## Reduces total vaccine volume by 37%
volume9_data <- volume9_data[Strategy != "SIA",]
sum(volume9_data$volume)

length(unique(volume9_data$ihme_loc_id))
table(volume9_data$ihme_loc_id, volume9_data$year_id)

##
trues <- data.table(table(volume9_data[, .(ihme_loc_id, year_id)]) >= 1)
sapply(trues, table)
94*18
##

## Write out full dataset
fwrite(volume9_data, paste0(out.dir, "total_volume_vaccines.csv"))

## -------------------------------------------------------------------------------------
## 4. Review results

volume9_data2 <- get_region(volume9_data)
volume9_data2[ihme_loc_id == 'TUV', 
             super_region_name := "Southeast Asia, East Asia, and Oceania"]

## Graph results by region-vaccine
volume9_data2 <- volume9_data2[, .(volume = sum(volume)),
                              .(super_region_name, vaccine, year_id)]

plot <- ggplot(volume9_data2, 
          aes(x = year_id, y = (volume / 1e6), 
              color = super_region_name)) +
    geom_point() + 
    scale_x_discrete(breaks = c(2000, 2010, 2017),
                     labels = c("   2000", "2010", "2017   ")) +
    ggtitle(paste0("Total volume of DOVE-reported vaccine doses ",
            "by vaccine and super-region, 2000-2017")) +
    xlab("") +
    ylab("\n\n\n\n\nVolume\n(in millions\nof doses)") +
    guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
    theme_minimal() + 
    theme(
      legend.position = 'bottom',
      axis.text.x = element_text(angle = 0),
      axis.title.y = element_text(angle = 0),
      panel.spacing = unit(0.3, "cm")) +
    facet_wrap(~ vaccine, nrow = 2)

print(plot)

ggsave(plot = plot,
       filename = paste0(out.dir, "total_volume_vaccines_by_sr_figure.pdf"),
       height = 8, width = 12)

## Graph global results by vaccine
volume9_data2 <- volume9_data2[, .(volume = sum(volume)),
                               .(vaccine, year_id)]

plot <- ggplot(volume9_data2, 
               aes(x = year_id, y = (volume / 1e6))) +
  geom_point() + 
  scale_x_discrete(breaks = c(2000, 2010, 2017),
                   labels = c("   2000", "2010", "2017   ")) +
  ggtitle(paste0("Global volume of DOVE-reported vaccine doses by vaccine, 2000-2017")) +
  xlab("") +
  ylab("\n\n\n\n\nVolume\n(in millions\nof doses)") +
  guides(color = guide_legend(nrow = 3, byrow = TRUE),
         shape = guide_legend(nrow = 3, byrow = TRUE)) +
  theme_minimal() + 
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(angle = 0),
    axis.title.y = element_text(angle = 0),
    panel.spacing = unit(0.3, "cm")) +
  facet_wrap(~ vaccine, nrow = 2)

print(plot)

ggsave(plot = plot,
       filename = paste0(out.dir, "total_volume_vaccines_gbl_figure.pdf"),
       height = 8, width = 12)

## Vaccine years with zero volume.
## Used to assess missingness in dataset
# fwrite(year_vaccine[volume == 0,], 
#        paste0(out.dir, "vaccine-years_with_zero_volume.csv"))
