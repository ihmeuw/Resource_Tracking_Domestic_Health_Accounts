## -------------------------------------------------------------------------------------
## Country-specific bar charts
## Author: Emilie Maddison, adapted from code by Ian Cogswell
## Date: July 30, 2020 - During the 2020 COVID pandemic
## Purpose: To create country-specific bar charts of spending source, grouped by
## super region
## -------------------------------------------------------------------------------------

rm(list = ls())

pacman::p_load(data.table, reshape2, ggplot2,RColorBrewer)

## defining j and h 
if (Sys.info()[1] == "Linux") {
  j <- FILEPATH
  h <- FILEPATH
  k <- FILEPATH
} else if (Sys.info()[1] == "Windows") {
  j <- FILEPATH
  h <- FILEPATH
  k <- FILEPATH
}

source(paste0(FILEPATH, "helper_functions.R"))

## set filepath and read in immunization country list
date <- format(Sys.Date(), '%m%d%y')
# input_data_folder <- FILEPATH
years <- 2000:2017
# imfin_country_list <- fread(paste0(FILEPATH, 'imfin_country_list_updated.csv'))[, .(ihme_loc_id, gavi_eligible)]

data <- fread(paste0(FILEPATH, "total_spending_country_level_072420.csv"))

data_iso <- data[, .(government = mean(government), 
                     oop = mean(oop),
                     ppp = mean(ppp),
                     dah = mean(dah),
                     all = mean(all),
                     all_lwr = quantile(all, 0.025),
                     all_upr = quantile(all, 0.975)),
                  .(ihme_loc_id, year_id)]

## data wide to long by source
data_iso <- melt.data.table(data_iso, id.vars = c('ihme_loc_id', 'year_id'), variable.name = 'source')
data_iso[, source := factor(source, levels = c('ppp', 'oop', 'dah', 'government'), ordered = T)]

data_iso <- merge(data_iso, locs[, .(ihme_loc_id, location_name, super_region_name)], 
                  by = 'ihme_loc_id', all.x = T)
data_iso <- data_iso[!is.na(source), ]
data_iso[ihme_loc_id == 'ARG', super_region_name := "Latin America and Caribbean"]

sr_locs <- sort(unique(data_iso$super_region_name))

pdf(paste0(FILEPATH, "country_specific_source_barchart_with_ppp_", date, "_h.pdf"),
    width = 14, height = 8)
for (i in c(1:5)) {
  plot <- ggplot() +
    geom_col(data = data_iso[super_region_name == sr_locs[i],], 
             aes(x = year_id, y = value / 1e6, fill = source), position = 'stack') +
    ggtitle(paste0(sr_locs[i], "- Spending by source")) + 
    xlab('') +
    ylab('Millions of 2019 USD') +
    scale_x_continuous(expand = c(0.01, 0)) +
    scale_fill_manual(values = c("#FDC12A", "#e31a1c", "#33a02c", "#1f78b4"),
                                   name = "Source of Financing",
                                   labels = c("Prepaid private spending", 
                                              "Out-of-pocket spending", 
                                              "Development assistance for health", 
                                              "Government spending")) +
    theme_classic() +
    theme(legend.position = 'top',
          axis.text.x = element_text(angle = 40, hjust = 1)) +
    facet_wrap( ~ location_name,  labeller = label_wrap_gen(28), scales = 'free_y')
  print(plot)
}
plot <- ggplot() +
  geom_col(data = data_iso[super_region_name == sr_locs[6], ], 
          aes(x = year_id, y = value / 1e6, fill = source), position = 'stack') +
  ggtitle(paste0(sr_locs[6], "- Spending by source")) +
  xlab('') +
  ylab('Millions of 2019 USD') +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_fill_manual(values = c("#FDC12A", "#e31a1c", "#33a02c", "#1f78b4"),
                                 name = "Source of Financing",
                                 labels = c("Prepaid private spending", 
                                            "Out-of-pocket spending", 
                                            "Development assistance for health", 
                                            "Government spending")) +
  theme_classic() +
  theme(legend.position = 'top', 
        axis.text.x = element_text(angle = 40, hjust = 1)) +
  facet_wrap( ~ location_name, ncol = 10, labeller = label_wrap_gen(16), scales = 'free_y')
print(plot)
dev.off()

## End of Script ##