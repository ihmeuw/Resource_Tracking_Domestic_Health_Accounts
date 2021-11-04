########################################################################################
## Stacked bar charts by Routine/supplementary, vaccine/devliery, gavi/non-gavi
## Author: Emilie Maddison, adapted from Ian Cogswell
## Date: 9 June 2020 
## This code was written during the COVID-19 pandemic
## Description: Those stacked bar charts we put in the powerpoint decks
########################################################################################
## -------------------------------------------------------------------------------------
## 1. Set up environment
## -------------------------------------------------------------------------------------

# ## Clear environment
# rm(list = ls())
# 
# ## Set filepaths to directories
# if (Sys.info()[1] == "Linux") {  
#   j <- FILEPATH  
#   h <- FILEPATH
#   k <- FILEPATH
# } else if (Sys.info()[1] == "Windows") {  
#   j <- FILEPATH  
#   h <- FILEPATH  
#   k <- FILEPATH  
# }  

## Load libraries
library(ggplot2)
library(ggpubr)
library(RColorBrewer)

## Today's date
# date1 <- format(Sys.time(), "%Y%m%d")
#date1 <- '20200629'
#tag1 <- "MRtest"
#tag <- "delivery"

## Output directory
out_dir <- FILEPATH

# price_volume_scalar4 <- fread(paste0("FILEPATH/data_OOP_by_country_", date1, "_", tag1, ".csv"))
# 
# price_volume_scalar_gbl <- fread(paste0("FILEPATH/gbl_OOP_", date1, "_", tag1, ".csv"))
# 
# price_volume_scalar_gavi <- fread(paste0("FILEPATH/gavi_non-gavi_OOP_", date1, "_", tag1, ".csv"))

## -------------------------------------------------------------------------------------

names(price_volume_scalar_gbl)

df_1 <- price_volume_scalar_gbl[, .(year_id, oop = oop_del, oop_lower = oop_del_lower,
                                    oop_upper = oop_del_upper, 
                                    component = 'Delivery spending')]
df_2 <- price_volume_scalar_gbl[, .(year_id, oop = oop_vac, oop_lower = oop_vac_lower,
                                    oop_upper = oop_vac_upper, 
                                    component = 'Vaccine spending')]
df_3 <- price_volume_scalar_gbl[, .(year_id, oop, oop_lower, oop_upper, 
                                    component = 'Routine spending')]


df_plot <- rbind(df_1, df_2, df_3)
max(df_plot$oop) / 1e6

## -------------------------------------------------------------------------------------
## function for building plots

  ## delivery/vaccine plot
  plot1 <- ggplot(data = df_plot[component == 'Routine spending', ],
                  aes(x = as.character(year_id), y = (oop / 1e6), fill = component)) +
    geom_col(position = 'stack') +
    geom_text(data = df_plot[component %in% c('Routine spending') & year_id %in% c(2000, 2017), ], 
              aes(label = round(oop / 1e6, 0), y = oop/1e6 + 5), position = 'stack') +
    xlab('Year') +
    ylab('Millions of 2019 USD') +
    theme_bw() +
    theme(legend.position = c(0.17, 0.8), 
          legend.direction = "vertical", 
          legend.title = element_text(),
          legend.text = element_text(),
          axis.title = element_blank(),
          axis.text.x = element_text(angle = 40, vjust = 0.85, hjust = .95),
          axis.text.y = element_text(colour = "black"),
          plot.subtitle = element_text(),
          plot.title = element_text(),
          panel.border = element_blank(), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black")) +
    scale_fill_manual(values = "#0570b0",
                      name = "Type of spending")
  
  ## routine/supplementary plot
  plot2 <- ggplot(data = df_plot[component %in% c('Vaccine spending', 'Delivery spending')],
                  aes(x = as.character(year_id), y = (oop / 1e6), fill = component)) +
    geom_col(position = 'stack') +
    geom_text(data = df_plot[component %in% c('Vaccine spending', 'Delivery spending') & year_id %in% c(2000, 2017), ], 
              aes(label = round(oop / 1e6, 0), y = oop/1e6 + 5), position = 'stack') +
    xlab('Year') +
    ylab('Billions of 2019 USD') +
    theme_bw() +
    theme(legend.position = c(0.17, 0.8), 
          legend.direction = "vertical", 
          legend.title = element_text(),
          legend.text = element_text(),
          axis.title = element_blank(),
          axis.text.x = element_text(angle = 40, vjust = 0.85, hjust = .95),
          axis.text.y = element_text(colour = "black"),
          plot.subtitle = element_text(),
          plot.title = element_text(),
          panel.border = element_blank(), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black")) +
    scale_fill_manual(values = c("#fd8d3c", "#e31a1c"),
                      name = "Type of spending")


print(plot1)
print(plot2)

ggsave(plot = plot1, 
       filename = paste0(out_dir, "OOP_routine_supp_figure_", date1, "_", tag, ".pdf"),
       width = 7, height = 7)
ggsave(plot = plot2, 
       filename = paste0(out_dir, "OOP_vac_delivery_figure_", date1, "_", tag, ".pdf"),
       width = 7, height = 7)


## -------------------------------------------------------------------------------------
df_1 <- price_volume_scalar_gavi[, .(year_id, gavi_eligible, oop = oop_del,
                                     oop_lower = oop_del_lower,
                                     oop_upper = oop_del_upper)]
df_1$component <- 'Delivery spending'
df_2 <- price_volume_scalar_gavi[, .(year_id, gavi_eligible, oop = oop_vac,
                                     oop_lower = oop_vac_lower,
                                     oop_upper = oop_vac_upper)]
df_2$component <- 'Vaccine spending'
df_3 <- price_volume_scalar_gavi[, .(year_id, gavi_eligible, oop, oop_lower, oop_upper)]
df_3$component <- 'Routine spending'
df_gavi <- rbind(df_1, df_2, df_3)


## routine/supplementary plot
plot3 <- ggplot(data = df_gavi[gavi_eligible == 'Y' & component %in% c('Vaccine spending', 'Delivery spending')],
                aes(x = as.character(year_id), y = (oop / 1e6), fill = component)) +
  geom_col(position = 'stack') +
  geom_text(data = df_gavi[gavi_eligible == 'Y' & 
                             component %in% c('Vaccine spending', 'Delivery spending') & 
                             year_id %in% c(2000, 2017), ], 
            aes(label = round(oop / 1e6, 0), y = oop/1e6 + 4), position = 'stack') +
  scale_y_continuous(limits = c(0,210), expand = c(0,0)) +
  scale_fill_manual(values = c("#fd8d3c", "#e31a1c"), name = "Type of spending") +
  xlab('Year') +
  ylab('Billions of 2019 USD') +
  theme_bw() +
  theme(legend.position = c(0.17, 0.8), 
        legend.direction = "vertical", 
        legend.title = element_text(),
        legend.text = element_text(),
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 40, vjust = 0.85, hjust = .95),
        plot.subtitle = element_text(),
        plot.title = element_text(),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


input_data <- df_gavi[gavi_eligible == 'N' & component %in% c('Vaccine spending', 'Delivery spending')]
plot4 <- ggplot(data = input_data,
                aes(x = (year_id), y = (oop / 1e6), fill = component)) +
  geom_col(position = 'stack') +
  geom_text(data = input_data[year_id %in% c(2000, 2017), ],
            aes(label = round(oop / 1e6, 0), y = (oop / 1e6) + 4), position = 'stack') +
  scale_y_continuous(limits = c(0, 315), expand = c(0,0)) +
  scale_fill_manual(values = c("#fd8d3c", "#e31a1c"), name = "Type of spending") +
  xlab('Year') +
  ylab('Billions of 2019 USD') +
  theme_bw() +
  theme(legend.position = c(0.17, 0.8), 
        legend.direction = "vertical", 
        legend.title = element_text(),
        legend.text = element_text(),
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 40, vjust = 0.85, hjust = .95),
        plot.subtitle = element_text(),
        plot.title = element_text(),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

print(plot3)
print(plot4)
ggsave(plot = plot3, 
       filename = paste0(out_dir, "OOP_gavi_vac_delivery_figure_", date1, "_", tag, ".pdf"),
       width = 5, height = 5)
ggsave(plot = plot4, 
       filename = paste0(out_dir, "OOP_nongavi_vac_delivery_figure_", date1, "_", tag, ".pdf"),
       width = 5, height = 5)

## -------------------------------------------------------------------------------------
df_boxplot <- price_volume_scalar4
df_boxplot_t <- df_boxplot[, .(ihme_loc_id, year_id, oop = oop_spi, 
                               oop_lower = oop_spi_lower, oop_upper = oop_spi_upper,
                               component = 'Total spending')]
df_boxplot_d <- df_boxplot[, .(ihme_loc_id, year_id, oop = oop_del_spi, 
                               oop_lower = oop_del_spi_lower, 
                               oop_upper = oop_del_spi_upper,
                               component = 'Delivery spending')]
df_boxplot_v <- df_boxplot[, .(ihme_loc_id, year_id, oop = oop_vac_spi, 
                               oop_lower = oop_vac_spi_lower, 
                               oop_upper = oop_vac_spi_upper,
                               component = 'Vaccine spending')]
df_boxplot1 <- rbind(df_boxplot_t, df_boxplot_d, df_boxplot_v)

## Read in list of Gavi countries
gavi <- fread("FILEPATH/imfin_country_list_updated.csv")
gavi <- unique(gavi[, .(ihme_loc_id, gavi_eligible)])

df_boxplot1 <- merge(df_boxplot1, gavi, by = 'ihme_loc_id', all.x = T)
df_boxplot1[gavi_eligible == 'Y', gavi := "Gavi"] 
df_boxplot1[gavi_eligible == 'N', gavi := 'non-Gavi'] 
df_boxplot2 <- copy(df_boxplot1)
df_boxplot2[, gavi := 'All\ncountries']

df_boxplot3 <- rbind(df_boxplot2, df_boxplot1)

df_boxplot3$component <- factor(df_boxplot3$component, c("Total spending", "Vaccine spending", "Delivery spending"))


vd <- ggplot(df_boxplot3[year_id == 2017,], aes(x = as.factor(gavi), y = round((oop), 2), fill = component)) +
  geom_boxplot(position = "dodge") +
  xlab("") + 
  ylab("OOP per surviving infant in 2019 USD (log scale)") +
  scale_y_continuous(trans = 'log10') +
  scale_fill_discrete(name = '') +
  theme_classic() +
  theme(legend.position = 'bottom')

print(vd)
ggsave(filename = paste0("FILEPATH/OOP_boxplot_", date1, "_", tag, ".pdf"),
       plot = vd, height = 6, width = 8)
## -------------------------------------------------------------------------------------

df_a <- price_volume_scalar_gbl[, .(year_id, oop_spi = oop_del_spi, 
                                    oop_spi_lower = oop_del_spi_lower,
                                    oop_spi_upper = oop_del_spi_upper, 
                                    component = 'Delivery spending')]
df_b <- price_volume_scalar_gbl[, .(year_id, oop_spi = oop_vac_spi, 
                                    oop_spi_lower = oop_vac_spi_lower,
                                    oop_spi_upper = oop_vac_spi_upper, 
                                    component = ' Vaccine spending')]
df_c <- price_volume_scalar_gbl[, .(year_id, oop_spi, oop_spi_lower, oop_spi_upper, 
                                    component = ' Total spending')]

df_d <- price_volume_scalar_gavi[, .(year_id, gavi_eligible, oop_spi = oop_del_spi, 
                                     oop_spi_lower = oop_del_spi_lower,
                                     oop_spi_upper = oop_del_spi_upper,
                                     component = 'Delivery spending')]
df_e <- price_volume_scalar_gavi[, .(year_id, gavi_eligible, oop_spi = oop_vac_spi, 
                                     oop_spi_lower = oop_vac_spi_lower,
                                     oop_spi_upper = oop_vac_spi_upper, 
                                     component = ' Vaccine spending')]
df_f <- price_volume_scalar_gavi[, .(year_id, gavi_eligible, oop_spi, oop_spi_lower, oop_spi_upper, 
                                     component = ' Total spending')]
df_all_gavi <- rbind(df_a, df_b, df_c, df_d, df_e, df_f, fill = T)

df_all_gavi[is.na(gavi_eligible), gavi_eligible := 'All countries']
df_all_gavi[gavi_eligible == 'Y', gavi_eligible := 'Gavi countries']
df_all_gavi[gavi_eligible == 'N', gavi_eligible := 'Non-Gavi countries']

df_all_gavi[component %in% c("Delivery spending", " Vaccine spending"), `:=`(spending_type = "V/D", width = 2)]
df_all_gavi[component %in% c(" Total spending"), `:=`(spending_type = "T", width = 4)]

## create text size variables
title_size <- 19
sub_tit <- 21
leg <- 20
ax <- 15
ax_tit <- 17

estimates <- df_all_gavi[year_id == 2017,]

bar <- ggplot(estimates, aes(x = spending_type, y = oop_spi, fill = component)) +
  geom_col(position = "dodge", width = 0.8) +
  geom_errorbar(aes(ymin = oop_spi_lower, ymax = oop_spi_upper), 
                width = 0.4, position = position_dodge(0.8), size = 0.7, alpha = 0.9) +
  #ggtitle("OOP spending per surviving infant in 2017") +
  xlab("") + 
  ylab("") +
  scale_fill_discrete(name = '', labels = c('Total immunization', 'Vaccine', 'Delivery')) +
  scale_y_continuous(limits = c(0, 10), expand = c(0,0)) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text( colour = "black"),
        axis.line = element_line(colour = "black"),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.position = 'bottom') +
  facet_wrap(~ gavi_eligible)
print(bar)

pdf(paste0(out_dir, "component_barchart_", date1, "_", tag, ".pdf"), height = 4.5, width = 6)
print(bar)
dev.off()

## -------------------------------------------------------------------------------------

 data <- merge(price_volume_scalar4, locs[, .(ihme_loc_id, location_name)],
               by = 'ihme_loc_id', all.x = T)
 fwrite(data[year_id == 2017, ], paste0("FILEPATH/2017_data_OOP_by_country_", date1, ".csv"))
 
## END OF SCRIPT ##