########################################################################################
## Clean up delivery cost data and prepare for modeling
## Author: Emilie Maddison
## Date: 20 May 2020
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

date1 <- format(Sys.time(), "%Y%m%d")

## Load required packages
require(data.table)
library(readxl)
library(ggplot2)
library(lme4)
library(merTools)

## Set directories
code.dir <- paste0(FILEPATH)
in.dir <- paste0(FILEPATH)
out.dir <- paste0(FILEPATH)
out.filename <- "Total delivery spending on Immunizations.pdf"

## Source mapping function, which will be called with "gbd_map"
source(paste0(FILEPATH, "GBD_WITH_INSETS_MAPPING_FUNCTION.R"))
source(paste0(FILEPATH, "get_location_metadata.R"))  
source(paste0(FILEPATH, "helper_functions.R"))
source(paste0(FILEPATH, "get_covariate_estimates.R"))
source(paste0(FILEPATH, "get_location_metadata.R"))
source(paste0(FILEPATH, "currency_conversion.R"))

## -------------------------------------------------------------------------------------
## 2. Read in raw data, clean and reformat
## -------------------------------------------------------------------------------------

sr_locs <- get_location_metadata(gbd_round_id = 6,
                                 location_set_id = 1)

sr_locs <- sr_locs[location_type == 'superregion' & location_name != "High-income", 
                   .(location_name, location_id)]

## -------------------------------------------------------------------------------------

## delivery data from the IDCC
delivery <- fread(paste0(FILEPATH, "total_delivery_costs.csv"))

names(delivery)

## Select rows of interest
delivery2 <- delivery[(Vaccine_of_interest == 1 & `Single vaccine` == 1 & 
                         delivery_only == 1 & financial_flag_ermadd == 1),]

delivery2 <- get_region(delivery2)
delivery2 <- delivery2[, c("ihme_loc_id", "Country", "super_region_name" ,
               "Vaccines costed", 
               "Reported base year \n(or year of data collection)",
               "Cost per capita without vaccine (2016 USD)",
               "Cost per dose without vaccine (2016 USD)",
               "Cost per person in the target population without vaccine (2016 USD)",
               "Cost per FIC without vaccine (2016 USD)")]

## Rename columns
setnames(delivery2, 
         old = c("Vaccines costed", 
               "Reported base year \n(or year of data collection)"),
         new = c('vaccine', 'year_id'))

## Melt so that "cost" is in a single column
delivery3 <- melt(delivery2,
     id.vars = c('ihme_loc_id', 'Country', 'super_region_name', 'vaccine', 'year_id'),
     measure.vars = c("Cost per capita without vaccine (2016 USD)",
                      "Cost per dose without vaccine (2016 USD)",
                      "Cost per person in the target population without vaccine (2016 USD)",
                      "Cost per FIC without vaccine (2016 USD)"),
     variable.name = 'cost_type',
     value.name = 'cost')

## Drop NA values
delivery3 <- delivery3[!is.na(cost), ]

boxplot(as.numeric(cost) ~ cost_type, data = delivery3)

## -------------------------------------------------------------------------------------
## Generate scalar for adjusting data
names(delivery2)

## set column names to something more manageable
setnames(delivery2, 
         old = c("Cost per capita without vaccine (2016 USD)",                         
                 "Cost per dose without vaccine (2016 USD)",                           
                 "Cost per person in the target population without vaccine (2016 USD)",
                 "Cost per FIC without vaccine (2016 USD)"),  
         new = c("c_percap",
                 "c_perdos",
                 "c_pertarget",
                 "c_perfic"))

## Calculate difference between different cost types using existing data
delivery2[, cap_adj := c_percap / c_perdos]
delivery2[, target_adj := c_pertarget / c_perdos]
delivery2[, fic_adj := c_perfic / c_perdos]

## Calculate average - decided not to use per_capita values.
mean(delivery2$cap_adj, na.rm = T)
mean(delivery2$target_adj, na.rm = T)
mean(delivery2$fic_adj, na.rm = T)

delivery2tar <- delivery2[is.na(c_perdos) & !is.na(c_pertarget), ]
delivery2fic <- delivery2[is.na(c_perdos) & is.na(c_pertarget) & !is.na(c_perfic),]
delivery2b <- delivery2[!is.na(c_perdos),]

## Use that average to adjust datapoints were per_dose does not exist
delivery2tar[!is.na(c_pertarget), 
             c_perdos := c_pertarget / mean(delivery2$target_adj, na.rm = T)]
delivery2fic[!is.na(c_perfic), 
             c_perdos := c_perfic / mean(delivery2$fic_adj, na.rm = T)]

delivery2c <- rbind(delivery2b, delivery2tar, delivery2fic)

unique(delivery2c$vaccine)

delivery2c[vaccine %like% "Rota", vaccine := "RVV"]
delivery2c[vaccine %like% "PCV", vaccine := "PCV"]
delivery2c[vaccine %like% "Menin", vaccine := "MenA"]
delivery2c[vaccine %like% "Measles", vaccine := "MR"]
delivery2c[vaccine %like% "DTwP", vaccine := "Penta"]

## drop uganda outliers
delivery_adj <- delivery2c[c_perdos < 14 & c_perdos > 0, 
                           .(ihme_loc_id, Country, super_region_name, vaccine, year_id, 
                             c_perdos)]
setnames(delivery_adj, "c_perdos", "cost")

# boxplot(as.numeric(cost) ~ cost_type, data = delivery3)
rm(delivery2, delivery2b, delivery2c, delivery2fic, delivery2tar, delivery3)

##--------------------------------------------------------------------------------------
## Currency conversion to 2019 USD
delivery_adj <- currency_conversion(delivery_adj,
                                col.loc = 'ihme_loc_id',
                                col.value = c('cost'),
                                currency = 'USD',
                                currency.year = 2016,
                                base.year = 2019,
                                base.unit = 'USD')

##------------------------------------------------------------------------------
## Scatterplot to observe adjusted delivery costs by vaccine type 

plot <- 
  ggplot(data = delivery_adj,
         aes(x = as.integer(year_id), y = as.integer(cost), color = super_region_name),
         size = 4, alpha = 0.5) +
  geom_jitter(width = 0.1, height = 0.1) +
  ggtitle("Adjusted raw data for delivery costs by vaccine") +
  xlab("") +
  ylab("Cost\nper dose\n(2016\nUSD)") +
  guides(color = guide_legend(nrow = 3)) +
  #scale_x_discrete(limits = c(2007, 2017)) +
  theme_bw() + 
  theme(
    legend.position = 'bottom',
    axis.text = element_text(angle = 0),
    axis.title.y = element_text(angle = 0),
    panel.grid.minor = element_blank()) +
  facet_wrap(~ vaccine)

print(plot)
# ggsave(plot = plot, 
#        filename = paste0(out.dir, "adjusted_cost_vaccine_delivery_cost_figure.pdf"),
#        height = 7,
#        width = 7)

##------------------------------------------------------------------------------
# ## Linear regression
# 
# names(delivery_adj)
# unique(delivery_adj$super_region_name)
# unique(delivery_adj$vaccine)
# 
# 
# ## Counts of datapoints by region and vaccine
# table(delivery_adj$super_region_name, delivery_adj$vaccine)
# 
# ## Year, region, vaccine
# summary(lm(cost ~ year_id + super_region_name + vaccine,
#            data = delivery_adj))
# 
# summary(lm(log(cost) ~ year_id + super_region_name + vaccine,
#      data = delivery_adj))
# 
# ## Year and vaccine
# summary(lm(cost ~ year_id + vaccine,
#           data = delivery_adj))
# summary(lm(log(cost) ~ year_id + vaccine,
#           data = delivery_adj))
# 
# ## Region and vaccine
# summary(lm(cost ~ super_region_name + vaccine,
#           data = delivery_adj))
# summary(lm(log(cost) ~ super_region_name + vaccine,
#           data = delivery_adj))
# 
# ## Region
# summary(lm((cost) ~ super_region_name,
#           data = delivery_adj))
# ## vaccine
# summary(lm((cost) ~ vaccine,
#           data = delivery_adj))

# # ## Year regression
# summary(lm(cost ~ year_id,
#            data = delivery_adj))
# 
# ## Line graph of time trend
# ggplot(data = delivery_adj, aes(y = as.numeric(cost), x = year_id)) +
#   geom_point() +
#   geom_smooth(method = "lm", aes(y = as.numeric(cost), x = year_id), color = 'black', se = FALSE) +
#   theme_bw()
# 
# 
# ## Boxplot of all-vaccine regional data
# boxplot(as.numeric(cost) ~ super_region_name, data = delivery_adj)
# 
# ## Cost type regression
# # summary(lm(cost ~
# #              c_percap + c_pertarget + c_perfic,
# #            data = delivery3))
# # 
# # boxplot(as.numeric(cost) ~ cost_type2, data = delivery_dummy)
# 
# ## Boxplot of all-region vaccine data
# boxplot(as.numeric(cost) ~ vaccine, data = delivery_adj)
# 
# delivery_adj2 <- merge(delivery_adj, ldi_pc[, .(location_name, year_id, mean_value)], 
#                        by.x = c('super_region_name', 'year_id'),
#                        by.y = c('location_name', 'year_id'),
#                        all = T)
# 
# all <- summary(lm(cost ~ super_region_name + vaccine, data = delivery_adj))
# all
# 
# ## 'Best' regresssion
# all2 <- lm(log(cost) ~ super_region_name + vaccine, data = delivery_adj)
# summary(all2)
# # plot(all2)
# 
# new <- CJ(
#   year_id = c(2000:2017),
#   super_region_name = unique(delivery_adj$super_region_name),
#   vaccine = c(unique(delivery_adj$vaccine)))
# 
# new2 <- exp(predict(all2, new, interval = 'confidence'))
# # new2 <- (predict(all2, new, interval = 'confidence'))
# new3 <- cbind(new, new2)
# 
# head(new3)
# 
# ## Fill missing data with averages
# reg_fill_data <- new3[, .(fit = mean(fit)), .(super_region_name, year_id)]
# reg_fill <- CJ(
#   year_id = c(2000:2017),
#   super_region_name = unique(delivery_adj$super_region_name),
#   vaccine = c('IPV', 'YF'))
# reg_fill <- merge(reg_fill, reg_fill_data, by = c('super_region_name', 'year_id'), all.x = T)
# 
# name_fill_data <- new3[, .(fit = mean(fit)), .(vaccine, year_id)]
# name_fill <- CJ(
#   year_id = c(2000:2017),
#   super_region_name = c('North Africa and Middle East'),
#   vaccine =  c(unique(delivery_adj$vaccine)))
# name_fill <- merge(name_fill, name_fill_data, by = c('vaccine', 'year_id'), all.x = T)
# 
# name2_fill <- CJ(
#   year_id = c(2000:2017),
#   super_region_name = c('North Africa and Middle East'),
#   vaccine =  c('IPV', 'YF'))
# reg2_fill <- reg_fill[, .(fit = mean(fit)), .(vaccine, year_id)]
# name2_fill <- merge(name2_fill, reg2_fill, by = c('vaccine', 'year_id'), all.x = T)
# 
# delivery_data <- rbind(new3[, .(super_region_name, vaccine, year_id, fit)], reg_fill, name_fill, name2_fill)
# setnames(delivery_data, 'fit', 'delivery_cost')
# 
# ## Check shape for missing rows
# nrow(delivery_data) == (2017 - 1999) * 6 * 9
# 
# # fwrite(delivery_data, paste0(FILEPATH, 'modeled_delivery_costs_', date1, '.csv'))

##--------------------------------------------------------------------------------------
## Linear regression with predicted draws
b <- lmer(log(cost) ~ vaccine + (1|super_region_name), delivery_adj)

summary(b)
new_pred <- CJ(
  year_id = c(2000:2017),
  super_region_name = sr_locs$location_name,
  vaccine = unique(delivery_adj$vaccine))

## The full model has narrower uncertainty intervals than fixed effects on its own.
test_results <- predictInterval(b, new_pred, 
                                which = "full", 
                                level = 0.95, n.sims = 1000, stat = "mean",
                                include.resid.var = FALSE, returnSims = TRUE, seed = NULL,
                                .parallel = FALSE, .paropts = NULL, 
                                fix.intercept.variance = FALSE,
                                ignore.fixed.terms = NULL)

new_pred2 <- cbind(new_pred, attr(test_results, 'sim.results'))
cols <- paste0('V', (1:1000))
new_pred2[ , (cols) := lapply(.SD, exp), .SDcols = cols]

new_pred3 <- melt(new_pred2, id.vars = c('year_id', 
                                         'super_region_name',
                                         'vaccine'),
                  variable.name = 'draw',
                  value.name = 'cost')
new_pred3$draw <- substring(new_pred3$draw, 2)

## -------------------------------------------------------------------------------------
## Fill averages for IPV and YF
## Fill missing data with averages
reg_fill_data <- new_pred3[, .(cost = mean(cost)), .(super_region_name, year_id, draw)]
reg_fill <- CJ(
  draw = as.character(c(1:1000)),
  year_id = c(2000:2017),
  super_region_name = sr_locs$location_name,
  vaccine = c('IPV', 'YF'))
reg_fill <- merge(reg_fill, reg_fill_data, 
                  by = c('super_region_name', 'year_id', 'draw'), all.x = T)
## Check length
nrow(reg_fill) == 1000 * 18 * 6 * 2

delivery_draws <- rbind(new_pred3[, .(super_region_name, vaccine, year_id, draw, cost)],
                        reg_fill)
setnames(delivery_draws, 'cost', 'delivery_cost')

## Check shape for missing rows
nrow(delivery_draws) == (2017 - 1999) * 6 * 9 * 1000
delivery_draws[, draw := as.integer(draw) - 1]
stop(max(delivery_draws$draw == 999))

# fwrite(delivery_draws, paste0(FILEPATH, "modeled_delivery_costs_draws_", date1, ".csv"))

## -------------------------------------------------------------------------------------
## Examine results 
mean_results <- delivery_draws[, .(mean = mean(delivery_cost),
                                   lower = quantile(delivery_cost, 0.025),
                                   upper = quantile(delivery_cost, 0.975)), 
                                  .(super_region_name, vaccine, year_id)]

## Summary table 
mean_results2017 <- mean_results[year_id == 2017, ]
mean_results2017 <- mean_results2017[, mean := round(mean, 2)]
mean_results2017 <- dcast(mean_results2017, super_region_name ~ vaccine,
                          value.var = 'mean')
fwrite(mean_results2017, paste0(FILEPATH, "2017_regional_delivery_cost_table.csv"))

ggplot(data = mean_results) +
  geom_line(aes(x = year_id, y = mean, color = super_region_name)) +
  geom_ribbon(aes(x = year_id, ymin = lower, ymax = upper, fill = super_region_name), 
              alpha = 0.1) +
  geom_point(data = delivery_adj, aes(x = year_id, y = cost, color = super_region_name)) +
  theme_classic() + 
  facet_wrap(~vaccine)


table(delivery_adj$super_region_name, delivery_adj$vaccine)

## compare to basic linear model
lm_results <- fread(paste0(FILEPATH, "modeled_delivery_costs_20200609.csv"))

ggplot(data = mean_results) +
  geom_line(aes(x = year_id, y = mean, color = super_region_name)) +
  geom_line(data = lm_results, aes(x = year_id, y = delivery_cost, color = super_region_name),
            linetype = 'dashed') +
  # geom_ribbon(aes(x = year_id, ymin = lower, ymax = upper, fill = super_region_name), 
  #             color = NA, alpha = 0.1) +
  geom_point(data = delivery_adj, aes(x = year_id, y = cost, color = super_region_name)) +
  theme_classic() + 
  facet_wrap(~vaccine)

