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

code.dir <- FILEPATH
out.dir <- FILEPATH

library(data.table)
source(paste0(FILEPATH, "helper_functions.R"))
source(paste0(FILEPATH, "get_population.R"))
## source mapping function
source(paste0(FILEPATH, "GBD_WITH_INSETS_MAPPING_FUNCTION.R"))


locs <- get_ig(locs)
locs_lm <- locs[income_group != 'H',]

volume1 <- fread(paste0(out.dir, "volume/135_total_volume_vaccines.csv"))
#setnames(volume1, "Index", "ihme_loc_id")
volume <- volume1

price1 <- fread(paste0(out.dir, "price/filled_price_9_vaccines.csv"))
# price <- merge(price1, locs[level == 3, .(location_name, ihme_loc_id)], 
#                by.x = 'Country', by.y = 'location_name', all.x = T)
# price <- merge(price1, locs[, .(location_name, ihme_loc_id)],
#                by.x = 'Country', by.y = 'location_name', all.x = T)
price <- copy(price1)


scalar1 <- fread(paste0(out.dir, "oop_scalar/oop_scalar_stgpr_20200517.csv"))
scalar <- merge(scalar1, locs[, .(location_id, ihme_loc_id)],
                by = 'location_id', all.x = T)
scalar <- scalar[, .(ihme_loc_id, location_id, year_id, gpr_mean, gpr_lower, gpr_upper, data, variance)]

# a <- unique(price[is.na(ihme_loc_id), .(Country)])
# fix_locs <- data.table(x = a, ihme_loc_id = c('BIH', 'BRN', 'CAF', 'CIV', 'CZE',
#                                   'COD', 'DOM', 'IRN', 'LAO', 'MKD',
#                                   'MDA', 'PRK', 'RUS', 'STP', 'KOR', 
#                                   'SWZ', 'SYR', 'TZA', 'TTO', 'USA'))
# 
# price <- merge(price, fix_locs, by.x='Country', by.y ='x.Country', all.x=T)
# price[is.na(ihme_loc_id.x), ihme_loc_id.x := ihme_loc_id.y]
# setnames(price, 'ihme_loc_id.x', 'ihme_loc_id')
# setnames(price, 'Year', 'year_id')

price[Vaccine == 'Rota', Vaccine := "RVV"]
volume[Vaccine %in% c('Measles 1st', 'Measles 2nd', 'Measles', 'Rubella'), Vaccine := 'MR']
unique(price$Vaccine)
unique(volume$Vaccine)

price_volume <- merge(price, volume, by = c('ihme_loc_id', 'year_id', 'Vaccine'),
                      all = T)
price_volume[, spending := price * volume]

price_volume_scalar <- merge(price_volume, scalar, 
                             by = c('ihme_loc_id', 'location_id', 'year_id'), all.x = T)
# price_volume_scalar[, oop_spending := spending * pct_private_imm]
price_volume_scalar[, oop_spending := spending * gpr_mean]

mapping.data <- price_volume_scalar
setnames(mapping.data, "oop_spending", "mapvar")

#make dataframes
mapping.data$mapvar <- as.numeric(mapping.data$mapvar)
mapping.data <- mapping.data[!is.na(mapvar), .(mapvar = sum(mapvar)), .(ihme_loc_id, location_id, year_id)]
#mapping.data <- mapping.data[year_id == 2016,]
length(unique(mapping.data$ihme_loc_id))
mapping.data <- merge(mapping.data, locs_lm[, .(ihme_loc_id, location_id)], 
                      by = c('ihme_loc_id', 'location_id'), all.y = T)
mapping.data[is.na(mapvar), mapvar := 0]
#mapping.data <- mapping.data[ihme_loc_id != 'UGA',]

out.dir <- FILEPATH
out.filename <- "oop_scalar/2016 OOP spending on Immunizations 20200517.pdf"

max(mapping.data$mapvar/1e6)

#map and output using the mapping function
gbd_map(data = mapping.data,
        # limits = c(0, 10E6, 20E6, 30E6, 40E6, 50E6, Inf),
        # labels = c("$0 to 10 milion", "$10 to 20 million", "$20 to 30 million", "$30 to 40 million", "$40 to 50 million", "$50 million plus"),
        limits = c(0, 0.001, 5E6, 10E6, 15E6, 20E6, 30E6, Inf),
        labels = c("No volume data", "$0 to 5 milion", "$5 to 10 million", "$10 to 15 million", "$15 to 20 million", "$20 to 30 million", "$30 million plus"),
        col = "RdYlBu", col.reverse = TRUE,
        na.color = "#999999",
        title = "2016 OOP spending",
        legend.title = "Dollars",
        legend.cex = 1,
        legend.shift = c(-5, -10),
        fname = paste0(out.dir,out.filename))

pops <- get_population(location_id = locs_lm$location_id, 
                       year_id = 2016, 
                       age_group_id = c(1, 6, 7), 
                       gbd_round_id = 6, 
                       decomp_step = 'step4')

pops <- pops[, .(population = sum(population)), .(location_id)]

mapping.data <- merge(mapping.data, pops[, .(location_id, population)],
                      by = 'location_id', all.x = T)

mapping.data[, mapvar := mapvar / population]

max(mapping.data$mapvar)

out.filename <- "oop_scalar/2016 OOP spending on Immunizations per cap 20200517.pdf"
#map and output using the mapping function
gbd_map(data = mapping.data,
        limits = c(0, 0.001, 0.5, 1, 2, 4, 8, Inf),
        labels = c("No volume data", "$0.001 to $0.50", "$0.50 to 1", "$1 to 2", 
                   "$2 to 4", "$4 to 8", ">$8"),
        col = "RdYlBu", 
        col.reverse = TRUE,
        na.color = "#999999",
        title = "2016 OOP spending per child under 15",
        legend.title = "Dollars",
        legend.cex = 1,
        legend.shift = c(-5, -10),
        fname = paste0(out.dir,out.filename))

library(ggplot2)

df <- get_region(mapping.data)
sr_locs <- unique(df$super_region_name)
pdf(paste0("FILEPATH/OOP_by_country_2000-2017_20200517.pdf"))
for (i in c(1:7)) {
    a <- ggplot(df[super_region_name == sr_locs[i],], aes(x = year_id, y = mapvar, color = super_region_name)) +
        geom_line() +
        ggtitle("Spending per capita under 15") +
        ylab("$USD") +
        theme_bw() +
        theme(legend.position = "top") +
        facet_wrap(~ihme_loc_id)
        print(a)
}
dev.off()


