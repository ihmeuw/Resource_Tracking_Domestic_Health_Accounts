## -------------------------------------------------------------------------------------
## 4. Map and visualize results
## -------------------------------------------------------------------------------------
mapping.data1 <- copy(price_volume_scalar4)
setnames(mapping.data1, "oop", "mapvar")

#make dataframes
mapping.data1$mapvar <- as.numeric(mapping.data1$mapvar)
length(unique(mapping.data1$ihme_loc_id))
mapping.data1 <- merge(mapping.data1, locs_lm[, .(ihme_loc_id, location_id)], 
                       by = c('ihme_loc_id'), all.y = T)

mapping.data <- copy(mapping.data1[(!is.na(mapvar) & year_id == 2017), ])

out.dir <- FILEPATH
out.filename <- paste0("2017 OOP vaccine-specific spending on Immunizations ", date1, "_", tag, ".pdf")

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
## -------------------------------------------------------------------------------------

setnames(mapping.data, "mapvar", "oop")
setnames(mapping.data, "oop_spi", "mapvar")
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

out.filename <- paste0("2017 OOP spending on Immunizations per surviving infant ", date1, "_", tag, ".pdf")
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

setnames(df, "mapvar", "oop")

vac_list <- unique(df$vaccine)
vac_list
sr_locs <- unique(df$super_region_name)
sr_locs

pdf(paste0("FILEPATH/vaccine_OOP_per_infant_by_country_2000-2017_", date1, "_", tag, "2.pdf"))
for (v in vac_list) {
  df2 <- df[vaccine == v, ]
  for (i in c(1:6)) {
    a <- ggplot(df2[super_region_name == sr_locs[i],], 
                aes(x = year_id, y = oop_spi, color = super_region_name)) +
      geom_line() +
      geom_ribbon(aes(ymin = oop_spi_lower, ymax = oop_spi_upper, fill = super_region_name), alpha = 0.5, color = NA) +
      ggtitle(paste0(v, " - Spending per surviving infant")) +
      ylab("$USD") +
      theme_bw() +
      theme(legend.position = "top") +
      facet_wrap(~ihme_loc_id)
    print(a)
  }
}
dev.off()

pdf(paste0("FILEPATH/vaccine_OOP_by_country_2000-2017_", date1, "_", tag, ".pdf"))
for (v in vac_list) {
  df2 <- df[vaccine == v, ]
  for (i in c(1:6)) {
    a <- ggplot(df[super_region_name == sr_locs[i],], 
                aes(x = year_id, y = oop, color = super_region_name)) +
      geom_line() +
      geom_ribbon(aes(ymin = oop_lower, ymax = oop_upper, fill = super_region_name), alpha = 0.5, color = NA) +
      ggtitle(paste0(v, "Total spending")) +
      ylab("$USD") +
      theme_bw() +
      theme(legend.position = "top") +
      facet_wrap(~ihme_loc_id)
    print(a)
  }
}
dev.off()

## END OF SCRIPT ##
