# Cleaning environment
rm(list = ls())

if (Sys.info()["sysname"] =="Linux") {
    root <-"ADDRESS"
} else {
    root <- "ADDRESS"
}


unit_cost <-  fread(paste0(root, 
                           "FILEPATH"))

preva <- fread(paste0(root, 
                      "FILEPATH"))

# I have to separate community from facility so I don't duplicate the row when 
# I merge the two dfs in subsequent steps

# cleaning unit cost
uc <- unit_cost[year_id == 2019, c('location_id', 'year_id', 'draw', 'severity', 'model','unit_cost_by_sev')]
uc_w <- dcast(uc, location_id + year_id + draw + severity ~ model, value.var = 'unit_cost_by_sev')


# merge data frames
data <- merge(preva, uc_w, by = c('location_id', 'year_id', 'draw', 'severity'))
head(data)

# multiply cost by volumn
data[, dx_com_cost := dx_com * com_tot]
data[, dx_ins_cost := dx_ins * fac_tot]
data[, no_dx_ins_cost := no_dx_ins * fac_tot]

# removing unnecessary columns
data_mini <- data[, c('location_id', 'draw', 'severity', 'age_group_id', 'sex_id', 'dx_com_cost', 'dx_ins_cost', 'no_dx_ins_cost')]

data_mini[, fac_spending := dx_ins_cost + no_dx_ins_cost]
data_mini[, fac_spending.1 := sum(fac_spending), by = c('location_id', 'draw')]
data_mini[, com_spending.1 := sum(dx_com_cost), by = c('location_id', 'draw')]

dm <- unique(data_mini[, c('location_id', 'draw', 'com_spending.1', 'fac_spending.1')])
head(dm)


dm[, com_spending_mean := mean(com_spending.1), by = 'location_id']
dm[, fac_spending_mean := mean(fac_spending.1), by = 'location_id']


data1 <- unique(dm[, c('location_id', 'com_spending_mean', 'fac_spending_mean')])
head(data1)

write.csv(data1,  paste0(root, "FILEPATH"), 
          row.names = F)