# Clean working directory
rm(list = ls())
library(data.table)
# Comparing the two dfs to see what are the differences

b_ins <- fread("FILEPATH")

ratios <- fread("FILEPATH")

data <- merge(b_ins, ratios)


data[, bi_mild := value * low_r]

data[, bi_mode := value * mod_r]

data[, bi_seve := value * sev_r]

data_bi <- data[year_id > 1999, 
                c('location_id', 'year_id', 'variable',
                  'bi_mild', 'bi_mode', 'bi_seve')]

fwrite(data_bi, "FILEPATH", row.names = F )