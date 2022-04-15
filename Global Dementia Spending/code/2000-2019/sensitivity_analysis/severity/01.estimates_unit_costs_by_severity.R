rm(list = ls())

library(data.table)

# loading ratios

if (Sys.info()["sysname"] =="Linux") {
    root <-"ADDRESS"
} else {
    root <- "ADDRESS"
}

ct_r <- fread(paste0(root, "FILEPATH"))
ft_r <- fread(paste0(root, "FILEPATH"))

ratio <- rbind(ct_r, ft_r)

head(ratio) # check that this is what you think it's.

ratios <- ratio[, 
                c('location_id', 'year_id', 'model', 'low_r', 'mod_r', 'sev_r')]

# Melting
id.vars <- c('location_id', 'year_id', 'model')

ratios_l <- melt(ratios, 
              id.vars = id.vars)

setnames(ratios_l, old = 'value', new = 'unit_cost_ratio')

ratios_l[variable == "low_r", severity := 'mild']
ratios_l[variable == "mod_r", severity := 'moderate']
ratios_l[variable == "sev_r", severity := 'severe']


# loading unit cost estimates
ct_uc <- fread(paste0(root, "FILEPATH"))
ct_uc[, model := 'com_tot']

ft_uc <- fread(paste0(root, "FILEPATH"))
ft_uc[, model := 'fac_tot']

unit_costs <- rbind(ct_uc, ft_uc)
head(unit_costs) # Does this look right?

setnames(unit_costs, old = 'value', new = 'unit_costs')
setnames(unit_costs, old = 'variable', new = 'draw')

data <- merge(unit_costs, ratios_l, by = c('location_id', 'year_id', 'model'))
head(data) 

data[, unit_cost_by_sev := unit_costs * unit_cost_ratio]

head(data) # Visual check before you save results

fwrite(data, paste0(root, "FILEPATH"), 
          row.names = F)

