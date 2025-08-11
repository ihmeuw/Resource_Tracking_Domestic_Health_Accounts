##########################################################################
### Author: USERNAME
### Date: 12/06/24
### Project: Health Spending Effectiveness 
### Purpose: Extracting under 5 mortality rate (U5M) from GBD
##########################################################################

# clear environment
rm(list=ls())


# load libraries
library(data.table)
library(assertthat)
# source shared functions
source(paste0("FILEPATH", "get_envelope.R"))
source(paste0("FILEPATH", "get_location_metadata.R"))
source(paste0("FILEPATH", "get_population.R"))

# get location metadata
locs = get_location_metadata(release_id = 16, location_set_id = 35)
# get country IDs 
country_ids <- locs[level == 3]$location_id # level 3 -> country

# set up function arguments
years = c(1995:2023)
under5 = 1 # age group
release = 16 # GBD 2023
sex = 3 # all sex

U5M = get_envelope(release_id = release,
                   year_id = years, # 1995 - 2023
                   location_id = country_ids,
                   sex_id = 3,
                   age_group_id = under5,
                   with_hiv = 1,
                   rates = 1)
head(U5M)

# get population for under 5 (to convert from death rate to death count)
popU5 <- get_population(release_id = 16, # GBD 2023
                        location_id = unique(U5M$location_id),
                        year_id = c(1995:2023), 
                        sex_id = 3, # 3 -> all sexes
                        age_group_id = 1) # 1 -> Under 5 

# merge in population data
U5M = merge(U5M, popU5[,.(year_id, location_id, popU5 = population)], 
            by = c("year_id", "location_id"))

# calculate death count and sum to global estimates:
U5M_global = U5M[,.(deaths = sum(popU5*mean), 
                    deaths_low = sum(popU5*lower), 
                    deaths_high = sum(popU5*upper)), 
                 by = c("year_id")]

# see 2021 estimate
U5M_global[year_id == 2021]


# check if all rows have real confidence intervals: 
nrow(U5M) - n_intervals

## create draws from confidence interval
U5M[, standard_error := (upper - lower) / 3.92]
# # replace with the mean SE
# mean_SE = mean(U5M[standard_error != 0]$standard_error)
# U5M[standard_error == 0, standard_error := mean_SE]

# remove rows without confidence intervals
# print which country-years are removed: 
paste("year:", U5M[standard_error == 0]$year_id, 
      "location_id:", U5M[standard_error == 0]$location_id)
U5M = U5M[standard_error != 0]


set.seed(123)
drawDT <- data.table(t(apply(U5M[,.(mean, standard_error)], 1,
                             FUN = function(x) rnorm(250, mean = x[1], sd = x[2]))))
U5M = cbind(U5M, drawDT) # add draws to original data

# remove unnecessary columns: 
U5M[, c("mean", "lower", "upper", "run_id") := NULL]

# pivot draws to long format
U5M_draws = melt(U5M,
                 id.vars = c("year_id", "location_id", "age_group_id",
                               "sex_id", "popU5", "standard_error"), 
                 variable.name = "draw", value.name = "U5M")

# remove "V" from the draw column
U5M_draws[, draw := as.numeric(gsub("V", "", draw))]
U5M_draws[, draw := paste0("draw_", draw-1)]

# see how many draws are outside the possible value range (0 - 1)
nrow(U5M_draws[U5M < 0 | U5M > 1]) / nrow(U5M_draws)

range(U5M_draws$U5M) # 9.711115e-05 - 0.0727870431

# calculate U5 Deaths
U5M_draws[, deaths := U5M*popU5]

# save U5M draws to file
output_dir = "FILEPATH"
fwrite(U5M_draws, paste0(output_dir, "U5M_envelope_draws_1995_2023.csv"))
