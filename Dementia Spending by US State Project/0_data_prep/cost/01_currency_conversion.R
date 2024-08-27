# Converting all currencies to 2019 $USD
# Author: Elye Bliss
# Date: 07/12/2023

# Clear memory of all objects
rm(list = ls())

# Loading libraries
library(data.table)

# reading data
data <- fread("FILEPATH/00_extracted_new_data.csv")


# CURRENCY
# creates currency_for_conversion variable. This variable is needed for the currency conversion function
# The only values that are allowed are 'USD', 'EUR', 'LCU', 'PPP'
data[ppp == "yes", currency_c := "PPP"] # 0 obs in new data
data[is.na(extracted_value), currency_c := NA] # 0 obs in new data
# Makes everything LCU if value is not USD, EUR, or PPP
data$prev_currency <- data$currency_c # Make temporary backup to spot check conversion
data$currency_c <- ifelse(!(data$currency_c %in% c("USD", "EUR", "PPP", NA)), "LCU", data$currency_c)

########################### Currency Conversion ################################

# Renaming `ppp` to `value_in_ppp` as in previous pipeline to avoid breaking
# currency conversion function
setnames(data, old = c("ppp"), new = c("value_in_ppp"))

# source most recent version of currency_conversion function to avoid NAs
source("FILEPATH/currency_conversion.R")

# Validate correct mapping from currency to USD, EUR, PPP or LCU
data[, .N, c("ihme_loc_id", "currency", "currency_c")]

# Only passing necessary columns as in previous pipeline to avoid breaking
# currency conversion function
dat_convert_in <- data[, .(index_col, ihme_loc_id, base_year_c, ext_val_c, currency_c)]
dat_convert_out <- currency_conversion(dat_convert_in,
  col.loc = "ihme_loc_id",
  col.value = "ext_val_c",
  col.currency.year = "base_year_c",
  col.currency = "currency_c",
  base.year = 2019,
  base.unit = "usd",
  converter.version = 6.2,
  simplify = F
)

# Check the dimensions of the new df
dim(dat_convert_out)

# dat_convert_out has more rows than dat_convert_in
setDT(dat_convert_out)
duplcated_indices <- dat_convert_out[duplicated(index_col)]$index_col
dat_convert_out[,duplicates := ifelse(index_col %in% duplcated_indices,T,F)]
dat_convert_out[duplicates==T] # Necessary to see duplicates together
# opting to discard duplicates.
dat_convert_out <- dat_convert_out[!duplicated(index_col)] # only removes 2nd obs

# merging new currency values with the rest of the data
data_full <- merge(data, dat_convert_out, by = c("index_col", "ihme_loc_id", "base_year_c", "ext_val_c", "currency_c"))

##############################  Check results ##################################

spot_check <- data_full[, .(ihme_loc_id, inflation, base_year_c, cost_components, 
                            unit, prev_currency, currency_c, extracted_value, 
                            ext_val_c, ext_val_c_new, deflator, lcu_usd, eur_usd)]


spot_check <- spot_check[unit == "Treated cases", ]
# Societal spending might be too large of a unit, with small rounding causing
# large discrepancies

spot_check_usd <- dat_convert_out[
  currency_c == "USD",
  manual_check := (ext_val_c / deflator) - ext_val_c_new
][currency_c == "USD"]
spot_check_usd[,manual_check_percent := round(abs(manual_check)/ext_val_c,0.01)]
# All round to 0% discrepancies

spot_check_lcu <- dat_convert_out[
  (currency_c != "EUR") & (currency_c != "USD"),
  manual_check := ((ext_val_c / lcu_usd) / deflator) - ext_val_c_new
][(currency_c != "EUR") & (currency_c != "USD"),] # Should be 0
spot_check_lcu[,manual_check_percent := round(abs(manual_check)/ext_val_c,0.01)]
# All round to 0% discrepancies

spot_check_eur <- dat_convert_out[
  currency_c == "EUR",
  manual_check := ((ext_val_c /deflator) / lcu_usd ) - ext_val_c_new
][currency_c == "EUR",] # Should be 0
spot_check_eur[,manual_check_percent := round(abs(manual_check)/ext_val_c,0.01)]
# All round to 0% discrepancies

# Find examples not likely to be rounding errors
spot_check <- spot_check[abs(manual_check) > 5, ]

########################### Save output ########################################

data$prev_currency <- NULL
# Write to Intermediate directory
fwrite(data_full, file = "FILEPATH/01_data_currency_converted.csv")
