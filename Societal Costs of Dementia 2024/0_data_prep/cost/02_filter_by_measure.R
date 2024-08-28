# Calculate costs in terms of per-case spending. Filter to studies that
# reported values already in terms of per-case spending.
# Author: Elye Bliss
# Date: 07/13/2023

# Clear environment
rm(list = ls())

# Loading libraries
library(data.table)
library(ggplot2)
library(dplyr)

options(scipen = 1000)

# Read file post-USD currency-conversion:
data <- fread("FILEPATH/01_data_currency_converted.csv")

# Read file containing original metadata subheaders explaining raw data column names:
col_explanation <- fread("FILEPATH/col_explanation.csv", header = T)

################################# Explore data #################################

# How many types of `estimation method`s are there?
unique(data$`estimation method`)
# Answer -> 3: 3a, 4a and 4C

# According to Methods_documentation.v1.docx, 3a is already in per-case units

# Here, we will filter to only include studies that already reported in per-case units.

############################## Filter data #####################################


# Limit data to estimation method '3a', which is already treatment per case
data <- data[`estimation method` == "3a", ] # drops 58 out of 273 obs

# Rename ext_val_c_new to be consistent with previous data
data$spending_clean <- data$ext_val_c_new

# Check box plots for a few cost components as a sanity check
ggplot(data[cost_components == "total", ],
  mapping = aes(x = year_estimate, y = spending_clean, group = year_estimate)
) +
  geom_boxplot()


# Limit variable selection to those selected in previous pipeline

prev_data <- fread("FILEPATH/10.adds_covariates.2021.csv")

for (c in colnames(prev_data)) {
  (
    if (!(c %in% colnames(data))) {
      print(c)
    })
}
# [1] "location_id"
# [1] "estimation_method_final"
# [1] "the_pc"
data <- data[, estimation_method_final := `estimation method`]
# Other variables not available

intersecting_cols <- colnames(prev_data)[colnames(prev_data) %in% colnames(data)]
# Keep severity from new data
data <- data %>%
  select(all_of(c(intersecting_cols,"severity")))


# "index_col"               "base_year_c"
# "ext_val_c"               "currency_c"
# "NID"                     "citation"
# "source_id"               "location_name_id"
# "super_region"            "year_published"
# "data_source"             "study_type"
# "year_start"              "year_end"
# "disease"                 "severity"
# "page_number"             "start_age"
# "end_age"                 "sex"
# "funding_source"          "annual_household_income"
# "extracted_value_time"    "extracted_value"
# "unit"                    "estimate"
# "mult_factor"             "currency"
# "inflation"               "base_year"
# "value_in_ppp"            "caregiver_hours"
# "number_hours_day"        "caregiver_type"
# "caregiver_age"           "caregiver_cost_type"
# "caregiver_job"           "notes"
# "initials"                "estimation method" (-> "estimation_method_final") 
# "location_country_name"   "year_mid_point"
# "prev_currency"           "base_year_c_new"
# "ext_val_c_new" (-> "spending_clean")          "currency_c_new"
# "eur_usd"                 "deflator"
# "lcu_usd"
########################### Reweight severity ##################################

by_severity <- data[severity != '']
no_severity <- data[severity == '']
nrow(by_severity)+nrow(no_severity)-nrow(data)
# [1] 0 sanity check

no_severity[,severity := NULL]

# Assume equal sample sizes for data where it was not reported (these get mostly
# removed later in different steps anyway)
by_severity[is.na(sample_size),sample_size:=1]

# To reweight:
# - keep names that don't change with aggregation sample_size in the `by`.
# - leave out variable to reweight across (in this case, severity)
by_severity_reweighted <- by_severity[,.(spending_clean = weighted.mean(spending_clean, 
                                                               sample_size),
                                         sample_size = sum(sample_size)),
                       by = .( ihme_loc_id,
                               year_estimate,
                               payer_type,
                               estimation_method_final,
                               cost_type,
                               care_type, 
                               cost_components)] 

# remove sample size (will be replaced later)
by_severity_reweighted[sample_size==3,sample_size:= NA_integer_]

data_reweighted <- rbind(no_severity,by_severity_reweighted)

########################### Save output ########################################

# Write to Intermediate directory
fwrite(data_reweighted, file = "FILEPATH/02_filtered_by_measure.csv")
