# Extract data from first sheet of 'cost extraction 2023_06_30.xlsx', prep
# for currency conversion and standardize some values for consistency
# Author: Elye Bliss
# Date: 07/12/2023

# Clear environment
rm(list = ls())

# Loading libraries
library(data.table)
library(readxl)

# Read raw data from latest excel file:
new_data <- read_excel("FILEPATH/cost extraction 2023_11_02.xlsx",
  sheet = "Extraction")
setDT(new_data)

# Note any new observations with latest version:
prior_new_data <- read_excel("FILEPATH/cost extraction 2023_06_30.xlsx",
                              sheet = "Extraction")
setDT(prior_new_data) 
nrow(new_data)-nrow(prior_new_data) # 22 new observations

# Make ID by concatenating citation, country and year
new_data[,ID := paste0(citation,location_name_id,year_start)]
prior_new_data[,ID := paste0(citation,location_name_id,year_start)]
new_obs <- new_data[!(ID %in% prior_new_data$ID)]
unique(new_obs$citation)
#[1] "Leicht 2011"  "Misplon 2004" "Zupanic 2022"

new_data[,ID := NULL]
rm(prior_new_data)

# Remove first row which contains explanations of variables, but save
# them as a reference in `col_explanation`
col_explanation <- data.frame(t(new_data[1, ]))
colnames(col_explanation) <- "explanation"
new_data <- new_data[-1, ]
# Add index column for downstream merging
new_data$index_col <- 1:dim(new_data)[1]

################################# Explore data #################################

# What are the 'year'-related variables?
colnames(new_data)[colnames(new_data) %like% "year"]
# Answer -> year_published, year_start, year_end, base_year

# What are the units of time extracted_value is measured in?
unique(new_data$extracted_value_time)
# Answer -> day, month, week, year

# What are the multiplication factors of the extracted values?
unique(new_data$mult_factor)
# Answer -> one, million

# Check for any non-numeric characters (e.g. ',') in `extracted_value`
regmatches(new_data$extracted_value, regexpr("[^0-9/.]", new_data$extracted_value))
# Answer -> none found

# What are the units of the cost estimate?
unique(new_data$unit)
# Answer -> Total spending, Treated cases

# Will NAs need to be addressed for the `base_year` variable?
new_data[is.na(base_year), .N]
# Answer -> Yes, 80 NAs

# Do all observations have at least one year-related non-NA value?
new_data[is.na(base_year) &
  is.na(year_start) &
  is.na(year_end) &
  is.na(year_published), .N]
# Answer -> Yes

################################# Clean variables ##############################

# Split `location_name_id` by '|' into country names and codes
new_data[, location_country_name := tstrsplit(location_name_id, "|", fixed = TRUE, keep = 1)]
# Keep `ihme_loc_id` variable name to be consistent with previous pipeline
new_data[, ihme_loc_id := tstrsplit(location_name_id, "|", fixed = TRUE, keep = 2)]


# Opt to replace `data` naming convention with IHME country naming conventions
current_names <- c("United States", "Iran", "South Korea", "Taiwan")
replace_names <- c(
  "United States of America", "Iran (Islamic Republic of)",
  "Republic of Korea", "Taiwan (Province of China)"
)

for (i in 1:length(current_names)) {
  new_data[
    location_country_name == current_names[i],
    location_country_name := replace_names[i]
  ]
}

# Split `currency` by ' | ' into country names and codes
new_data[, currency_c := tstrsplit(currency, "|", fixed = TRUE, keep = 2)]
new_data[, currency_c := trimws(currency_c)] # removed white spaces

# Convert numeric variables to numeric data types
numeric_cols <- c(
  "NID", "source_id", "year_published", "year_start",
  "year_end", "page_number", "sample_size", "start_age",
  "end_age", "annual_household_income", "extracted_value",
  "base_year", "number_hours_day"
) # edit as needed

new_data[, paste0(numeric_cols) := lapply(.SD, as.numeric), .SDcols = numeric_cols]
# warning thrown because of NAs, but this is expected for empty ch strings

# Base year variable
# When base year is blank replace with the value in the year_end variable
new_data[, year_mid_point := (year_start + year_end) / 2]

# Initialize with copy of base_year_variable
new_data$base_year_c <- new_data$base_year

new_data[
  (is.na(base_year) &
    !(is.na(year_end))),
  base_year_c := year_end
]

new_data[
  (is.na(base_year) &
    is.na(year_end) &
    !(is.na(year_mid_point))),
  base_year_c := year_mid_point
]

new_data[
  (is.na(base_year) &
    is.na(year_end) &
    is.na(year_mid_point) &
    !(is.na(year_start))),
  base_year_c := year_start
]

new_data[
  (is.na(base_year) &
    is.na(year_end) &
    is.na(year_mid_point) &
    is.na(year_start) &
    !(is.na(year_published))),
  base_year_c := year_published
]

# Make year_estimate same as in previous pipeline
new_data$year_estimate <- new_data$year_end # making a copy of year_end

new_data[
  (is.na(year_end) &
    !(is.na(data$year_mid_point))),
  year_estimate := year_mid_point
]

new_data[
  (is.na(year_end) &
    is.na(year_mid_point) &
    !(is.na(year_start))),
  year_estimate := year_start
]

new_data[
  (is.na(year_end) &
    is.na(year_mid_point) &
    is.na(year_start) &
    !(is.na(year_published))),
  year_estimate := year_published
] # covers remaining cases

# Turn values into full numbers based on mult_factor
million <- 1000000

new_data[, ext_val_c := extracted_value]
new_data[mult_factor == "million", ext_val_c := ext_val_c * million]

# Turn all non-annual values into annual
days <- 365
months <- 12
weeks <- 52

new_data[extracted_value_time == "day", ext_val_c := ext_val_c * days]
new_data[extracted_value_time == "month", ext_val_c := ext_val_c * months]
new_data[extracted_value_time == "week", ext_val_c := ext_val_c * weeks]

# Standardize capitalization
unique(new_data$cost_components)
new_data[, cost_components := tolower(cost_components)]


################################# Save output ##################################

# Write to Intermediate directory
fwrite(new_data, file = "FILEPATH/00_extracted_new_data.csv")

col_explanation$col_name <- row.names(col_explanation)
fwrite(col_explanation, file = "FILEPATH/col_explanation.csv")

# Check new observations after ST-GPR run
fwrite(new_obs, file = "FILEPATH/new_obs_11_02.csv")
