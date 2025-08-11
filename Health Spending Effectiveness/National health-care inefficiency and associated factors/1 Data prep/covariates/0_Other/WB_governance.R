##########################################################################
### Author: USERNAME
### Date: 11/23/24
### Project: Health Spending Effectiveness 
### Purpose: Get World Bank governance variables in draw space
##########################################################################

WB_path <- "FILEPATH"


prep_wb_data <- function(cov_name, df){
  # split into two DFs - value and std. error
  cov_estimate <- df[`Series Name` == paste0(cov_name,": Estimate")]
  cov_std_error <- df[`Series Name` == paste0(cov_name,": Standard Error")]

  cov_estimate[, `Series Name` := NULL]
  cov_estimate[, `Series Code` := NULL]
  cov_std_error[, `Series Name` := NULL]
  cov_std_error[, `Series Code` := NULL]

  # convert columns named "1996 [YR1996]" to "1996"
  year_list <- colnames(cov_estimate)[3:length(colnames(cov_estimate))]
  year_list <- gsub(" \\[YR[0-9]{4}\\]", "", year_list)
  colnames(cov_estimate)[3:length(colnames(cov_estimate))] <- year_list
  colnames(cov_std_error)[3:length(colnames(cov_std_error))] <- year_list

  # melt cov_estimate to long based on year
  estimate_long <- melt(cov_estimate, 
                        id.vars = c("Country Name", "Country Code"),
                        variable.name = "year_id",
                        value.name = "val")

  # melt cov_std_error to long based on year
  std_error_long <- melt(cov_std_error, 
                           id.vars = c("Country Name", "Country Code"),
                           variable.name = "year_id",
                           value.name = "val_se")

  # merge estimate_long and std_error_long
  estimate_long <- merge(estimate_long, std_error_long, by = c("Country Name", "Country Code", "year_id"))
  setnames(estimate_long, c("Country Name","Country Code"), c("location_name","ihme_loc_id"))

  # adjust year from factor to numeric
  estimate_long[, year_id := as.character(year_id)]
  estimate_long[, year_id := as.numeric(year_id)]

  # convert estimate_long to draw space
  # convert val and val_se to numeric
  estimate_long[, val := as.numeric(val)]
  estimate_long[, val_se := as.numeric(val_se)]
  # remove NA values
  estimate_long <- estimate_long[!is.na(val) & !is.na(val_se)]
  tempDT <- data.table(t(apply(estimate_long[,c("val","val_se")], 1, FUN = function(x) rnorm(500, mean = x[1], sd = x[2]))))
  estimate_long <- cbind(estimate_long, tempDT)

  # remove unnecessary columns - draws are now columns named V1 through V500, which will be reshaped to long
  estimate_long <- estimate_long[, c("val","val_se") := NULL]

  # melt draws to long
  estimate_draws <- melt(estimate_long, 
                         id.vars = c("year_id", "location_name","ihme_loc_id"),
                         variable.name = "draw")
  
  # remove "V" from the draw column
  estimate_draws[, draw := as.numeric(gsub("V", "", draw))]
  #setnames(corruption_draws, "value", "WB_corruption")

  estimate_draws[, year_id := as.numeric(year_id)]
  return(estimate_draws)
}

##############################################
### governance indicators data from World Bank (Worldwide Governance Indicators)
##############################################
# read in data
data_path = 'FILEPATH'
wb_df = fread(paste0(data_path, "WB_governance_indicators.csv"))

# run prep_wb_data for "Government Effectivness"
effectiveness <- prep_wb_data("Government Effectiveness", wb_df)
setnames(effectiveness, "value", "effectiveness")
stability <- prep_wb_data("Political Stability and Absence of Violence/Terrorism", wb_df)
setnames(stability,"value","stability")
reg_quality <- prep_wb_data("Regulatory Quality",wb_df)
setnames(reg_quality,"value","reg_quality")
rule_of_law <- prep_wb_data("Rule of Law",wb_df)
setnames(rule_of_law,"value","rule_of_law")
voice <- prep_wb_data("Voice and Accountability",wb_df)
setnames(voice,"value","voice")

# write out new covs
fwrite(effectiveness, paste0(WB_path, "gov_effectiveness.csv"))
fwrite(stability,paste0(WB_path,"gov_stability.csv"))
fwrite(reg_quality,paste0(WB_path,"gov_regulation_quality.csv"))
fwrite(rule_of_law,paste0(WB_path,"gov_rule_of_law.csv"))
fwrite(voice,paste0(WB_path,"gov_voice.csv"))

