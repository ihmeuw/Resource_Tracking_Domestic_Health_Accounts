# ##############################################################################
# 
# This script reads betas and gammas and calculate the probablility of living in 
# a facility given that you have a diagnosis for each severity level
#
# The betas and the gammas that we have are for all dementia so I am going to read
# some ratios of beta(mild), beta(moderate), and beta(severe) that I previously 
# calculated with the little data we have. 
# I'm going to multiply the ratios by the draws and then 
# calculate the probability of living in a facility given that you have a dx 
# for each severity level
#
# ##############################################################################

# clearing environment
rm(list = ls())

library(data.table)
#library(caret) # This is for OOSV
library(corrplot)
library(ggplot2)
library(ggrepel) #



if (Sys.info()["sysname"] =="Linux") {
    root <-"FILEPATH"
} else {
    root <- "FILEPATH"
}


# loading gamma and calculating gamma by severity
cs <- fread(paste0(root, "FILEPATH"))
head(cs)
setnames(cs, old = 'value', new = 'prob_ins')
head(cs)

# This is how we calculate the ratios to apply to the gamma estimates from STGPR
cs_ratios <- fread(paste0(root, "FILEPATH"))
head(cs_ratios)

cs_mil <- cs_ratios[severity == "mild", mean(ratio)]
cs_mod <- cs_ratios[severity == "moderate", mean(ratio)]
cs_sev <- cs_ratios[severity == "severe", mean(ratio)]

head(cs)

# multiplying ratios by estimats to get care settings by severity
cs[, ins_mild := prob_ins * cs_mil]
cs[, ins_mode := prob_ins * cs_mod]
cs[, ins_seve := prob_ins * cs_sev]

## Saving gamma by severity although we don't really need it.
fwrite(cs,
          paste0(root, "FILEPATH"),
          row.names = F)



# loading beta and calculating beta by severity
dx <- fread(paste0(root, "FILEPATH"))
setnames(dx, old = 'value', new = 'prob_dx')

# This is how we calculate the ratios to apply to the beta estimates from STGPR
dx_ratios <- fread(paste0(root, "FILEPATH"))
head(dx_ratios)

dx_mil <- dx_ratios[severity == "mild", median(ratio)]
dx_mil
dx_mod <- dx_ratios[severity == "moderate", median(ratio)]
dx_mod
dx_sev <- dx_ratios[severity == "severe", median(ratio)]
dx_sev

# multiplying ratios by estimates to get care settings by severity
dx[, dx_mild := prob_dx * dx_mil]
dx[, dx_mode := prob_dx * dx_mod]
dx[, dx_seve := prob_dx * dx_sev]


# calculating no dx rates
dx[, no_dx_mild := 1 - dx_mild]
dx[, no_dx_mode := 1 - dx_mode]
dx[, no_dx_seve := 1 - dx_seve]

head(dx)

### 
# Saving dx by severity here so I can read it in the aggregation script
#
#####

fwrite(dx, paste0(root, "FILEPATH"))

