##########################################################################
### Author: USERNAME
### Date: 10/28/24
### Project: Health Spending Effectiveness 
### Purpose: Process individual result draws into a single dataframe
###           also compare inefficiency scores from draw space to the results in mean space
###########################################################################

# clear environment
rm(list=ls())

library(data.table)
setDTthreads(2)

# initialize version and outcome names
version <- "T" 
# outcome = "HALE"
outcome = "U5M"

# read file names from output directory 
outcome_dir = file.path('FILEPATH',
                        outcome, version,'result_draws')
outcome_files = list.files(outcome_dir) # list all file names

##### Inefficiency Frontier Results
# filter to just the frontier results
frontier_files = outcome_files[grep("frontier_ineffs", outcome_files)]
# frontier_files = frontier_files_1[grep("_FGH23_fix", frontier_files_1)]
length(frontier_files) # 250 draw files


# read in and append all together: 
frontier_draws <- rbindlist(lapply(file.path(outcome_dir, frontier_files), fread))
head(frontier_draws)

# Save out the result draws in a single file
results_out = file.path("FILEPATH",
                        outcome, version)
fwrite(frontier_draws, file.path(results_out, "pooled_frontier_draws_RTR.csv"))

##### Frontier Model Coefficients
# filter to just the model coefficients
beta_files = outcome_files[grep("model_betas", outcome_files)]
# beta_files = beta_files_1[grep("_FGH23_fix", beta_files_1)]
length(beta_files) # 250 draw files

# read in and append all together: 
beta_draws <- rbindlist(lapply(file.path(outcome_dir, beta_files), 
                                function(x) {
                                  fread(x)[, draw := gsub(".*(draw_\\d+).*", "\\1", x)]
                                })) # add a draw variable by extracting the draw ID from the file name
head(beta_draws)

# Save out the result draws in a single file
results_out = file.path("FILEPATH",
                        outcome, version)
fwrite(beta_draws, file.path(results_out, "beta_draws_RTR.csv"))

##### THEpc Grid Slope results
# filter to just the slope results
slope_files = outcome_files[grep("grid_slopes", outcome_files)]
# slope_files = slope_files_1[grep("_FGH23_fix", slope_files_1)]
length(slope_files) # 250 draw files

# read in and append all together: 
slope_draws <- rbindlist(lapply(file.path(outcome_dir, slope_files), 
                                function(x) {
                                  fread(x)[, draw := gsub(".*(draw_\\d+).*", "\\1", x)]
                                })) # add a draw variable by extracting the draw ID from the file name
head(slope_draws)

# Save out the result draws in a single file
results_out = file.path("FILEPATH",
                        outcome, version)
fwrite(slope_draws, file.path(results_out, "THE_grid_slopes_RTR.csv"))
