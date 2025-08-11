###########################################
# Author: USERNAME
# Date:    2025-04-04
#' Taking a formatted dataset of regression coefficient values and labels and creating a forest plot with distinct categories
#' 
#' @param df [data.frame/data.table] A dataframe or datatable. MUST SPECIFY
#' @param col.loc [str] Column name that identify iso3 code, usually `ihme_loc_id`. MUST SPECIFY
#' @param col.value [str, vector] Column names that needs to be converted, take single or multiple. MUST SPECIFY
#' @param col.currency [str] Column that has the raw currency. This column takes values ['lcu','usd','eur','ppp'] and is not cap sensitive. LEAVE BLANK IF \param{currency} is specified
#' @param currency [str] Raw currency. LEAVE BLANK IF \param{col.currency} is specified
#' @param col.currency.year [str] Column that has raw currency year. LEAVE BLANK IF \param{currency.year} is specified
#' @param currency.year [int] Year of raw currency . LEAVE BLANK IF \param{col.currency.year} is specified
#' @param base.year  [int] Default is set to `the latest`. Year you want to base the output unit
#' @param base.unit [str] Default is `usd`. Please choose 'ppp' or 'usd'.
#' @param converter.version [num] Default is set to `the latest`. IHME Versions for xrates, deflator, PPP, choose from [1:2017/11, 2: 2018/5, 3: 2018/11, 3.1: 2019/2 fix VEN (final FGH 2018), 4.1: 2019/9 (initial estimates FGH 2019), 4.2: 2019/10 (updated IMF raw data), 4.3: 2019/11 (stop using WB XR data and old WHO PPP data), 4.4: 2019/11, (fix VEN), 4.5: 2019/11 (fix SOM and LBR XR), 4.6: 2019/12 (first submission FGH2019, fix PPP for VEN, STP, MRT, LBR), 4.7 (new WHO GHED data)]
#' @param inflate.usd [logical] Default is `FALSE`. If TRUE, nominal LCUs are converted to nominal USD, then inflated in USD
#' @param simplify  [logical] Default is `TRUE`. Return the simplified data with values converted, no other helper variables
#' @param LBR_WHO [logical] Default is `FALSE`. By default, converts Liberia data using the IHME series, which is based on the standard currency of Liberian dollars. Set this to TRUE if your Liberia input data comes from WHO. If set to TRUE, uses WHO converters only for Liberia, since WHO reports Liberia data in a different non-standard currency.

#' @return [data.table] A datatable in the same format as input, but \param{col.value} converted


###########################################

library(data.table)
library(RColorBrewer)
library(ggplot2)

### required inputs: 
## df - dataframe with following columns:
# indep_var - name of the variable
# beta - mean estimate of the coefficient value 
# beta_lower - lower bound of the coefficient value
# beta_upper - upper bound of the coefficient value
# nsamples - number of samples associated with the model results 
# category - category of the variable (header in the plot)

## fig_title - title of figure
## beta_min - minimum value of the coefficient axis
## beta_max - maximium value of the coefficient axis 
## flip_sig - whether or not to flip the color coordination of the significance values 
## ie does significant positive association = green or red (default green)



label_forest <- function(df,
                         fig_title = NULL,
                         beta_min = NULL,
                         beta_max = NULL,
                         flip_sig = FALSE
) {
  # set df as data.table
  setDT(df)
  
  # first evaluate significance of coefficients
  df[, sig := ifelse(beta_lower<0 & beta_upper<0, '-1',
                     ifelse(beta_lower>0 & beta_upper>0, '1', '0'))]
  
  # define colors
  red <- brewer.pal(3, 'Set1')[1]
  green <- brewer.pal(3, 'Set1')[3]
  black <- '#000000'
  
  # add empty variables for section headers and define plot labels
  cat_names = unique(df$category)
  plot_labs = c()
  for (i in 1:length(cat_names)) {
    cat_name = cat_names[i]
    # add row to df for category label
    header_row = data.table(indep_var = cat_name, 
                            beta = NA,
                            beta_lower = NA,
                            beta_upper = NA,
                            nsamples = NA,
                            sig = "",
                            category = "")
    df = rbind(df, header_row)
    
    # add empty row to have space between categories  
    if (i != length(cat_names)) { # (if not the last category name)
      blank_name = ""
      for(j in 1:i){
        # need different numbers of spaces for ggplot to recognize each blank row as unique 
        blank_name = paste0(blank_name, " ") 
      }
      blank_row = data.table(indep_var = blank_name, 
                              beta = NA,
                              beta_lower = NA,
                              beta_upper = NA,
                              nsamples = NA,
                              sig = "",
                              category = "")
      df = rbind(df, blank_row)
    }
    
    # create plot lables 
     plot_labs = c(plot_labs, paste0('***', cat_name, '***'))
     cat_vars = df[category == cat_name]$indep_var
     for (var in cat_vars) {
       plot_labs = c(plot_labs, 
                     paste0(var, " (N = ", df[indep_var == var]$nsamples, ")"))
     }
     
     # add blank space at the end of category variable names for blank row label 
     # (if not the last category )
     if (i != length(cat_names)) {
       plot_labs = c(plot_labs, " ")
     }
    
  }
  # to do: create plot
  
}
