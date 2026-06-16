
# Preparation of data for estimating out-of-pocket spending on family planning

We load contraceptive use data from the Demographic Health Surveys (DHS) by which, for each country-year possible, we calculate the proportion of women of reproductive age using the different contraceptive methods available in the data, who got the contraceptive from a private source (as opposed to a public source).  
Because it came from a private source, such as a hospital, private clinic, or shop, we assume this is paid for out-of-pocket and thus represents OOP usage.  

- We use DHS data that is manually downloaded from statcompiler.com instead of querying the API because statcompiler.com makes available some additional variables that are not available as indicators through the API - specifically, we use the variables that represent the proportion of respondents that are using a given contraceptive method (e.g., female condom or pill) from a given source (e.g., public medical source or private medical source).  
    - (And this prevents us from having to download the raw DHS data and process it ourselves)
    
- We also use the contraceptive method indicators which give the current use of a given contraceptive method across all interviewed women. For example, "FP_CUSA_W_PIL" gives the percentage of all women using the pill ( where all women refers to all women interviewed, which consist of women 15-49 years of age, so we assume it's an estimate of the proportion of all women of reproductive age currently using the pill in the country-year).  

- Then, for each given country-year, we can multiply the proportions from these 2 data sets because it gives us exactly our quantity of interest:  
$$
\frac{\text{users of method M from source S}}{\text{reproductive population}} = \frac{\text{users of method M from source S}}{\text{users of method M}} \times \frac{\text{users of method M}}{\text{reproductive population}}
$$  
where $M$ is a given contraceptive method and $S$ is a given source.  



We then model these proportions using a ST-GPR model for each contraceptive method to complete the time-series for every country.  

Next, we convert the estimated proportions to counts of users by multiplying by the reproductive population in each country-year.  

Next, we use data from the Reproductive Health Supplies Coaliation (RHSC) which produces estimates of the costs of different types of contraceptives from private sources, across countries.  
They provide such estimates for years 2018 and 2019 so we calculate contraceptive costs across time for each country as the average.  

  - RHSC provides estimates for costs of the contraceptive methods in the public and private sector. We focus on the private sector costs since we make the assumption that private sector spending is OOP spending.  
  - In the RHSC data, private costs are also broken out by the cost when the contraceptive method is government subsidized versus non-subsidized. The data also provides the proportion of private sector users that are subsidized and non-subsidized (these proportions sum to 1). So once we have our estimate of the number of users for a contraceptive method in the private sector (from DHS data), we multiply that by the proportion of private sector users that paid a government subsidized cost, and then multiply by the government subsidized cost per user, to get the estimate of total spending on the contraceptive method across private sector users who paid a subsidized cost. We repeat this using the non-subsidized proportions and costs, and then sum the two to get the total spending across private sector users on the given contraceptive method.  
  - There are some countries which are missing either cost-per-user values or proportion-of-user values in the RHSC data. We impute the relevant variables for these countries using simple linear hierarchical models, before performing the above calculations.  


Finally, summing across contraceptive methods gives us our estimate of OOP spending on family planning.  


