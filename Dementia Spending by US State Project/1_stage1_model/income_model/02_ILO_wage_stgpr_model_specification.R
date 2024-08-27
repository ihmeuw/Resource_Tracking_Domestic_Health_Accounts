# Model ILO wage data using GDP per capita as main covariate
# Author: Elye Bliss
# Date: Aug 11, 2023

rm(list = ls())

library(data.table)
library(ggplot2)
library(corrplot)
library(stargazer)
library(lme4)
library(caret)

ILO_dt <- fread('FILEPATH/ILO_covars_merged.csv')

############################# Explore data #####################################

# Check correlation
cor(ILO_dt[,income],ILO_dt[,gdp_pc])
# [1] -0.04092194 
# This is raw before any log transformation

# Try excluding Belarus, which is an anomoly
# Add to outlier_dt
outliers <- ILO_dt[location_name=='Belarus',]

ILO_dt <- ILO_dt[location_name!='Belarus'] #2747 obs left
cor(ILO_dt[,income],ILO_dt[,gdp_pc])
# [1] 0.8104229 

# Make quick heat maps of 2019 average wages and gdp_pc as a validation of 
# expectations (ignoring non-matching country names, this is a quick check)

# ILO wages heat map
world_shape <- map_data("world")

ILO_map <- copy(ILO_dt)
setnames(ILO_map,old='location_name',new='region')
ILO_map <- ILO_map[year_id==2019,.(region,income)]


world_shape <- left_join(world_shape,ILO_map,by = 'region')
world_shape$subregion <- NULL
world_shape <- unique(world_shape)
world_shape_ILO <- world_shape

# Create a heat map for ILO
ILO_wage_map <- ggplot(world_shape_ILO) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill=income),
    color = "white", 
    linewidth = .1        
  ) +
  coord_map(xlim=c(-180,180)) + 
  scale_fill_continuous(low = "white", high = "blue")+
  labs(title='ILO heat map of average annual wages',
       x= NULL,
       y=NULL,
       fill = 'Annual wages')

plot(ILO_wage_map)
# Looks approximately correct, i.e. values range from <10k to >80k, with OECD
# countries at the higher end.

# Repeat for GDP per capita
world_shape <- map_data("world")

ILO_map <- copy(ILO_dt)
setnames(ILO_map,old='location_name',new='region')
ILO_map <- ILO_map[year_id==2019,.(region,gdp_pc)]


world_shape <- left_join(world_shape,ILO_map,by = 'region')
world_shape$subregion <- NULL
world_shape <- unique(world_shape)
world_shape_ILO <- world_shape

# Create a heat map for GDP pc
gdp_map <- ggplot(world_shape_ILO) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill=gdp_pc),
    color = "white", 
    linewidth = .1        
  ) +
  coord_map(xlim=c(-180,180)) + 
  scale_fill_continuous(low = "white", high = "blue")+
  labs(title='Heat map of GDP per capita',
       x= NULL,
       y=NULL,
       fill = 'GDP pc')

plot(gdp_map)
# Visually looks very correlated with wages. 

# Box and whiskers plot of wages by year
output_dir = "FILEPATH"
pdf(paste0(output_dir,"global_income_box_and_whiskers.pdf")) # remember to update the run number or old plots may be overwritten

b_and_w <- ggplot(ILO_dt, aes(x=year_id, y=income)) +
  geom_boxplot(aes(group=year_id))+
  labs(title="Box and whiskers plot of average income of all countries by year",
       x="Year",
       y="Annual income",
       color=NULL)+
  theme_minimal()
plot(b_and_w)
dev.off()

# Make scatter plot
# After a first look, it's worth labeling two remaining outliers: highest 
# income/gdp_pc ratio and lowest. These first need to be calculated and then
# removed if they are not the max/min
output_dir = "FILEPATH"
pdf(paste0(output_dir,"income_gpd_scatter_raw.pdf")) # remember to update the run number or old plots may be overwritten

ILO_dt[,income_gdp_ratio := income/gdp_pc]

ILO_dt[!(income_gdp_ratio %in% c(max(income_gdp_ratio),
                                    min(income_gdp_ratio))),income_gdp_ratio := NA]

Income_scatter_plot <- ggplot(ILO_dt,aes(x=gdp_pc,y=income))+
  geom_point(aes(color=ihme_loc_id),alpha=0.6,show.legend = FALSE) + 
  geom_smooth(method = "lm", se = FALSE)+
  labs(title="Annual income vs GDP per capita",
       subtitle="With OLS best fit",
       x="GDP per capita",
       y="Income",
       color=NULL)+
  geom_text(aes(label=ifelse(!is.na(income_gdp_ratio),paste0(location_name,', ',year_id),''),hjust=0,vjust=1))+
  theme_minimal()

plot(Income_scatter_plot)
dev.off()
# Note: the OLS fit looks fairly well-justified

########################### Fit linear model ###################################

# Convert to logged variables
ILO_dt[,lg_income := log(income)]
ILO_dt[,lg_gdp_pc := log(gdp_pc)]


# Make first model to determine outliers using cook's distance
ILO_dt[,is_outlier := NA]

model0 <- lm(
  lg_income ~ lg_gdp_pc + as.factor(sex),
  data = ILO_dt
) 

# Cook's distance
plot(model0, 4)
# Residuals vs Leverage
plot(model0, 5)

ILO_dt$cooksd <- cooks.distance(model0)

is_outlier <- function(x, y) {
  if (x > y) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Using 4/n as the cook's distance cutoff threshold
ILO_dt$is_outlier <- sapply(ILO_dt$cooksd, FUN = is_outlier, y = 4 / nrow(ILO_dt))

# Add to outlier_dt
outliers <- rbind(outliers,ILO_dt[is_outlier == T, ],fill=TRUE)

# Save outlier_dt for future reference
fwrite(outliers, "FILEPATH/removed_outliers.csv")

ILO_dt <- ILO_dt[is_outlier == F, ]

# Remake scatter plot with outliers removed
output_dir = "FILEPATH"
pdf(paste0(output_dir,"income_gpd_scatter_outliers_removed.pdf")) # remember to update the run number or old plots may be overwritten

ILO_dt[,income_gdp_ratio := income/gdp_pc]

ILO_dt[!(income_gdp_ratio %in% c(max(income_gdp_ratio),
                                 min(income_gdp_ratio))),income_gdp_ratio := NA]

Income_scatter_plot <- ggplot(ILO_dt,aes(x=gdp_pc,y=income))+
  geom_point(aes(color=ihme_loc_id),alpha=0.6,show.legend = FALSE) + 
  geom_smooth(method = "lm", se = FALSE)+
  labs(title="Annual income vs GDP per capita",
       subtitle="With OLS best fit",
       x="GDP per capita",
       y="Income",
       color=NULL)+
  geom_text(aes(label=ifelse(!is.na(income_gdp_ratio),paste0(location_name,', ',year_id),''),hjust=0,vjust=1))+
  theme_minimal()

plot(Income_scatter_plot)
dev.off()

# Refit model
model1 <- lm(
  lg_income ~ lg_gdp_pc + as.factor(sex),
  data = ILO_dt
) 

# Cook's distance
plot(model1, 4) # Looks better
# Residuals vs Leverage
plot(model1, 5)

# Compare default fixed effects model against mixed effects specification with
# random effect on country. 

all_results <- data.table(
  model = character(),
  MSE = double(),
  RMSE = double(),
  MAE = double(),
  fold = integer()
)
# randomize index_col
n_rand <- sample(ILO_dt$index_col, length(ILO_dt$index_col))
flds <- createFolds(n_rand, k = 10, list = TRUE, returnTrain = FALSE)

# Loop over k-folds (not sure if there's an easy way to avoid a loop here)
for (i in 1:length(flds)) {
  # split into train and test
  test <- ILO_dt[flds[[i]], ]
  train <- anti_join(ILO_dt, test)
  
  model1 <- lm(
    lg_income ~ lg_gdp_pc + as.factor(sex),
    data = train
  )
  

  model2 <- lme4::lmer(lg_income ~ lg_gdp_pc + as.factor(sex)
                       + (1 | ihme_loc_id), data = train)

  # Specify re.form = ~0 to predict random effects variable on any unseen categories
  test$model1_pred <- predict(model1, test) 
  test$model2_pred <- predict(model2, test, re.form = ~0)

  # Summarize results 
  results <- data.table(test)[,.(lg_income, model1_pred, model2_pred)]
  
  results <- melt(results,id.vars='lg_income')
  setnames(results,old=c('variable','value'),new=c('model','predictions'))
  results <- results[,.(MSE = mean((predictions - lg_income)^2),
                        RMSE = sqrt(mean((predictions - lg_income)^2)),
                        MAE = mean(abs(predictions - lg_income))),
                     by=model]
  
  results$fold <- i
  all_results <- rbind(all_results, results)
}

# Take average of k results:
manual_CV_results <- all_results[, .(
  avg_MSE = mean(MSE),
  avg_RMSE = mean(RMSE),
  avg_MAE = mean(MAE)
), by = model]

manual_CV_results <- manual_CV_results[order(avg_RMSE, decreasing = FALSE), ]

# Answer -> Fixed effects model outperforms mixed effects model across all measures
fwrite(manual_CV_results, "FILEPATH/2-way_manual_CV.csv")

# Interpret model coefficients
# Refit model
model1 <- lm(
  lg_income ~ lg_gdp_pc + as.factor(sex),
  data = ILO_dt
) 
sg <- stargazer(model1,
                title = "Regression Results", type = "text"
) 

######################## Checking for heteroscedasticity #######################


dev.off()
pdf("FILEPATH/model1_heteroscedasticity.pdf")
par(mfrow = c(3, 2))
plot(model1)
# First visualize the residuals
res <- resid(model1)
# Create density plot of residuals
plot(density(res)) # mean slightly skewed to right
# look at residuals against actually observations
plot(ILO_dt$lg_income, res) 
dev.off()

############# Save model and full data with outliers removed ###################

# Save final filtered data used for model 
fwrite(ILO_dt,"FILEPATH/ILO_removed_outliers.csv")

# saving the model
saveRDS(model1,"FILEPATH/stage1_ILO_wage_model.rda")

