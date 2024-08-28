# Clear memory of all objects
rm(list=ls())


#Loading libraries
library("metafor")
library("dplyr")
library(data.table)
library(esc)
library(multcomp)

library(reticulate)

# load crosswalk function - this allows us to convert the point estimates
# and standard errors to logit space for the meta-analysis
reticulate::use_python("FILEPATH")
mr <- import("mrtool")
cw <- import("crosswalk")

today <- gsub('-','_',Sys.Date())

#Load data
caregiving_data <- fread("FILEPATH/00_clean_caregiving_attributable_fraction.csv")


caregiving_data[, var := (1/sample_size)]

# we don't have a sample size for Hurd, use average sample size
avg_ss <- mean(caregiving_data$sample_size, na.rm=T)
caregiving_data[is.na(var), var := 1/avg_ss]
caregiving_data[, se := sqrt(var)]

# use delta_transform function
caregiving_data[, c("logit_mean", "logit_se")] <- cw$utils$linear_to_logit(
  mean = array(caregiving_data$attributable_fraction), 
  sd = array(caregiving_data$se))

ma_model_cs <- rma(logit_mean,
                   sei=logit_se, 
                   method="REML" ,
                   data = caregiving_data, 
                   slab = paste(caregiving_data$citation, caregiving_data$location, sep="  "), )

summary(ma_model_cs)

# Forest plot
pdf("FILEPATH/caregiving_att_fraction_plot.pdf")
forest.rma(ma_model_cs, 
           atransf = transf.ilogit,
           slab = paste(caregiving_data$citation, caregiving_data$location, sep="  "), 
           xlab="Fraction of health spending attributable to dementia", 
           mlab="", 
           psize=1, 
           col="blue",
           header="Author(s) and Year")
grid.text("Caregiving Attributable Fraction - all sources", .5, .85, gp=gpar(cex=1.5))
dev.off()


# Simulating 1000 draws 
# rbinom(n=20, size=1, prob=.4) 
caregiving_af_draws <- as.data.table(rnorm(1000, ma_model_cs$beta[,1], ma_model_cs$se))


# take inverse logit of caregiving_af_draws
caregiving_af_draws_inv <- exp(caregiving_af_draws)/(1+exp(caregiving_af_draws))
mean(caregiving_af_draws_inv$V1)

fwrite(caregiving_af_draws_inv, paste0("FILEPATH/caregiving_af_draws_",today,".csv"))

# save out model to .Rda file to use for creating the meta-anlysis figure
caregiving_af_model <- ma_model_cs
save(caregiving_af_model,caregiving_data, file = "FILEPATH/cg_model_RTR.rda")

