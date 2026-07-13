# # Before you start: Setting up environment ===================================
# # Clean environment
# rm(list = ls(all.names = TRUE)) # will clear all objects including hidden objects
# gc() # free up memory and report the memory usage
# options(max.print = .Machine$integer.max, scipen = 999, stringsAsFactors = F, dplyr.summarise.inform = F) # avoid truncated output in R console and scientific notation

# Introduction to Survival Analysis in R  ======================================

# 1. Getting started ===========================================================
## 1.1 Load libraries ----
# base libraries
# remember to install libraries if you have not used them before!
library(tidyverse) # includes ggplot2, for data visualisation. dplyr, for data manipulation.
library(survival)  # core survival analysis function

# libraries for data visualizaiton
library(survminer) # recommended for visualizing survival curves
library(ggsurvfit) # alternative package for plotting time-to-event endpoints
library(patchwork) # optional, allows for easy formatting and annotation of plots

# aditional libraries
library(car)
library(emmeans)
library(easystats)


## 1.2 Load and investigate dataset ----

# dummy dataset simulating survival data for Helicoverpa zea feeding on insecticide-treated plant material. 
dat <- read.csv("Survival Lecture/helicoverpa_survival.csv")


dim(dat) # returns the dimensions of the data frame
head(dat) # returns the first few rows from the data
str(dat)

# 2. Survival Object ===========================================================
# Surv() creates an `survival object` with one entry for each subject:
# survival time, followed by `+` if censored

class(Surv(dat$time, dat$status))
Surv(dat$time, dat$status)

# 3 Kaplan-Meier Survival curves ==============================================
# survfit() function creates survival curves using Kaplan-Meier method

## 3.1 Complete cohort ----
# Create simple survival curve
s1 <- survfit(Surv(time, status) ~ 1, data = dat)
# the summary table shows a row for each time point where either event occurred or sample was censored
summary(s1)
# n.risk shows number (i.e., subjects ) at risk and cumulative survival at that timepoint

# short summary of survival curve
s1 

# basic KM plot
ggsurvplot(s1)

# refined KM plot
ggsurvplot(s1,
           palette = "Dark2",
           risk.table = "abs_pct",
           surv.median.line = "hv",  # add the median survival pointer.
           # add title and rename legends & axis labels
           title = "Survival of Helicoverpa zea feeding on insecticide-treated plants",
           xlab = "Days",
           ylab = "Survival propability",
           legend = "none",
           ggtheme = theme_bw())

### 3.1.1 Estimating probability of survival after x days ----
# "surv": probability to not have experienced the event at time x
summary(s1, times = c(3, 5, 10, 15, 20))$surv

## 3.2 Population ----
# Does survival probability differ between the populations of H. zea?
s2 <- survfit(Surv(time, status) ~ population, data = dat)
# the summary is now split into one table for each population
summary(s2)
# short summary of survival curves
s2

ggsurvplot(s2,  
           palette = "Dark2",
           conf.int = TRUE,
           risk.table = "abs_pct",
           # rename legends and axis labels
           legend.title = "Population",
           legend.labs = c("Florida", "Texas"),
           legend = "right",
           xlab = "Days",
           ylab = "Survival propability",
           ggtheme = theme_bw()) 

# Note: each "+" represents a censored specimen
# The curves are slightly divergent, but is the difference significant? 

### 3.2.1 Log rank test: cultivar ----
# We can compute the Log rank test of the difference between the curves using survdiff to compare groups
logrank_pop <- survdiff(Surv(time, status) ~ population, data = dat)
logrank_pop

# we also can show the p-value of the log-rank test directly in the plot: 
ggsurvplot(s2,  
           palette = "Dark2",
           conf.int = TRUE,
           risk.table = "abs_pct",
           # rename legends and axis labels
           legend.title = "Population",
           legend.labs = c("Florida", "Texas"),
           legend = "right",
           xlab = "Days",
           ylab = "Survival propability",
           ggtheme = theme_bw(),
           # add p.value
           pval = TRUE) 

# With p-val > 0.05, there is no significant difference between ........

# 4. Cox Proportional Hazard (PH) ==============================================

# Univariate vs Multivariate model
# The log rank test doesn’t let us the more than one variable into account when comparing survival (or, as in this case, development) times.
# Cox regression analysis, on the other hand, allows us to fit a model with multiple covariates we think may influence the time to event.

## 4.1 Basic Cox PH model ----
# To fit a Cox model, use the "coxph()" function from the survival package:
cox_m1 <- coxph(Surv(time, status) ~ 1, data = dat)

# Visualizing the estimated distribution of survival times
ggsurvplot(survfit(cox_m1), data = dat)
# does it differ from the KM survival curve? hint: ggsurvplot(s1)
# - yes, but only slightly
ggsurvplot(s1)
## 4.2 Differences between populations? ----

cox_m2 <- coxph(Surv(time, status) ~ population, data = dat)
summary(cox_m2)
cox_m2

# Notice that we have one row with the coefficient for the "Texas" population, and the hazard ratio (exponentiated coefficients).
# For a more direct interpretation of how each variable influences incidence, take a look at the hazard ratios.

## About references – increased development compared to what? ----

# By default, the model will use the first group listed for each variable as reference.
# In case of categorical groups, the order is determined by alphabetical order.
# Here, "population" includes two different groups, i.e., "Florida" and "Texas", with "Florida" serving as reference.
# A hazard ratio of approximately 0.84 means that the hazard (or the risk of time) for Texas is about 84% of the hazard for Florida. 
# In other words, this indicates about a 16% reduction in the hazard over time for the Texas group compared to Florida,
# although this difference is not statistically significant (p = 0.55)

### > Changing a reference ----

# to change the reference group, we need make the variable of interest a factor and redefine the levels with "ref"
dat2 <- dat %>% 
  mutate(population = as.factor(population))
# investigate levels in new data set
levels(dat2$population)
# refeine the levels using the 'relevel()' function with ref.
# e.g., set "Texas" as reference:
dat2$population <- relevel(dat2$population, ref = "Texas")           
levels(dat2$population)

cox_m2b <- coxph(Surv(time, status) ~ population, data = dat2)
summary(cox_m2b)

# The new reference for population is now "Texas"
# exp(coef) = 1.1883
# This value suggests that the hazard (risk of time) for the Florida group is about 1.188 times (or 18.8% higher than) that of Texas,
# but this difference is not significant.

anova(cox_m2)

## 4.3 Does insecticide residue influence survival? ----
# # cox ph model with 'residue' as predictor
cox_m3 <- coxph(Surv(time, status) ~ residue, data = dat)
# use summary to examine the fitted object
summary(cox_m3)
anova(cox_m3)

# # cox ph model with 'population' and 'residue' as predictor
cox_m4 <- coxph(Surv(time, status) ~ population + residue, data = dat)
anova(cox_m4)
# In summary, this analysis shows that insecticide residue has a significant influence on survival,
# whereas the population variable does not add much explanatory power

# # cox ph model that includes "population", "residue" and the interaction term 
cox_m5 <- coxph(Surv(time, status) ~ population*residue, data = dat)
anova(cox_m5)

# use anova() to compare the fitted Cox objects
anova(cox_m4, cox_m5)
# There is a marginal improvement (p = 0.06579) when adding the interaction term.
# This indicates that the relationship between insecticide residue and survival could be moderated by population.
# i.e., the effect of residue is not uniform across the two populations.

## 4.3 Verify proportional hazard assumptions ----

# Assumptions: non-informative censoring, proportional hazards

# # Censoring type: non-informative censoring
# # Linearity of continuous covariates
# # Hazard is proportional

### 4.3.1 Proportional hazards assumption ----

# use the cox.zph() function form the survival package:
test.ph <- cox.zph(cox_m5)
test.ph
# Plot the Schoenfeld residuals over time for each covariate
ggcoxzph(test.ph)

# for population: p < 0.05: there is evidence against proportional hazards assumption!
# this can be resolved by: stratification (for categorical variables) or adding a covariate*time interaction.
# NOTE: Stratification should only be used for confounding variables that are not investigated for main effect!
# Stratification allows the baseline hazard h0 to differ between groups

# # cox_m5 corrected for ph violation of "population"
cox_m5b <- coxph(Surv(time, status) ~ strata(population)*residue, data = dat)
anova(cox_m5b)

# no violation of PH assumption 
test.ph <- cox.zph(cox_m5b)
test.ph

### 4.3.2 Linearity ----
fit <- cox_m5b

# checking for linearity using MARTINGALE residuals
plot(predict(fit), residuals(fit, type="martingale"),
     xlab= "fitted values", ylab = "Martingale residuals",
     main = "Residual Plot", las=1)
abline(h=0)
lines(smooth.spline(predict(fit),
                    residuals(fit, type = "martingale")), col = "red")

# Alternatively: visual assessment of linearity using DEVIANCE residuals
ggcoxdiagnostics(fit,
                 type = "deviance",
                 ox.scale = "linear.predictions")

## 4.4 Model interpretation ----
summary(cox_m5b)

# total individuals: n = 55
# events: 49, i.e., 6 specimen were censored (lost or still alive at end of study)

### 4.4.1 Coefficients ----

# # fixed effect: residue

# Coefficient 3.8179, HR (exp(coeff)): 45.51
# 95% CI: [6.32, 327.47]
# p-value: <0.001 ***
# Interpretation (for the reference group, i.e. Florida population):
# For each 1-unit increase in residue, the hazard of death increases by a factor of ~45
# i.e., more residue = much higher risk of mortality
# This is a strong and highly significant relationship.

# # Interaction Texas population : residue 

# Coefficient –3.1056, HR (exp(coeff)): 0.0448
# 95% CI: [0.00429, 0.4677]
# p-value: 0.009 **
# Interpretation
# Interaction term modifies the residue effect for Texas population
# negative coefficient: effect of residue less severe in Texas than in Florida pop.
# Net effect of residue in Texas:
# 3.8179 + (–3.1056) ≈ 0.7123 
# exp(0.7123) ≈ 2.04
# --> for the Tx population, each 1-unit increase in residue doubles the hazard (HR ≈ 2)

# can also use emtrends to get slopes
emtrends(cox_m5b, pairwise~population, var = "residue") ## compare slopes
emtrends(cox_m5b, ~population, var = "residue") %>%     ## expential slopes for each population
  as.data.frame() %>% 
  mutate(across(where(is.numeric), ~ exp(.x)))


### 4.4.2 Model performance  ----

# Concordance = 0.678 (SE = 0.05):
# moderate predictive accuracy (0.5 = as good as random. Values closer to 1 are better!)

# Global Model Tests:
# highly significant (p < 0.001), i.e., model explains variation in survival better than chance.

# 5. Visualize results =========================================================
# Reminder: fit is a term to store the final model
# you can also work work with cox_m5b instead

# Adjusted survival curves
# adjusted survival curves present expected survival curves calculated based on Cox model separately for sub-populations
# you can use the ggadjustedcurves() function from the survminer package:
adj_sc <- ggadjustedcurves(fit,
                 data = dat,
                 variable = "population",
                 palette = "Dark2",
                 legend.title = "Population",
                 legend = "bottom",
                 xlim = c(0, 20),
                 title = "Adjusted survival curves for Helicoverpa zea",
                 xlab = "Days",
                 ylab = "Survival propability",
                 ggtheme = theme_bw()) 
adj_sc

# compare to kaplan meier survival curve
# use the patchwork framework to combine the different plots for easy comparison

sc <- ggsurvplot(s2,  
           palette = "Dark2",
           # rename legends and axis labels
           title = "Survival curves for Helicoverpa zea",
           legend.title = "Population",
           legend.labs = c("Florida", "Texas"),
           legend = "none",
           xlab = "Days",
           ylab = "Survival propability",
           ggtheme = theme_bw()) 

comp <- sc$plot / adj_sc +
  plot_layout(axes = "collect")

# display plot
comp 

  

                       