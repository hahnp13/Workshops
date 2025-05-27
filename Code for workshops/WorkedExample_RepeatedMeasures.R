#### Repeated measures -- worked example ####
library(tidyverse)
library(car)
library(emmeans)
library(MuMIn)
library(glmmTMB)
library(agridat)
library(viridis)
library(DHARMa)
library(ggeffects)
library(performance)
library(easystats)

## load dataset
wat <- harris.wateruse %>% filter(age=='A1')
?harris.wateruse

# STEP 0. EXPLORE DATA ####
head(wat)
str(wat)
summary(wat)
hist(wat$water)

ggplot(data=wat, aes(x=day, y=water, color=species))+
  geom_point()+
  scale_color_viridis(discrete = T, end=.8) +
  theme_bw(base_size = 16)

#########################################################################################################
# STEP 1. Fit several models, one that accounts for tree ID as a regular random effect and one that accounts 
#     for temporal autocorrelation. Additionally, it seems like there is some non-linearity so try a quadratic term. 
#     Choose the right fixed effects to answer the question: do the species differ in water use and does this change through time?


### set of candidate models to test quadratic term and AR1 temporal autoregression
mod1 <- glmmTMB(water ~ day * species + (1|tree), data=wat) # tree as random effect
mod2 <- glmmTMB(water ~ day * species + I(day^2) + (1|tree), data=wat) # with quadratic term
mod3 <- glmmTMB(water ~ day * species + ar1(0+as.factor(day)|tree), data=wat) # include autoregressive correlation
mod4 <- glmmTMB(water ~ day * species + I(day^2) + ar1(0+as.factor(day)|tree), data=wat) # quadratic and autoreg

# STEP 2. Examine residuals and colinearity
simulateResiduals(mod1, plot=T)
simulateResiduals(mod2, plot=T)
simulateResiduals(mod3, plot=T)
simulateResiduals(mod4, plot=T)

check_collinearity(mod4) # note about autocorr; not problematic in this case, can scale ^2 term to eliminate


# STEP 2/3 MODEL SELECITON using AIC ####
AIC(mod1,mod2,mod3,mod4) # mod4 best

compare_performance(mod1,mod2,mod3,mod4) # from easystats package


# STEP 3. CHECK RANDOM EFFECTS, AUTOCORREATION, ETC.
summary(mod4) # very strong autocorrelation (r = 0.96)



# STEP 4. CHECK SIGNIFICANCE ####
Anova(mod4)   # all terms highly significant



#### 3. Does water use differ between the species at these three time points: 175, 225, and 275 days?
emmeans(mod4, pairwise ~ species|day, at=list(day=c(175,225,275))) # no difference early, but yes later on



## plot data with ggplot; close approximation of model

# LINEAR MODEL
ggplot(data=wat, aes(x=day, y=water, color=species))+
  geom_point()+
  scale_color_viridis(discrete = T, end=.8) +
  geom_smooth(method="lm", formula = y~x) +
  theme_bw(base_size = 16)

# QUADRATIC MODEL
ggplot(data=wat, aes(x=day, y=water, color=species))+
  geom_point()+
  scale_color_viridis(discrete = T, end=.8) +
  geom_smooth(method="lm", formula = y~x+I(x^2)) +
  theme_bw(base_size = 16)
