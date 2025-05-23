#### R CHALLENGE 8 w/ answers ####
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

wat <- harris.wateruse %>% filter(age=='A1')
head(wat)
str(wat)

ggplot(data=wat, aes(x=day, y=water, color=species))+
  geom_point()+
  scale_color_viridis(discrete = T, end=.8) +
  theme_bw(base_size = 16)

#########################################################################################################
#### 1. Fit several models, one that accounts for tree ID as a regular random effect and one that accounts 
####     for temporal autocorrelation. Additionally, it seems like there is some non-linearity so try a quadratic term. 
####     Choose the right fixed effects to answer the question: do the species differ in water use and does this change through time?


### set of candidate models to test quadratic term and AR1 temporal autoregression
mod1 <- glmmTMB(water ~ day * species + (1|tree), data=wat)
mod2 <- glmmTMB(water ~ day * species + I(day^2) + (1|tree), data=wat)
mod3 <- glmmTMB(water ~ day * species + ar1(0+as.factor(day)|tree), data=wat)
mod4 <- glmmTMB(water ~ day * species + I(day^2) + ar1(0+as.factor(day)|tree), data=wat)
#mod5 <- glmmTMB(water ~ day * species + I(day^2) + toep(0+as.factor(day)|tree), data=wat, dispformula = ~0)
#mod4 <- glmmTMB(water ~ day * species + I(day^2) * species + ar1(0+as.factor(day)|tree), data=wat)


plot(residuals(mod1)~fitted(mod1)) ## resids from first model
plot(residuals(mod2)~fitted(mod2)) ## resids from first model
plot(residuals(mod3)~fitted(mod3)) ## resids from first model
plot(residuals(mod4)~fitted(mod4)) ## resids from first model

check_collinearity(mod2)

# 
# mod3 <- glmmTMB(water ~ day * species + I(day^2) * species + (1|tree), data=wat)
# mod4 <- glmmTMB(water ~ day * species + ar1(0+as.factor(day)|tree), data=wat)
# mod5 <- glmmTMB(water ~ day * species + I(day^2) + ar1(0+as.factor(day)|tree), data=wat)
# mod6 <- glmmTMB(water ~ day * species + species * I(day^2) + ar1(0+as.factor(day)|tree), data=wat)


#### 2. Examine the residuals and AIC for your models. Which is best? If your model has temporal autocorrelation, how strong is it?
AIC(mod1,mod2,mod3,mod4) # mod3 best
BIC(mod1,mod2,mod3,mod4) # mod3 best

compare_performance(mod1,mod2,mod3,mod4)

plot(simulateResiduals(mod1)) # residuals ok, not perfect but passable
plot(simulateResiduals(mod2)) # residuals ok, not perfect but passable
plot(simulateResiduals(mod3)) # residuals ok, not perfect but passable
plot(simulateResiduals(mod4)) # residuals ok, not perfect but passable
plot(simulateResiduals(mod5)) # residuals ok, not perfect but passable



summary(mod4) # very strong autocorrelation
Anova(mod4)




## plot data with ggplot; looks better than default ggpredict plot; close approximation of model
ggplot(data=wat, aes(x=day, y=water, color=species))+
  geom_point()+
  scale_color_viridis(discrete = T, end=.8) +
  geom_smooth(method="lm", formula = y~x) +
  theme_bw(base_size = 16)

ggplot(data=wat, aes(x=day, y=water, color=species))+
  geom_point()+
  scale_color_viridis(discrete = T, end=.8) +
  geom_smooth(method="lm", formula = y~x+I(x^2)) +
  theme_bw(base_size = 16)

#### 3. Does water use differ between the species at these two time points: 175 days and 275 days?
emmeans(mod4, pairwise ~ species|day, at=list(day=c(175,225,275))) # no difference
emmeans(mod4, pairwise ~ species, at=list(day=275)) # yes difference
