# GLMMs  ##########################################
library(tidyverse)
library(emmeans)
library(car)
library(agridat)
library(glmmTMB)
library(DHARMa)
library(performance)
library(MuMIn)
library(bbmle)
library(easystats)

# Beall webworm data example ####
## Load in and read about the beall.webworms dataset. #### 
## The variables of interest are the y-count of webworms, 
## spray- spray treatment, and lead-lead treatment. Don't worry about the block or other variables for now.

data("beall.webworms")
d1 <- beall.webworms
?beall.webworms  ## info about the beall.webworms dataset
head(d1) ## view data set

# 0. examine plot of data ####
hist(d1$y)

ggplot(d1, aes(x=y))+
  geom_histogram(binwidth = 1, fill="grey75", color="white")+
  #facet_wrap(~trt)+
  theme_bw(base_size = 20)

## log transform doesn't help ####
ggplot(d1, aes(x=log(y+1)))+
  geom_histogram(binwidth = .5, fill="grey75", color="white")+
  #facet_wrap(~trt)+
  theme_bw(base_size = 20)

### add normal density curve -- mean is about right, but variance way off ####
d1$logy <- log(d1$y+1)

ggplot(d1, aes(x=logy))+
  geom_histogram(aes(y = ..density..), binwidth = .5, fill="grey75", color="white")+
  stat_function(
    fun = dnorm,
    args = list(
      mean = mean(d1$logy),
      sd = sd(d1$logy)),
    color = "red", linetype = "solid", size = 2) +
  #facet_wrap(~trt)+
  theme_bw(base_size = 20)+
  expand_limits(x = -1)

## Try a poisson distribution ####
lambda <- mean(d1$y)
xvals <- seq(min(d1$y), max(d1$y))
pois_df <- data.frame(
  y = xvals,
  dens = dpois(xvals, lambda = lambda))

ggplot(d1, aes(x=y))+
  geom_histogram(aes(y = ..density..), binwidth = 1, fill="grey75", color="white")+
  geom_line(data = pois_df, aes(x = y, y = dens), color = "purple", size = 2, linetype = "solid") +
  #facet_wrap(~trt)+
  theme_bw(base_size = 20)

## Add NB density curve -- fits well over the zeros ####
mu <- mean(d1$y)
var <- var(d1$y)
size <- mu^2 / (var - mu)      # size (dispersion)
prob <- size / (size + mu)     # probability

pois_df2 <- data.frame(
  y = xvals,
  Poisson = dpois(xvals, lambda = lambda),
  NegBinom = dnbinom(xvals, size = size, mu = mu))

ggplot(d1, aes(x=y))+
  geom_histogram(aes(y = ..density..), binwidth = 1, fill="grey75", color="white")+
  geom_line(data = pois_df2, aes(x = y, y = Poisson), color = "purple", size = 2, linetype = "solid") +
  geom_line(data = pois_df2, aes(x = y, y = NegBinom), color = "green", size = 2, linetype = "dashed") +  
  #facet_wrap(~trt)+
  theme_bw(base_size = 20)

## also there are nine blocks that differ
ggplot(d1, aes(x=spray, y=y, fill=lead)) + geom_violin(scale="width", adjust=2) + 
  geom_point(position = position_jitterdodge(jitter.width=.5, jitter.height=.1, dodge.width = 1), alpha=.1)+
  facet_wrap(~block)



# STEP 1. construct poisson and negative binomial models ####
r0 <- glmmTMB(log(y+1) ~ spray * lead , data=d1, family="gaussian") # log-transformed model, bad bad bad
r1 <- glmmTMB(y ~ spray * lead , data=d1, family="poisson") # poisson model
r2 <- glmmTMB(y ~ spray * lead , data=d1, family="nbinom2") # negative binomial model

# STEP 2. Examine residuals and test for overdispersion ####
check_model(r0) # performance package check
plot(simulateResiduals(r0)) ## DHARMa package simulated residuals
hist(simulateResiduals(r0)) ## histogram should be flat
check_overdispersion(r0) # overdispersion ratio calculator from performance


plot(simulateResiduals(r1)) ## DHARMa package simulated residuals
hist(simulateResiduals(r1)) ## histogram should be flat
check_overdispersion(r1) # overdispersion ratio calculator from performance
check_model(r1) # performance package check

plot(simulateResiduals(r2)) ## DHARMa package simulated residuals
hist(simulateResiduals(r2)) ## histogram should be flat
check_overdispersion(r2) # overdispersion ratio calculator from performance
check_model(r2) # performance package check

## QUESTIONS: What's next? Any aspects of the experimental design missing from the model? ####
####            Construct a model that includes any missing factors.

## 1. construct model (again!!) ####
r3 <- glmmTMB(y ~ spray * lead + (1|block), data=d1, family="poisson")   ### poisson w/ block
r4 <- glmmTMB(y ~ spray * lead + (1|block), data=d1, family="nbinom2")   ### nb w/ block

#### Two models (r2 and r3) above differ only random effects. Fixed effects are the same.
#### Can use AIC for selecting the most appropriate random effects.
#### for model selection, use AIC or likihood ratio test
#### Note that I've included the models with the block. Because 'block' was part of the expeirment, it should be in the model no matter what.

## 2. examine residuals ####
plot(simulateResiduals(r4)) ## DHARMa package simulated residuals
hist(simulateResiduals(r4)) ## histogram should be flat
check_overdispersion(r4) # overdispersion ratio calculator from performance
check_model(r4) # performance package check

# STEP 3. Check model basics, fixed effects, random effects, family/link ####
summary(r4)


## 4. can compare models with and without random effects ####
## If the random effect was part of your experimental design you don't need to do this (just keep block in!)
AICtab(r2,r4,base=T,logLik=T,weights=T)    ## from bbmle


# STEP 4. Check significance ####
Anova(r4)

Anova(r2) ## compare with NB to poisson


# STEP 5. Calculate emmeans ####
# STEP 6. contrasts         ####
emmeans(r4, pairwise ~ spray:lead)
emmeans(r4, pairwise ~ spray:lead, type="response")

# STEP 7. Calculate R2m and R2c ####
r2(r4)

# STEP 8. Make figure ####

## emmeans w/ data points

## bar plot 
