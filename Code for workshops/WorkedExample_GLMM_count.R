# worked example of GLMMs  ##########################################
## load libraries; make sure all packages are installed #############
library(tidyverse)
library(emmeans)
library(car)
library(agridat)
library(glmmTMB)
library(DHARMa)
library(easystats)
library(broom.mixed)
library(multcomp)
library(multcompView)

# Beall webworm data example ####
## Load in and read about the beall.webworms dataset. #### 
## The variables of interest are the y-count of webworms, 
## spray- spray treatment, and lead-lead treatment. Don't worry about the block or other variables for now.

data("beall.webworms")
d1 <- beall.webworms
?beall.webworms  ## info about the beall.webworms dataset
head(d1) ## view data set
str(d1)
# STEP 0. examine plot of data ####
hist(d1$y)

## histogram of data, lots of zero's and one's, highly skewed
ggplot(d1, aes(x=y))+
  geom_histogram(binwidth = 1, fill="grey75", color="white")+
  #facet_wrap(~trt)+
  theme_bw(base_size = 20)

## log transform doesn't help ####
ggplot(d1, aes(x=log(y+1)))+
  geom_histogram(binwidth = .5, fill="grey75", color="white")+
  #facet_wrap(~trt)+
  theme_bw(base_size = 20)


## also there are thirteen blocks that differ
ggplot(d1, aes(x=spray, y=y, fill=lead)) + 
  geom_boxplot() + 
  geom_point(position = position_jitterdodge(jitter.width=.5, jitter.height=.1, dodge.width = 1), alpha=.2)+
  facet_wrap(~block)+
  theme_bw(base_size = 18)



# STEP 1. construct basic model ####
mod_logtrans <- glmmTMB(log(y+1) ~ spray * lead , data=d1) # log-transformed model, bad bad bad


# STEP 2. Examine residuals and test for overdispersion ####
## check conventional residuals from the log-transformed model ####
plot(resid(mod_logtrans)~fitted(mod_logtrans))  ## residuals should be evenly dispersed
abline(h=0)
qqPlot(resid(mod_logtrans)) # points should line up on the line

## check simulated residuals ####
plot(simulateResiduals(mod_logtrans)) ## DHARMa package simulated residuals

## Step 1b. update model with poisson distribution for count data ####
mod_poisson <- glmmTMB(y ~ spray * lead, family="poisson", data=d1) # poisson model

### check residuals for poisson  ####
plot(simulateResiduals(mod_poisson)) ## DHARMa package simulated residuals
check_overdispersion(mod_poisson) # overdispersion ratio calculator from performance

## Step 1c. update to nbinom2 to correct for overdispersion ####
mod_nbinom <- glmmTMB(y ~ spray * lead, family="nbinom2", data=d1) # negative binomial model

plot(simulateResiduals(mod_nbinom)) ## DHARMa package simulated residuals
check_overdispersion(mod_nbinom) # overdispersion ratio calculator from performance


## QUESTIONS: What's next? Any aspects of the experimental design missing from the model? ####
####            Construct a model that includes any missing factors.

## modify Step 1. construct model (again!!) by adding the block as a random effect ####
mod_nbinom_blk <- glmmTMB(y ~ spray * lead + (1|block), data=d1, family="nbinom2")   ### nb w/ block


## 2. examine residuals ####
plot(simulateResiduals(mod_nbinom_blk)) ## DHARMa package simulated residuals
check_overdispersion(mod_nbinom_blk) # overdispersion ratio calculator from performance

## The last two models differ only random effects. Fixed effects are the same.
## Can use AIC (or likelihood ratio) for selecting the most appropriate random effects.
## If the random effect was part of your experimental design you don't need to do this (just keep block in!; but we will demo)

AIC(mod_nbinom, mod_nbinom_blk)  # compare AIC for models without and with block   
anova(mod_nbinom, mod_nbinom_blk) # likelihood ratio test for models without and with block


# STEP 3. Check model basics, fixed effects, random effects, family/link ####
summary(mod_nbinom_blk)

## check random effect values
blk_vals <- tidy(mod_nbinom_blk, effects="ran_vals") %>% select(-component, -group)
blk_vals
var(blk_vals$estimate) ## variance of the random effect (block) estimate is (roughly) the variance component

# STEP 4. Check significance ####
Anova(mod_nbinom_blk) # Anova function from car package


# STEP 5. Calculate estimated marginal means using emmeans ####
emmeans(mod_nbinom_blk, ~ spray:lead) # means on log-scale
emmeans(mod_nbinom_blk, ~ spray:lead, type="response") # means on count-scale

# STEP 6. Calculate contrasts, also using emmeans          ####
emmeans(mod_nbinom_blk, pairwise ~ spray:lead, type="response") # means back-transformed 

## if you want compact-letter display (note the messages): ####
mod_emm <- emmeans(mod_nbinom_blk, ~ spray:lead, type="response") |>
multcomp::cld(Letters="ABCD", alpha=.05)
mod_emm

# STEP 7. Calculate R2m and R2c ####
r2(mod_nbinom_blk)
icc(mod_nbinom_blk)

# STEP 8. Make figure ####

## boxplot of original data
ggplot(data = beall.webworms, 
       mapping = aes(x = trt, y = y)) +
  geom_boxplot() +
  labs(x = "Treatment (Spray x Lead)",y = "Counts of Webworms") +
  theme_modern()

## bar plot from emmeans with SE
ggplot(data = mod_emm, 
       mapping =  aes(x = interaction(spray, lead),y = response)) +
  geom_col(fill = "#c9cfd2", color = "black", width = 0.7) +  
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.1) +
  geom_text(aes(label = .group, y = asymp.UCL + 0.05), size = 5, vjust = 0.1) +
  labs(x = "Treatment (Spray × Lead)", y = "Counts of Webworms ± 95% CI") +
  theme_modern()

## Points
ggplot(data = mod_emm, 
       mapping =  aes(x = interaction(spray, lead), 
                      y = response)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.1, alpha = 0.7) +
  geom_text(aes(label = .group, y = asymp.UCL + 0.05), size = 5) +
  labs(x = "Treatment (Spray x Lead)",
       y = "Counts of Webworms ± 95% CI") +
  theme_modern()




