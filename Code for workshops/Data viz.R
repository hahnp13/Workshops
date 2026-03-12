# Load packages ####

## Install packages as needed
# install.packages("tidyverse")
# install.packages("palmerpenguins")
# install.packages("agridata")
# install.packages("viridis")
# install.packages("ggsci")

library(palmerpenguins) # penguin data
library(agridata)       # example data for agriculture
library(tidyverse)      # data manipulation and visualization
library(viridis)        # color palette (good for continuous variables)
library(ggsci)          # color palette (good for discrete variables)

# Penguin data -- Explore data ####
head(penguins)
summary(penguins)


# Plot body mass by species ####

## make the most basic of boxplots ####
ggplot(penguins, aes(x = species, y = body_mass_g)) +
  geom_boxplot()

### add points to the boxplot #####
ggplot(penguins, aes(x = species, y = body_mass_g)) +
  geom_boxplot() +
  geom_jitter(width = 0.2)

### suppress outliers on boxplot #####
ggplot(penguins, aes(x = species, y = body_mass_g)) +
  geom_boxplot(outlier.shape=NA) +
  geom_jitter(width = 0.2)

### add fill to the boxplot ####
ggplot(penguins, aes(x = species, y = body_mass_g, fill=species)) +
  geom_boxplot(outlier.shape=NA) +
  geom_jitter(width = 0.2) +
  scale_fill_viridis(discrete = TRUE, direction = -1)

### change theme ####
ggplot(penguins, aes(x = species, y = body_mass_g, fill=species)) +
  geom_boxplot(outlier.shape=NA) +
  geom_jitter(width = 0.2) +
  scale_fill_viridis(discrete = TRUE, direction = -1)+
  theme_bw(base_size = 16)

## make a similar barplot ####
ggplot(penguins, aes(x = species, y = body_mass_g)) +
  geom_bar(stat = "summary", fun = "mean")

### add errorbars to the barplot ####
### first summarize the data to calculate mean and se 
penguins_summary <- penguins %>%
  group_by(species) %>%
  summarise(body_mass_mean = mean(body_mass_g, na.rm = TRUE),
            body_mass_sd = sd(body_mass_g, na.rm = TRUE),
            body_mass_se = sd(body_mass_g, na.rm = TRUE) / sqrt(n()))


## plot with standard deviation or standard error?
ggplot(penguins_summary, aes(x = species, y = body_mass_mean)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = body_mass_mean - body_mass_sd,
                    ymax = body_mass_mean + body_mass_sd,
                    width = 0.2))


  