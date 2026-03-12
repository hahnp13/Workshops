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


## plot points with error bars using pointrange and jitter ####
ggplot(penguins_summary, aes(x = species, y = body_mass_mean)) +
  geom_jitter(data = penguins, aes(x = species, y = body_mass_g), width = 0.2)+
  geom_pointrange(aes(ymin = body_mass_mean - body_mass_sd,
                      ymax = body_mass_mean + body_mass_sd),
                  size=2, linewidth = 2, color="blue")       ## note that order of layers matters here!


## ggplot with geom_density_ridges ####
library(ggridges)

ggplot() +
  geom_density_ridges(data=penguins, aes(y = species, x = body_mass_g, fill=species),
                      scale=.5, alpha=0.5) + 
  geom_jitter(data=penguins, aes(y = species, x = body_mass_g, fill=species), height = 0.1)+
  geom_pointrange(data=penguins_summary,
                  aes(y=species, x=body_mass_mean, color=species,
                      xmin = body_mass_mean - body_mass_sd,
                      xmax = body_mass_mean + body_mass_sd),
                  size=1.5, linewidth = 1.5) +
  scale_fill_viridis(discrete = TRUE, direction = -1)+
  scale_color_viridis(discrete = TRUE, direction = -1)+
  theme_bw(base_size = 16)+
  coord_flip()+
  labs(x="Body mass (g)", y="Species")

