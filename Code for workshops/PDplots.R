library(ggplot2)

# Set parameters for the normal distribution ####
mu <- 0      # mean
sigma <- 1   # standard deviation

# Create a sequence of x values
x <- seq(-4, 4, length.out = 200)

# Compute the corresponding density values
df <- data.frame(
  x = x,
  density = dnorm(x, mean = mu, sd = sigma)
)

# Create the ggplot
ggplot(df, aes(x = x, y = density)) +
  geom_line(color = "blue", size = 1.5) +
  labs(
    #title = "Example of a Gaussian (Normal) Probability Distribution",
    x = "x",
    y = "Density"
  ) +
  theme_bw(base_size = 20)

ggplot(df, aes(x = x, y = density)) +
  geom_line(color = "blue", size = 1.5) +
  geom_vline(xintercept = mu, color = "red", linetype = "solid", size = 1.2) +
  geom_vline(xintercept = mu + sigma, color = "orange", linetype = "dashed", size = 1.2) +
  geom_vline(xintercept = mu - sigma, color = "orange", linetype = "dashed", size = 1.2) +
  geom_vline(xintercept = mu + 2*sigma, color = "darkgreen", linetype = "dashed", size = 1.2) +
  geom_vline(xintercept = mu - 2*sigma, color = "darkgreen", linetype = "dashed", size = 1.2) +
  annotate("text", x = mu, y = 0.42, label = "mu", color = "red", vjust = -0.8, fontface = "bold", size = 7) +
  annotate("text", x = mu - sigma, y = 0.22, label = "mu - 1 sd", color = "orange", vjust = -0.8, fontface = "bold", size = 6) +
  annotate("text", x = mu + sigma, y = 0.22, label = "mu + 1 sd", color = "orange", vjust = -0.8, fontface = "bold", size = 6) +
  annotate("text", x = mu - 2*sigma, y = 0.05, label = "mu - 2 sd", color = "darkgreen", vjust = -0.8, fontface = "bold", size = 6) +
  annotate("text", x = mu + 2*sigma, y = 0.05, label = "mu + 2 sd", color = "darkgreen", vjust = -0.8, fontface = "bold", size = 6) +
  labs(
    #title = "Gaussian Distribution with Mean (mu), ±1 SD, and ±2 SD",
    x = "x",
    y = "Density"
  ) +
  theme_bw(base_size = 20)


# Parameters for the two distributions
mu <- 0
sds <- c(1, 2)

# Create data for both sd values
curve_data <- expand_grid(sd = sds, x = seq(-6, 6, length.out = 300)) %>%
  mutate(mu = mu,
         density = dnorm(x, mean = mu, sd = sd),
         group = paste0("sd = ", sd))

# For vertical lines (mu and mu±sd)
vlines <- tibble(
  sd = rep(sds, each = 3),
  x = c(mu, mu-1, mu+1, mu, mu-2, mu+2),
  label = rep(c("mu", "mu-1sd", "mu+1sd"), 2),
  group = paste0("sd = ", rep(sds, each = 3))
) %>%
  mutate(x = case_when(
    label == "mu-1sd" ~ mu - sd,
    label == "mu+1sd" ~ mu + sd,
    TRUE ~ mu
  ))

# Create the faceted plot
ggplot(curve_data, aes(x = x, y = density)) +
  geom_line(color = "blue", size = 1.2) +
  geom_vline(data = vlines, aes(xintercept = x), 
             color = "red", linetype = "dashed", size = 1) +
  facet_wrap(~ group, nrow = 2) +
  theme_bw(base_size = 18)

# neg binom histos ####
library(ggplot2)
library(dplyr)
library(tidyr)

# Define parameters for four Negative Binomial distributions
params <- tibble(
  mu  = c(5, 5, 10, 10),
  phi = c(1, 5, 1, 5)
) %>%
  mutate(label = paste0("mu = ", mu, ", phi = ", phi))

# Generate data: x from 0 to some max count
max_x <- 30

# Function to compute NB pmf for given mu and phi
# Note: In R, Negative Binomial uses size parameter = phi,
# and prob = phi / (mu + phi)
get_nb_pmf <- function(x, mu, phi) {
  size <- phi
  prob <- phi / (mu + phi)
  dnbinom(x, size = size, prob = prob)
}

# Create data frame with pmf values for each combination
plot_data <- params %>%
  rowwise() %>%
  do({
    tibble(
      x = 0:max_x,
      pmf = get_nb_pmf(0:max_x, .$mu, .$phi),
      label = .$label,
      mu = .$mu,
      phi = .$phi
    )
  }) %>%
  ungroup()

# Plot
nbplot <- ggplot(plot_data, aes(x = x, y = pmf)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
  facet_wrap(~ label, ncol = 2) +
  labs(
    title = "Negative Binomial Distributions with Varying mu and phi",
    x = "Count (x)",
    y = "Probability Mass Function (PMF)"
  ) +
  theme_bw(base_size = 16) +
  theme(strip.text = element_text(size = 12))
ggsave(nbplot, file="nbplot.tiff", width=8, height=6, dpi=600, compression = "lzw")

## plots for bealls.webworm example ####
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
