library(ggplot2)

# Set parameters for the normal distribution
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
