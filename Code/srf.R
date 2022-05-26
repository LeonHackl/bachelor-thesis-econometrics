# load all necessary libraries
library(ggplot2)
library(broom)
library(tikzDevice)

# define population size and parameters of the PRF
num_obs <- 100
beta_0 <- 5
beta_1 <- 2
sigma <- 3

# define error term
set.seed(3)
error <- rnorm(n = num_obs, mean = 0, sd = sigma)

# compute x and y values based on PRF
x_vals <- seq(from = 0, to = 10, length.out = num_obs)
y_vals <- beta_0 + beta_1*x_vals + error

# draw 20 data points from the simulated population
df <- data.frame(x_vals, y_vals)
df_sample <- df[sample(nrow(df), 20), ]

# fit PRF and SRF based an respective data
sim_fit <- lm(y_vals ~ x_vals)
sim_fit_sample <- lm(df_sample$y_vals ~ df_sample$x_vals)

model.diag.metrics <- augment(sim_fit)
model.diag.metrics_sample <- augment(sim_fit_sample)

# export figure as latex-code
tikz(file = "SRF.tex", width = 5, height = 2.7)
colors <- c("$\\widehat{y}=\\widehat{\\beta}_{0}+\\widehat{\\beta}_{1}x$" = "#77ACF1", 
            "$E[y|x]=\\beta_{0}+\\beta_{1}x$" = "#04009A", "$\\hat{\\varepsilon}_{i}$" = "black", "$y_{i}$" = "red")

# plot PRF and SRF
theme_set(theme_classic())
plot <- ggplot(model.diag.metrics_sample, aes(df_sample$x_vals, df_sample$y_vals)) +
  geom_line(data = model.diag.metrics, aes(x_vals, .fitted, color = "$E[y|x]=\\beta_{0}+\\beta_{1}x$"), size = 1) +
  stat_smooth(method = lm, se = FALSE, aes(color = "$\\widehat{y}=\\widehat{\\beta}_{0}+\\widehat{\\beta}_{1}x$")) +
  geom_segment(aes(xend = df_sample$x_vals, yend = .fitted, color = "$\\hat{\\varepsilon}_{i}$"), size = 0.3) +
  geom_point(data = df, aes(x_vals, y_vals), shape = 1, color = "red", alpha = 0.5) +
  geom_point(aes(color = "$y_{i}$")) +
  labs(x = "$x$", y = "$y$", color = "") +
  scale_color_manual(values = colors) +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_text(size = rel(0.9)),
        axis.title.y = element_text(size = rel(0.9), angle = 360, vjust = 0.5),
        legend.position = c(.95, .95), legend.justification = c("left", "top"), 
        legend.box.just = "left", legend.margin = margin(-5, 6, 6, -300), 
        legend.background = element_rect(fill = "transparent"))

print(plot)
dev.off()

