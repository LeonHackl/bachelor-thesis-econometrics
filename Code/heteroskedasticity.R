# load all necessary libraries
library(ggplot2)
library(broom)
library(tikzDevice)

# define population size and parameters of the PRF
num_obs <- 70
beta_0 <- 5
beta_1 <- 2

# define heteroskedastic error term
set.seed(3)
error <- rnorm(n = num_obs, mean = 0, sd = x_vals)

# compute x and y values based on PRF
x_vals <- seq(from = 0, to = 10, length.out = num_obs)
y_vals <- beta_0 + beta_1*x_vals + error

# fit the model
sim_fit <- lm(y_vals ~ x_vals)
model.diag.metrics <- augment(sim_fit)

# export figure as latex-code
tikz(file = "Heteroskedasticity.tex", width = 5, height = 2.7)
colors <- c("$E[y|x]=\\beta_{0}+\\beta_{1}x$" = "#04009A", "$\\varepsilon_{i}$" = "black", "$y_{i}$" = "red")

# plot PRF with heteroskedastic error term
theme_set(theme_classic())
plot <- ggplot(model.diag.metrics, aes(x_vals, y_vals)) +
  stat_smooth(method = lm, se = FALSE, aes(color = "$E[y|x]=\\beta_{0}+\\beta_{1}x$")) +
  geom_segment(aes(xend = x_vals, yend = .fitted, color = "$\\varepsilon_{i}$"), size = 0.3) +
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

