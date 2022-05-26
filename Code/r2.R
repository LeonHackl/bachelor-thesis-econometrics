# load all necessary libraries
library(ggplot2)
library(broom)

# define sample size and parameters of the SRF
num_obs <- 5
beta_0 <- 5
beta_1 <- 2
sigma <- 3

# define error term
set.seed(3)
error <- rnorm(n = num_obs, mean = 0, sd = sigma)

# compute x and y values based on SRF
x_vals <- seq(from = 0, to = 10, length.out = num_obs)
y_vals <- beta_0 + beta_1*x_vals + error

# fit the model
sim_fit <- lm(y_vals ~ x_vals)
model.diag.metrics <- augment(sim_fit)

# export figure as latex-code
tikz(file = "R2.tex", width = 5, height = 2.7)
colors <- c("$\\widehat{y}=\\widehat{\\beta}_{0}+\\widehat{\\beta}_{1}x$" = "#77ACF1", 
            "$\\varepsilon_{i}$" = "black", "$y_{i}$" = "red", "$\\bar{y}$" = "#04009A")

# plot SRF with ANOVA
theme_set(theme_classic())
plot <- ggplot(model.diag.metrics, aes(x_vals, y_vals)) +
  annotate("rect", xmin = 6.3, xmax = 7.5, ymin = 13.82, ymax = 19.3, 
           color = NA, fill = "orange", alpha = 0.5) +
  annotate("rect", xmin = 7.5, xmax = 8.11, ymin = 13.82, 
           ymax = 16.54, color = NA, fill = "blue", alpha = 0.5) +
  annotate("rect", xmin = 7.5, xmax = 8.11, ymin = 16.54, ymax = 19.3, 
           color = NA, fill = "black", alpha = 0.5) +
  stat_smooth(method = lm, se = FALSE, aes(color = "$\\widehat{y}=\\widehat{\\beta}_{0}+\\widehat{\\beta}_{1}x$")) +
  geom_segment(aes(xend = x_vals, yend = .fitted), size = 0.3) +
  geom_point(aes(color = "$y_{i}$")) +
  geom_hline(aes(yintercept = mean(y_vals), color = "$\\bar{y}$"), linetype = "dashed") +
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
