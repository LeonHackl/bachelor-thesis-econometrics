# load all necessary libraries
library(ggplot2)
library(broom)
library(tikzDevice)

# export figure as latex-code
tikz(file = "Unbiasedness-Efficiency.tex", width = 5, height = 2.7)

# plot distributions
theme_set(theme_classic())
plot <- ggplot(data = data.frame(x = c(-4.5, 4.5)), aes(x)) +
  stat_function(fun = dnorm, n = 200, color = "#04009A", lwd = 1, args = list(mean = 0, sd = 1)) +
  stat_function(fun = dnorm, n = 200, color = "#77ACF1", lwd = 1, args = list(mean = 0, sd = 1.5)) +
  scale_y_continuous(breaks = NULL) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 0.4), lty = 2, col = "black") +
  labs(x = "", y = "", color = "") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_text(size = rel(0.9)),
        axis.title.y = element_text(size = rel(0.9), angle = 360, vjust = 0.5))

print(plot)
dev.off()

