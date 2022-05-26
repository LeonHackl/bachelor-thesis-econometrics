# load all necessary libraries
library(ggplot2)
library(broom)
library(tikzDevice)

# export figure as latex-code
tikz(file = "Hypothesis-t-test.tex", width = 5, height = 2.7)

# plot distribution with decision rule
theme_set(theme_classic())
plot <- ggplot(data = data.frame(x = c(-4, 4)), aes(x)) +
  stat_function(fun = dt, n = 200, color = "#04009A", lwd = 1, args = list(df = 100)) +
  geom_area(stat = "function", fun = dnorm, fill = "#04009A", alpha = 0.4, xlim = c(1.9, 4)) +
  geom_area(stat = "function", fun = dnorm, fill = "#04009A", alpha = 0.4, xlim = c(-4, -1.9)) +
  geom_area(stat = "function", fun = dnorm, fill = "#77ACF1", alpha = 0.4, xlim = c(-1.9, 1.9)) +
  geom_segment(aes(x = -1.9, xend = -1.9, y = 0, yend = 0.065), lty = 2, col = "red") +
  geom_segment(aes(x = 1.9, xend = 1.9, y = 0, yend = 0.065), lty = 2, col = "red") +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 0.398), lty = 2, col = "black") +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "", color = "") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_text(size = rel(0.9)),
        axis.title.y = element_text(size = rel(0.9), angle = 360, vjust = 0.5))

print(plot)
dev.off()
