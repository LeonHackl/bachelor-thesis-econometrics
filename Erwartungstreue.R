library(ggplot2)
library(broom)
library(tikzDevice)

tikz(file = "Erwartungstreue.tex", width = 5, height = 2.7)
colors <- c("Verteilung von $b$" = "#04009A", "Verteilung von $\\widetilde{\\beta}$" = "#77ACF1")

theme_set(theme_classic())
plot <- ggplot(data = data.frame(x = c(-4.5, 4.5)), aes(x)) +
  stat_function(fun = dnorm, n = 200, color = "#04009A", lwd = 1, args = list(mean = 0, sd = 1)) +
  stat_function(fun = dnorm, n = 200, color = "#77ACF1", lwd = 1, args = list(mean = 0, sd = 1.5)) +
  scale_y_continuous(breaks = NULL) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 0.4), lty = 2, col = "black") +
  labs(x = "", y = "", color = "") +
  scale_color_manual(values = colors) +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_text(size = rel(0.9)),
        axis.title.y = element_text(size = rel(0.9), angle = 360, vjust = 0.5),
        legend.position = c(.93, .95), legend.justification = c("left", "top"), 
        legend.box.just = "left", legend.margin = margin(-5, 6, 6, -300), 
        legend.background = element_rect(fill = "transparent"))

print(plot)
dev.off()

