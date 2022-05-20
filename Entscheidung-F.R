library(ggplot2)
library(broom)
library(tikzDevice)

tikz(file = "F-Test.tex", width = 5, height = 2.7)

theme_set(theme_classic())
plot <- ggplot(data = data.frame(x = c(0, 5)), aes(x)) +
  stat_function(fun = df, n = 500, color = "#04009A", lwd = 1, args = list(df1 = 3, df2 = 200)) +
  stat_function(fun = df, geom = "area", fill = "#77ACF1", alpha = 0.4, args = list(df1 = 3, df2 = 200)) +
  stat_function(fun = df, geom = "area", fill = "#04009A", alpha = 0.4, xlim = c(2.65, 5), args = list(df1 = 3, df2 = 200)) +
  geom_segment(aes(x = 2.65, xend = 2.65, y = 0, yend = 0.065), lty = 2, col = "red") +
  scale_y_continuous(breaks = NULL) +
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
