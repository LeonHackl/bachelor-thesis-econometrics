# load all necessary libraries
library(ggplot2)
library(broom)
library(tikzDevice)

# export figure as latex-code
tikz(file = "Hypothesis-f-test.tex", width = 5, height = 2.7)

# plot distribution with decision rule
theme_set(theme_classic())
plot <- ggplot(data = data.frame(x = c(0, 5)), aes(x)) +
  stat_function(fun = df, n = 500, color = "#04009A", lwd = 1, args = list(df1 = 3, df2 = 200)) +
  stat_function(fun = df, geom = "area", fill = "#77ACF1", alpha = 0.4, args = list(df1 = 3, df2 = 200)) +
  stat_function(fun = df, geom = "area", fill = "#04009A", alpha = 0.4, xlim = c(2.65, 5), args = list(df1 = 3, df2 = 200)) +
  geom_segment(aes(x = 2.65, xend = 2.65, y = 0, yend = 0.065), lty = 2, col = "red") +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "", color = "") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_text(size = rel(0.9)),
        axis.title.y = element_text(size = rel(0.9), angle = 360, vjust = 0.5))

print(plot)
dev.off()
