# load all necessary libraries
library(lmtest)
library(car)
library(sandwich)
library(ggplot2)
library(broom)
library(tikzDevice)

# load data and define "bmi30" variable
df <- read.csv("insurance_transformed.csv")
df$bmi30 <- ifelse(df$bmi >= 30, 1, 0)

# fit model and print summary
model <- lm(df$charges ~ df$age + df$bmi + df$children + df$sex + df$smoker + 
            df$region_nw + df$region_se + df$region_sw + df$bmi30 + df$bmi30*df$smoker)
summary(model)

# fit second model for F-Test (interaction term)
model_2 <- lm(df$charges ~ df$age + df$bmi + df$children + df$sex + 
              df$region_nw + df$region_se + df$region_sw + df$bmi30)

# fit third model for F-Test (interaction term)
model_3 <- lm(df$charges ~ df$age + df$bmi + df$children + df$sex + df$smoker + 
                df$region_nw + df$region_se + df$region_sw)

# create data frame with fitted values and residuals for plotting
model_data <- data.frame(fitted(model), residuals(model))

# export figure as latex-code
tikz(file = "Residuals.tex", width = 5, height = 2.7)

# plot model residuals
theme_set(theme_classic())
plot <- ggplot(data = model_data, aes(x = fitted.model., y = residuals.model.)) +
          geom_point(shape = 1) +
          geom_hline(aes(yintercept = mean(residuals.model.)), color = "red", linetype = "dashed", lwd = 0.7) +
          labs(x = "$\\widehat{y}_{i}$", y = "$\\hat{\\varepsilon}_{i}$") +
          theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(),
                axis.ticks.y = element_blank(), axis.text.y = element_blank(),
                axis.title.x = element_text(size = rel(0.9)),
                axis.title.y = element_text(size = rel(0.9), angle = 360, vjust = 0.5))

print(plot)
dev.off()
