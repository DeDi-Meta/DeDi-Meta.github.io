
if (!require("meta")) install.packages("meta")
if (!require("metafor")) install.packages("metafor")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")


library(meta)
library(metafor)
library(dplyr)
library(ggplot2)


study <- c("Abdullahi S. Aminu2017", "Mihyun Jeong2021", "Waleed M Sweileh2014", "Weijun Zhang2015")
AOR <- c(2.04, 2.05, 2.78, 4.39)
lower_ci <- c(0.80, 1.11, 1.23, 0.89)
upper_ci <- c(5.25, 3.77, 6.27, 21.77)


data <- data.frame(study, AOR, lower_ci, upper_ci)


data$lnAOR <- log(data$AOR)
data$se <- (log(data$upper_ci) - log(data$lower_ci)) / (2 * 1.96)


random_model <- rma(yi = lnAOR, sei = se, data = data, method = "REML")


summary(random_model)

forest_plot <- forest(random_model, 
                      slab = data$study,
                      xlab = "Adjusted Odds Ratio",
                      refline = 1,
                      digits = 2,
                      transf = exp)  

