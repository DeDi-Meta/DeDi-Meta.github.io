
if(!require(meta)) install.packages("meta")
if(!require(metafor)) install.packages("metafor")
if(!require(dplyr)) install.packages("dplyr")
if(!require(ggplot2)) install.packages("ggplot2")
library(meta)
library(metafor)
library(dplyr)
library(ggplot2)

study <- c("Shahad Abduljalil Abualhamael2024", "Abdullahi S. Aminu2017", 
           "anteneh Messele Birhanu2016", "Habtamu Birhanu2022", 
           "Tania Dehesh2020", "Mohamed Ebrahim2021", 
           "Mohamed Hassan Elnaem2025", "Malgorzata Gorska-Ciebiada2014", 
           "Sheikh Mohammed Shariful Islam2015", "Mihyun Jeong2021", 
           "Steven M. Kogan2007", "Nelda Mier2008②+A", 
           "Nur Adam Mohamed2024", "Mussa R. Mussa2023", 
           "Kabtamu Nigussie2023", "Avinash K. Sunny2019", 
           "Waleed M Sweileh2014", "Allan Oliver Dampil2019", 
           "Yiting Wang2016", "Weijun Zhang2015")


aor <- c(0.536, 2.36, 0.77, 2.26, 1.38, 1.50, 1.175, 2.43, 1.9, 1.22, 
         3.82, 3.51, 0.893, 1.763, 1.85, 0.22, 1.04, 2.011, 2.1, 0.89)


lower <- c(0.249, 0.84, 0.33, 1.30, 1.09, 1.39, 0.727, 1.47, 1.3, 0.70, 
           1.25, 1.19, 0.549, 0.94, 1.09, 0.04, 0.57, 0.999, 1.4, 0.45)

upper <- c(1.153, 6.62, 1.79, 3.95, 2.04, 2.73, 1.902, 4.01, 3.0, 2.12, 
           11.69, 10.40, 1.45, 3.31, 3.15, 1.05, 1.92, 4.051, 3.1, 1.75)

data <- data.frame(study, aor, lower, upper)


data$se <- (log(data$upper) - log(data$lower)) / (2 * 1.96)


meta_analysis <- metagen(
  TE = log(aor),
  seTE = data$se,
  studlab = study,
  data = data,
  sm = "OR",
  common = FALSE,
  random = TRUE,
  method.tau = "REML",
  method.random.ci = TRUE,
  prediction = TRUE,
  title = "Meta-analysis of factors influencing the prevalence of depression in diabetic patients by gender"
)


summary(meta_analysis)

forest(meta_analysis, 
       sortvar = TE,
       prediction = TRUE, 
       print.tau2 = TRUE,
       leftlabs = c("study", "AOR", "95% CI"),
       xlab = "AOR for depression risk in women compared to men",
       weight.study = "random",
       col.diamond = "blue",
       col.predict = "red")
