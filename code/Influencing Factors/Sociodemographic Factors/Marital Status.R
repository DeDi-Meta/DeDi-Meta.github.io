
if(!require(meta)) install.packages("meta")
if(!require(metafor)) install.packages("metafor")
if(!require(ggplot2)) install.packages("ggplot2")
library(meta)
library(metafor)
library(ggplot2)


study <- c("Muhammad Atif2018", "anteneh Messele Birhanu2016", 
           "Malgorzata Gorska-Ciebiada2014", "Weijun Zhang2015")


aor <- c(1.075, 1.11, 1.93, 2.51)
lower_ci <- c(0.480, 0.52, 1.23, 1.29)
upper_ci <- c(2.408, 2.36, 3.04, 4.90)


sei <- (log(upper_ci) - log(lower_ci)) / (2 * 1.96)


data <- data.frame(
  study = study,
  aor = aor,
  lower_ci = lower_ci,
  upper_ci = upper_ci,
  sei = sei
)

meta_result <- metagen(
  TE = log(aor),        
  seTE = sei,           
  studlab = study,      
  sm = "OR",            
  method.tau = "REML",    
  method.random.ci = FALSE,          
  title = "Meta-analysis of the effect of marital status on depression in diabetic patients",
  common = TRUE,    
  random = TRUE    
)


summary(meta_result)

forest(meta_result, 
       leftcols = c("studlab"),
       rightcols = c("effect", "ci", "w.random"),
       leftlabs = c("study"),
       rightlabs = c("AOR", "95% CI", "weights(%)"),
       xlab = "Adjusted ratio of unmarried vs. married (AOR)",
       fontsize = 10,
       squaresize = 0.8,
       col.diamond = "blue",
       col.square = "darkblue",
       col.study = "black",
       col.random = "red",
       text.random = "Combined effects (random effects model)",
       text.fixed = "Combined effects (fixed effects model)",
       print.I2 = TRUE,
       print.I2.ci = TRUE,
       print.tau2 = TRUE,
       sortvar = TE)
