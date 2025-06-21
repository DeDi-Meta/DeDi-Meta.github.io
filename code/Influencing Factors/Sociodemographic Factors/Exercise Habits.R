
if(!require(meta)) install.packages("meta")
if(!require(metafor)) install.packages("metafor")
if(!require(ggplot2)) install.packages("ggplot2")


library(meta)
library(metafor)
library(ggplot2)

data <- data.frame(
  study = c("Shahad Abduljalil Abualhamael2024", "anteneh Messele Birhanu2016", 
            "Habtamu Birhanu2022", "Tania Dehesh2020", 
            "Mohamed Hassan Elnaem2025", "Malgorzata Gorska-Ciebiada2014", 
            "Nur Adam Mohamed2024"),
  logor = c(log(1.08), log(1.33), log(1.68), log(1.64), 
            log(0.770), log(3.1), log(1.79)),
  se = c((log(2.34) - log(0.50))/(2*1.96), 
         (log(3.45) - log(0.52))/(2*1.96), 
         (log(3.00) - log(0.94))/(2*1.96), 
         (log(2.22) - log(1.18))/(2*1.96), 
         (log(1.189) - log(0.499))/(2*1.96), 
         (log(5.02) - log(1.92))/(2*1.96), 
         (log(2.79) - log(1.14))/(2*1.96))
)


print(data)

meta_result <- metagen(TE = logor,
                       seTE = se,
                       studlab = study,
                       data = data,
                       sm = "OR",
                       method.tau = "REML",
                       method.random.ci = TRUE,
                       title = "Effect of regular exercise on the risk of depression in diabetic patients")

print(meta_result)
summary(meta_result)

forest(meta_result, 
       leftlabs = c("study", "OR", "95% CI"),  
       xlab = "(AOR)",
       text.random = "random effects model",
       text.common = "fixed effects model",  
       digits = 2,
       print.tau2 = TRUE,
       print.I2 = TRUE,
       print.pval.Q = TRUE)


funnel(meta_result, 
       studlab = TRUE, 
       xlab = "(OR)")