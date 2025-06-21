
if (!require("metafor")) install.packages("metafor")
library(metafor)


meta_data <- data.frame(
  study = c("Malgorzata Gorska-Ciebiada2014", 
            "Kankana Karpha2022",
            "Steven M. Kogan2007",
            "Avinash K. Sunny2019",
            "Weijun Zhang2015"),
  aor = c(2.01, 3.596, 2.11, 9.11, 1.93),          
  ci_lower = c(1.26, 1.249, 0.68, 2.98, 0.99),     
  ci_upper = c(3.2, 10.351, 6.56, 27.81, 3.82)     
)


meta_data$log_aor <- log(meta_data$aor)
meta_data$se <- (log(meta_data$ci_upper) - log(meta_data$ci_lower)) / (2 * qnorm(0.975))


result <- rma(yi = log_aor, sei = se, data = meta_data, method = "REML")



forest(result, 
       slab = meta_data$study, 
       xlab = "AOR(Insulin use vs unused)", 
       mlab = "random effects model(REML)",
       header = "study",
       transf = exp,         
       refline = 1,          
       xlim = c(0.1, 30),    
       at = c(0.5, 1, 2, 5, 10, 20))  