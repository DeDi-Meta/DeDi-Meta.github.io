
if (!require("metafor")) install.packages("metafor")
library(metafor)


meta_data <- data.frame(
  study = c("Mohamed Abd-Elgawad2023", 
            "Hesham Abuhegazy2022",
            "Firdous Jahan2011",
            "Kankana Karpha2022",
            "Allan Oliver Dampil2019"),
  aor = c(2.25, 2.35, 4.56, 1.801, 1.606),     
  ci_lower = c(1.38, 1.22, 1.71, 0.727, 0.836),  
  ci_upper = c(3.70, 4.53, 12.15, 4.458, 3.083)  
)


meta_data$log_aor <- log(meta_data$aor)
meta_data$se <- (log(meta_data$ci_upper) - log(meta_data$ci_lower)) / (2 * qnorm(0.975))


result <- rma(yi = log_aor, sei = se, data = meta_data, method = "REML")




forest(result, 
       slab = meta_data$study, 
       xlab = "AOR(Diabetic neuropathy vs no neuropathy)", 
       mlab = "random effects model",
       header = "study",
       transf = exp,         
       refline = 1,          
       xlim = c(0.1, 15),    
       at = c(0.5, 1, 2, 4, 8, 15))  