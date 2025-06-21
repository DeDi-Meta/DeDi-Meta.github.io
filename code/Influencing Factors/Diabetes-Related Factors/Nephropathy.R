
if (!require("metafor")) install.packages("metafor")
library(metafor)


meta_data <- data.frame(
  study = c("Firdous Jahan2011", 
            "Kankana Karpha2022",
            "Thitiphan Thaneerat2009"),
  aor = c(4.10, 2.165, 2.39),          
  ci_lower = c(1.26, 0.802, 1.14),     
  ci_upper = c(13.33, 5.841, 5.00)     
)


meta_data$log_aor <- log(meta_data$aor)
meta_data$se <- (log(meta_data$ci_upper) - log(meta_data$ci_lower)) / (2 * qnorm(0.975))


result <- rma(yi = log_aor, sei = se, data = meta_data, method = "REML")



forest(result, 
       slab = meta_data$study, 
       xlab = "AOR（Diabetic nephropathy vs no diabetic nephropathy）", 
       mlab = "random effects model",
       header = "study",
       transf = exp,         
       refline = 1,          
       xlim = c(0.1, 15),    
       at = c(0.5, 1, 2, 4, 8, 15))  