
library(metafor)


data <- data.frame(
  study = c("Shahad Abduljalil Abualhamael2024", 
            "Tania Dehesh2020",
            "Malgorzata Gorska-Ciebiada2014",
            "Weijun Zhang2015",
            "Ashmita Karki2024"),
  
  aor = c(1.078, 1.09, 3.74, 1.02, 1.55),  
  
  lower_ci = c(0.502, 0.63, 1.56, 0.45, 0.82),  
  
  upper_ci = c(2.316, 1.13, 8.91, 2.32, 2.93)   
)

data$log_aor <- log(data$aor)
data$se <- (log(data$upper_ci) - log(data$lower_ci)) / (2 * qnorm(0.975))


result <- rma(
  yi = log_aor,
  sei = se,
  data = data,
  method = "REML",
  slab = study
)


forest(result, 
       atransf = exp,
       header = "Study",
       xlab = "Adjusted Odds Ratio (AOR)",
       mlab = "Random Effects Model")


title("Meta-analysis of Smoking on Depression Risk in Diabetics")


summary <- data.frame(
  Combined_AOR = exp(result$beta),
  CI_lower = exp(result$ci.lb),
  CI_upper = exp(result$ci.ub),
  P_value = result$pval,
  I2 = result$I2,
  P_heterogeneity = result$QEp
)


print(summary, row.names = FALSE)