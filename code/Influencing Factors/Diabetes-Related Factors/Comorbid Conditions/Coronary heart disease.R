
install.packages("metafor")  
library(metafor)


data <- data.frame(
  study = c("Mihyun Jeong2021", 
            "Kankana Karpha2022",
            "Eva O. Melin2017", 
            "Allan Oliver Dampil2019"),
  aor = c(1.76, 5.646, 2.6, 0.483),
  lower_ci = c(0.96, 1.923, 0.3, 0.129),
  upper_ci = c(3.23, 16.577, 22.6, 1.814)
)


data$log_aor <- log(data$aor)
data$se <- (log(data$upper_ci) - log(data$lower_ci)) / (2 * qnorm(0.975))


meta_result <- rma(
  yi = log_aor, 
  sei = se, 
  data = data, 
  method = "REML",
  test = "knha"  
)


results <- data.frame(
  AOR = exp(meta_result$b),
  lower_95CI = exp(meta_result$ci.lb),
  upper_95CI = exp(meta_result$ci.ub),
  p_value = meta_result$pval,
  I2 = meta_result$I2,
  tau2 = meta_result$tau2
)


cat("Meta-analysis results: \n")
print(results, row.names = FALSE)


forest(meta_result, 
       slab = data$study, 
       atransf = exp,
       xlab = "(AOR)",
       main = "Effect of combined coronary heart disease on the prevalence of depression in diabetic patients")