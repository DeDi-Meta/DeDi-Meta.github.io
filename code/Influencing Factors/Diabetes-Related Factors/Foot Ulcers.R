
install.packages("metafor") 
library(metafor)


data <- data.frame(
  study = c("Mohamed Hassan Elnaem2025", "Eva O. Melin2017"),
  aor = c(2.99, 8.5),
  lower_ci = as.numeric(gsub("，", ".", c("1.82", "1.9"))),  
  upper_ci = as.numeric(gsub("，", ".", c("4.88", "37.4")))
)


data$log_aor <- log(data$aor)
data$se <- (log(data$upper_ci) - log(data$lower_ci)) / (2 * qnorm(0.975))


meta_result <- rma(
  yi = log_aor, 
  sei = se, 
  data = data, 
  method = "REML"
)


results <- data.frame(
  AOR = exp(meta_result$b),
  lower_95CI = exp(meta_result$ci.lb),
  upper_95CI = exp(meta_result$ci.ub),
  p_value = meta_result$pval,
  I2 = meta_result$I2,
  tau2 = meta_result$tau2
)


print(results, row.names = FALSE)


forest(meta_result, 
       slab = data$study, 
       atransf = exp,
       xlab = "Adjusted Odds Ratio (AOR)",
       main = "The effect of diabetic foot ulcers on the prevalence of depression")