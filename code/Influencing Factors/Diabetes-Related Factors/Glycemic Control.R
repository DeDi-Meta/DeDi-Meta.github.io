
install.packages("metafor") 
library(metafor)


data <- data.frame(
  study = c("Seid Yimam Ali2023", 
            "Gedion Asnake Azeze2020",
            "anteneh Messele Birhanu2016",
            "Allan Oliver Dampil2019"),
  aor = c(1.93, 6.23, 0.90, 1.444),
  lower_ci = as.numeric(gsub("，", ".", c("1.05", "3.65", "0.47", "0.591"))),
  upper_ci = as.numeric(gsub("，", ".", c("3.53", "10.54", "1.71", "3.523")))
)


data$log_aor <- log(data$aor)
data$log_lower <- log(data$lower_ci)
data$log_upper <- log(data$upper_ci)


data$se <- (data$log_upper - data$log_lower) / (2 * qnorm(0.975))


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


print(results, row.names = FALSE)


forest(meta_result, 
       slab = data$study, 
       atransf = exp,
       xlab = "Adjusted Odds Ratio (AOR)",
       main = "Poor glycemic control(poor vs good)Impact on the prevalence of depression")