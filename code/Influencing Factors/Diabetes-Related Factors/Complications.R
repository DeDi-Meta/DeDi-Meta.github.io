
install.packages("metafor")  
library(metafor)


data <- data.frame(
  study = c("Mohamed Abd-Elgawad2023", 
            "Seid Yimam Ali2023",
            "Abdullahi S. Aminu2017", 
            "Muhammad Atif2018",
            "anteneh Messele Birhanu2016",
            "Tania Dehesh2020",
            "Mohamed Ebrahim2021",
            "Makda Abate Belew2023",
            "Lili Husniati Yaacob2012",
            "Weijun Zhang2015"),
  aor = c(1.30, 2.02, 2.62, 1.199, 1.65, 1.06, 1.78, 0.75, 3.09, 1.12),
  lower_ci = c(0.62, 1.09, 1.21, 0.338, 0.88, 0.85, 0.83, 0.44, 1.47, 0.94),
  upper_ci = c(2.80, 3.74, 5.67, 4.260, 3.08, 1.31, 3.83, 1.27, 6.59, 1.34)
)


data$lower_ci <- as.numeric(gsub("，", ".", data$lower_ci))
data$upper_ci <- as.numeric(gsub("，", ".", data$upper_ci))


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


print(results, row.names = FALSE)


forest(meta_result, 
       slab = data$study, 
       atransf = exp,
       xlab = "(AOR)",
       main = "The impact of diabetes complications on the prevalence of depression")