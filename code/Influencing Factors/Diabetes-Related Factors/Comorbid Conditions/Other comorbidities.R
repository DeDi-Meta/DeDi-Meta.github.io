
install.packages("metafor")  
library(metafor)


data <- data.frame(
  study = c("Abdullahi S. Aminu2017", 
            "anteneh Messele Birhanu2016",
            "Mohamed Ebrahim2021", 
            "Annie C. H. Fung2018"),
  aor = c(1.32, 0.84, 1.52, 2.853),
  lower_ci = c(0.64, 0.29, 0.4, 1.354),
  upper_ci = c(2.72, 2.39, 3.14, 6.013)
)


data$log_aor <- log(data$aor)
data$se <- (log(data$upper_ci) - log(data$lower_ci)) / (2 * qnorm(0.975))


meta_result <- rma(yi = log_aor, 
                   sei = se, 
                   data = data, 
                   method = "REML")


summary_result <- data.frame(
  AOR = exp(meta_result$b),
  lower_95CI = exp(meta_result$ci.lb),
  upper_95CI = exp(meta_result$ci.ub),
  p_value = meta_result$pval,
  I2 = meta_result$I2,
  tau2 = meta_result$tau2
)


print(summary_result, row.names = FALSE)


forest(meta_result, 
       slab = data$study, 
       atransf = exp,
       xlab = "(AOR)",
       main = "Meta-analysis: the effect of co-morbidities on the prevalence of depression in diabetic patients")