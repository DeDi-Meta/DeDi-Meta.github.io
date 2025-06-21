
install.packages("metafor")  
library(metafor)


data <- data.frame(
  study = c("anteneh Messele Birhanu2016", 
            "Mohamed Hassan Elnaem2025",
            "Makda Abate Belew2023"),
  
  
  aor_5_10 = c(0.77, 0.84, 1.11),
  lower_5_10 = as.numeric(gsub("，", ".", c("0.41", "0.22", "0.68"))),  
  upper_5_10 = as.numeric(gsub("，", ".", c("1.43", "3.22", "1.82"))),
  
 
  aor_over10 = c(0.27, 0.31, 1.51),
  lower_over10 = as.numeric(gsub("，", ".", c("0.07", "0.16", "0.85"))),
  upper_over10 = as.numeric(gsub("，", ".", c("0.92", "0.59", "2.71")))
)


run_meta <- function(aor, lower, upper, group_name) {
  
  log_aor <- log(aor)
  se <- (log(upper) - log(lower)) / (2 * qnorm(0.975))
  
  
  meta_result <- rma(
    yi = log_aor, 
    sei = se, 
    method = "REML",
    test = "knha"
  )
  
  
  results <- data.frame(
    Group = group_name,
    AOR = exp(meta_result$b),
    lower_95CI = exp(meta_result$ci.lb),
    upper_95CI = exp(meta_result$ci.ub),
    p_value = meta_result$pval,
    I2 = meta_result$I2,
    tau2 = meta_result$tau2
  )
  
  
  forest(meta_result, 
         slab = data$study, 
         atransf = exp,
         xlab = "Adjusted Odds Ratio (AOR)",
         main = paste("Duration of diabetes", group_name, "group on the prevalence of depression"))
  
  return(results)
}


results_5_10 <- run_meta(
  aor = data$aor_5_10,
  lower = data$lower_5_10,
  upper = data$upper_5_10,
  group_name = "5-10 years"
)


results_over10 <- run_meta(
  aor = data$aor_over10,
  lower = data$lower_over10,
  upper = data$upper_over10,
  group_name = ">10 years"
)


final_results <- rbind(results_5_10, results_over10)

print(final_results, row.names = FALSE)