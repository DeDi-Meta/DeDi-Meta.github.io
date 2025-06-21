
library(meta)
library(dplyr)


data <- data.frame(
  study = c("Nigus Alemnew Engidaw2020", "Hina Sharif2023"),
  
  # 101-125mg/dl 
  aor_101_125 = c(1.308, 1.40),        
  ci_low_101_125 = c(0.53, 0.43),      
  ci_high_101_125 = c(3.19, 2.89),     
  
  # ≥126mg/dl 
  aor_126 = c(2.01, 3.01),             
  ci_low_126 = c(0.83, 1.03),          
  ci_high_126 = c(4.82, 5.02)          
)


run_meta <- function(aor, ci_low, ci_high, study_names, group_name) {
  
  log_aor <- log(aor)
  se <- (log(ci_high) - log(ci_low)) / (2 * qnorm(0.975))
  
  
  meta_result <- metagen(
    TE = log_aor,
    seTE = se,
    studlab = study_names,
    sm = "OR",
    common = FALSE,
    random = TRUE,
    method.tau = "REML"
  )
  
 
  list(
    group = group_name,
    i2 = meta_result$I2,
    p_heterogeneity = meta_result$pval.Q,
    aor_combined = exp(meta_result$TE.random),
    ci_low_combined = exp(meta_result$lower.random),
    ci_high_combined = exp(meta_result$upper.random),
    p_value = meta_result$pval.random
  )
}


result_101_125 <- run_meta(
  data$aor_101_125, data$ci_low_101_125, data$ci_high_101_125,
  data$study, "101-125mg/dl"
)

result_126 <- run_meta(
  data$aor_126, data$ci_low_126, data$ci_high_126,
  data$study, "≥126mg/dl"
)


results_table <- bind_rows(
  result_101_125,
  result_126
) %>%
  mutate(
    meta_result = sprintf("%.2f (%.2f~%.2f)", aor_combined, ci_low_combined, ci_high_combined),
    i2 = sprintf("%.1f%%", i2),
    p_heterogeneity = ifelse(p_heterogeneity < 0.001, "<0.001", sprintf("%.3f", p_heterogeneity)),
    p_value = ifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value))
  ) %>%
  select(
    `FBGgroup` = group,
    `I²` = i2,
    `p_heterogeneity` = p_heterogeneity,
    `AOR (95%CI)` = meta_result,
    `p_value` = p_value
  )


print(results_table)

forest(
  metagen(
    TE = log(data$aor_101_125),
    seTE = (log(data$ci_high_101_125) - log(data$ci_low_101_125)) / (2 * qnorm(0.975)),
    studlab = data$study,
    sm = "OR"
  ),
  leftcols = "studlab",
  leftlabs = "Study",
  xlab = "AOR (95% CI)",
  label.right = "Higher risk",
  col.square = "blue"
)