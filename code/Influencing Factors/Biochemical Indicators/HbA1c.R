
library(meta)
library(dplyr)
library(stringr)


# =================================================================
data <- data.frame(
  study = c(
    "Shahad Abduljalil Abualhamael2024",
    "Muhammad Atif2018",
    "Tania Dehesh2020",
    "Annie C. H. Fung2018",
    "Eva O. Melin2017",
    "Weijun Zhang2015"
  ),
  
  # AOR
  aor = c(0.515, 2.67, 1.54, 2.50, 6.4, 1.89),
  
  
  ci_raw = c(
    "0.263,1.009", 
    "1.35,5.29", 
    "1.12–2.03",    
    "1.17,5.32", 
    "1.6,26.0", 
    "1.10,3.53"
  )
) %>%
  
  mutate(
    ci = str_replace_all(ci_raw, "[–−]", ","),  
    ci_low = as.numeric(str_split_fixed(ci, ",", 2)[, 1]),
    ci_high = as.numeric(str_split_fixed(ci, ",", 2)[, 2])
  )

# =================================================================

run_meta <- function(aor, ci_low, ci_high, study_names) {
 
  log_aor <- log(aor)
  se <- (log(ci_high) - log(ci_low)) / (2 * qnorm(0.975))
  
  
  meta_result <- metagen(
    TE = log_aor,
    seTE = se,
    studlab = study_names,
    sm = "OR",
    common = FALSE,
    random = TRUE,
    method.tau = "REML",  
    method.random.ci = TRUE,           
    title = "Elevated HbA1c and risk of depression"
  )
  
  
  list(
    n_studies = length(study_names),
    i2 = meta_result$I2,
    p_heterogeneity = meta_result$pval.Q,
    aor_combined = exp(meta_result$TE.random),
    ci_low_combined = exp(meta_result$lower.random),
    ci_high_combined = exp(meta_result$upper.random),
    p_value = meta_result$pval.random
  )
}

# =================================================================

result <- run_meta(data$aor, data$ci_low, data$ci_high, data$study)


results_table <- data.frame(
  factor = "Elevated HbA1c",
  Numberofliteratureincluded = sprintf("%d [%s]", result$n_studies, paste(1:nrow(data), collapse = ",")),
  heterogeneitytest = sprintf(
    "I²=%.1f%%, P=%s", 
    result$i2,
    ifelse(result$p_heterogeneity < 0.001, "<0.001", sprintf("%.3f", result$p_heterogeneity))
  ),
  effectmodel = "random effects model",
  Meta = sprintf(
    "AOR=%.2f (95%%CI: %.2f~%.2f), P=%s",
    result$aor_combined,
    result$ci_low_combined,
    result$ci_high_combined,
    ifelse(result$p_value < 0.001, "<0.001", sprintf("%.3f", result$p_value))
  )
)


print(results_table, row.names = FALSE)


forest(
  metagen(
    TE = log(data$aor),
    seTE = (log(data$ci_high) - log(data$ci_low)) / (2 * qnorm(0.975)),
    studlab = data$study,
    sm = "OR",
    method.random.ci = TRUE,
    method.tau = "REML"
  ),
  leftcols = c("studlab", "TE", "ci"), 
  leftlabs = c("study", "AOR", "95% CI"),
  xlab = "(AOR)",
  col.square = "#2E75B6",
  col.diamond = "#C00000",
  col.predict = "gray80",
  test.overall = TRUE
)