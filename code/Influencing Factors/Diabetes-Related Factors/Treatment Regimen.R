
if (!require("pacman")) install.packages("pacman")
pacman::p_load(metafor, dplyr, tidyr)


data_insulin <- data.frame(
  study = c("Hesham Abuhegazy2022", "Mohamed Ebrahim2021"),
  aor = c(1.92, 1.08),
  lower = c(1.08, 0.64),
  upper = c(3.40, 1.83)
)


data_insulin <- data_insulin %>%
  mutate(
    lower = as.numeric(gsub("，", ".", gsub("（", "", lower))),
    upper = as.numeric(gsub("）", "", gsub("，", ".", upper))),
    log_aor = log(aor),
    se = (log(upper) - log(lower)) / (2 * qnorm(0.975))  
  )


res_insulin <- rma(
  yi = log_aor,
  sei = se,
  data = data_insulin,
  method = "REML"
)


results_insulin <- data.frame(
  factors = "insulintherapy",
  Numberofliteratureincluded = nrow(data_insulin),
  `I2(%)` = paste0(round(res_insulin$I2, 1), "%"),
  HeterogeneityP-value = ifelse(res_insulin$QEp < 0.001, "<0.001", round(res_insulin$QEp, 3)),
  effectmodel = "random",
  `AOR(95%CI)` = sprintf("%.2f (%.2f~%.2f)", 
                           exp(res_insulin$b), 
                           exp(res_insulin$ci.lb), 
                           exp(res_insulin$ci.ub)),
  P值 = ifelse(res_insulin$pval < 0.001, "<0.001", round(res_insulin$pval, 3))
)


data_both <- data.frame(
  study = c("Hesham Abuhegazy2022", "Mohamed Ebrahim2021"),
  aor = c(1.65, 1.33),
  lower = c(0.92, 1.13),
  upper = c(2.95, 2.80)
)


data_both <- data_both %>%
  mutate(
    lower = as.numeric(gsub("，", ".", gsub("（", "", lower))),
    upper = as.numeric(gsub("）", "", gsub("，", ".", upper))),
    log_aor = log(aor),
    se = (log(upper) - log(lower)) / (2 * qnorm(0.975))
  )


res_both <- rma(
  yi = log_aor,
  sei = se,
  data = data_both,
  method = "REML"
)


results_both <- data.frame(
  factors = "Insulin+oralmedication",
  Numberofliteratureincluded = nrow(data_both),
  `I2(%)` = paste0(round(res_both$I2, 1), "%"),
  HeterogeneityP-value = ifelse(res_both$QEp < 0.001, "<0.001", round(res_both$QEp, 3)),
  Effectmodel = "random",
  `AOR(95%CI)` = sprintf("%.2f (%.2f~%.2f)", exp(res_both$b), exp(res_both$ci.lb), exp(res_both$ci.ub)),
  P-value = ifelse(res_both$pval < 0.001, "<0.001", round(res_both$pval, 3))
)


final_results <- rbind(results_insulin, results_both)
colnames(final_results) <- c("factors", "Numberofliteratureincluded", "I2(%)", "HeterogeneityP-value", "Effectmodel", "AOR(95%CI)", "P-value")
print(final_results)


forest(res_insulin, 
       slab = data_insulin$study,
       xlab = "AOR (95% CI)",
       mlab = "Insulin treatment (REML + Knapp-Hartung)",
       transf = exp,
       refline = 1,
       header = c("study", "AOR [95% CI]"))


forest(res_both, 
       slab = data_both$study,
       xlab = "AOR (95% CI)",
       mlab = "Insulin+oralmedication (REML)",
       transf = exp,
       refline = 1,
       header = c("study", "AOR [95% CI]"))