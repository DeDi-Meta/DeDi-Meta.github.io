
if (!require("pacman")) install.packages("pacman")
pacman::p_load(metafor, dplyr, tidyr)


data <- data.frame(
  study = c("anteneh Messele Birhanu2016", 
            "Nigus Alemnew Engidaw2020",
            "Makda Abate Belew2023"),
  aor = c(1.23, 2.60, 1.22),
  lower = c(0.58, 1.26, 0.48),
  upper = c(2.61, 5.37, 3.13)
)

data <- data %>%
  mutate(
    lower = as.numeric(gsub("，", ".", gsub("（", "", lower))),
    upper = as.numeric(gsub("）", "", gsub("，", ".", upper))),
    log_aor = log(aor),
    se = (log(upper) - log(lower)) / (2 * qnorm(0.975)) 
  )


res <- rma(
  yi = log_aor,
  sei = se,
  data = data,
  method = "REML"
)


results <- data.frame(
  factors = "TT2DM (refer to T1DM)",
  Numberofliteratureincluded = nrow(data),
  `I2(%)` = paste0(round(res$I2, 1), "%"),
  HeterogeneityP-value = ifelse(res$QEp < 0.001, "<0.001", round(res$QEp, 3)),
  Effectmodel = "random",
  `AOR(95%CI)` = sprintf("%.2f (%.2f~%.2f)", exp(res$b), exp(res$ci.lb), exp(res$ci.ub)),
  P-value = ifelse(res$pval < 0.001, "<0.001", round(res$pval, 3))
)


colnames(results) <- c("factors", "Numberofliteratureincluded", "I2(%)", "HeterogeneityP-value", "Effectmodel", "AOR(95%CI)", "P-value")
print(results)


forest(res, 
       slab = data$study,
       xlab = "AOR (95% CI)",
       mlab = "random effects model (REML + Knapp-Hartung)",
       transf = exp,
       refline = 1,
       header = c("study", "AOR [95% CI]"),
       cex = 0.8)