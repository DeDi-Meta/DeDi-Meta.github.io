
if (!require("pacman")) install.packages("pacman")
pacman::p_load(metafor, dplyr, tidyr)


data <- data.frame(
  study = c("Nigus Alemnew Engidaw2020", 
            "Mussa R. Mussa2023",
            "Hina Sharif2023"),
  aor = c(4.70, 1.4, 4.70),
  lower = c(1.28, 0.41, 1.28),
  upper = c(17.17, 5.01, 6.17)
)


data <- data %>%
  mutate(
    log_aor = log(aor),
    log_lower = log(lower),
    log_upper = log(upper),
    se = (log_upper - log_lower) / (2 * qnorm(0.975))  
  )


res <- rma(
  yi = log_aor, 
  sei = se, 
  data = data,
  method = "REML"
)


results <- data.frame(
  `AOR` = exp(res$b),
  `95%CI` = paste0("[", round(exp(res$ci.lb), 2), ", ", round(exp(res$ci.ub), 2), "]"),
  `I2` = paste0(round(res$I2, 1), "%"),
  `heterogeneity p-value` = round(res$QEp, 3),
  `Combined P-value` = round(res$pval, 3)
)


print(results)


forest(res, slab = data$study,
       xlab = "AOR (95% CI)",
       mlab = "Random Effects Model",
       transf = exp,
       refline = 1,
       header = "Study")