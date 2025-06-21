
if (!require("pacman")) install.packages("pacman")
pacman::p_load(metafor, dplyr, tidyr)


data <- data.frame(
  study = c("Mohamed Abd-Elgawad2023", 
            "Gedion Asnake Azeze2020",
            "Kankana Karpha2022",
            "Mussa R. Mussa2023",
            "Allan Oliver Dampil2019"),
  aor = c(1.10, 2.13, 5.521, 2.8, 2.361),
  lower = c(0.67, 0.81, 2.193, 1.45, 1.158),
  upper = c(1.81, 5.59, 13.903, 5.28, 4.813)
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
  method = "REML",
  test = "knha"  
)


results <- data.frame(
  Numberofliteratureincluded = nrow(data),
  `I2(%)` = paste0(round(res$I2, 1), "%"),
  HeterogeneityP-value = ifelse(res$QEp < 0.001, "<0.001", round(res$QEp, 3)),
  Effectmodel = "Random",
  AOR = sprintf("%.2f (%.2f~%.2f)", exp(res$b), exp(res$ci.lb), exp(res$ci.ub)),
  P-value = ifelse(res$pval < 0.001, "<0.001", round(res$pval, 3))
)


print(results)


forest(res, 
       slab = data$study,
       xlab = "AOR (95% CI)",
       mlab = "Random Effects Model ",
       transf = exp,
       refline = 1,
       header = c("Study", "AOR [95% CI]"))