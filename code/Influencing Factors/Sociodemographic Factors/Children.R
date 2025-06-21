
library(meta)
library(dplyr)


data <- data.frame(
  study = c("Gedion Asnake Azeze2020", "anteneh Messele Birhanu2016"),
  AOR = c(3.32, 0.88),
  lower = c(1.88, 0.41),
  upper = c(5.86, 1.88)
)


print(data)


meta_result <- metagen(
  TE = log(data$AOR),   
  lower = log(data$lower),
  upper = log(data$upper),
  studlab = data$study,
  sm = "OR",
  method.tau = "REML",
  random = TRUE,
  backtransf = TRUE     
)


summary(meta_result)


forest(meta_result,
       leftcols = c("studlab", "effect", "ci"),
       leftlabs = c("study", "AOR", "95%CI"),
       xlab = "(AOR)",
       col.square = "#2E8B57",
       col.diamond = "#B22222",
       fontsize = 10)

results_table <- data.frame(
  factors = "Withchildren (vs.withoutchildren)",
  Numberofliteratureincluded = nrow(data),
  `I²(%)` = sprintf("%.1f", meta_result$I2 * 100),
  `P-valueforheterogeneity` = ifelse(meta_result$pval.Q < 0.001, "<0.001", 
                   format.pval(meta_result$pval.Q, digits = 2)),
  effectmodel = "random",
  `AOR(95%CI)` = sprintf("%.2f (%.2f~%.2f)", 
                           exp(meta_result$TE.random),
                           exp(meta_result$lower.random),
                           exp(meta_result$upper.random)),
  `P-value` = ifelse(meta_result$pval.random < 0.001, "<0.001",
                format.pval(meta_result$pval.random, digits = 2)),
  check.names = FALSE
)


print(results_table, row.names = FALSE)
