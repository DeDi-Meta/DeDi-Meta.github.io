
library(meta)
library(dplyr)


data <- data.frame(
  study = c("Nelda Mier2008②+A", "Avinash K. Sunny2019", 
            "Thitiphan Thaneerat2009", "Weijun Zhang2015"),
  AOR = c(4.16, 8.07, 4.17, 0.94),
  lower = c(1.55, 2.82, 1.99, 0.45),
  upper = c(11.16, 23.07, 8.74, 1.80)
) %>% 
  
  mutate(across(c(lower, upper), ~as.numeric(gsub("，", ".", .x))))


print(data)

meta_result <- metagen(TE = log(data$AOR),   
                       lower = log(data$lower),
                       upper = log(data$upper),
                       studlab = data$study,
                       sm = "OR",
                       method.tau = "REML",
                       random = TRUE,
                       method.random.ci = TRUE,
                       backtransf = TRUE)

summary(meta_result)

forest(meta_result,
       leftcols = c("studlab", "TE", "ci"),
       leftlabs = c("study", "AOR", "95%CI"),
       xlab = "（AOR）",
       col.square = "#4A708B")


results_table <- data.frame(
  factors = "Educational level (≥ high school)",
  Numberofliteratureincluded = nrow(data),
  `I²(%)` = sprintf("%.1f", meta_result$I2*100),
  `HeterogeneityP-value` = ifelse(meta_result$pval.Q < 0.001, "<0.001", 
                   format.pval(meta_result$pval.Q, digits = 2)),
  effectmodel = "random",
  `AOR(95%CI)` = sprintf("%.2f (%.2f~%.2f)", 
                           exp(meta_result$TE.random),
                           exp(meta_result$lower.random),
                           exp(meta_result$upper.random)),
  `P值` = ifelse(meta_result$pval.random < 0.001, "<0.001",
                format.pval(meta_result$pval.random, digits = 2)),
  check.names = FALSE
)


print(results_table, row.names = FALSE)
