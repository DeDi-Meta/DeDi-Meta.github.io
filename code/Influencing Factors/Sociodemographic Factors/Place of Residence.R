
library(meta)
library(dplyr)


data <- data.frame(
  study = c("Abdullahi S. Aminu2017", "anteneh Messele Birhanu2016",
            "Mohamed Hassan Elnaem2025", "Nigus Alemnew Engidaw2020",
            "Ashmita Karki2024", "Hina Sharif2023"),
  AOR = c(1.87, 1.12, 0.548, 0.43, 1.16, 0.43),
  lower = c(0.905, 0.39, 0.342, 0.13, 0.67, 0.13),
  upper = c(3.88, 3.23, 0.880, 1.39, 1.99, 1.39)
) %>% 
  
mutate(across(c(lower, upper), ~as.numeric(gsub("[，\\(\\)]", ".", .x))))

print(data)

meta_result <- metagen(
  TE = log(data$AOR),   
  lower = log(data$lower),
  upper = log(data$upper),
  studlab = data$study,
  sm = "OR",
  method.tau = "REML",
  random = TRUE,
  method.random.ci = TRUE,
  backtransf = TRUE     
)


summary(meta_result)



forest(meta_result,
       leftcols = c("studlab", "effect", "ci"),
       leftlabs = c("study", "AOR", "95%CI"),
       xlab = "rural vs urban（AOR）",
       col.square = "#2E8B57",
       col.diamond = "#B22222",
       fontsize = 10)


results_table <- data.frame(
  factors = "Place of residence (rural vs. urban)",
  Numberofliteratureincluded = nrow(data),
  `I²(%)` = sprintf("%.1f", meta_result$I2 * 100),
  `HeterogeneityP-value` = ifelse(meta_result$pval.Q < 0.001, "<0.001", 
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