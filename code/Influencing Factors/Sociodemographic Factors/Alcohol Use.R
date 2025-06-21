
library(meta)
library(dplyr)


data <- data.frame(
  study = c("Habtamu Birhanu2022", "Tania Dehesh2020", 
            "Makda Abate Belew2023", "Weijun Zhang2015"),
  AOR = c(3.55, 1.11, 1.41, 0.65),
  lower = c(1.20, 0.48, 0.88, 0.30),
  upper = c(10.52, 1.64, 2.25, 1.42)
)


print(data)


meta_result <- metagen(TE = log(data$AOR),  
                       lower = log(data$lower),
                       upper = log(data$upper),
                       studlab = data$study,
                       sm = "OR",            
                       method.tau = "REML",
                       method.random.ci = TRUE,
                       random = TRUE,   
                       backtransf = TRUE)   


summary(meta_result)

forest(meta_result, 
       leftcols = c("studlab", "TE", "ci"), 
       leftlabs = c("Study", "AOR", "95% CI"),
       xlab = "Adjusted Odds Ratio (AOR)",
       col.square = "blue")


results_table <- data.frame(
  `factors` = "alcoholconsumption",
  `Numberofliteratureincluded` = nrow(data),
  `I²(%)` = round(meta_result$I2*100, 1),
  `HeterogeneityP-value` = ifelse(meta_result$pval.Q < 0.001, "<0.001", 
                   round(meta_result$pval.Q, 3)),
  `effectmodel` = "random",
  `AOR(95%CI)` = sprintf("%.2f (%.2f~%.2f)", 
                           exp(meta_result$TE.random), 
                           exp(meta_result$lower.random), 
                           exp(meta_result$upper.random)),
  `P-value` = ifelse(meta_result$pval.random < 0.001, "<0.001", 
                round(meta_result$pval.random, 3)),
  check.names = FALSE
)

print(results_table, row.names = FALSE)