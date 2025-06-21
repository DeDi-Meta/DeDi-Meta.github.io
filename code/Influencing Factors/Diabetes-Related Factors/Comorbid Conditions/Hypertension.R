
if (!require("metafor")) install.packages("metafor")
library(metafor)


data <- data.frame(
  study = c("Mohamed Abd-Elgawad2023", "Gedion Asnake Azeze2020", 
            "Tania Dehesh2020", "Annie C. H. Fung2018", "Firdous Jahan2011",
            "Mihyun Jeong2021", "Kankana Karpha2022", "Allan Oliver Dampil2019"),
  AOR = c(1.23, 5.66, 1.39, 3.42, 2.75, 1.30, 0.167, 0.703),
  lower = c(0.84, 3.24, 1.09, 1.38, 0.99, 0.76, 0.065, 0.355),
  upper = c(1.80, 9.86, 2.17, 8.48, 7.37, 2.23, 0.431, 1.395)
)


data$logAOR <- log(data$AOR)
data$SE <- (log(data$upper) - log(data$lower)) / (2 * qnorm(0.975))  
data$vi <- data$SE^2  


meta_result <- rma(
  yi = logAOR,
  vi = vi,
  data = data,
  method = "REML",
  slab = study,
  test = "knha"
)



print(meta_result)


forest(meta_result, 
       header = "study",
       xlab = " (AOR)",
       mlab = "random effects model",
       refline = 1,
       atransf = exp,  
       digits = 2,
       xlim = c(0.1, 20),  
       alim = c(0.1, 10))  


results <- data.frame(
  I2 = paste0(round(meta_result$I2, 1), "%"),
  Q_p = ifelse(meta_result$QEp < 0.001, "<0.001", round(meta_result$QEp, 3)),
  pooled_AOR = sprintf("%.2f (%.2f–%.2f)", 
                       exp(meta_result$b), 
                       exp(meta_result$ci.lb), 
                       exp(meta_result$ci.ub)),
  AOR_p = ifelse(meta_result$pval < 0.001, "<0.001", round(meta_result$pval, 3))
)


print(results, row.names = FALSE)