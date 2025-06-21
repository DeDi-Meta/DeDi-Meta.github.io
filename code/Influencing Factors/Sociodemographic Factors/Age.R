
library(meta)
library(metafor)
library(ggplot2)
library(dplyr)


study <- c("Sheikh Mohammed Shariful Islam2015", "Rehanguli Maimaitituerxun2023", "Nhu Minh Hang Tran2021")
aor <- c(2.1, 1.27, 2.64)
lower_ci <- c(1.2, 0.74, 1.38)
upper_ci <- c(3.6, 2.18, 5.03)
older_total <- c(83, 271, 134)
older_depressed <- c(37, 77, 22)
younger_total <- c(432, 225, 82)
younger_depressed <- c(282, 58, 28)
older_prevalence <- c(0.445800, 0.284133, 0.164179)
younger_prevalence <- c(0.652800, 0.257778, 0.341463)


data <- data.frame(
  study = study,
  aor = aor,
  lower_ci = lower_ci,
  upper_ci = upper_ci,
  older_total = older_total,
  older_depressed = older_depressed,
  younger_total = younger_total,
  younger_depressed = younger_depressed,
  older_prevalence = older_prevalence,
  younger_prevalence = younger_prevalence
)

data$logaor <- log(data$aor)
data$se <- (log(data$upper_ci) - log(data$lower_ci)) / (2 * 1.96)

meta_result <- metagen(
  TE = logaor,
  seTE = se,
  studlab = study,
  data = data,
  sm = "OR",
  method.tau = "REML",
  method.random.ci = FALSE,
  prediction = TRUE,
  common = FALSE,
  random = TRUE
)


summary(meta_result)

forest(meta_result, 
       leftlabs = c("Study", "AOR", "95% CI"),
       xlab = "Adjusted Odds Ratio",
       rightlabs = c("AOR", "95% CI", "Weight"),
       fontsize = 10,
       squaresize = 0.5,
       plotwidth = "7cm",
       colgap = "1mm",
       col.square = "blue",
       col.diamond = "red",
       col.predict = "darkred",
       prediction = TRUE,
       digits = 2)


print("Heterogeneity Test:")
print(paste("I² =", round(meta_result$I2 * 100, 1), "%"))
print(paste("Chi² =", round(meta_result$Q, 2), ", df =", meta_result$df.Q, ", p =", round(meta_result$pval.Q, 4)))
print(paste("Tau² =", round(meta_result$tau2, 4)))
