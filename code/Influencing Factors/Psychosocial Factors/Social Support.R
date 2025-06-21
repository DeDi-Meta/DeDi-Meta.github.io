
if (!require("meta")) install.packages("meta")
if (!require("metafor")) install.packages("metafor")
library(meta)
library(metafor)


studies <- c(
  "Habtamu Birhanu2022",
  "Mohamed Ebrahim2021",
  "Nigus Alemnew Engidaw2020",
  "Kabtamu Nigussie2023",
  "Hina Sharif2023"
)


mod_aor <- c(2.63, 1.90, 1.42, 1.85, 1.42)
mod_lower <- c(1.34, 1.14, 0.66, 0.69, 0.66)
mod_upper <- c(5.16, 3.17, 3.08, 3.05, 3.08)

mod_data <- data.frame(
  study = studies,
  TE = log(mod_aor),
  lower = log(mod_lower),
  upper = log(mod_upper)
)


mod_result <- metagen(
  TE = TE,
  lower = lower,
  upper = upper,
  studlab = study,
  data = mod_data,
  sm = "OR",
  method.tau = "REML",
  method.random.ci = FALSE,
  title = "The effect of moderate social support on depression in diabetes"
)


poor_aor <- c(2.46, 1.79, 3.61, 2.35, 3.41)
poor_lower <- c(1.10, 1.07, 1.76, 1.12, 1.76)
poor_upper <- c(5.49, 2.98, 7.36, 6.03, 6.36)


poor_data <- data.frame(
  study = studies,
  TE = log(poor_aor),
  lower = log(poor_lower),
  upper = log(poor_upper)
)


poor_result <- metagen(
  TE = TE,
  lower = lower,
  upper = upper,
  studlab = study,
  data = poor_data,
  sm = "OR",
  method.tau = "REML",
  method.random.ci = FALSE,
  title = "The effect of low social support on depression in diabetes"
)



forest(mod_result,
       leftcols = c("studlab", "TE", "ci"),
       leftlabs = c("study", "log(AOR)", "95% CI"),
       xlab = "Moderate social supportAOR",
       col.square = "#4B0082",
       col.diamond = "#32CD32")


forest(poor_result,
       leftcols = c("studlab", "TE", "ci"),
       leftlabs = c("study", "log(AOR)", "95% CI"),
       xlab = "Poor social supportAOR",
       col.square = "#8A2BE2",
       col.diamond = "#00CED1")
