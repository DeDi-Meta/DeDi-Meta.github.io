
if (!require("meta")) install.packages("meta")
if (!require("metafor")) install.packages("metafor")
library(meta)
library(metafor)


studies <- c(
  "Mihyun Jeong2021",
  "Mussa R. Mussa2023",
  "Nhu Minh Hang Tran2021"
)


aor <- c(6.98, 4.4, 11.18)
lower <- c(4.27, 2.44, 2.18)
upper <- c(11.40, 8.10, 57.33)


data <- data.frame(
  study = studies,
  TE = log(aor),        
  lower = log(lower),   
  upper = log(upper)
)


meta_result <- metagen(
  TE = TE,
  lower = lower,
  upper = upper,
  studlab = study,
  data = data,
  sm = "OR",
  method.tau = "REML",  
  method.random.ci = FALSE,          
  title = "The effect of high stress on depression in diabetes"
)


summary(meta_result)

forest(meta_result,
       leftcols = c("studlab", "TE", "ci"),
       leftlabs = c("study", "log(AOR)", "95% CI"),
       xlab = "high stress AOR",
       col.square = "#6A5ACD",
       col.diamond = "#FF6347",
       xlim = c(0.1, 100))  

