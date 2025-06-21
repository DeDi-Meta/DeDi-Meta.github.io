
if (!require("meta")) install.packages("meta")
if (!require("metafor")) install.packages("metafor")
library(meta)
library(metafor)


studies <- c(
  "Shahad Abduljalil Abualhamael2024",
  "Hesham Abuhegazy2022",
  "Mohamed Hassan Elnaem2025",
  "Mussa R. Mussa2023",
  "Thitiphan Thaneerat2009",
  "Nhu Minh Hang Tran2021"
)


aor <- c(2.11, 2.14, 4.270, 5.7, 2.85, 2.44)
lower <- c(0.73, 1.01, 2.430, 2.12, 1.30, 1.07)
upper <- c(6.10, 4.53, 7.501, 15.18, 6.23, 5.53)


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
  title = "Meta-analysis of the effect of medication adherence on depression in diabetic patients"
)


summary(meta_result)


forest(meta_result,
       sortvar = TE,
       leftcols = c("studlab", "TE", "ci"),
       leftlabs = c("study", "log(AOR)", "95% CI"),
       xlab = "(AOR)",
       col.square = "#1E90FF",
       col.diamond = "#FF4500")

