
if (!require("meta")) install.packages("meta")
if (!require("metafor")) install.packages("metafor")
library(meta)
library(metafor)


studies <- c(
  "Mohamed Abd-Elgawad2023",
  "Eva O. Melin2017",
  "Thitiphan Thaneerat2009"
)


aor <- c(3.15, 11.0, 6.35)
lower <- c(2.21, 2.5, 2.91)
upper <- c(4.52, 47.7, 13.86)


data <- data.frame(
  study = studies,
  TE = log(aor),        
  lower = log(lower),   
  upper = log(upper)
)


meta_result <- metagen(TE = TE,
                       lower = lower,
                       upper = upper,
                       studlab = study,
                       data = data,
                       sm = "OR",       
                       method.tau = "REML",  
                       method.random.ci = TRUE,     
                       title = "Meta-analysis of Anxiety as Risk Factor")


summary(meta_result)

forest(meta_result,
       sortvar = TE,
       leftcols = c("studlab", "TE", "ci"),
       leftlabs = c("Study", "log(AOR)", "95% CI"),
       xlab = "Adjusted Odds Ratio",
       col.square = "blue",
       col.diamond = "red")

