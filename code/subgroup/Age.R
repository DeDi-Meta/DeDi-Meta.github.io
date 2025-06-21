install.packages(c("meta", "metafor", "ggplot2"))
library(meta)
library(metafor)
library(ggplot2)

data <- data.frame(
  Study = c(
    "Hesham Abuhegazy2022", "Seid Yimam Ali2023", "Gedion Asnake Azeze2020",
    "anteneh Messele Birhanu2016", "Habtamu Birhanu2022", "Ashmita Karki2024",
    "Kabtamu Nigussie2023", "Avinash K. Sunny2019", "Weijun Zhang2015",
    "Hesham Abuhegazy2022", "Seid Yimam Ali2023", "Gedion Asnake Azeze2020",
    "anteneh Messele Birhanu2016", "Habtamu Birhanu2022", "Ashmita Karki2024",
    "Rehanguli Maimaitituerxun2023", "Kabtamu Nigussie2023", "Avinash K. Sunny2019",
    "Weijun Zhang2015", "Hesham Abuhegazy2022", "Seid Yimam Ali2023",
    "Gedion Asnake Azeze2020", "anteneh Messele Birhanu2016", "Habtamu Birhanu2022",
    "Sheikh Mohammed Shariful Islam2015", "Mihyun Jeong2021", "Ashmita Karki2024",
    "Rehanguli Maimaitituerxun2023", "Makda Abate Belew2023", "Kabtamu Nigussie2023",
    "Avinash K. Sunny2019", "Nhu Minh Hang Tran2021", "Weijun Zhang2015"
  ),
  Age_Group = rep(c("<40", "40-60", "≥60"), times = c(9, 10, 14)),
  Total_n = c(
    25, 77, 94, 161, 111, 48, 63, 33, 26,
    152, 127, 249, 201, 127, 287, 271, 215, 170, 165,
    173, 59, 67, 53, 72, 83, 1121, 146, 225, 151, 157, 75, 134, 221
  ),
  Depression_n = c(
    16, 25, 24, 24, 33, 11, 13, 2, 17,
    53, 64, 72, 31, 51, 66, 58, 90, 38, 39,
    59, 35, 24, 9, 45, 37, 115, 46, 77, 71, 92, 23, 22, 86
  )
)

meta_analysis <- metaprop(
  event = Depression_n,
  n = Total_n,
  data = data,
  subgroup = Age_Group,
  method.tau = "REML",  
  sm = "PFT",        
  common = FALSE,  
  random = TRUE,
  method.random.ci = TRUE,  
)

forest(meta_analysis,
       sortvar = TE,
       leftcols = c("studlab", "event", "n", "effect", "ci"),
       leftlabs = c("Study", "Depression", "Total", "Prevalence", "95% CI"),
       rightcols = FALSE,
       xlab = "Depression Prevalence (logit scale)",
       col.subgroup = "black",
       print.subgroup.name = TRUE)

meta_reg <- metareg(meta_analysis, ~ Age_Group)
summary(meta_reg)