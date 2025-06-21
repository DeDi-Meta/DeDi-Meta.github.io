install.packages("meta")
library(meta)

data <- data.frame(
  Study = c(
    "Mohamed Abd-Elgawad2023", "Seid Yimam Ali2023", "Gedion Asnake Azeze2020",
    "anteneh Messele Birhanu2016", "Habtamu Birhanu2022", "Mohamed Ebrahim2021",
    "Nigus Alemnew Engidaw2020", "Makda Abate Belew2023", "Nur Adam Mohamed2024",
    "Mussa R. Mussa2023", "Kabtamu Nigussie2023", "Shahad Abduljalil Abualhamael2024",
    "Hesham Abuhegazy2022", "Abdullahi S. Aminu2017", "Muhammad Atif2018",
    "Tania Dehesh2020", "Mohamed Hassan Elnaem2025", "Annie C. H. Fung2018",
    "Sheikh Mohammed Shariful Islam2015", "Firdous Jahan2011", "Mihyun Jeong2021",
    "Ashmita Karki2024", "Kankana Karpha2022", "Nuket Bayram Kayar2017",
    "Rehanguli Maimaitituerxun2023", "Lili Husniati Yaacob2012", "Hina Sharif2023",
    "Avinash K. Sunny2019", "Waleed M Sweileh2014", "Thitiphan Thaneerat2009",
    "Nhu Minh Hang Tran2021", "Allan Oliver Dampil2019", "Weijun Zhang2015",
    "Malgorzata Gorska-Ciebiada2014", "Eva O. Melin2017", "Steven M. Kogan2007",
    "Nelda Mier2008①+M", "Nelda Mier2008②+A", "Yiting Wang2016"
  ),
  Country_Group = factor(c(
    rep("African", 11), 
    rep("Asian", 22),
    rep("European", 2),
    rep("North America", 4)
  ), levels = c("African", "Asian", "European", "North America")),
  Total_n = c(
    679, 263, 410, 415, 310, 401, 403, 426, 360, 267, 416,
    251, 350, 200, 400, 1500, 606, 325, 515, 320, 1529, 481,
    152, 154, 496, 260, 493, 278, 294, 250, 216, 476, 412,
    276, 148, 200, 200, 172, 2182
  ),
  Depression_n = c(
    232, 124, 120, 64, 129, 196, 86, 203, 161, 194, 176,
    124, 128, 75, 270, 885, 343, 42, 319, 56, 149, 123,
    60, 28, 135, 54, 152, 63, 120, 70, 50, 386, 142,
    82, 17, 72, 81, 67, 256
  )
)

meta_continent <- metaprop(
  event = Depression_n,
  n = Total_n,
  studlab = Study,
  data = data,
  subgroup = Country_Group,  
  method.tau = "REML",       
  sm = "PFT",             
  method.random.ci = TRUE,
  common = FALSE,        
  random = TRUE
)

forest(meta_continent,
       leftcols = c("studlab", "event", "n", "effect", "ci"),
       leftlabs = c("Study", "Depression", "Total", "Prevalence", "95% CI"),
       xlab = "Depression Prevalence (logit scale)",
       col.subgroup = "black",
       print.subgroup.name = TRUE,  
       print.I2 = TRUE,             
       print.tau2 = TRUE)           