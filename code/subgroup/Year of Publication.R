
library(meta)
library(metafor)
library(dplyr)
library(ggplot2)

data <- data.frame(
  Study = c("Steven M. Kogan2007", "Nelda Mier2008①+M", "Nelda Mier2008②+A", 
            "Thitiphan Thaneerat2009", "Malgorzata Gorska-Ciebiada2014", 
            "Sheikh Mohammed Shariful Islam2015", "Firdous Jahan2011", 
            "Lili Husniati Yaacob2012", "Waleed M Sweileh2014", 
            "Weijun Zhang2015", "Abdullahi S. Aminu2017", "Muhammad Atif2018", 
            "Gedion Asnake Azeze2020", "anten eh Messele Birhanu2016", 
            "Tania Dehesh2020", "Nigus Alemnew Engidaw2020", "Annie C. H. Fung2018", 
            "Nuket Bayram Kayar2017", "Eva O. Melin2017", "Avinash K. Sunny2019", 
            "Allan Oliver Dampil2019", "Yiting Wang2016", "Mohamed Abd-Elgawad2023", 
            "Shahad Abduljalil Abualhamael2024", "Hesham Abuhegazy2022", 
            "Seid Yimam Ali2023", "Habtamu Birhanu2022", "Mohamed Ebrahim2021", 
            "Mohamed Hassan Elnaem2025", "Mihyun Jeong2021", "Ashmita Karki2024", 
            "Kankana Karpha2022", "Rehanguli Maimaitituerxun2023", 
            "Makda Abate Belew2023", "Nur Adam Mohamed2024", "Mussa R. Mussa2023", 
            "Kabtamu Nigussie2023", "Hina Sharif2023", "Nhu Minh Hang Tran2021"),
  Year_Group = c(rep("Before2020", 22), rep("After2020", 17)),
  Total_n = c(200, 200, 172, 250, 276, 515, 320, 260, 294, 412, 200, 400, 
              410, 415, 1500, 403, 325, 154, 148, 278, 476, 2182, 679, 251, 
              350, 263, 310, 401, 606, 1529, 481, 152, 496, 426, 360, 267, 
              416, 493, 216),
  Depression_n = c(72, 81, 67, 70, 82, 319, 56, 54, 120, 142, 75, 270, 
                   120, 64, 885, 86, 42, 28, 17, 63, 386, 256, 232, 124, 
                   128, 124, 129, 196, 343, 149, 123, 60, 135, 203, 161, 
                   194, 176, 152, 50)
)

data <- data %>%
  mutate(
    Prevalence = Depression_n / Total_n,
    SE = sqrt((Prevalence * (1 - Prevalence)) / Total_n),
    CI_lower = Prevalence - 1.96 * SE,
    CI_upper = Prevalence + 1.96 * SE
  )

head(data)

meta_subgroup <- metaprop(
  event = Depression_n,
  n = Total_n,
  studlab = Study,
  data = data,
  sm = "PFT",
  method = "Inverse",
  method.tau = "REML",
  random = TRUE,
  common = FALSE,
  prediction = TRUE,
  subgroup = Year_Group,
  method.random.ci = TRUE,
  subgroup.name = "Publication Year"
)

summary(meta_subgroup)

forest(meta_subgroup, 
       leftcols = c("studlab", "event", "n", "Year_Group"),
       leftlabs = c("Study", "Cases", "Total", "Year Group"),
       xlab = "Prevalence of Depression",
       smlab = "Prevalence (95% CI)",
       col.square = "blue",
       col.diamond = "red",
       col.predict = "darkred",
       print.I2 = TRUE,
       print.tau2 = TRUE,
       print.pval.Q = TRUE,
       print.subgroup.labels = TRUE,
       bylab = "Publication Year")
