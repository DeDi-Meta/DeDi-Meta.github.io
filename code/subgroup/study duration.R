
library(meta)
library(metafor)
library(ggplot2)
library(dplyr)

data <- data.frame(
  Study = c("Mohamed Abd-Elgawad2023", "Seid Yimam Ali2023", "Muhammad Atif2018", 
            "Gedion Asnake Azeze2020", "anteneh Messele Birhanu2016", "Habtamu Birhanu2022", 
            "Tania Dehesh2020", "Mohamed Ebrahim2021", "Nigus Alemnew Engidaw2020", 
            "Kankana Karpha2022", "Makda Abate Belew2023", "Nur Adam Mohamed2024", 
            "Kabtamu Nigussie2023", "Hina Sharif2023", "Weijun Zhang2015", 
            "Shahad Abduljalil Abualhamael2024", "Hesham Abuhegazy2022", 
            "Abdullahi S. Aminu2017", "Mohamed Hassan Elnaem2025", "Lili Husniati Yaacob2012", 
            "Mussa R. Mussa2023", "Waleed M Sweileh2014", "Thitiphan Thaneerat2009", 
            "Annie C. H. Fung2018", "Sheikh Mohammed Shariful Islam2015", 
            "Rehanguli Maimaitituerxun2023", "Eva O. Melin2017", "Nelda Mier2008①+M", 
            "Nelda Mier2008②+A", "Avinash K. Sunny2019", "Nhu Minh Hang Tran2021", 
            "Allan Oliver Dampil2019"),
  Month_Group = c(rep("≤3", 15), rep("3-6", 8), rep("＞6", 9)),
  Total_n = c(679, 263, 400, 410, 415, 310, 1500, 401, 403, 152, 426, 360, 416, 493, 412, 
              251, 350, 200, 606, 260, 267, 294, 250, 325, 515, 496, 148, 200, 172, 278, 216, 476),
  Depression_n = c(232, 124, 270, 120, 64, 129, 885, 196, 86, 60, 203, 161, 176, 152, 142, 
                   124, 128, 75, 343, 54, 194, 120, 70, 42, 319, 135, 17, 81, 67, 63, 50, 386)
)

data <- data %>%
  mutate(
    Prevalence = Depression_n / Total_n,
    SE = sqrt((Prevalence * (1 - Prevalence)) / Total_n),
    CI_lower = Prevalence - 1.96 * SE,
    CI_upper = Prevalence + 1.96 * SE
  )

data_leq3 <- subset(data, Month_Group == "≤3")
data_3to6 <- subset(data, Month_Group == "3-6")
data_gt6 <- subset(data, Month_Group == "＞6")

meta_leq3 <- metaprop(Depression_n, Total_n, data = data_leq3, 
                      studlab = Study, sm = "PFT", method.tau = "REML",method.random.ci = TRUE)
meta_3to6 <- metaprop(Depression_n, Total_n, data = data_3to6, 
                      studlab = Study, sm = "PFT", method.tau = "REML",method.random.ci = TRUE)
meta_gt6 <- metaprop(Depression_n, Total_n, data = data_gt6, 
                     studlab = Study, sm = "PFT", method.tau = "REML",method.random.ci = TRUE)

meta_overall <- metaprop(Depression_n, Total_n, data = data, 
                         studlab = Study, sm = "PFT", method.tau = "REML", method.random.ci = TRUE,
                         subgroup = Month_Group)

print(meta_leq3)
print(meta_3to6)
print(meta_gt6)
print(meta_overall)

forest(meta_overall, 
       leftcols = c("studlab", "event", "n", "effect", "ci"),
       leftlabs = c("Study", "Depression", "Total", "Prevalence", "95% CI"),
       smlab = "Prevalence of Depression (95% CI)",
       subgroup = TRUE,
       print.subgroup.name = FALSE,
       col.diamond = "blue",
       col.diamond.lines = "black",
       col.subgroup = "black")
