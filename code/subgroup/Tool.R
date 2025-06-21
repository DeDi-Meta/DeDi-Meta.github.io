
library(meta)
library(metafor)
library(ggplot2)

data <- data.frame(
  Study = c(
    "Mohamed Abd-Elgawad2023", "Rehanguli Maimaitituerxun2023", "Eva O. Melin2017", 
    "Kabtamu Nigussie2023", "Thitiphan Thaneerat2009", "Seid Yimam Ali2023", 
    "Abdullahi S. Aminu2017", "Gedion Asnake Azeze2020", "anteneh Messele Birhanu2016", 
    "Mohamed Ebrahim2021", "Nigus Alemnew Engidaw2020", "Sheikh Mohammed Shariful Islam2015", 
    "Ashmita Karki2024", "Kankana Karpha2022", "Makda Abate Belew2023", 
    "Mussa R. Mussa2023", "Hina Sharif2023", "Allan Oliver Dampil2019", 
    "Habtamu Birhanu2022", "Mohamed Hassan Elnaem2025", "Mihyun Jeong2021", 
    "Nhu Minh Hang Tran2021", "Yiting Wang2016", "Steven M. Kogan2007", 
    "Nelda Mier2008①+M", "Nelda Mier2008②+A", "Shahad Abduljalil Abualhamael2024", 
    "Nur Adam Mohamed2024", "Avinash K. Sunny2019", "Waleed M Sweileh2014"
  ),
  Tool = c(
    "HADS-D≥8", "HADS-D≥8", "HADS-D≥8", "HADS-D≥8", "HADS-D≥8", 
    "PHQ-9≥5", "PHQ-9≥5", "PHQ-9≥5", "PHQ-9≥5", "PHQ-9≥5", "PHQ-9≥5", 
    "PHQ-9≥5", "PHQ-9≥5", "PHQ-9≥5", "PHQ-9≥5", "PHQ-9≥5", "PHQ-9≥5", 
    "PHQ-9≥5", "PHQ-9≥10", "PHQ-9≥10", "PHQ-9≥10", "PHQ-9≥10", "PHQ-9≥10", 
    "CES-D≥16", "CES-D≥16", "CES-D≥16", "DASS-21≥10", "DASS-21≥10", 
    "BDI-Ⅱ≥16", "BDI-Ⅱ≥16"
  ),
  Total_n = c(
    679, 496, 148, 416, 250, 263, 200, 410, 415, 401, 403, 515, 
    481, 152, 426, 267, 493, 476, 310, 606, 1529, 216, 2182, 200, 
    200, 172, 251, 360, 278, 294
  ),
  Depression_n = c(
    232, 135, 17, 176, 70, 124, 75, 120, 64, 196, 86, 319, 123, 
    60, 203, 194, 152, 386, 129, 343, 149, 50, 256, 72, 81, 67, 
    124, 161, 63, 120
  )
)

data$Study <- gsub("[^[:alnum:][:space:]-]", "", data$Study)  
data$Tool <- gsub("[^[:alnum:][:space:]-≥]", "", data$Tool)   

data$Prevalence <- data$Depression_n / data$Total_n
data$LowerCI <- mapply(function(x, n) binom.test(x, n)$conf.int[1], data$Depression_n, data$Total_n)
data$UpperCI <- mapply(function(x, n) binom.test(x, n)$conf.int[2], data$Depression_n, data$Total_n)

meta_result <- metaprop(
  event = Depression_n,
  n = Total_n,
  data = data,
  studlab = Study,
  subgroup = Tool,  
  method.tau = "REML",    
  sm = "PFT",   
  random = TRUE,
  common = FALSE,
  method.random.ci = TRUE      
)

summary(meta_result)


print(meta_result$bylevs)  
print(meta_result$Q.b.random)  

forest(meta_result, 
       sortvar = Prevalence,
       leftcols = c("studlab", "event", "n", "effect", "ci"),
       leftlabs = c("Study", "Cases", "Total", "Prevalence", "95% CI"),
       subgroup = TRUE,
       print.subgroup.name = FALSE,
       col.subgroup = "black",
       test.subgroup = TRUE)
