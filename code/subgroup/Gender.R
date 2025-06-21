install.packages("meta")
library(meta)


data <- data.frame(
  Study = c(
    "Mohamed Abd-Elgawad2023", "Mohamed Abd-Elgawad2023",
    "Hesham Abuhegazy2022", "Hesham Abuhegazy2022",
    "Abdullahi S. Aminu2017", "Abdullahi S. Aminu2017",
    "Gedion Asnake Azeze2020", "Gedion Asnake Azeze2020",
    "anteneh Messele Birhanu2016", "anteneh Messele Birhanu2016",
    "Habtamu Birhanu2022", "Habtamu Birhanu2022",
    "Tania Dehesh2020", "Tania Dehesh2020",
    "Mohamed Ebrahim2021", "Mohamed Ebrahim2021",
    "Mohamed Hassan Elnaem2025", "Mohamed Hassan Elnaem2025",
    "Annie C. H. Fung2018", "Annie C. H. Fung2018",
    "Malgorzata Gorska-Ciebiada2014", "Malgorzata Gorska-Ciebiada2014",
    "Sheikh Mohammed Shariful Islam2015", "Sheikh Mohammed Shariful Islam2015",
    "Firdous Jahan2011", "Firdous Jahan2011",
    "Mihyun Jeong2021", "Mihyun Jeong2021",
    "Ashmita Karki2024", "Ashmita Karki2024",
    "Nuket Bayram Kayar2017", "Nuket Bayram Kayar2017",
    "Rehanguli Maimaitituerxun2023", "Rehanguli Maimaitituerxun2023",
    "Eva O. Melin2017", "Eva O. Melin2017",
    "Nur Adam Mohamed2024", "Nur Adam Mohamed2024",
    "Lili Husniati Yaacob2012", "Lili Husniati Yaacob2012",
    "Kabtamu Nigussie2023", "Kabtamu Nigussie2023",
    "Avinash K. Sunny2019", "Avinash K. Sunny2019",
    "Waleed M Sweileh2014", "Waleed M Sweileh2014",
    "Nhu Minh Hang Tran2021", "Nhu Minh Hang Tran2021",
    "Allan Oliver Dampil2019", "Allan Oliver Dampil2019",
    "Weijun Zhang2015", "Weijun Zhang2015"
  ),
 
  Gender_Group = factor(rep(c("M", "F"), times = 26), levels = c("M", "F")),
  Total_n = c(
    235, 444, 166, 184, 91, 108, 200, 210, 213, 202, 161, 149,
    730, 770, 197, 204, 215, 391, 211, 114, 127, 149, 227, 288,
    182, 138, 749, 780, 254, 227, 72, 82, 284, 212, 83, 65, 124, 236,
    124, 136, 218, 198, 188, 90, 130, 164, 102, 114, 174, 302, 207, 205
  ),
  Depression_n = c(
    69, 163, 57, 71, 23, 51, 58, 62, 31, 33, 58, 71,
    329, 385, 88, 108, 111, 232, 28, 14, 15, 67, 115, 204,
    24, 32, 50, 99, 60, 63, 8, 20, 68, 67, 10, 7, 53, 108,
    21, 33, 79, 97, 36, 27, 43, 77, 26, 24, 21, 69, 73, 69
  )
)

male <- subset(data, Gender_Group == "M")
female <- subset(data, Gender_Group == "F")


paired_data <- data.frame(
  Study = male$Study,
  Total_M = male$Total_n,
  Depression_M = male$Depression_n,
  Total_F = female$Total_n,
  Depression_F = female$Depression_n
)

meta_or <- metabin(
  event.e = paired_data$Depression_F,  
  n.e = paired_data$Total_F,          
  event.c = paired_data$Depression_M,  
  n.c = paired_data$Total_M,          
  studlab = paired_data$Study,
  sm = "OR",          
  method = "Inverse", 
  random = TRUE,      
  title = "Female vs Male"
)


forest(meta_or,
       leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c", "effect", "ci"),
       leftlabs = c("Study", "Depression (F)", "Total (F)", "Depression (M)", "Total (M)", "OR", "95% CI"),
       xlab = "Odds Ratio (Female vs Male)",
       col.square = "blue",
       col.diamond = "red",
       print.I2 = TRUE,
       print.tau2 = TRUE)
print(meta_or)

meta_gender <- metaprop(
  event = data$Depression_n,
  n = data$Total_n,
  studlab = data$Study,
  subgroup = data$Gender_Group,  
  method.tau = "REML",
  sm = "PFT",
  method.random.ci = TRUE  
)


meta_gender <- metaprop(
  event = Depression_n,    
  n = Total_n,             
  studlab = Study,         
  data = data,             
  subgroup = Gender_Group, 
  method.tau = "REML",
  sm = "PFT",
  method.random.ci = TRUE  
)

forest(meta_gender,
       leftcols = c("studlab", "event", "n", "effect", "ci"),
       leftlabs = c("Study", "Depression", "Total", "Prevalence", "95% CI"),
       xlab = "Depression Prevalence (logit scale)",
       col.subgroup = "black",
       print.subgroup.name = TRUE)

meta_reg <- metareg(meta_gender, ~ Gender_Group)  
summary(meta_reg)