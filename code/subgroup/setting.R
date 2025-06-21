
library(meta)
library(metafor)
library(dplyr)
library(ggplot2)

data <- data.frame(
  Study = c("Mohamed Abd-Elgawad2023", "Shahad Abduljalil Abualhamael2024", "Hesham Abuhegazy2022", 
            "Seid Yimam Ali2023", "Muhammad Atif2018", "Gedion Asnake Azeze2020", 
            "anteneh Messele Birhanu2016", "Habtamu Birhanu2022", "Tania Dehesh2020", 
            "Mohamed Ebrahim2021", "Nigus Alemnew Engidaw2020", "Annie C. H. Fung2018", 
            "Malgorzata Gorska-Ciebiada2014", "Sheikh Mohammed Shariful Islam2015", 
            "Firdous Jahan2011", "Kankana Karpha2022", "Nuket Bayram Kayar2017", 
            "Rehanguli Maimaitituerxun2023", "Makda Abate Belew2023", "Eva O. Melin2017", 
            "Nelda Mier2008①+M", "Nelda Mier2008②+A", "Nur Adam Mohamed2024", 
            "Lili Husniati Yaacob2012", "Mussa R. Mussa2023", "Kabtamu Nigussie2023", 
            "Waleed M Sweileh2014", "Thitiphan Thaneerat2009", "Nhu Minh Hang Tran2021", 
            "Allan Oliver Dampil2019", "Weijun Zhang2015", "Abdullahi S. Aminu2017", 
            "Ashmita Karki2024", "Steven M. Kogan2007", "Hina Sharif2023", "Avinash K. Sunny2019"),
  Setting = c(rep("Hospital", 31), rep("community", 5)),
  Total_n = c(679, 251, 350, 263, 400, 410, 415, 310, 1500, 401, 403, 325, 276, 515, 
              320, 152, 154, 496, 426, 148, 200, 172, 360, 260, 267, 416, 294, 250, 
              216, 476, 412, 200, 481, 200, 493, 278),
  Depression_n = c(232, 124, 128, 124, 270, 120, 64, 129, 885, 196, 86, 42, 82, 319, 
                   56, 60, 28, 135, 203, 17, 81, 67, 161, 54, 194, 176, 120, 70, 50, 
                   386, 142, 75, 123, 72, 152, 63)
)

data <- data %>%
  mutate(
    prevalence = Depression_n / Total_n,
    se = sqrt((prevalence * (1 - prevalence)) / Total_n),
    lower_ci = prevalence - 1.96 * se,
    upper_ci = prevalence + 1.96 * se
  )

desc_stats <- data %>%
  group_by(Setting) %>%
  summarise(
    n_studies = n(),
    total_participants = sum(Total_n),
    total_cases = sum(Depression_n),
    mean_prevalence = mean(prevalence),
    median_prevalence = median(prevalence),
    min_prevalence = min(prevalence),
    max_prevalence = max(prevalence)
  )
print(desc_stats)

data$Study <- paste(data$Study, " (", data$Setting, ")", sep="")

meta_hospital <- metaprop(Depression_n, Total_n, 
                          data = data[data$Setting == "Hospital",],
                          sm = "PFT",
                          method.tau = "REML",
                          random = TRUE,
                          method.random.ci = TRUE      
)

meta_community <- metaprop(Depression_n, Total_n, 
                           data = data[data$Setting == "community",],
                           sm = "PFT",
                           method.tau = "REML",
                           random = TRUE,
                           method.random.ci = TRUE      
)

meta_subgroup <- metaprop(Depression_n, Total_n, 
                          data = data,
                          sm = "PFT",
                          method.tau = "REML",
                          random = TRUE,
                          subgroup = Setting,
                          subgroup.name = "Setting",
                          method.random.ci = TRUE      
)

forest(meta_subgroup,
       sortvar = TE,
       leftcols = c("studlab", "event", "n", "effect", "ci"),
       leftlabs = c("Study", "Cases", "Total", "Prevalence", "95% CI"),
       rightcols = FALSE,
       subgroup = TRUE,
       print.subgroup.name = FALSE,
       col.square = "blue",
       col.diamond = "red",
       col.subgroup = "black",
       xlab = "Depression Prevalence (95% CI)",
       smlab = "Random Effects Model",
       studlab = data$Study)  

print(meta_hospital$I2)
print(meta_hospital$tau2)


print(meta_community$I2)
print(meta_community$tau2)
