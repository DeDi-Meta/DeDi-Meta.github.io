
library(meta)
library(ggplot2)


study_data <- data.frame(
  study = c(
    "Mohamed Abd-Elgawad2023", "Shahad Abduljalil Abualhamael2024", 
    "Hesham Abuhegazy2022", "Seid Yimam Ali", "Abdullahi S. Aminu2017",
    "Muhammad Atif2018", "Gedion Asnake Azeze2020", 
    "antenaeh Messele Birhanu2016", "Habtamu Birhanu2022", "Tania Dehesh2020",
    "Mohamed Ebrahim2021", "Mohamed Hassan Elnaem2025", 
    "Nigus Alemnew Engidaw2020", "Annie C. H. Fung2018", 
    "Malgorzata Gorska-Ciebiada2014", "Sheikh Mohammed Shariful Islam2015",
    "Firdous Jahan2011", "Mihyun Jeong2021", "Ashmita Karki2024", 
    "Kankana Karpha2022", "Nuket Bayram Kayar2017", "Steven M. Kogan2007",
    "Rehanguli Maimaitituerxun2023", "Makda Abate Belew2023", 
    "Eva O. Melin2017", "Nelda Mier2008①", "Nelda Mier2008②", 
    "Nur Adam Mohamed2024", "Lili Husniati Yaacob2012", "Mussa R. Mussa2023",
    "Kabtamu Nigussie2023", "Hina Sharif2023", "Avinash K. Sunny2019",
    "Waleed M Sweileh2014", "Thitiphan Thaneerat2009", 
    "Nhu Minh Hang Tran2021", "Allan Oliver Dampil2019", "Yiting Wang2016",
    "Weijun Zhang2015"
  ),
  events = c(232, 124, 128, 124, 75, 270, 120, 64, 129, 885,
             196, 343, 86, 42, 82, 319, 56, 149, 123, 60,
             28, 72, 135, 203, 17, 81, 67, 161, 54, 194,
             176, 152, 63, 120, 70, 50, 386, 256, 142),
  total = c(679, 251, 350, 263, 200, 400, 410, 415, 310, 1500,
            401, 606, 403, 325, 276, 515, 320, 1529, 481, 152,
            154, 200, 496, 426, 148, 200, 172, 360, 260, 267,
            416, 493, 278, 294, 250, 216, 476, 2182, 412)
)


meta_total <- metaprop(
  event = study_data$events,
  n = study_data$total,
  sm = "PFT",
  method.tau = "DL",
  random = TRUE,
  method.random.ci = TRUE
)

sensitivity_results <- data.frame(
  omitted_study = character(),
  prevalence = numeric(),
  lower = numeric(),
  upper = numeric()
)

for (i in 1:nrow(study_data)) {
  temp_data <- study_data[-i, ]
  
  meta_temp <- metaprop(
    event = temp_data$events,
    n = temp_data$total,
    sm = "PFT",
    method.tau = "DL",
    random = TRUE,
    method.random.ci = "HK"
  )
  
  sensitivity_results <- rbind(sensitivity_results, data.frame(
    omitted_study = study_data$study[i],
    prevalence = meta_temp$TE.random,
    lower = meta_temp$lower.random,
    upper = meta_temp$upper.random
  ))
}


transform_to_proportion <- function(y) (sin(y))^2

sensitivity_results$prevalence_p <- transform_to_proportion(sensitivity_results$prevalence)
sensitivity_results$lower_p <- transform_to_proportion(sensitivity_results$lower)
sensitivity_results$upper_p <- transform_to_proportion(sensitivity_results$upper)


total_prevalence <- transform_to_proportion(meta_total$TE.random)


p <- ggplot(sensitivity_results, aes(x = reorder(omitted_study, prevalence_p), y = prevalence_p)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower_p, ymax = upper_p), width = 0.2) +
  geom_hline(yintercept = total_prevalence, linetype = "dashed", color = "red", size = 1) +
  labs(
    x = "Omitted Study",
    y = "Pooled Prevalence (95% CI)",
    title = "Sensitivity Analysis of Depression Prevalence in Diabetic Patients"
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0.2, 0.7)) +
  coord_flip() +
  theme_bw() +
  theme(
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.margin = margin(1, 1, 1, 2, "cm")
  )


ggsave("sensitivity_analysis_600dpi.png", plot = p, 
       width = 12, height = 15, dpi = 600, units = "in")
ggsave("sensitivity_analysis.pdf", plot = p, 
       width = 12, height = 15, device = "pdf")
ggsave("sensitivity_analysis.tiff", plot = p, 
       width = 12, height = 15, dpi = 600, compression = "lzw")


print(p)
getwd()