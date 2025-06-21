
library(metafor)
library(ggplot2)


data <- data.frame(
  study = c(
    "Mohamed Abd-Elgawad2023", "Shahad Abduljalil Abualhamael2024", 
    "Hesham Abuhegazy2022", "Seid Yimam Ali", "Abdullahi S. Aminu2017",
    "Muhammad Atif2018", "Gedion Asnake Azeze2020", "anteneh Messele Birhanu2016",
    "Habtamu Birhanu2022", "Tania Dehesh2020", "Mohamed Ebrahim2021",
    "Mohamed Hassan Elnaem2025", "Nigus Alemnew Engidaw2020", "Annie C. H. Fung2018",
    "Malgorzata Gorska-Ciebiada2014", "Sheikh Mohammed Shariful Islam2015",
    "Firdous Jahan2011", "Mihyun Jeong2021", "Ashmita Karki2024", 
    "Kankana Karpha2022", "Nuket Bayram Kayar2017", "Steven M. Kogan2007",
    "Rehanguli Maimaitituerxun2023", "Makda Abate Belew2023", "Eva O. Melin2017",
    "Nelda Mier2008①", "Nelda Mier2008②", "Nur Adam Mohamed2024",
    "Lili Husniati Yaacob2012", "Mussa R. Mussa2023", "Kabtamu Nigussie2023",
    "Hina Sharif2023", "Avinash K. Sunny2019", "Waleed M Sweileh2014",
    "Thitiphan Thaneerat2009", "Nhu Minh Hang Tran2021", "Allan Oliver Dampil2019",
    "Yiting Wang2016", "Weijun Zhang2015"
  ),
  country = c(
    "Egypt", "Saudi Arabia", "Saudi Arabia", "Ethiopia", "India", 
    "Pakistan", "Ethiopia", "Ethiopia", "Ethiopia", "Iran", "Ethiopia",
    "Malaysia", "Ethiopia", "China", "Poland", "Bangladesh", "Pakistan",
    "South Korea", "Nepal", "India", "Turkey", "USA", "China", "Ethiopia",
    "Sweden", "Mexico", "USA", "Somalia", "Malaysia", "Tanzania", "Ethiopia",
    "Pakistan", "Nepal", "Palestine", "Thailand", "Vietnam", "Philippines",
    "USA", "China"
  ),
  sample_size = c(
    679, 251, 350, 263, 200, 400, 410, 415, 310, 1500, 401, 606, 403, 325, 276,
    515, 320, 1529, 481, 152, 154, 200, 496, 426, 148, 200, 172, 360, 260, 267,
    416, 493, 278, 294, 250, 216, 476, 2182, 412
  ),
  depression_cases = c(
    232, 124, 128, 124, 75, 270, 120, 64, 129, 885, 196, 343, 86, 42, 82, 319, 56,
    149, 123, 60, 28, 72, 135, 203, 17, 81, 67, 161, 54, 194, 176, 152, 63, 120,
    70, 50, 386, 256, 142
  ),
  mean_age = c(
    51.67, 50.1, 61.4, 50.21, 63, 64, 47.4, 44.2, 47.22, 47.12, 48.6, NA, 46.4, 69,
    73.6, 49.94, 54.54, 63.34, 54, 55.13, 54.8, 52.46, 59.57, NA, 46.33, 55.8, 57.8,
    46.7, 50.9, 50, 53.5, 48.3, 54.3, 59, 62.58, 64.73, 58.3, NA, 59.77
  ),
  survey_year = c(
    2020, 2021, 2017, 2022, 2016, 2015, 2019, 2014, 2021, 2018, 2020,
    2022, 2019, 2013, 2014, 2013, 2011, 2016, 2023, 2022, 2017, 2007,
    2021, 2022, 2009, 2004, 2004, 2023, 2007, 2019, 2022, 2022, 2015,
    2012, 2008, 2018, 2018, 2008, 2012
  )
)


continent_mapping <- c(
  "Egypt" = "Africa", "Saudi Arabia" = "Asia", "Ethiopia" = "Africa", 
  "India" = "Asia", "Pakistan" = "Asia", "Iran" = "Asia", "Malaysia" = "Asia",
  "China" = "Asia", "Poland" = "Europe", "Bangladesh" = "Asia", 
  "South Korea" = "Asia", "Nepal" = "Asia", "Turkey" = "Asia", 
  "USA" = "Americas", "Sweden" = "Europe", "Mexico" = "Americas", 
  "Somalia" = "Africa", "Tanzania" = "Africa", "Palestine" = "Asia",
  "Thailand" = "Asia", "Vietnam" = "Asia", "Philippines" = "Asia"
)


data$continent <- continent_mapping[data$country]


data$pft_effect <- with(data, 0.5 * (
  asin(sqrt(depression_cases / (sample_size + 1))) + 
    asin(sqrt((depression_cases + 1) / (sample_size + 1)))
))


data$pft_variance <- with(data, 1 / (sample_size + 0.5))

                        
                        data_clean <- data[!is.na(data$mean_age), ]
                        
                        
                        multiv_model <- rma(
                          yi = pft_effect, 
                          vi = pft_variance, 
                          mods = ~ mean_age + survey_year + continent,
                          data = data_clean
                        )
                        
                      
                        year_min <- min(data_clean$survey_year)
                        year_max <- max(data_clean$survey_year)
                        year_range <- year_max - year_min
                        
                       
                        regression_plot <- ggplot(data_clean, aes(x = mean_age, y = pft_effect)) +
                          
                          geom_point(aes(color = continent, 
                                         size = survey_year),  
                                     alpha = 0.8, shape = 19) +
                          
                          
                          geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
                          
                          
                          scale_color_manual(
                            values = c(Africa = "#E69F00", Asia = "#56B4E9", Europe = "#009E73", Americas = "#D55E00"),
                            name = "Continent"
                          ) +
                          
                          
                          scale_size_continuous(
                            name = "Survey Year",
                            breaks = c(2005, 2010, 2015, 2020, 2023),  
                            range = c(3, 12),  
                            labels = function(x) as.integer(x)  
                          ) +
                          
                         
                          annotate("text", 
                                   x = min(data_clean$mean_age), 
                                   y = max(data_clean$pft_effect),
                                   label = sprintf(
                                     "Multivariable Meta-Regression\n\nAge: β = %.4f, p = %.3f\nYear: β = %.4f, p = %.3f\nModel: QM(df=3) = %.2f, p = %.3f",
                                     multiv_model$beta[2], multiv_model$pval[2],
                                     multiv_model$beta[3], multiv_model$pval[3],
                                     multiv_model$QM, multiv_model$QMp
                                   ),
                                   hjust = 0, vjust = 1, size = 5) +
                          
                          labs(
                            x = "Mean Age of Participants (Years)",
                            y = "PFT-Transformed Depression Prevalence",
                            title = "Meta-Regression of Depression Prevalence in Diabetic Patients",
                            subtitle = "Effects of Age, Survey Year, and Geographic Region"
                          ) +
                          
                          
                          theme_minimal(base_size = 14) +
                          theme(
                            plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
                            plot.subtitle = element_text(hjust = 0.5, size = 16),
                            axis.title = element_text(size = 16),
                            axis.text = element_text(size = 14),
                            legend.title = element_text(size = 14),
                            legend.text = element_text(size = 12),
                            legend.position = "right",
                            legend.background = element_rect(fill = "white", colour = "black"),
                            legend.key = element_rect(fill = "white", colour = "black"),
                            panel.grid.major = element_line(color = "grey90"),
                            panel.grid.minor = element_blank(),
                            panel.background = element_rect(fill = "white", color = "black"),
                            plot.background = element_rect(fill = "white", color = NA),
                            plot.margin = margin(20, 20, 20, 20)
                          ) +
                          
                          
                          guides(
                            color = guide_legend(override.aes = list(size = 5)),  
                            size = guide_legend()
                          )
                        
                        
                        ggsave("multivariable_meta_regression.tiff", regression_plot,
                               width = 12, height = 9, dpi = 1200, compression = "lzw", bg = "white")
                        
                        
                        ggsave("multivariable_meta_regression.pdf", regression_plot,
                               width = 12, height = 9, device = cairo_pdf, bg = "white")
                        
                       
                        print(regression_plot)