update.packages(ask = FALSE, oldPkgs = c("meta", "metafor", "showtext"))

target_packages <- c("meta", "metafor", "showtext")
install.packages(target_packages)

if(!require(meta)) install.packages("meta")
if(!require(showtext)) install.packages("showtext")
install.packages("metafor")


library(meta)
library(showtext)
library(metafor)


event <- c(232, 124, 128, 124, 75, 270, 120, 64, 129, 885,
           196, 343, 86, 42, 82, 319, 56, 149, 123, 60,
           28, 72, 135, 203, 17, 81, 67, 161, 54, 194,
           176, 152, 63, 120, 70, 50, 386, 256, 142)

n <- c(679, 251, 350, 263, 200, 400, 410, 415, 310, 1500,
       401, 606, 403, 325, 276, 515, 320, 1529, 481, 152,
       154, 200, 496, 426, 148, 200, 172, 360, 260, 267,
       416, 493, 278, 294, 250, 216, 476, 2182, 412)

study <- c("Mohamed Abd-Elgawad2023", "Shahad Abduljalil Abualhamael2024",
           "Hesham Abuhegazy2022", "Seid Yimam Ali", "Abdullahi S. Aminu2017",
           "Muhammad Atif2018", "Gedion Asnake Azeze2020", 
           "anteneh Messele Birhanu2016", "Habtamu Birhanu2022", "Tania Dehesh2020",
           "Mohamed Ebrahim2021", "Mohamed Hassan Elnaem2025", 
           "Nigus Alemnew Engidaw2020", "Annie C. H. Fung2018", 
           "Malgorzata Gorska-Ciebiada2014", "Sheikh Mohammed Shariful Islam2015",
           "Firdous Jahan2011", "Mihyun Jeong2021", "Ashmita Karki2024",
           "Kankana Karpha2022", "Nuket Bayram Kayar2017", "Steven M. Kogan2007",
           "Rehanguli Maimaitituerxun2023", "Makda Abate Belew2023",
           "Eva O. Melin2017", "Nelda Mier2008 (1)", "Nelda Mier2008 (2)",  # 修改这里
           "Nur Adam Mohamed2024", "Lili Husniati Yaacob2012", "Mussa R. Mussa2023",
           "Kabtamu Nigussie2023", "Hina Sharif2023", "Avinash K. Sunny2019",
           "Waleed M Sweileh2014", "Thitiphan Thaneerat2009", 
           "Nhu Minh Hang Tran2021", "Allan Oliver Dampil2019", 
           "Yiting Wang2016", "Weijun Zhang2015")




meta_pft <- metaprop(
  event = event, 
  n = n,
  studlab = study,
  sm = "PFT",
  method.tau = "DL",
  random = TRUE,
  method.random.ci=TRUE
)


par(family = "unicode_font")  


forest(
  meta_pft,
  leftcols = c("studlab", "event", "n", "effect", "ci"),
  leftlabs = c("Study", "Events", "Total", "Proportion", "95% CI"),
  rightcols = c("w.random"),
  rightlabs = c("Weight"),
  col.square = "blue",
  col.diamond = "red",
  xlab = "Depression Prevalence",
  print.I2 = TRUE,
  print.tau2 = TRUE,
  print.pval.Q = TRUE
)

egger_test <- metabias(meta_pft, method = "linreg", k.min = 10)
print(egger_test)


funnel(meta_pft, 
       studlab = FALSE, 
       contour = c(0.9, 0.95, 0.99),  
       col.contour = c("gray50", "gray70", "gray90"),
       xlab = "Transformed Proportion (PFT)",
       main = "Funnel Plot with Confidence Contours")

rma_pft <- rma(
  yi = meta_pft$TE,          
  sei = meta_pft$seTE,       
  method = "DL",             
  measure = "PFT",           
  data = meta_pft$data
)


tf_analysis <- trimfill(rma_pft)
print(tf_analysis)  


funnel(tf_analysis, 
       xlab = "Transformed Proportion (PFT)",
       main = "Trim and Fill Adjusted Funnel Plot")

transf.ipft.hm <- function(yi, ni) {
  yi <- sin(yi)^2                
  yi <- (yi * (ni + 0.5)) - 0.5  
  yi <- yi / ni
  pmax(0, pmin(1, yi))           
}

adjusted_pft <- tf_analysis$beta


mean_n <- mean(n)
total_n <- length(n) + 6  
adjusted_prop <- transf.ipft.hm(adjusted_pft, mean_n)

ci_lb <- transf.ipft.hm(tf_analysis$ci.lb, mean_n)
ci_ub <- transf.ipft.hm(tf_analysis$ci.ub, mean_n)

