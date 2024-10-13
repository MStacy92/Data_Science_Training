# Project 6

#Set Up #####
pacman::p_load(pacman, tidyverse)

# Prompt 1 #####
kc_house_data <- read.csv("Project6.csv")
head(kc_house_data, 10)
View(kc_house_data)

str(kc_house_data)
kc_house_data$waterfront <- as.factor(kc_house_data$waterfront)
kc_house_data$grade <- as.factor(kc_house_data$grade)
kc_house_data$view <- as.factor(kc_house_data$view)
kc_house_data$condition <- as.factor(kc_house_data$condition)
str(kc_house_data)

levels(kc_house_data$view)
levels(kc_house_data$waterfront)
levels(kc_house_data$condition)
levels(kc_house_data$grade)

# Prompt 2 #####
lm.result2 <- lm(price ~ bedrooms + bathrooms + sqft_living, data = kc_house_data)
summary(lm.result2)
summary(lm.result2)$coefficients
summary(lm.result2)$r.squared
summary(lm.result2)$adj.r.squared

# Price = 74662.0988 + (-57906.6307 * bedrooms) + (7928.7080 * bathrooms) + (309.6048 * sqft_living)
# R Squared = 0.5069364
# Adjusted R Squared = 0.5068679

# Prompt 3 #####
lm.result3 <- lm(price ~ bedrooms * bathrooms * sqft_living, data = kc_house_data)
summary(lm.result3)
summary(lm.result3)$coefficients
summary(lm.result3)$r.squared
summary(lm.result3)$adj.r.squared

levels(kc_house_data$grade)

# Price = 433859.56819 + (-46698.90493 * bedrooms) + (-120340.09037 * bathrooms) + (-24.90185 * sqft_living)
#                      + (-4824.74735 * bedrooms * bathrooms) + (32.27340 * bedrooms * sqft_living)
#                      + (112.10829 * bathrooms * sqft_living) + (-10.14940 * bedrooms * bathrooms * sqft_living)
# R Squared = 0.5436438
# Adjusted R Squared = 0.5434959

# Prompt 4 ######
lm.result4 <- lm(price ~ bedrooms + bathrooms + sqft_living + waterfront + grade, data = kc_house_data)
summary(lm.result4)
summary(lm.result4)$coefficients
summary(lm.result4)$r.squared
summary(lm.result4)$adj.r.squared

# Price (waterfront=0, grade=1)  = 93456.7068 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=0, grade=2)  = NO ENTRIES FOR GRADE = 2
# Price (waterfront=0, grade=3)  = 122946.5287 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=0, grade=4)  = 132760.0232 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=0, grade=5)  = 115604.9668 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=0, grade=6)  = 147770.1142 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=0, grade=7)  = 180341.0712 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=0, grade=8)  = 241512.2488 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=0, grade=9)  = 362307.2526 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=0, grade=10) = 544467.3331 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=0, grade=11) = 811083.1869 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=0, grade=12) = 1274451.0030 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=0, grade=13) = 2566744.7070 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=1, grade=1)  = 860439.2428 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=1, grade=2)  = NO ENTRIES FOR GRADE = 2
# Price (waterfront=1, grade=3)  = 889929.0647 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=1, grade=4)  = 899742.5592 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=1, grade=5)  = 882587.5028 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=1, grade=6)  = 914752.6502 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=1, grade=7)  = 947323.6072 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=1, grade=8)  = 1008494.7850 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=1, grade=9)  = 1129289.7860 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=1, grade=10) = 1311449.8690 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=1, grade=11) = 1578065.7230 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=1, grade=12) = 2041433.5390 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=1, grade=13) = 3333727.2430 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# R Squared = 0.6156873
# Adjusted R Squared = 0.6154204


# Prompt 5 #####
lm.result5 <- lm(price ~ . - 1 - id - date - zipcode - lat - long, data  = kc_house_data)
summary(lm.result5)
summary(lm.result5)$coefficients
summary(lm.result5)$r.squared
summary(lm.result5)$adj.r.squared

# Price (waterfront=0, grade=1)  = 93456.7068 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=0, grade=2)  = NO ENTRIES FOR GRADE = 2
# Price (waterfront=0, grade=3)  = 122946.5287 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=0, grade=4)  = 132760.0232 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=0, grade=5)  = 115604.9668 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=0, grade=6)  = 147770.1142 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=0, grade=7)  = 180341.0712 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=0, grade=8)  = 241512.2488 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=0, grade=9)  = 362307.2526 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=0, grade=10) = 544467.3331 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=0, grade=11) = 811083.1869 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=0, grade=12) = 1274451.0030 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=0, grade=13) = 2566744.7070 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=1, grade=1)  = 860439.2428 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=1, grade=2)  = NO ENTRIES FOR GRADE = 2
# Price (waterfront=1, grade=3)  = 889929.0647 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=1, grade=4)  = 899742.5592 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=1, grade=5)  = 882587.5028 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=1, grade=6)  = 914752.6502 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=1, grade=7)  = 947323.6072 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=1, grade=8)  = 1008494.7850 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=1, grade=9)  = 1129289.7860 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=1, grade=10) = 1311449.8690 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=1, grade=11) = 1578065.7230 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=1, grade=12) = 2041433.5390 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# Price (waterfront=1, grade=13) = 3333727.2430 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
# R Squared = 0.8994609
# Adjusted R Squared = 0.8993444

# Prompt 6 #####
new.house <- data.frame(bedrooms = 4, bathrooms = 2, sqft_living = 2560, sqft_lot = 7650,
                        floors = 1.5, waterfront = "1", view = "3", condition = "5", grade = "10")
lm.result6 <- lm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + waterfront
                 + view + condition + grade, data = kc_house_data)
summary(lm.result6)
summary(lm.result6)$coefficients
summary(lm.result6)$r.squared
summary(lm.result6)$adj.r.squared

predict(lm.result6,
        newdata = new.house,
        interval = "predict")

# Average = 1659564
# 95% Prediction = [1226456,2092673]

#Clean Up #####

# Once completed, you may clear your R Studio with the following:

#Clear Plots
dev.off()

# Clear Environments
rm(list = ls())

#Clear Console
cat("\014") #Or ctrl/cmd + L

