# Project 7

#Set Up #####
pacman::p_load(pacman, tidyverse)

kc_house_data <- read.csv("Project7.csv")
head(kc_house_data, 10)

str(kc_house_data)
kc_house_data$waterfront <- as.factor(kc_house_data$waterfront)
kc_house_data$grade <- as.factor(kc_house_data$grade)
kc_house_data$view <- as.factor(kc_house_data$view)
str(kc_house_data)

# Prompt 1 #####
summary(kc_house_data$price)
sd(kc_house_data$price)

# Prompt 2A #####
ggplot(data = kc_house_data,
       aes(price)) + 
  geom_histogram(aes(y = ..count..),
                 color = "black",
                 fill = "slategray4",
                 bins = 30) +
  geom_vline(aes(xintercept = mean(price)),
             color = "firebrick2",
             linetype = "dashed",
             size = 1
  ) +
  ggtitle("Histogram of Price") +
  xlab("Price") +
  ylab("Frequency")

# Prompt 2B #####
ggplot(data = kc_house_data,
       aes(x = view,
           y = price)) +
  geom_boxplot(aes(col = view),
               outlier.color = "red",
               outlier.shape = 1,
               outlier.size = 3,
               notch = T
  ) +
  ggtitle("Box Plot of Price") +
  xlab("View") +
  ylab("Price")

# Prompt 2C #####
ggplot(data = kc_house_data,
       aes(x = sqft_living,
           y  = price)) +
  geom_point() +
  geom_smooth(method = "gam",
              formula = y ~ s(x, bs = "cs")) +
  ggtitle("Scatter Plot of Price vs. Square Foot Living") +
  xlab("Square Foot Living") +
  ylab("Price")



# Prompt 3A #####
lm.result3a <- lm(price ~ bedrooms + bathrooms + sqft_living, data = kc_house_data)
summary(lm.result3a)
summary(lm.result3a)$coefficients
summary(lm.result3a)$r.squared
summary(lm.result3a)$adj.r.squared

# Price = 74662.0988 + (-57906.6307 * bedrooms) + (7928.7080 * bathrooms) + (309.6048 * sqft_living)
# R Squared = 0.5069364
# Adjusted R Squared = 0.5068679

# Prompt 3B #####
lm.result3b <- lm(price ~ bedrooms * bathrooms * sqft_living, data = kc_house_data)
summary(lm.result3b)
summary(lm.result3b)$coefficients
summary(lm.result3b)$r.squared
summary(lm.result3b)$adj.r.squared

# Price = 433859.56819 + (-46698.90493 * bedrooms) + (-120340.09037 * bathrooms) + (-24.90185 * sqft_living)
#                      + (-4824.74735 * bedrooms * bathrooms) + (32.27340 * bedrooms * sqft_living)
#                      + (112.10829 * bathrooms * sqft_living) + (-10.14940 * bedrooms * bathrooms * sqft_living)
# R Squared = 0.5436438
# Adjusted R Squared = 0.5434959

# Prompt 3C #####
lm.result3c <- lm(price ~ bedrooms + bathrooms + sqft_living + waterfront + grade, data = kc_house_data)
summary(lm.result3c)
summary(lm.result3c)$coefficients
summary(lm.result3c)$r.squared
summary(lm.result3c)$adj.r.squared

# Price (waterfront=0, grade=1)  = 93456.7068 + (-15519.6654 * bedrooms) + (-6546.5098 * bathrooms) + (167.3907 * sqft_living)
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

#Clean Up #####

# Once completed, you may clear your R Studio with the following:

#Clear Plots
dev.off()

# Clear Environments
rm(list = ls())

#Clear Console
cat("\014") #Or ctrl/cmd + L

