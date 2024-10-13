# Project 5

#Set Up #####
pacman::p_load(pacman, tidyverse)

# Prompt 1 #####
loan <- read.csv("Project5.csv")
               #This will assign the imported file to our object.
head(loan, 10) #This displays the first 10 rows of the object within the console.
               #Since this is a large dataset, it will take a few moments before the command is complete.

# Prompt 2 #####
hist(loan$loan_amnt,                              #This will produce a histogram for the selected data.
     main = "Histogram & Density of Loan Amount", #This will assign the main title of the plot.
     xlab = "Loan Amount",                        #This will assign a title for the x-axis.
     ylab = "Proportion",                         #This will assign a title for the y-axis.
     freq = F,                                    #This will turn off the frequencies and turn on proportions.
     breaks = 30
     )
lines(density(loan$loan_amnt),                    #This will produce a line made up of densities of the values.
      lwd = 2,                                    #This will assign a width to the line.
      col = "steelblue4"                          #This will assign a color to the line.
      )
abline(v = mean(loan$loan_amnt),
       col = "firebrick2",
       lwd = 2)

# Prompt 3 #####
ggplot(data = loan,
       aes(loan_amnt)) + 
  geom_histogram(aes(y = ..density..),
                 color = "black",
                 fill = "white",
                 bins = 30) +
  geom_density(alpha = .2,
               fill = "plum2") +
  geom_vline(aes(xintercept = mean(loan_amnt)),
             color = "firebrick2",
             linetype = "dashed",
             size = 1
             ) +
  ggtitle("Histogram & Density of Loan Amount") +
  xlab("Loan Amount") +
  ylab("Density")

# Prompt 4 ######
plot(loan$annual_inc, loan$loan_amnt,
     main = "Scatter Plot of Loan Amount vs. Annual Income",
     xlab = "Annual Income",
     ylab = "Loan Amount"
)
abline(lm(loan_amnt~annual_inc, data = loan),
       lwd = 2,
       col = "steelblue4"
       )

# Prompt 5 #####
loan %>% 
  drop_na(loan_amnt, annual_inc) %>% 
  ggplot(aes(x = annual_inc, y  = loan_amnt)) +
  geom_point() +
  geom_smooth(method = "gam",
              formula = y ~ s(x, bs = "cs")) +
  ggtitle("Scatter Plot of Loan Amount vs. Annual Income") +
  xlab("Annual Income") +
  ylab("Loan Amount")

# Prompt 6 #####
barplot(table(loan$term, loan$grade),
        main = "Barplot of Term & Grade",
        xlab = "Frequency",
        ylab = "Grades",
        horiz = T,
        border = "black",
        col = c("slategray4", "azure3"),
        legend = rownames(table(loan$term, loan$grade))
        )

# Prompt 7 #####
ggplot(data = loan,
       aes(x = grade,
           y = ..count..)) +
  geom_bar(aes(fill = term),
           position = "dodge") +
  coord_flip() +
  ggtitle("Barplot of Term & Grade") +
  xlab("Grades") +
  ylab("Frequency")

# Prompt 8 #####
jpeg("Project5_basic.jpg")
boxplot(loan_amnt ~ term, data = loan,
        main = "Box Plot of Loan Amount",
        xlab = "Term",
        ylab = "Loan Amount",
        notch = T,
        col = "slategray4"
)
dev.off()

# Prompt 9 #####
ggplot(data = loan,
       aes(x = term,
           y = loan_amnt)) +
  geom_boxplot(aes(col = term),
               outlier.color = "red",
               outlier.shape = 16,
               outlier.size = 3,
               notch = T
               ) +
  ggtitle("Box Plot of Loan Amount") +
  xlab("Term") +
  ylab("Loan Amount")
ggsave("Project5_ggplot2.jpg")

#Clean Up #####

# Once completed, you may clear your R Studio with the following:

#Clear Plots
dev.off()

# Clear Environments
rm(list = ls())

#Clear Console
cat("\014") #Or ctrl/cmd + L

