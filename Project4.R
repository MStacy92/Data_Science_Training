# Project 4

# Prompt 1 #####
loan <- read.csv("Project4.csv")
               #This will assign the imported file to our object.
head(loan, 10) #This displays the first 10 rows of the object within the console.
               #Since this is a large dataset, it will take a few moments before the command is complete.

# Prompt 2 #####
str(loan) #This displays the structure of our data frame in the console.

# Prompt 3 #####
summary(loan$loan_amnt) #Calculates the 3 quartile values, mean, minimum, and maximum.
sd(loan$loan_amnt)      #Standard Deviation

# Prompt 4 ######
summary(loan$int_rate) #Calculates the 3 quartile values, mean, minimum, and maximum.
sd(loan$int_rate)      #Standard Deviation

# Prompt 5 #####
cor(loan$int_rate, loan$installment)   #Determines the correlation between two numerical variables.
plot(loan$int_rate, loan$installment)  #Shows the plot of correlation between two numerical variables.

# Prompt 6 #####
table(loan$term)                                    #This will calculate the frequencies of each level within a categorical variable.
names(sort(table(loan$term), decreasing = TRUE))[1] #This will display the most abundant level.

# Prompt 7 #####
prop.table(table(loan$loan_status))                  #This will calculate the proportions of each level within a categorical variable.
names(sort(-prop.table(table(loan$loan_status))))[1] #This will display the most abundant level.

# Prompt 8 #####
xtabs(~loan_status + term, data = loan)                         #This displays a joint frequency table between two variables.
prop.table(xtabs(~loan_status + term, data = loan), margin = 1) #This displays proportions between two variables in respect to row.
prop.table(xtabs(~loan_status + term, data = loan), margin = 2) #This displays proportions between two variables in respect to column.

# Prompt 9 #####
summary(loan) #Calculates the 3 quartile values, mean, minimum, and maximum.

#Clean Up #####

# Once completed, you may clear your R Studio with the following:

#Clear Plots
dev.off()

# Clear Environments
rm(list = ls())

#Clear Console
cat("\014") #Or ctrl/cmd + L

