# Project 2

# Set Up: 
# To allow us to perform multiple commands in a single run of the command line, we will load tidyverse.
# This will allow us to pipe our commands from one to another and allow for easier use.
pacman::p_load(pacman, readxl, tidyverse) #To load tidyverse and rmarkdown, we can use pacman to conviently load the necessary packages.

# Prompt 1 #####
SP500 <- read_xlsx("Project2.xlsx",
                   sheet = "Data") %>% 
  print()       #This will assign the imported file to our object and then displays that object within the console.
view(SP500)     #This displays the object within a new script window.

# Prompt 2 #####
dim(SP500) #This displays the number of rows and columns found within the data set.
           #It will display the numbers of rows first, followed by the number of columns.

# Prompt 3 #####
SP500[ , c("SP500", "CPI", "Rate")] #This will select the three desired columns that we wish to see printed in the console.
                                    #Since we will not call on this selection later, we will not assign it to an object.

# Prompt 4 ######
SP500[c(10, 100, 500, 1500), ] #This will select the desired rows that we wish to see printed in the console.
                               #Since we will not call on this selection later, we will not assign it to an object.

# Prompt 5 #####
subset(SP500, (SP500 > 2000) | (CPI < 100)) #This displays the observations where SP500 is greater than 2000 OR 
                                            #where CPI is less than 100.
                                            #Since we will not call on this selection later, we will not assign it to an object.

# Prompt 6 #####
subset(SP500, (Earnings > 50) & (Rate < 3), select = c(SP500, Dividend)) #This displays the observations where Earnings is greater
                                                                         #than 50 AND Rate is less than 3. It will then only show
                                                                         #columns SP500 and Dividend in the console.

# Prompt 7 #####
SP500$Rate <- NULL #This selects the Rate column within the SP500 object and defines that column with zero rows.
colnames(SP500)    #This displays the column names to show that the Rate column is no longer present in the object.

# Prompt 8 #####
SP500$RealPrice <- (SP500$SP500)*(SP500$CPI)/last(SP500$CPI)
print(SP500) #This will assign a new column name as RealPrice and apply the following formula to populate the rows with the calculated
             #values and the result will be printed in the console shown in the first 10 rows of the data frame proper.

# Prompt 9 #####
SP500$RealEarnings <- (SP500$Earnings)*(SP500$CPI)/last(SP500$CPI)
print(SP500) #This will assign a new column name as RealEarnings and apply the following formula to populate the rows with the calculated
             #values and the result will be printed in the console shown in the first 10 rows of the data frame proper.

# Prompt 10 #####
SP500$PERatio <- (SP500$RealPrice/SP500$RealEarnings)
print(SP500) #This will assign a new column name as PERatio and apply the following formula to populate the rows with the calculated
             #values and the result will be printed in the console shown in the first 10 rows of the data frame proper.

# Once completed, you may clear your R Studio with the following:

# Clear Environments
rm(list = ls())

#Clear Console
cat("\014") #Or ctrl/cmd + L
