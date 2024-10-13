# Project 1

# Set Up: 
# To allow us to perform multiple commands in a single run of the command line, we will load tidyverse.
# This will allow us to pipe our commands from one to another and allow for easier use.
pacman::p_load(pacman, readxl, tidyverse) #To load tidyverse, we can use pacman to conviently load the necessary packages.

# Prompt 1 #####
#library(readxl) #This will allow us to read excel files.
Insurance <- read_xlsx("Project1.xlsx") %>% 
  print(n = Inf) #This will assign the imported file to our object and then displays that object within the console.
                 #Inf stands for infinity; this command will display all rows within our object.
view(Insurance) # This displays the object within a new script window.

# Prompt 2 #####
nrow(Insurance) #This displays the number of rows present in the data frame.

# Prompt 3 #####
ncol(Insurance) #This displays the number of columns present in the data frame.

#Prompt 4 ######
first.eight.rows <- head(Insurance, 8) %>% #This assigns the first 8 rows of values to a new object.
  print() #This will print the result of the command in the console.

# Prompt 5 #####
five.rows <- tail(Insurance, 5) %>% #This assigns the last 5 rows of values to a new object.
  print() #This will print the result of the command in the console.

# Prompt 6 #####
ls() #This is one way to list the object(s) found within the environment.
objects() #This is another way to list the object(s) found within the environment.

# Prompt 7 #####
summary(Insurance) #This will acquire simple metrics of our data.

# Once completed, you may clear your R Studio with the following:

# Clear Environments
rm(list = ls())

#Clear Console
cat("\014") #Or ctrl/cmd + L
