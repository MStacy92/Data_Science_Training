# Project 3

# Prompt 1 #####
pacman::p_load(readxl) #This will allow us to read excel files.
BlackFriday <- read_xlsx("Project3.xlsx",
                   sheet = "BlackFriday") #This will assign the imported file to our object.
print(BlackFriday)                        #This displays that object within the console.
                                          #Since this is a large dataset, it will take a few moments before the command is complete.

# Prompt 2 #####
sumAll <- 0 #This initials the sum to zero so that the following loop will start with adding the first value to zero.
{pb <- txtProgressBar(min = 0, max = nrow(BlackFriday), initial = 0, style = 3) #This initializes a progress bar. This is done so that
                                                                                #we can see the progress of our commands as they are
                                                                                #being accomplished.
for(i in c(1:nrow(BlackFriday))){               #This starts the for loop, for every row, it will do the following:
  sumAll <- sumAll + BlackFriday[i, "Purchase"] #Add the purchase amount to the current total of sum.
  setTxtProgressBar(pb, i)                      #Continues the progress bar forward until completion; appears in the console.
}}

avgAll <- sumAll_other/nrow(BlackFriday) #We then take the total sum and divide by the total number of rows to achieve average of all.
print(avgAll)                      #We then print the average.
class(avgAll)

# Prompt 3 #####
sumAll <- 0 #This initials the sum to zero so that the following loop will start with adding the first value to zero.
i <- 1      #This initials the index to one so that the following loop will start on the first iteration (or first row).
while(i <= nrow(BlackFriday)){ #This starts the while loop, for every row that the index is less than or equal to the number of rows,
                               #it will do the following:
  sumAll <- sumAll + BlackFriday[i, "Purchase"] #Add the purchase amount to the current total of sum.
  i <- i + 1                                    #Adds one to the current index number (or row number).
  setTxtProgressBar(pb, i)                      #Continues the progress bar forward until completion; appears in the console.
}
avgAll <- sumAll/nrow(BlackFriday) #We then take the total sum and divide by the total number of rows to achieve average of all.
print(avgAll)                      #We then print the average.

# Prompt 4 ######
sumAll <- 0 #This initials the sum to zero so that the following loop will start with adding the first value to zero.
i <- 1      #This initials the index to one so that the following loop will start on the first iteration (or first row).
repeat{                                          #This starts the repeat loop, for every row that the index is less than or equal
                                                 #to the number of rows, it will do the following:
  sumAll <- sumAll + BlackFriday[i, "Purchase"]  #Add the purchase amount to the current total of sum.
  i <- i + 1                                     #Adds one to the current index number (or row number).
  setTxtProgressBar(pb, i)                       #Continues the progress bar forward until completion; appears in the console.
  if (i > nrow(BlackFriday)){ #This is the condition that stops the repeat loop. The repeat loop will end as soon as the index value
                              #is greater than the number of rows.
    break
  }
}
avgAll <- sumAll/nrow(BlackFriday) #We then take the total sum and divide by the total number of rows to achieve average of all.
print(avgAll)                      #We then print the average.

# Prompt 5 #####
sumF <- 0   #This initials the sum to zero so that the following loop will start with adding the first value to zero.
for(i in c(1:nrow(BlackFriday))){             #This starts the for loop, for every row, it will do the following:
  if(BlackFriday[i, "Gender"] == "F"){        #This applies an argument to only select the data if the gender is female.
    sumF <- sumF + BlackFriday[i, "Purchase"] #Add the purchase amount to the current female total of sum.
    setTxtProgressBar(pb, i)                  #Continues the progress bar forward until completion; appears in the console.
  }
}
avgF <- sumF/nrow(BlackFriday[BlackFriday$Gender == "F", ]) #We then take the total female sum and divide by the total number of rows
                                                            #to achieve average of the females.
print(avgF)                                                 #We then print the average.

# Prompt 6 #####
sumF <- 0 #This initials the sum to zero so that the following loop will start with adding the first value to zero.
i <- 1    #This initials the index to one so that the following loop will start on the first iteration (or first row).
while(i <= nrow(BlackFriday)){ #This starts the while loop, for every row that the index is less than or equal to the number of rows,
                               #it will do the following:
  setTxtProgressBar(pb, i)     #Continues the progress bar forward until completion; appears in the console.
  if(BlackFriday[i, "Gender"] == "F"){        #This applies an argument to only select the data if the gender is female.
    sumF <- sumF + BlackFriday[i, "Purchase"] #Add the purchase amount to the current female total of sum.
  }
  i <- i + 1                                  #Adds one to the current index number (or row number).
}
avgF <- sumF/nrow(BlackFriday[BlackFriday$Gender == "F", ]) #We then take the total female sum and divide by the total number of rows
                                                            #to achieve average of the females.
print(avgF)                                                 #We then print the average.

# Prompt 7 #####
sumF <- 0 #This initials the sum to zero so that the following loop will start with adding the first value to zero.
i <- 1    #This initials the index to one so that the following loop will start on the first iteration (or first row).
repeat{                                       #This starts the repeat loop, for every row that the index is less than or equal
                                              #to the number of rows, it will do the following:
  if(BlackFriday[i, "Gender"] == "F")         #This applies an argument to only select the data if the gender is female.
    sumF <- sumF + BlackFriday[i, "Purchase"] #Add the purchase amount to the current female total of sum.
    i <- i + 1                                #Adds one to the current index number (or row number).
    setTxtProgressBar(pb, i)                  #Continues the progress bar forward until completion; appears in the console.
  if (i > nrow(BlackFriday)){ #This is the condition that stops the repeat loop. The repeat loop will end as soon as the index value
                              #is greater than the number of rows.
    break
  }
}
avgF <- sumF/nrow(BlackFriday[BlackFriday$Gender == "F", ]) #We then take the total female sum and divide by the total number of rows
                                                            #to achieve average of the females.
print(avgF)                                                 #We then print the average.

# Prompt 8 #####
avgM <- abs(sumAll - sumF)/nrow(BlackFriday[BlackFriday$Gender == "M", ]) #This takes the absolute value of the difference of the total
                                                                          #sum minus the female sum and then divides the result by the
                                                                          #number of rows with a gender of male.
print(avgM)                 #We then print the average.
avgDiff <- abs(avgM - avgF) #This is the difference of the male and female averages.
print(avgDiff)              #We then print the difference in average.

#Clean Up #####

# Once completed, you may clear your R Studio with the following:

# Clear Environments
rm(list = ls())

#Clear Console
cat("\014") #Or ctrl/cmd + L

