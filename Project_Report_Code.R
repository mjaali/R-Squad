

#'[ Installing all necessary packages into R

install.packages("tidyverse",  repos = "http://cran.us.r-project.org")
install.packages("pacman",     repos = "http://cran.us.r-project.org")
install.packages("writexl",    repos = "http://cran.us.r-project.org")
install.packages("readxl",     repos = "http://cran.us.r-project.org")
install.packages("psych",      repos = "http://cran.us.r-project.org")
install.packages("scales",     repos = "http://cran.us.r-project.org")





#'[ loading the above packages into R (run one line to load all)

library(tidyverse); library(pacman); library(writexl); library(readxl); library(scales); library(readr); library(psych); library(dplyr)

library(tidyverse)    # mostly for dplyr and other necessary packages
library(pacman)       # for package installation
library(writexl)      # i prefer writexl because i can import as sheets
library(readxl)       # i prefer readxl because i can extract sheets
library(scales)       # to format certain variables to tables
library(psych)        # used for multiple linear regression


# ----------------- 

# loading in the file
AbsEmployees <- read_excel("../MIS341-02_Group 4/1. input/AbsEmployees.xlsx", sheet = 1)


#duplicating the data set to re-code if necessary
emp <- AbsEmployees

# converting tenure from years to hours
emp$length_hours <- emp$LengthService * 8760; view(emp)


# creating a data frame to calculate frequency of Jobs
Freq_jobTitle          <- data.frame(table(emp$JobTitle)); view(Freq_jobTitle)
Freq_jobTitle          <- rename(Freq_jobTitle, jobTitle = Var1 ); view(Freq_jobTitle)
Freq_jobTitle$Average  <-  (Freq_jobTitle$Freq /  sum(Freq_jobTitle$Freq)); view(Freq_jobTitle)
Freq_jobTitle$Average  <- label_percent()(Freq_jobTitle$Averages)
View(Freq_jobTitle)


Freq_division <- data.frame(table(emp$Division)); View(Freq_division)

summarise(emp)



glimpse(Freq_jobTitle)
label_percent()(Freq_jobTitle$Averages)

percent(Freq_jobTitle, accuracy = 1)

table(emp$JobTitle, emp$Division)


#'[ predictive analytics
colnames(emp)
Model_1 <- lm(emp$AbsentHours ~ Gender + JobTitle + DepartmentName + StoreLocation  + Division + Age + LengthService + BusinessUnit, data = emp); summary(Model_1)














