


#'[ Kindly refer to the sections ( Ctrl + Shift + O) to quickly browse the code

# ----------------------- [ 0 - 0 ] Packages and Libraries  -------------------- ####

#'[ Installing all necessary packages into R
#'
# i will install pacman, then use p_load() to quickly install&load all my necessary packages
install.packages("pacman",     repos = "http://cran.us.r-project.org")



#'[ loading the above packages into R (run one line to load all)

pacman::p_load(
  tidyverse, # mostly for dplyr and other necessary packages
  writexl,   # i prefer writexl because i can import as sheets
  readxl,    # i prefer readxl because i can extract sheets
  scales,    # to format certain variables to percent.. etc
  psych,     # used for multiple linear regression
)



# note : a lot of data wrangling functions used are among the packages included in tidyverse

# ----------------------- [ 1 - 0 ] assigning the data frame  ------------------ ####


# loading in the file
AbsEmployees <- read_excel("../MIS341-02_Group 4/1. input/AbsEmployees.xlsx", sheet = 1)


#duplicating the data set to re-code if necessary
emp <- AbsEmployees



# ----------------------- [ 1 - 1 ] data cleaning  ----------------------------- ####



# ----------------------- [ 1 - 1 ] data wrangling  ---------------------------- ####



#'[ creating a data frame to calculate frequency of Jobs

Freq_jobTitle          <- data.frame(table(emp$JobTitle))
Freq_jobTitle          <- rename(Freq_jobTitle, jobTitle = Var1 )
Freq_jobTitle$Average  <-  (Freq_jobTitle$Freq /  sum(Freq_jobTitle$Freq))
Freq_jobTitle$Average  <- round(Freq_jobTitle$Average, digits = 3)
Freq_jobTitle$Average  <- label_percent()(Freq_jobTitle$Average)
View(Freq_jobTitle)



emp$new <- case_when(emp$Age > 17 & emp$Age <= 25 ~ "young",
                   emp$Age > 25 & emp$Age <= 35 ~ "mid life crisis",
                   emp$Age > 35 & emp$Age <= 50 ~ "old",
                   emp$Age > 50 ~ "about to die")
View(emp )

summarise(emp)



glimpse(Freq_jobTitle)
label_percent()(Freq_jobTitle$Averages)

percent(Freq_jobTitle, accuracy = 1)

table(emp$JobTitle, emp$Division)


#'[ predictive analytics














