


#' #'[ Kindly refer to the sections ( Ctrl + Shift + O) to quickly browse the code
#' 
#' # ----------------------- [ 0 - 0 ] Packages and Libraries  -------------------- ####
#' 
#' #'[ Installing all necessary packages into R
#' #'
#' # i will install pacman, then use p_load() to quickly install&load all my necessary packages
 install.packages("pacman",     repos = "http://cran.us.r-project.org");
 install.packages("tidyverse");
 install.packages("writexl");
 install.packages("readxl");
 install.packages("scales");
 install.packages("psych");
#'
#' packages installeds by MQ
install.packages("descr")
library(descr)
#' 
#' 
#' #'[ loading the above packages into R (run one line to load all)
 library(pacman);
 library(tidyverse);
 library(writexl);
 library(readxl);
 library(scales);
 library(psych);
#' 
#' require(pacman)
#' pacman::p_load(
#'   tidyverse, # mostly for dplyr and other necessary packages
#'   writexl,   # i prefer writexl because i can import as sheets
#'   readxl,    # i prefer readxl because i can extract sheets
#'   scales,    # to format certain variables to percent.. etc
#'   psych,     # used for multiple linear regression
#' )



# note : a lot of the data wrangling functions used are among the packages included in tidyverse

# ----------------------- [ o - 0 ] assigning the data frame  ------------------ ####


# loading in the file

setwd( dir = "C:\\Users\\Lenovo\\Documents\\MIS431- Project\\")
AbsEmployees <- read_excel("../MIS431- Project/AbsEmployees.xlsx", sheet = 1)

# Loading file - github fetch version
#setwd( dir = "/Users/mjaali/webapps/R-Squad/Assets/");
#AbsEmployees <- read_excel("MJ Version - AbsEmployees.xlsx", sheet = 1);
#AbsEmployees <- read_excel("Abdulrahman Version - AbsEmployees.xlsx", sheet = 1);



#duplicating the data set to re-code if necessary
emp  <- AbsEmployees
empR <- emp

#'* NOTE *
#'[ emp  : will house the data ready for analysis ( Whenever I want to go back to my original data before any adjustments, I will use AbsEmployees )
#'[ empR : will house the data archived from emp  (I never delete data, I always archive it in case I need to go back to it for the case of mistakes)

# ----------------------- [ 1 - 1 ] data cleaning ( archiving dropped data ) ----------------------------- ####

# creating new columns that will help with the analysis (will be removed later form the environments and data frame )
empR$dup <- data.frame(paste(empR$Surname, empR$GivenName, empR$Gender, empR$JobTitle, empR$DepartmentName, empR$StoreLocation ))


#'[ Employee Age cleaning ]

# extracting age restrictions
empR_age <- subset(empR, empR$Age < 18 | empR$Age > 65 | empR$Age - empR$LengthService < 18 )


# providing a columns with explanation to why the row was removed from the analysis
empR_age$drop <-  case_when( empR_age$Age < 18 ~ "Under Age",
                             empR_age$Age > 65 ~ "Over Age",
                             empR_age$Age - empR_age$LengthService < 18 ~ "Under Age")


#'[ Archiving duplicated Employee Entries ]

# extracting duplicated entries
empR_dupli <- empR[duplicated(empR$dup), ]

# providing a column with explanation to why the row was removed from the analysis
empR_dupli$drop <- paste("duplicated entry")

# merging archived data
empR <- rbind(empR_age, empR_dupli)

# deleting extra columns and removing variables from environment
empR <- subset(empR, select = -c(dup))
rm(empR_age, empR_dupli)

 

# ----------------------- [ 1 - 2 ] data cleaning ( cleaning the data )  --------------------------------- ####

# creating new columns that will help with the analysis (will be removed later form the environments and data frame )
emp$dup <- data.frame(paste(emp$Surname, emp$GivenName, emp$Gender, emp$JobTitle, emp$DepartmentName, emp$StoreLocation ))


#'[ Deleting Under/over age Employee Entries ]
emp <- emp %>% 
  filter(, Age > 18) %>% 
  filter(, Age < 65) %>% 
  filter(, Age - LengthService > 18)


#'[ Deleting duplicated Employee Entries ]

# deleting duplicate entries
emp <- emp[!duplicated(emp$dup), ]

# deleting excess columns
emp <- subset(emp, select = -c(dup))


# ----------------------- [ 1 - 3 ] Data Classification and Tabulation  ---------------------------------- ####



# creating a proportional Absence hours per year in the company
emp$'AbsHrs/year'   <- (emp$AbsentHours/emp$LengthService)


# classifying employees by duty, will be used for a later classification prediction regarding importance of attendance
emp$employeeType <- ifelse( emp$BusinessUnit %in% "HeadOffice" , "Staff", "Worker")
emp$employeeType <- as.factor(emp$employeeType)


# identifying the quartiles, in order to set a real attendance classification levels
boxplot(emp$`AbsHrs/year`, outline = F)
quantile(emp$`AbsHrs/year`)

# classifying employees as per attendance per year
emp$TenureStanding <- case_when( emp$`AbsHrs/year` < 5       ~ "Great Attendence",
                                 emp$`AbsHrs/year` < 20   ~ "Standard Attendence",
                                 emp$`AbsHrs/year` >= 20       ~ "Bad Attendence")

# factoring and leveling the attendance standings
emp$TenureStanding <- factor(emp$TenureStanding, levels = c("Great Attendence", "Standard Attendence", "Bad Attendence"))

#'[ TRY THE data.frame (x = c("aksjdfalsdf"), y = c().... bla bla bla  )                   

# finding the relationship between gender and division for proportional absence hours
empTable_DivGender <- xtabs(`AbsHrs/year` ~ Division + Gender, data = emp)
empTable_DivGender <- cbind(empTable_DivGender, Total = rowSums(empTable_DivGender))
empTable_DivGender <- rbind(empTable_DivGender, Total = colSums(empTable_DivGender))


# finding the relationship between job title and division for proportion absence hours
empTable_DivJobT <- (xtabs(`AbsHrs/year` ~ Division + JobTitle, data = emp))
empTable_DivJobT <- cbind(empTable_DivJobT, Total = rowSums(empTable_DivJobT))
empTable_DivJobT <- rbind(empTable_DivJobT, Total = colSums(empTable_DivJobT))


# finding the store location with the highest proportional absence as per its own frequency of employees
empTable_AbsLocation <- data.frame(xtabs(`AbsHrs/year` ~ StoreLocation, data = emp))
empTable_AbsLocation$count <- table(emp$StoreLocation)
empTable_AbsLocation$propor <- round((empTable_AbsLocation$Freq/empTable_AbsLocation$count), 2)
empTable_AbsLocation <- empTable_AbsLocation %>% 
  rename(, Abs_hrs_per_year = Freq )
empTable_AbsLocation$country <- paste("Canada")

# I need this data to be ordered this way to be able to use it quickly in Power Bi
empTable_AbsLocation <- empTable_AbsLocation[, c("country", "StoreLocation", "count", "Abs_hrs_per_year", "propor")]







# Adding the HasManager new variable to the data frame





# ----------------------- [ 2 - 1 ] data wrangling  ------------------------------------------------------ ####



# ----------------------- [ 2 - 1 ] data wrangling  ------------------------------------------------------ ####
#'[ predictive analytics
colnames(emp)
Model_1 <- lm(AbsentHours ~ Gender + JobTitle + DepartmentName + StoreLocation  + Division + Age + LengthService + BusinessUnit, data = emp); summary(Model_1)


# ----------------------------------- 


#'[ DEADZONE ( anything below this sections are codes removed, changed, or to be brought back again in case we need it )

loca <- as.data.frame.matrix(table( x = emp$JobTitle, y = emp$StoreLocation))
loca$meow <- unique(emp$JobTitle)
write_xlsx(loca, "../MIS431- Project/fasttablu.xlsx")
export(loca, "../MIS431- Project/fasttabluRIO.xlsx")


loca <- data.frame(table(emp$JobTitle, emp$StoreLocation))
loca <- data.frame(table(emp$StoreLocation, emp$JobTitle)); View(loca)
crosstab(emp, row.vars = "JobTitle", col.vars = "StoreLocation", type = "f")



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

tacos <- filter(emp, emp$JobTitle %in% cat )
cat <- c("Bakery Manager", "Trainer")
rm(cat, tacos)

colnames(emp)
colnames(empR)
glimpse(emp)

(neW <- data.frame(aggregate( `AbsHrs/year` ~ JobTitle, data = emp, sum)))
(olD <- data.frame(aggregate( `AbsHrs/year` ~ StoreLocation, data = emp, sum)))


# quartilers and tings like that 

meow <- ggplot(data = emp, aes(y = `AbsHrs/year`))
meow + geom_boxplot(outlier.shape = NA) + coord_cartesian(ylim = c(0, 50))

boxplot(emp$`AbsHrs/year`, outline = F)
quantile(emp$`AbsHrs/year`)

# 
# # creating a proportional hours per tenure year column, rounding it to two decimals
# emp$'AbsHrs/year'   <- round((emp$AbsentHours/emp$LengthService), 2)
# emp$AbsentHours     <- round(emp$AbsentHours, 0) 
# emp$LengthService   <- round(emp$LengthService, 0)
# emp$Age             <- round(emp$Age, 0)



AbsEmployees$'AbsHrs/year'   <- as.integer((AbsEmployees$AbsentHours/AbsEmployees$LengthService))
AbsEmployees$AbsentHours     <- as.integer(AbsEmployees$AbsentHours)
AbsEmployees$LengthService   <- as.integer(AbsEmployees$LengthService)
AbsEmployees$Age             <- as.integer(AbsEmployees$Age)
glimpse(AbsEmployees)




# creating a proportional hours per tenure year column, rounding it to two decimals
emp$'AbsHrs/year'   <- as.integer((emp$AbsentHours/emp$LengthService))
emp$AbsentHours     <- as.integer(emp$AbsentHours)
emp$LengthService   <- as.integer(emp$LengthService)
emp$Age             <- as.integer(emp$Age)
glimpse(emp)


empTable_DivGender_new <- data.frame(unique(emp$Division))
empTable_DivGender_new[nrow(empTable_DivGender_new) + 1, ] <- c("total")
empTable_DivGender <- as.data.frame(empTable_DivGender)
empTable_DivGender$Division <- as.data.frame(empTable_DivGender_new)
rm(empTable_DivGender_new)

write_xlsx(empTable_DivGender, "../MIS431- Project/2. output/Division Vs Gender1.xlsx")
write_excel(empTable_DivGender, "../MIS431- Project/2. output/Division Vs Gender.xlsx")

typeof(empTable_DivGender)

OLSdata <- emp
# ###################################### 
# Packages installed by MQ
install.packages('fastDummies')
library('fastDummies')
# #########################################

OLSdata
# In this step, we will drop the data that is not helpful in our regression, and will affect our model and results in insufficient way

OLSdataDrop = subset(OLSdata, select = -c(JobTitle, EmployeeNumber, Surname, GivenName, DepartmentName ) )

#'
# Creating dummy variables

OLSdataDrop <- dummy_cols(OLSdataDrop, select_columns = c('Gender', 'StoreLocation', 'Division', 'BusinessUnit', 'new', 'employeeType', 'TenureStanding'))

# Deleting the columns 

OLSdataDrop.2 <- dummy_cols(OLSdataDrop, select_columns = c('Gender', 'StoreLocation', 'Division', 'BusinessUnit', 'new', 'employeeType', 'TenureStanding'),
           remove_selected_columns = TRUE)
#'
#'
gsub(" ", "_", OLSdata)
 
#'
model2 <- lm(`AbsHrs/year` ~ `Age` + `LengthService` + `AbsentHours` + `Gender_F` + `Gender_M` + `StoreLocation_Abbotsford` + `StoreLocation_Aldergrove` + `StoreLocation_Bella Bella` + `StoreLocation_Blue River` + `StoreLocation_Burnaby` + `StoreLocation_Chilliwack` + `StoreLocation_Cortes Island` + `StoreLocation_Cranbrook` + `StoreLocation_Dawson Creek` + `StoreLocation_Dease Lake` + `StoreLocation_Fort Nelson` + `StoreLocation_Fort St John` + `StoreLocation_Grand Forks` + `StoreLocation_Haney` + `StoreLocation_Kamloops` + `StoreLocation_Kelowna` + `StoreLocation_Langley` + `StoreLocation_Nanaimo` + `StoreLocation_Nelson` + `StoreLocation_New Westminister` + `StoreLocation_New Westminster` + `StoreLocation_North Vancouver` + `StoreLocation_Ocean Falls` + `StoreLocation_Pitt Meadows` + `StoreLocation_Port Coquitlam` + `StoreLocation_Prince George` + `StoreLocation_Princeton` + `StoreLocation_Quesnel` + `StoreLocation_Richmond` + `StoreLocation_Squamish` + `StoreLocation_Surrey` + `StoreLocation_Terrace` + `StoreLocation_Trail` + `StoreLocation_Valemount` + `StoreLocation_Vancouver` + `StoreLocation_Vernon` + `StoreLocation_Victoria` + `StoreLocation_West Vancouver` + `StoreLocation_White Rock` + `StoreLocation_Williams Lake` + `Division_Executive` + `Division_FinanceAndAccounting` + `Division_HumanResources` + `Division_InfoTech` + `Division_Legal` + `Division_Stores` + `BusinessUnit_HeadOffice` + `BusinessUnit_Stores` + `new_about to die` + `new_mid life crisis` + `new_old` + `new_young` + `employeeType_Staff` + `employeeType_Worker` + `TenureStanding_Great Attendence` + `TenureStanding_Standard Attendence` + `TenureStanding_Bad Attendence` , data = OLSdataDrop.2)

#write.csv(OLSdataDrop.2,"C:\\Users\\Lenovo\\Documents\\MIS431- Project\\Dummy.csv", row.names = TRUE)

model2 <- lm(AbsentHours ~ 'Age' + LengthService, data = OLSdataDrop.2)

