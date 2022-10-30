<!-- ---
title: "MIS-341 Employee Absenteesim Course Project"
author: "Group 4 - Section 2"
date: "Oct 25, 2022"
output: 
  html_document:
    toc: true
    theme: readable
--- -->

<img src="https://mjaali.com/R-Squad/Assets/Brand/Rsquad%20Project%20Report%20Header.png"/>

| Title        | MIS-341 Employee Absenteesim Course Project           |
| :------------- |:-------------|
| Author    | Sec. 2 - G4|
| Date | Oct 25, 2022|



-----

# TOC

- [Cover page](#cover-page)
- [TOC](#toc)
- [Execututive Summary](#execututive-summary)
- [Introduction](#introduction)
  * [1-    Problem Definition](#1-----problem-definition)
  * [2-   Problem Decomposition](#2----problem-decomposition)
  * [3-   Data Prepossessing](#3----data-prepossessing)
    + [3-i       Data Exploration](#3-i-------data-exploration)
    + [3-ii       Data Cleaning](#3-ii-------data-cleaning)
    + [3-iii       Data Wrangling](#3-iii-------data-wrangling)
    + [3-iv       Data Classifications and Finalization](#3-iv-------data-classifications-and-finalization)
- [Analysis](#analysis)
  * [1-   What is happening?](#1----what-is-happening-)
    + [1-i   Locational Analysis... ????](#1-i---locational-analysis-----)
  * [2-   What is going to happen?](#2----what-is-going-to-happen-)
    + [2-i   locational analysis... ????](#2-i---locational-analysis-----)
- [Recommendations](#recommendations)



-----

my second favorite theme is         $Cerulean$


# Execututive Summary

[press this linked to be hacked](https://www.youtube.com/watch?v=tKUufzpoHDE&t=183s&ab_channel=JalayerAcademy)

I guess i can type `use the tilde symbol ~ , not the half quotation '' ` here, so i would like to > quote that i am amazing

just use the keyboard shortcut **Ctrl Alt i** so this is the best ever
and to quickly knit the rmd press **Ctrl Shift K**

i can write things as in subscript in r studio ~SomethingLikeThis~ , to use subscript, don't ~UseSpacesBetweenWords~

> below are the R packages/libraries to be used in the report __*help*__


```{r Packages}
#'[ Installing all necessary packages into R

install.packages("tidyverse",  repos = "http://cran.us.r-project.org")
install.packages("pacman",     repos = "http://cran.us.r-project.org")
install.packages("writexl",    repos = "http://cran.us.r-project.org")
install.packages("readxl",     repos = "http://cran.us.r-project.org")
install.packages("rmarkdown",  repos = "http://cran.us.r-project.org")       
install.packages("here",       repos = "http://cran.us.r-project.org")

#'[ loading the above packages into R

library(tidyverse)
library(pacman)
library(writexl)
library(readxl)
library(rmarkdown)
library(here)
```

-----

# Introduction
This project is chartered to solve the absenteeism issue at __*Canada Stores ltd*__ using data analytics. The absenteeism issue costs the company enormous expenses, and the use of data analytics can identify the most contributing factors that lead to absenteeism and can tap into recommendations to tackle this HR challenge.

Using the provided data, we'll define the underlying problem using root-cause analysis. Then, investigate the available data elements and features at our disposal to inspire insights. And finally, evaluate and enhance data quality.

## 1-    Problem Definition
We have a set of data features obtained from HR records about employees containing __*8336*__ entries. 

Rolls Royce-Quality Data

| Feature        | Type           |
| :------------- |:-------------:|
| `EmployeeNumber` | 
| `Surname` | 
| `GivenName` | 
| `Gender` | 
| `ProbableGenderByGivenName` | :new:	
| `JobTitle` | 
| `DepartmentName` | 
| `StoreLocation` | 
| `Longitude` | :new:	
| `Latitude` | :new:	
| `StoreType` | :new:	
| `Division` | 
| `Age` | 
| `LengthService` | 
| `AbsentHours` | 
| `AbsentHoursPerTenure` | :new:	
| `BusinessUnit` | 

`ProbableGenderByGivenName` is a fetched from an extername names database to cross-check data quality.
`AbsentHoursPerTenure` was added to better inform the annual absence for an employee regardless of their service years.
`Longitude` & `Latitude` were added beacuse `StoreLocation` is not standardized, i.e. can be a city name or a neighborhood name. It's a good substitute for a branch name, except Business Unit of Head Office in Vancouver. We elicited the corresponding values of stores. These values are to proxy the location in a standard manner and __*do not represent store coordinates*__. This will come in handy for map visualization.

`StoreType` are classified into 3 types of stores. If a customer service manager exsists and more than 40 cashoiers/

Small stores vs big stores absentieesm (سقط الاثم عن الباقين في الستور الكبير)
what number of store optimizes the absentism? (HR added units economic theory)


Cashiers per custmer service manager ratio
Meat cutter per Meat manager ratio
Store manger per store emplyee ratio


Does having a custmer service manager reduce absentieesm?

## 2-   Problem Decomposition

Multiple angles were examined to identify the most prominent congestions of absenteeism.

The below table investigates the breakdown of abisentism by division. 

Division | # Employees | Avg tenure | Total Absenteeism | Avg. Abs. / tenure | Absenteeism Culture |
| :------------- |:-------------|:-------------|:-------------|:-------------|:-------------|
Stores | 8163 | 5 | 502722 | 16 | 29053 | 
FinanceAndAccounting | 73 | 12 | 2918 | 13 | 80 | 
HumanResources | 76 | 19 | 4137 | 5 | 20 | 
Executive | 11 | 11 | 532 | 8 | 8 | 
InfoTech | 10 | 12 | 401 | 4 | 3 | 
Legal | 3 | 11 | 154 | 6 | 1 | 

The added metric `Culture of Absenteeism` is a synthatic measure for how strong is the absenteesm culture is in the divison. It's the byproduct of the (__*number of employees*__ x __*average absence hrs per tenure for employees in that division*__). if the avg. abs. per tenure increases, we assume that it's normalized in that division to be absent or late cumulatively, and when the number of employees is higher, it's more difficult to break and change this culture. More investments, policy, training, and time are required to influence that culture.

We will further break this down by `DepartmentName` and `JobTitle` in the 2 following tables, respectively. 

- Table 1

- Table 2

TWO VERY IMPORTANT TO ADD:
`HasManager`
`IsManager`


We find out that the most diffucilt culture in the company is __*Customer Service*__. And the job title of Cashier is the most difficlt under the customer service departnemtn.
We also hypothise that this job title is mission-critial and a bottle neck. It may be influencing other job titles in stores, i.e. if the cashier is late or absent, there's no need for it builds less importance with meat cutter if.
It's also important to note that if the CRM/PoS system is connected to the cashier terminal, we can explain the high co-relation between cashier absentieesm and other store functions (job titles).

## 3-   Data Prepossessing

there will be a lot of text written here and a lot of description of the text that is going to be written down
under each one of these parts and sections, there will be text to describe them properly and so on and what not

### 3-i       Data Exploration

there will be a lot of text written here and a lot of description of the text that is going to be written down
under each one of these parts and sections, there will be text to describe them properly and so on and what not

### 3-ii       Data Cleaning

- Trim Age ends.
- Remove duplicate values.
- Clean cities with 2 employees.

### 3-iii       Data Wrangling

there will be a lot of text written here and a lot of description of the text that is going to be written down
under each one of these parts and sections, there will be text to describe them properly and so on and what not


### 3-iv       Data Classifications and Finalization

there will be a lot of text written here and a lot of description of the text that is going to be written down
under each one of these parts and sections, there will be text to describe them properly and so on and what not


-----

#   Analysis

there will be a lot of text written here and a lot of description of the text that is going to be written down
under each one of these parts and sections, there will be text to describe them properly and so on and what not

## 1-   What is happening?

there will be a lot of text written here and a lot of description of the text that is going to be written down
under each one of these parts and sections, there will be text to describe them properly and so on and what not

### 1-i   Locational Analysis... ????

there will be a lot of text written here and a lot of description of the text that is going to be written down
under each one of these parts and sections, there will be text to describe them properly and so on and what no

### 1-ii Gender equality analysis


## 2-   What is going to happen?

there will be a lot of text written here and a lot of description of the text that is going to be written down
under each one of these parts and sections, there will be text to describe them properly and so on and what not


### 2-i   locational analysis... ????

there will be a lot of text written here and a lot of description of the text that is going to be written down
under each one of these parts and sections, there will be text to describe them properly and so on and what no


-----

# Recommendations

The Cashier has the highest Absinteesm Culture level. In the city of Vancouver, the age `24.51` y/o and beyond is where Absenteeism hours escalate with a strong correlation for both males and females at a `slope of 0.9395x`. Since our x-intercept is `24.51`, or in other words, Cashiers below the age of 24.51 have no absinteesm hours `(0 Hrs / year)`. The avg. tenure of the Cashier is `4.58 years`. A policy to hire Cashiers of a maximum of `20 y/o` will ensure most cashiers will clear out of the company before they turn 25 and reduce overall company absenteeism significantly.

# Future works
- in our next phase of the engagment, we can investigate how can we reduce absence hours as emplyees grow service years possibly by enhancing the promotion or re-location.
- We can quantify the exact effect of asintesm by emplyee on the revenue to find the best optimization of effort that will drive sales.
- We can look into the fininical penalties to emplyees as they increase absentesem or show up late to reduce the waste and effect on revenues and company opertional costs. if high absence and no effect on work or rev. then consider terminating them.
- When do latness and absentisem happen? are they related to seasons? or mostly happen in low rush hours?
- Imporving the data collection mechanism to enchance data quality and integrity, such as systems or employee attendance managnment in stores.
- we need to analyze the payroll system and look into the process of deboarding/retiring 65 y/o and above.
