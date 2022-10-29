<!-- ---
title: "MIS-341 Employee Absenteesim Course Project"
author: "Group 4 - Section 2"
date: "Oct 25, 2022"
output: 
  html_document:
    toc: true
    theme: readable
--- -->

| Title        | MIS-341 Employee Absenteesim Course Project           |
| :------------- |:-------------|
| Author    | Sec. 2 - G4|
| Date | Oct 25, 2022|

# Cover Page
<img src="https://mjaali.github.io/R-Squad/Assets/Brand/Rsquad%20Project%20Report%20Header.png"/>

there will be a lot of text written here and a lot of description of the text that is going to be written down
under each one of these parts and sections, there will be text to describe them properly and so on and what not
there will be a lot of text written here and a lot of description of the text that is going to be written down
under each one of these parts and sections, there will be text to describe them properly and so on and what not


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

# install.packages("tidyverse",  repos = "http://cran.us.r-project.org")
# install.packages("pacman",     repos = "http://cran.us.r-project.org")
# install.packages("writexl",    repos = "http://cran.us.r-project.org")
# install.packages("readxl",     repos = "http://cran.us.r-project.org")
# install.packages("rmarkdown",  repos = "http://cran.us.r-project.org")       
# install.packages("here",       repos = "http://cran.us.r-project.org")

#'[ loading the above packages into R

# library(tidyverse)
# library(pacman)
# library(writexl)
# library(readxl)
# library(rmarkdown)
# library(here)
```

-----

# Introduction
This project is chartered to solve the absenteeism issue at __*Canada Stores ltd*__ using data analytics. The absenteeism issue costs the company enormous expenses, and the use of data analytics can identify the most contributing factors that lead to absenteeism and can tap into recommendations to tackle this HR challenge.

Using the provided data, we'll define the underlying problem using root-cause analysis. Then, investigate the available data elements and features at our disposal to inspire insights. And finally, evaluate and enhance data quality.

## 1-    Problem Definition
We have a set of data features obtained from HR records about employees containing __*8336*__ entries. 

| Feature        | Type           |
| :------------- |:-------------|
| `EmployeeNumber` | 
| `Surname` | 
| `GivenName` | 
| `Gender` | 
| `JobTitle` | 
| `DepartmentName` | 
| `StoreLocation` | 
| `Division` | 
| `Age` | 
| `LengthService` | 
| `AbsentHours` | 
| `BusinessUnit` | 
| `AbsentHoursPerTenure` | :new:	

## 2-   Problem Decomposition

Multiple angles were examined to identify the most prominent congestions of absenteeism.

The below table investigates the breakdown of abisentism by division. with an added metric for `Culture of Absenteeism`.

Division | # Employees | Avg tenure | Total Absenteeism | Avg. Abs. / tenure | Absenteeism Culture* |
| :------------- |:-------------|:-------------|:-------------|:-------------|:-------------|
Stores | 8163 | 5 | 502722 | 16 | 29053 | 
FinanceAndAccounting | 73 | 12 | 2918 | 13 | 80 | 
HumanResources | 76 | 19 | 4137 | 5 | 20 | 
Executive | 11 | 11 | 532 | 8 | 8 | 
InfoTech | 10 | 12 | 401 | 4 | 3 | 
Legal | 3 | 11 | 154 | 6 | 1 | 

* This is a synthatic measure 


## 3-   Data Prepossessing

there will be a lot of text written here and a lot of description of the text that is going to be written down
under each one of these parts and sections, there will be text to describe them properly and so on and what not

### 3-i       Data Exploration

there will be a lot of text written here and a lot of description of the text that is going to be written down
under each one of these parts and sections, there will be text to describe them properly and so on and what not

### 3-ii       Data Cleaning

there will be a lot of text written here and a lot of description of the text that is going to be written down
under each one of these parts and sections, there will be text to describe them properly and so on and what not

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
