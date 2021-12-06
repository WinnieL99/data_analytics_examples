# HW2, Yingxin Lin, Oct 4,2021
######### I-Applying Computational Tools:R


#Install packages
install.packages("plyr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages(c("tidyverse"), dependencies = TRUE)

library(plyr)  #A data manipulation library - always load this first
library(dplyr) #Another data manipulation 
library(readr)
library(tidyverse)

getwd() #The current location where R is looking for files 
dir() #The files in the working directory. 

# (a) Read in the data asec2014 pubuse.csv household level data.
#######  If you want to test the code, you need to change the file location
asec2014_house <- read_fwf(file="HWData/asec2014_pubuse.dat", fwf_positions(c(1,2,25,39,42,49,53),c(1,6,25,39,43,51,53),col_names = c("type","hid","htype","region","state","country","metro_status")))

# (b) Keep if type == 1 (i.e. it is a "household" observation).
asec2014_house <- asec2014_house %>%
  dplyr::filter(type == 1)

# (c) Drop variable type.
asec2014_house <- select(asec2014_house,-c("type"))

# (d) Sort observations by hid.
asec2014_house$hid <- as.numeric(asec2014_house$hid)
asec2014_house <- asec2014_house %>%
  dplyr::arrange(hid) 

# (e) Save as an R dataset and write the dataset to cps14_house.csv
readr::write_csv(x = asec2014_house, path = "cps14_house.csv")

# (f) Read in the data asec2014_pubuse.csv person level data.
#######  If you want to test the code, you need to change the file location
asec2014_personal <- read_fwf(file="HWData/asec2014_pubuse.dat", fwf_positions(c(1,2,15,19,21,24,25,27,48,364),c(1,6,16,20,21,24,26,28,49,370),col_names = c("type","hid","relationship","age","marital_status","sex","educ","race","fid","wsal")))

# (g) Keep if type == 3 (i.e. it is a "person" observation)
asec2014_personal <- asec2014_personal %>%
  dplyr::filter(type == 3)

# (h) Drop type.
asec2014_personal <- select(asec2014_personal,-type)

# (i) Sort by hid.
asec2014_personal$hid <- as.numeric(asec2014_personal$hid)
asec2014_personal <- asec2014_personal %>%
  dplyr::arrange(hid)

# (j) Save as an R dataset and write the dataset to cps14_person.csv
readr::write_csv(x = asec2014_personal, path = "cps14_person.csv")

# (k) Merge the household and person level datasets on hid.
# (l) Only keep observations that appeared in both datasets.
merged_file <- merge(asec2014_house,asec2014_personal)

# (m) Write the merged dataset to cps14_merge.csv.
readr::write_csv(x = merged_file, path = "cps14_merge.csv")

# (n) We only want the ages of the working population and people who have finished their education.
# So let's keep people with age >= 25 and age <=64.
asec2014_personal$age <- as.numeric(asec2014_personal$age)
asec2014_personal$educ <- as.numeric(asec2014_personal$educ)
asec2014_personal <- asec2014_personal %>%  
  dplyr::filter(age >= 25 & age<=64)

# (o) Drop all entries with metro_status == 3, since this corresponds to an unidentified metro area
asec2014_house <- asec2014_house %>% 
  dplyr::filter(metro_status != 3)

# (p) We want to rule out part time workers, so drop entries where wsal < 10000.
asec2014_personal$wsal <- as.numeric(asec2014_personal$wsal)
asec2014_personal <- asec2014_personal %>%  
  dplyr::filter(wsal >= 10000)

# (q) To make our data easier to read, replace wsal with wsal / 1000.
asec2014_personal <- asec2014_personal %>%  
  mutate(wsal = wsal / 1000)

# (r) Set hs == 1 if educ >= 39 and educ <= 42, otherwise hs == 0.
asec2014_personal <- asec2014_personal %>% 
  add_column(hs = if_else(.$educ >= 39 & .$educ <=42, 1, 0))

# (s) Set col == 1 if educ >= 43, otherwise col == 0.
asec2014_personal <- asec2014_personal %>% 
  add_column(col = if_else(.$educ >= 43, 1, 0))

# (t) Set female == 1 if sex == 2, otherwise female == 0.
asec2014_personal <- asec2014_personal %>% 
  add_column(female = if_else(.$sex == 2, 1, 0))

# Generate a table of summary statistics for hs, col, female, and wsal - TO THREE DIGITS. 
# Include the number of observations, mean, minimum and maximum values.
# Include your R script and your summary statistics as part of your homework. 
# You do not need to include any of the datasets in your homework submission.

new_personal <- subset(asec2014_personal,select=c(hs,col,female,wsal))
summary(new_personal, digits = 3)



######### II - Applying Theory - Regression Analysis
# 2. For each of the predictors, X1, X2, X3, X4, X5, 
# fit a simple linear regression model to predict the response.
data <- read.csv("gpa_f20.csv")

## Model for X1:sat
fit1 <- lm(colgpa ~ sat, data=data)
jtools::summ(fit1, confint = TRUE, digits = 4)

## Model for X2:tothrs
fit2 <- lm(colgpa ~ tothrs, data=data)
jtools::summ(fit2, confint = TRUE, digits = 4)

## Model for X3:hsize
fit3 <- lm(colgpa ~ hsize, data=data)
jtools::summ(fit3, confint = TRUE, digits = 4)

## Model for X4:hsrank
fit4 <- lm(colgpa ~ hsrank, data=data)
jtools::summ(fit4, confint = TRUE, digits = 4)

## Model for X5:hsperc
fit5 <- lm(colgpa ~ hsperc, data=data)
jtools::summ(fit5, confint = TRUE, digits = 4)

# 3. Fit a multiple regression model to predict the response 
# using all of the predictors for which you rejected the null hypothesis in the previous question.
# fit6: not removing hsize to a multiple regression model
fit6 <- lm(colgpa ~ sat + tothrs + hsize + hsrank + hsperc, data=data)
jtools::summ(fit6, confint = TRUE, digits = 4)

# fit7: removing the hsize to fit a multiple regression model
fit7 <- lm(colgpa ~ sat + tothrs + hsrank + hsperc, data=data)
jtools::summ(fit7, confint = TRUE, digits = 4)

# 4. Based on your results from the previous question, holding all other predictors steady, 
# how much would you expect a student's predicted GPA to increase for every 100 points increase in their SAT score?
# Using the fit1 model to predict
coef(fit1)[2]*100
# Using the fit7 model to predict
coef(fit7)[2]*100


# 5. Take your model from question (3) and add another predictor for the variable female. 
# This is a binary predictor which is returns 1 for female and 0 for non-female. 
# Based on this multiple linear regression, what is the impact of being female on colgpa?
fit8 <- lm(colgpa ~ sat + tothrs + hsrank + hsperc + female, data=data)
jtools::summ(fit8, confint = TRUE, digits = 4)
