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
