rm(list = ls())
setwd('/home/laurits/Desktop/BGSE/Term 2/Financial Econometrics/forecast-competition')
data <- 'forecast-competition-training.csv'

library(dplyr)
library(ggplot2)
library(tseries)
library(moments)
library(sandwich)
library(lmtest)
library(forecast)
library(fGarch)
library(vars)


df <- read.csv(data)
