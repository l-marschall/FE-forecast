###########################################################################
## Setup
###########################################################################

rm(list=ls())
setwd(paste0("/home/jan/Documents/Courses/T2_FE/Assignments/",
             "ForecastCompetition"))

## Load packages
source("tarch.R") # function written by CB to run tarch
require(tseries) # to perform ADF test

###########################################################################
## Get data
###########################################################################

df <- read.table("forecast-competition-training.csv", header=TRUE, sep=',')
target <- df[,1] # variable we wish to forecast

###########################################################################
## Clean data
###########################################################################

# check for non-stationarity and if necessary calculate differences
adf.test(target) # whole series is stationary
adf.test(target[300:500]) # last 200 observations are NOT stationary

target_d1 <- diff(target) # calculate first differences
adf.test(target_d1) # whole series of 1Diff is stationary
adf.test(target_d1[300:500]) # last 200 observations of 1Diff ARE stationary


tarch <- Tgarch11(x=ret)

###########################################################################
###########################################################################
###########################################################################
## OLD
###########################################################################
###########################################################################
###########################################################################

plot(target[100:500],type="l")
plot(target_d1, type = "l")
plot(target_d2, type = "l")
