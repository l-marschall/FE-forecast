rm(list=ls())
##########################################################
# FORECAST COMPETITION
##########################################################
wd <- '/Users/jordi/Box/12F005 Financial Econometrics/forecast-competition/'
setwd(wd)
data <- read.csv('forecast-competition-training.csv',header=T)
# Need to forecast the TARGET variable
# target <- data['TARGET']
# we can use the past of the series as well as past realizations of other series
# in the panel

################ PREDICTION: #############
# load libraries
library("caret")

prepProc <- function(df, method){
  if (method == 'center'){
    #center the data
    preProc <- preProcess(df, method = c("center"))
    df.centered <- predict(preProc, df)
    return(df.centered)
  }
  
  if (method =='scale'){
    # Scale the data
    preProc <- preProcess(df, method = c("scale"))
    df.scaled <- predict(preProc, df)
    return(df.scaled)
  }
  
  # Rescale the data to a [0,1] range.
  preProc <- preProcess(df, method = c("range"))
  df.range <- predict(preProc, df)
  return(df.range)
}

