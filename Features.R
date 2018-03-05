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

feature.engineering <- function(X){
  
}