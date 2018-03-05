###########################################################################
## Setup
###########################################################################

rm(list=ls())
setwd(paste0("/home/jan/Documents/Courses/T2_FE/Assignments/",
             "ForecastCompetition/FE-forecast/"))

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

###########################################################################
## Do tarch
###########################################################################

tarch_forecast <- function(data){
  ## IN
  ###### data (vector): input data used for forecasting
  ## OUT
  ###### forecast (vector): forecasts done
  #########################################################################
  tarch <- Tgarch11(x=data) # estimate tarch to get parameters
  m <- tarch$par[1]
  w <- tarch$par[2]
  a <- tarch$par[3]
  g <- tarch$par[4]
  b <- tarch$par[5]
  r_t1 <- data[length(data)] # last periods return
  vol_t1 <- tarch$volatility[length(tarch$volatility)]# last periods vol.
  ## forecast volatility
  vol_t_2 <- w + a*r_t1^2 + g*r_t1^2*(r_t1<0) + b*vol_t1^2
  ## forecast returns
  print("lala")
  print(vol_t_2)
  print("lala")
  r_t <- sqrt(vol_t_2)*rnorm(1,0,1)
  return(r_t)  
}

tarch_forecast(data=target_d1[100:300])


rnorm(1,0,1)
3+(4>3)
tarch <- Tgarch11(x=target_d1[300:499])
tarch <- Tgarch11(x=target_d1)

lala <- target_d1[1:499]

tarch <- Tgarch11(x=lala)

tarch$par
tarch$residuals
tail(tarch$volatility)

###########################################################################
###########################################################################
###########################################################################
## OLD
###########################################################################
###########################################################################
###########################################################################

plot(target[1:500],type="l")
plot(target_d1, type = "l")
plot(target_d2, type = "l")
