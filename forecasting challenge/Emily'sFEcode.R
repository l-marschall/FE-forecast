rm(list=ls())
library(caret)
library(klaR)
source('tarch.R')
df <- read.csv("forecast-competition-training.csv") # training data


# -------------------------------------- ARMA(1,1) Model -------------------------------------- #

# ARMA11
target_pred1 <- function(x){
  arma11 <- arima(x$TARGET, order = c(1,0,1))
  pred_arma11 <- predict(arma11, n.ahead=1)
  forecast <- pred_arma11$pred
  return(forecast)
}

# ARMA22
target_pred2 <- function(x){
  arma11 <- arima(x$TARGET, order = c(0,0,2))
  pred_arma11 <- predict(arma11, n.ahead=1)
  forecast <- pred_arma11$pred
  return(forecast)
}

# ARMA33
target_pred3 <- function(x){
  arma11 <- arima(x$TARGET, order = c(1,0,3))
  pred_arma11 <- predict(arma11, n.ahead=1)
  forecast <- pred_arma11$pred
  return(forecast)
}

forecasts1 <- c()
for (i in 1:50){
  train <- df[i:(i+449),]
  forecasts1[i] <- target_pred1(train)
}

forecasts2 <- c()
for (i in 1:50){
  train <- df[i:(i+449),]
  forecasts2[i] <- target_pred2(train)
}

forecasts3 <- c()
for (i in 1:50){
  train <- df[i:(i+449),]
  forecasts3[i] <- target_pred3(train)
}

forecasts

source("cross_validation.R")

cross_mse(forecasts1, 50, df)
cross_mse(forecasts2, 50, df)
cross_mse(forecasts3, 50, df)

# -------------------------- Rolling (50 periods) -------------------------- #
# ARMA(1,1): MSE=0.631
# ARMA(2,2): MSE=0.593
    # AR(2): MSE=0.562          #
    # MA(2): MSE=0.864
    # ARMA(2,1): MSE=0.5894
    # ARMA(1,2): MSE=0.5895
# ARMA(3,3): MSE=0.601
    # AR(3): MSE=0.585
    # ARMA(3,1): MSE=0.584     
    # ARMA(3,2): MSE=0.589
    # MA(3): MSE=0.794
    # ARMA(1,3): MSE=0.0.586       
    # ARMA(2,3): MSE=0.593
# ARIMA(1,1,1): MSE=0.576
# ARIMA(2,1,2): MSE=0.616

# -------------------------------------- ARMA(1,1) Model -------------------------------------- #

# ARMA11
target_pred01 <- function(x){
  arma11 <- arima(x$TARGET, order = c(1,0,1))
  pred_arma11 <- predict(arma11, n.ahead=1)
  forecast <- pred_arma11$pred
  return(forecast)}

# ARMA22
target_pred02 <- function(x){
  arma11 <- arima(x$TARGET, order = c(0,0,2))
  pred_arma11 <- predict(arma11, n.ahead=1)
  forecast <- pred_arma11$pred
  return(forecast)}

# ARMA33
target_pred03 <- function(x){
  arma11 <- arima(x$TARGET, order = c(3,0,3))
  pred_arma11 <- predict(arma11, n.ahead=1)
  forecast <- pred_arma11$pred
  return(forecast)}

forecasts01 <- c()
for (i in 1:50){
  train <- df[1:(i+449),]
  forecasts01[i] <- target_pred01(train)}

forecasts02 <- c()
for (i in 1:50){
  train <- df[1:(i+449),]
  forecasts02[i] <- target_pred02(train)}

forecasts03 <- c()
for (i in 1:50){
  train <- df[1:(i+449),]
  forecasts03[i] <- target_pred03(train)
}


cross_mse(forecasts01, 50, df)
cross_mse(forecasts02, 50, df)
cross_mse(forecasts03, 50, df)



# -------------------------- NOT Rolling (50 periods) -------------------------- #

# ARMA(1,1): MSE=0.608
# ARMA(2,2): MSE=0.567        
    # ARMA(2,1): MSE=0.572
    # ARMA(1,2): MSE=0.573
    # AR(2): MSE=0.557        #
    # MA(2): MSE=0.849
# ARMA(3,3): MSE=0.597
    # AR(3): MSE=0.573
    # ARMA(3,1): MSE=0.570     
    # ARMA(3,2): MSE=0.571
    # MA(3): MSE=0.779
    # ARMA(1,3): MSE=0.570       
    # ARMA(2,3): MSE=0.
# ARIMA(1,1,1): MSE=0.
# ARIMA(2,1,2): MSE=0.
