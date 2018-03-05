rm(list = ls())
setwd('/home/laurits/Desktop/BGSE/Term 2/Financial Econometrics/FE-forecast')


cross_periods <- length(prediction)
mse <- rep(0, cross_periods)
  
for (i in 1:cross_periods) {
  mse[i] <- (target[i] - prediction[i])**2 
}

avg_mse <- mean(mse)