# cross validation function

cross_mse = function(prediction, cross_periods, df) {
  p <- length(prediction)
  prediction <- prediction[(p-cross_periods):p]
  target <- df[,1]
  n <- length(target)
  target <- target[(n - cross_periods):n]
  mse <- rep(0, p)
  for (i in 1:cross_periods) {
    mse[i] <- (target[i] - prediction[i])**2 
  }
  avg_mse <- mean(mse)
}
