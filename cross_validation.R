# cross validation function

cross_mse = function(prediction, cross_periods, df) {
  p <- length(prediction)
  if (cross_periods <= p) {
    prediction <- prediction[(p-cross_periods):p]
    target <- df[,1]
    n <- length(target)
    target <- target[(n - cross_periods):n]
    mse <- rep(0, p)
    for (i in 1:cross_periods) {
      mse[i] <- (target[i] - prediction[i])**2 
    }
    avg_mse <- mean(mse)
    print(avg_mse)
  }
  else print('More cross_periods selected than available prediction periods!')
}
