# cross validation function

cross_mse = function(prediction, cross_periods, df) {
  p <- length(prediction)
  if (cross_periods <= p) {
    prediction <- prediction[(p-cross_periods):p]
    target <- df[,1]
    n <- length(target)
    target <- target[(n - cross_periods):n]
    mse <- c()
    for (i in 1:cross_periods) {
      mse[i] <- (target[i] - prediction[i])**2 
    }
    avg_mse <- mean(mse)
    var_mse <- var(mse)
    print(paste0('Mean:', round(avg_mse, digits = 4)))
    print(paste0('Variance:', round(var_mse, digits = 4)))
  }
  else print('More cross_periods selected than available prediction periods!')
}



lm_prediction <- function(df){
  n <- nrow(df)
  df.train <- df[1:(n-1),]
  df.test <- df[(n-1):n,]
  lm.fit <- lm(TARGET ~ ., df.train)
  pred_lm.fit <- predict(lm.fit, newdata = df.test)
  return(pred_lm.fit)
}

lm_forecast <- c()
for (i in 1:50){
  train <- df[i:(i+449),]
  lm_forecast[i] <- lm_prediction(train)
}
lm_forecast

cross_mse(lm_forecast, 50, df)
