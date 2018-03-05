rm(list=ls())
library(caret)
library(klaR)
setwd("Dropbox/BGSE/DataScience/TERM_2/FE/Forecasting_Comp/")
setwd("TERM_2/FE/Forecasting_Comp/")
setwd("Forecasting_Comp/")
source('tarch.R')
df <- read.csv("forecast-competition-training.csv") # training data
head(df)
View(df)

# ----------------------------------- Splitting Train:Test ----------------------------------- #
#n <- floor(0.90 * nrow(df))
#set.seed(123)
#trainIndex <- sample(seq_len(nrow(df)), size = n)
#trainIndex <- createDataPartition(df$TARGET, p=0.90, list=FALSE)
#df_train2 <- df[trainIndex, ]
#df_test2 <- df[-trainIndex, ]
T <- nrow(df)
tr <- T - 50
df_train <- df[1:tr,]
df_test <- df[(tr+1):T,]
# -------------------------------------- ARMA(1,1) Model -------------------------------------- #

arma11 <- arima(df_train$TARGET, order = c(1,0,1))
arma11
target_pred_arma11 <- predict(arma11, newdata = df_test)

target_bench_arma11  <- rep(mean(df_train$TARGET, dim(df_test)[1])) # vector of in_sample y's

###
H <- nrow(df_test)
N <- nrow(df_train)-H

# ARMA(1)
arma11_predicted <- predict(arma11, n.ahead=1)
arma11_mse <- (mean((df_test - as.numeric(arma11_predicted$pred))**2))
arma11_mse
###

arma11_pred <- rep(0,H)

for( m in 0:(H-1) ){
  y <- df_train[1:(N+m),2]
  arma11 = arima(y,order=c(1,0,1))
  arma11_pred[1+m] <- predict( arma11 , n.ahead=1 )$pred
}
y


train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
modelarma11 <- train(arma11, data=df_train2, trControl=train_control)
# summarize results
print(model)

e_0_arma11 <- df_test$TARGET - target_bench_arma11  # demeaned
e_1_arma11 <- df_test$TARGET - target_pred_arma11    # 

mse_0_arma11 <- mean(e_0_arma11**2)        
mse_1_arma11 <- mean(e_1_arma11**2)
      
R2_0_arma11 <- 1 - mse_0_arma11/var(df_test$TARGET)
R2_1_arma11 <- 1 - mse_1_arma11/var(df_test$TARGET)

# -------------------------------------- TARCH Model -------------------------------------- #


t.n <- Tgarch11(df_train$TARGET[1:tr])
Q12 <- round(c(t.n$par["omega"],t.n$par["alpha"],t.n$par["gam1"],t.n$par["beta"]),3)







# ------------------------------------------------------------------------------------- #




## --------------------------- CB's code example --------------------------- ##

predictor.one <- function(data){          # input data is the txN df up to time t
  T <- nrow(data)
  y.in <- data[2:T,1]
  X.in <- data[1:(T-1),1]
  X.out<- data[T,1]
  mdl <- lm( y.in ~ X.in )
  beta <- coef(mdl)
  f <- beta[1] + beta[2]*X.out[1]
  df_train$TARGETurn( f )                            # f is the scalar forecast for t+1
}

## ------------------------------------------------------------------------ ##

CREATIVE PART
 VECM
 xLASSO
 Markov Chain/HMM