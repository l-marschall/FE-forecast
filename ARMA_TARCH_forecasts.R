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

S <- 1
T <- 501
h <- 10


## S (int): starting point in series
## T (int): ending point of series
## h (int): number of observations
########## All observations
## Take first differences of all series 
df_d1_all <- apply(X=df, MARGIN = 2, FUN = diff)
## Create lagged dataframe
df_d1_all_lag <- cbind(df_d1_all[(S+1):(T-h-1),1],df_d1_all[S:(T-1-h-1),1:50])
df_d1_all_lag <- as.data.frame(df_d1_all_lag)
## Run model
lm_all <- lm(data = df_d1_all_lag, formula = V1 ~ .)
## Predict first difference for t+1
values <- cbind(rep(1,h),df_d1_all[(T-h-1):(T-1-1),1:50])
coefs <- lm_all$coefficients
d1_predict <- (values%*%coefs)
final_predict <- d1_predict + df[(T-h):(T-1),1]


dim(df_d1_all[(T-h):(T-1),1:50])

dim(df_d1_all)

model_all_pars <- function(S=1,T=500,h=10){
  ## S (int): starting point in series
  ## T (int): ending point of series
  ## h (int): number of observations
  ########## All observations
  ## Take first differences of all series 
  df_d1_all <- apply(X=df, MARGIN = 2, FUN = diff)
  ## Create lagged dataframe
  df_d1_all_lag <- cbind(df_d1_all[(S+1):(T-h-1),1],df_d1_all[S:(T-1-h-1),1:50])
  df_d1_all_lag <- as.data.frame(df_d1_all_lag)
  ## Run model
  lm_all <- lm(data = df_d1_all_lag, formula = V1 ~ .)
  ## Predict first difference for t+1
  values <- cbind(rep(1,h),df_d1_all[(T-h):(T-1),1:50])
  coefs <- lm_all$coefficients
  d1_predict <- (values%*%coefs)
  final_predict <- d1_predict + df[(T-h):(T-1),1]
  return(final_predict)
}


sum((model_all_pars(S=1,T=500,h=100) - df[401:500,1])^2)/100
sum((model_all_pars(S=1,T=500,h=20) - df[481:500,1])^2)/20

model_all_pars(S=1,T=501,h=10)[10]
df[500,1]


model_all_pars(S=1,T=500,h=10)
df[491:500,1]

dim(values)
length(coefs)

dim(df_d1_all[(T-h):T,1:50])
dim(df_d1_all)
values%*%coefs

########## All observations
## Take first differences of all series 
df_d1_all <- apply(X=df, MARGIN = 2, FUN = diff)
## Create lagged dataframe
df_d1_all_lag <- cbind(df_d1_all[2:499,1],df_d1_all[1:498,1:50])
df_d1_all_lag <- as.data.frame(df_d1_all_lag)
## Run model
lm_all <- lm(data = df_d1_all_lag, formula = V1 ~ .)
## Predict first difference for t+1
values <- c(1,df_d1_all[499,1:50])
coefs <- lm_all$coefficients
d1_predict <- sum(values*coefs)
final_predict <- d1_predict + df[500,1]


df[495:500,1]

?predict
## Calculate absolute correlations to TARGET and sort
sort(abs(cor(df_d1_all))[,1], decreasing = TRUE)
## Result: UKSKO, IMBFM above 30%

########## Last 200 observations
## Take first differences of all series 
df_d1_small <- apply(X=df[300:500,], MARGIN = 2, FUN = diff)
## Calculate absolute correlations to TARGET and sort
sort(abs(cor(df_d1_small))[,1], decreasing = TRUE)
## Result: UKSKO, IMBFM, OIRTT above 30%

## Model for all data
mod <- lm(data = as.data.frame(df_d1_all), 
          formula = TARGET[300:499] ~ TARGET[299:498] + UKSKO[299:498])
mod$coefficients
predict(mod)

## Model for small data
?lm
###########################################################################
###########################################################################
###########################################################################
## OLD
###########################################################################
###########################################################################
###########################################################################
df[1:3,1:3]
df_d1[1:3,1:3]

# check for non-stationarity and if necessary calculate differences
adf.test(target) # whole series is stationary
adf.test(target[300:500]) # last 200 observations are NOT stationary

target_d1 <- diff(target) # calculate first differences
adf.test(target_d1) # whole series of 1Diff is stationary
adf.test(target_d1[300:499]) # last 200 observations of 1Diff ARE stationary

sort(cor(df)[1,])













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

plot(target[1:500],type="l")
plot(target_d1, type = "l")
plot(target_d2, type = "l")
