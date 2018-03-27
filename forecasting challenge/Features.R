rm(list=ls())
##########################################################
# FORECAST COMPETITION
##########################################################
wd <- '/'
setwd(wd)
data <- read.csv('forecast-competition-training.csv',header=T)
# Need to forecast the TARGET variable
# target <- data['TARGET']
# we can use the past of the series as well as past realizations of other series
# in the panel

################ PREDICTION: #############

# Lasso for subset selection
library(glmnet)
X <- as.matrix(data[,2:50])
target <- data[,1]
lasso <- cv.glmnet(x = X, y=target)
betaslas <- coef(lasso)
# select relevant vars
relev_vars <- data[,betaslas@i]

# linear model with relevant variables AR(1) multivariate
y <- target[2:500]
X.lag1 <- as.matrix(relev_vars[1:499,])
mod <- lm(y ~ X.lag1)

y <- predict(object = mod, )