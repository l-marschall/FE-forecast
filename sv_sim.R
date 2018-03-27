########################################################################
## SETUP
########################################################################

rm(list = ls())

## Load libraries


########################################################################
## Functions
########################################################################

sv_sim <- function(theta, T){
  ## Assign variables from theta vector
  const <- theta[1]
  phi <- theta[2]
  tau2 <- theta[3]
  
  ## Initiate variables
  alpha <- rep(0,T) # should be a column vector
  y <- rep(0,T) # should be a column vector
  
  ## Get random vectors
  eta <- rnorm(n=T,mean=0,sd=sqrt(tau2)) #should be column vector
  z <- rnorm(n=T,mean=0,sd=1)
  nu <- runif(n=T,min=0,max=1)
  ?runif
  alpha[1] <- const
  y[1] = z[1] * exp(alpha[1]/2)
  for (t in 2:T){
    alpha[t] <- const + phi*(alpha[t-1] + eta[t])
    y[t] <- z[t] * exp(alpha[t]/2)
  }
  
  ## Return results
  return(list(y, alpha))
}
########################################################################
## Evaluate
########################################################################
sv_sim(c(1,2,3),10)

########################################################################
########################################################################
########################################################################
## Run everything outside functions
########################################################################
########################################################################
########################################################################

theta <- c(1,2,3)
T <- 10


## Start function


## Assign variables from theta vector
const <- theta[1]
phi <- theta[2]
tau2 <- theta[3]

## Initiate variables
alpha <- rep(0,T) # should be a column vector
y <- rep(0,T) # should be a column vector

## Get random vectors
eta <- rnorm(n=T,mean=0,sd=sqrt(tau2)) #should be column vector
z <- rnorm(n=T,mean=0,sd=1)
nu <- runif(n=T,min=0,max=1)
?runif
alpha[1] <- const
y[1] = z[1] * exp(alpha[1]/2)
for (t in 2:T){
  alpha[t] <- const + phi*(alpha[t-1] + eta[t])
  y[t] <- z[t] * exp(alpha[t]/2)
}

## Print results
y 
alpha

##Old######################################################

lala <- c(10,30,1.5,sqrt(2))
lele <- c(3,2,4,1)

new_mat <- cbind(lala,lele)

order_mat <- new_mat[order(new_mat[, "lele"]),]

order_mat[,1]



index <- c(6,7,1,2,5,3,4)
numbers <- c(100,2,5,3.8,92,56,70)

o_mat1 <- cbind(index,numbers)
o_mat2 <- o_mat1[order(o_mat1[,"index"]),]


