########################################################################
## SETUP
########################################################################

rm(list = ls())

## Load libraries


########################################################################
## Functions
########################################################################

########################################################################
## Evaluate
########################################################################


########################################################################
########################################################################
########################################################################
## Run everything outside functions
########################################################################
########################################################################
########################################################################

## Inputs

theta <- c(1,2,3)
T <- 10
y <- sv_sim(theta,T)[[1]]
P <- 10
eta_sim <- matrix(data=rnorm(n=P*T,mean=0,sd=1),nrow=P,ncol=T)
u_sim <- matrix(data=runif(n=P*T,min=0,max=1),nrow=P,ncol=T)
# alpha_up <-
# alpha_wt <- runif(n=P,min=0.001,max=3)

## Start Function

T <- length(y)
P <- length(alpha_up)

const <- theta[1]
phi <- theta[2]
tau2 <- theta[3]

alpha_up_pr <- matrix(data=rep(0,T*4),nrow=T,ncol=4)

loglik <- 0

for (t in 1:T){
  alpha_pr <- const + phi * alpha_up + sqrt(tau2) + eta_sim[,t]
  
  ## lik <- some normpdf stuff
  
  if (is.finite(log(mean(lik)))){
    loglik <- loglik - log(mean(lik))
  } else {
    print(paste("Likelihood not finite after iteration: ",t))
    loglik <- Inf
  }
  
}

lala <- Inf
