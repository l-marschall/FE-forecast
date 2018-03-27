########################################################################
## SETUP
########################################################################

rm(list = ls())

## Load libraries
source("/home/jan/Documents/Courses/T2_FE/Project/sv_sim.R") # for y
source("/home/jan/Documents/Courses/T2_FE/Project/sv_loglik.R") # for y

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

## Initialize
theta <- c(1,2,3)
T <- 10
y <- sv_sim(theta,T)[[1]]
P <- 10
estimate <- TRUE

## Start Code 

T <- length(y)

alpha_up_0 <- rnorm(n=P,mean=0,sd=1) # column vector
alpha_wt_0 <- rep(1,P)/P # column vector
eta_sim <- matrix(data=rnorm(n=P*T,mean=0,sd=1),nrow=P,ncol=T)
u_sim <- matrix(data=runif(n=P*T,min=0,max=1),nrow=P,ncol=T)

## Sort each column of u_sim in increasing order
u_sim <- apply(u_sim, 2, sort)

if (estimate = TRUE){
  ## some weird options stuff (still to do)
  # options
  
  lb <- rep(0,length(theta)) + 0.001 # not sure about this part, should be vec
  ub <- rep(1,length(theta)) - 2*1e-10 # take it from options in his code
  
  ## some weird fmincon stuff
} else {
  theta_sml <- NULL
  theta_se <- NULL
}

## We need the sv_loglik herre

## Return

loglik
theta_sml
theta_se
alpha_up_quant
