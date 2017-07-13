rm(list=ls())

#############################################################################################
# Simulate data
#############################################################################################
set.seed(3847) # Reproducible!

n<-100
x<-runif(n, 0,1)

beta <- c(1.3, 2.2)

y.hat<-exp(beta[1] + beta[2]*x) # core model
y<-rpois(n, lambda=y.hat) # shell

x.dum <- seq(min(x), max(x), 0.01)
y.dum <- exp(beta[1]+beta[2]*x.dum)

plot(x,y)
lines(x.dum, y.dum, lwd=3)
#############################################################################################
# Approach #1: Likelihood
#############################################################################################

# Likelihood function
NLL<-function(params){
  lambda = exp(params[1] + params[2]*x)
  LL<-dpois(y, lambda = lambda, log=TRUE)	
  return(-sum(LL))
}

out<-optim(c(1,1), NLL)

beta.like<-out$par

lines(x.dum, exp(beta.like[1]+beta.like[2]*x.dum), col=2)
#############################################################################################
# Approach #2: Bayesian approach
#############################################################################################

# Define priors
prior.means<-c(1,3)
prior.sd<-c(0.5, 0.5)

# Plot priors and "true" values (that we only know because we simulated the data)
x.dummy<-seq(-10,10,0.1)
plot(x.dummy, dnorm(x.dummy, prior.means[1], prior.sd[1]), "l")
lines(x.dummy, dnorm(x.dummy, prior.means[2], prior.sd[2]), col=2)
legend('topleft', lwd=1, col=c(1,2), c(expression(beta[1]), expression(beta[2])))
abline(v=beta, col=c(1,2), lty=2)

# Function returning prior log density for a given params
prior<-function(params){
  pr<-dnorm(params, mean = prior.means, sd = prior.sd, log=TRUE)
  return(sum(pr))
}

# Function returning log posterior (non-normalized...don't know probability of the data!)
posterior<-function(params){
  logPost<- -NLL(params) + prior(params)
  return(logPost)
}

#------------------------------------------------------------------------------------
# Using R package MCMC
#------------------------------------------------------------------------------------

library(mcmc)
iter<-10000 # number of mcmc iterations we want to run

set.seed(42) # to get reproducible results
beta.init <- c(2,2) # initial guess at parameter values to start mcmc

# Metropolis algorithm:
out.mcmc <- metrop(obj = posterior, initial = beta.init, nbatch = iter, scale = 0.08)
# scale = how far you jump, nbatch = how many iterations, initial is where you start out
# ex. starting the drunkard at beta1 = 2, beta2 = 2.

names(out.mcmc)

out.mcmc$accept # we want an acceptance rate of ~ 20%, can adjust scale
# plot(out.mcmc$batch[,1]) # bigger steps = lower acceptance rate because it's not exploring
# the posterior surface very well.

beta.mcmc<-apply(out.mcmc$batch, 2, mean)
abline(v=beta.mcmc, col=c(1,2))

# Plot posterior distribution:
hist(out.mcmc$batch [ ,1]) 
abline(v = beta[1], col = "red") # True value




#------------------------------------------------------------------------------------
# Writing our own Metropolis algorithm!!
#------------------------------------------------------------------------------------

params.g<-matrix(nrow=iter, ncol=2)
params.g[1,]<-beta.init

post.g<-numeric(iter)
post.g[1]<-posterior(params.g[1,])

accept<-0

for(g in 2:iter){
  
  # Jump distribution
  params.star<-rnorm(2, mean=params.g[g-1,], sd=1)
  post.star<-posterior(params.star)
  
  a<-min(1, exp(post.star - post.g[1]))
  z<-rbinom(1, 1, prob=a)
  
  if(z==0){
    params.g[g,]<-params.g[g-1,]
    post.g[g]<-post.g[g-1]
  }else{
    accept<-accept+1
    params.g[g,]<-params.star
    post.g[g]<-post.star
  }
  
}

accept/iter # As sd increases, you increase the chance of overstepping, and this ratio
# goes down. 

beta.ours<-apply(params.g, 2, mean)

abline(beta.ours, col=c(1,2), lty=3,lwd=2)


#############################################################################################
# Summary
#############################################################################################

rbind(beta, beta.like, beta.mcmc, beta.ours)

par(mfrow=c(2,1), mar=c(4,4,2,1))

