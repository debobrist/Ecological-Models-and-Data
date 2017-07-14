rm(list = ls())
set.seed(123)
#function to simulate the number of prey eaten by a foraging predator
#size = number of grid cells per side of the square foraging 'arena'
#n = number of prey individuals at the start of the simulation
#time = number of timesteps over which to run the simulation
#handling.time = number of timesteps a predator must wait to restart foraging, 
#after capturing a prey
#draw.plot = whether or not to plot the arena as the simulation proceeds 
#(NOTE: plotting will SLOW DOWN the simulation and open a new plotting window)
sim.predation = function(size =30, n=100, time=100, handling.time=5, draw.plot=F) {
  
  #set up a data frame to represent the prey 
  prey = data.frame(x.pos=floor(runif(n,1,1+size)),
                    y.pos=floor(runif(n,1,1+size)),
                    born = 0, #when were the prey individuals created
                    died = NA)
  
  #another for the predators
  pred = data.frame(x.pos=floor(runif(1,1,1+size)), 
                    y.pos=floor(runif(1,1,1+size)), 
                    state=0) #hunting=0, handling>0
  
  if(draw.plot) {
    x11() #open a new plot window	
  }
  
  #loop through simulation for set number of timesteps
  for(t in 1:time){
    live.prey=which(is.na(prey$died)) #which prey are currently alive
    dead.prey=which(is.na(prey$died)==F) #which prey are dead
    
    #if there are living prey, allow them to move randomly    
    #x.pos can change 1-, 0, or +1... same with y.pos
    prey$x.pos[live.prey] = prey$x.pos[live.prey] + sample(c(-1,0,1),length(live.prey),replace=TRUE)		
    prey$y.pos[live.prey] = prey$y.pos[live.prey] + sample(c(-1,0,1),length(live.prey),replace=TRUE)		
    # if prey is in position <1, so 0, it gets moved to "size", which in this case is 30. If it
    # is in xposition "size", it gets moved to position 1. It accounts for prey that are about to
    # exit the arena.
    prey$x.pos[prey$x.pos<1] = size; prey$x.pos[prey$x.pos>size] = 1
    prey$y.pos[prey$y.pos<1] = size; prey$y.pos[prey$y.pos>size] = 1
    
    #keep track of predators' remaining handling time
    if(sum(pred$state)>0) {
      pred$state[pred$state>0] = pred$state[pred$state>0] - 1 #decrement 'state' if handling time hasn't run out
    }
    
    #if the predator is hunting, simulate its actions
    if(any(pred$state==0)) {
      #movement (same system as for prey)
      pred[pred$state==0,]$x.pos = pred[pred$state==0,]$x.pos + sample(c(-1,0,1),sum(pred$state==0),replace=TRUE)		
      pred[pred$state==0,]$y.pos = pred[pred$state==0,]$y.pos + sample(c(-1,0,1),sum(pred$state==0),replace=TRUE)		
      pred$x.pos[pred$x.pos<1] = size; pred$x.pos[pred$x.pos>size] = 1
      pred$y.pos[pred$y.pos<1] = size; pred$y.pos[pred$y.pos>size] = 1
      
      hunting.preds = which(pred$state==0) #which predators are hunting (we already know at least one is)
      #cycle through the hunting predators... see if they caught anything
      for(hunting.pred in hunting.preds) {
        #when a predator is at the same position as a live prey item
        prey.sighted = live.prey[prey$x.pos[live.prey]==pred[hunting.pred,]$x.pos &
                                   prey$y.pos[live.prey]==pred[hunting.pred,]$y.pos]
        
        if(length(prey.sighted)>0) { #if a predation event occured
          pred[hunting.pred,]$state = handling.time
          
          #decide which of the sighted prey get eaten, since multiple prey can be in the same position
          eaten.prey = prey.sighted[sample(length(prey.sighted),1)] 
          prey$died[eaten.prey] = t
          
          #add a new prey item to keep n constant
          prey[nrow(prey)+1,] = c(floor(runif(1,1,1+size)),floor(runif(1,1,1+size)), t, NA)
        }#end predation event
      }#end for(hunting.pred)
    }
    
    #if desired, plot the arena
    if(draw.plot) {	
      #live prey are blue
      plot(prey$x.pos[live.prey],prey$y.pos[live.prey], xlim=c(1,size), ylim=c(1,size), col="blue", cex=1.2,
           xlab="x position",ylab="y position", main="PREDATION ARENA of DEATH")
      par(new=TRUE)
      #hunting predators are blue
      plot(pred$x.pos[pred$state==0],pred$y.pos[pred$state==0], 
           xlim=c(1,size), ylim=c(1,size),pch=4,cex=3,lwd=3,
           col="blue",axes=FALSE,
           xlab=NA, ylab=NA)
      par(new=TRUE)
      #handling predators are grey
      plot(pred[pred$state>0,]$x.pos,pred[pred$state>0,]$y.pos, 
           xlim=c(1,size), ylim=c(1,size),pch=4,cex=3,col="grey",lwd=3,axes=FALSE,
           xlab=NA, ylab=NA)
      par(new=TRUE)
      #dead prey are red
      plot(prey$x.pos[dead.prey],prey$y.pos[dead.prey], 
           xlim=c(1,size), ylim=c(1,size),pch=16,col="red", axes=FALSE,
           xlab=NA, ylab=NA)
      Sys.sleep(1) #delay iteration (so the plot sticks around long enough to see it)
    }	
    
  }
  
  return(prey)
  
}#end sim.predation

sim.predation()

#### Q1: ####

# Randomly choose 100 numbers between 1 and 1000.  
N <- c(1:1000)

# Make empty vector to store values for prey eaten.
prey.eaten <- rep(0, length(N))

# Loop through iterations with different numbers of prey. For i = 1, this means, how many
# prey were eaten if there was 1 prey available? If i = 100, how many prey were eaten out of the
# 100 available? 

for (i in 1:length(N)){
  prey <- sim.predation(size = 30, 
                        n = N[i], 
                        time = 100, 
                        handling.time = 5, 
                        draw.plot = F)
  prey.eaten[i] <- sum(!is.na(prey$died))
}


plot(prey.eaten ~ N, 
     pch = 16,
     col = "mediumseagreen", 
     cex = 0.5,
     xlab = "Prey Abundance",
     ylab = "Prey Eaten")


# Make matrices for estimates and for predictions
estimates <- matrix(NA, nrow=1000, ncol=2)
predictions <- matrix(NA, nrow=1000, ncol=1000)
N.predict <- c(1:1000)


# Write a function that accepts alpha, Th, and N, keep Tt constant at 100.
type2 <- function(alpha, N, Th) {
  prey.eaten <- (100*alpha*N)/(1+(Th*alpha*N))
  return(prey.eaten)
}

# my.nls <- nls(prey.eaten ~ type2(),
#               start=list(alpha=0.001,Th=5), algorithm="port", lower=c(0,0),
#               upper=c(10,100))

# Tt = time period
# alpha = encounter rate
# n = prey abundance
# Th = handling time

# The loop works! 
for (i in 1:1000){
  boot <- sample(c(1:length(N)), replace=T) #randomly sample from numbers from 1:100, which is the
  # length of N, and assign them to "boot"
  re.N <- N[boot] # resample from N (prey abundance), call this re.N
  re.prey.eaten <- prey.eaten[boot] # resample from prey eaten, call it re.prey.eaten
  nls.fit <- nls(re.prey.eaten ~ (Tt*alpha*re.N)/(1+(Th*alpha*re.N)), # fit NLS using resampled data
                 start=list(alpha=0.001, Th=5), 
                 algorithm="port", 
                 lower=c(0,0),
                 upper=c(10,100))
  estimates[i,1] <- summary(nls.fit)$coefficients[1,1] # populate estimates column for alpha
  estimates[i,2] <- summary(nls.fit)$coefficients[2,1] # populate estimates column for Th
  predictions[i, ]<- type2(alpha = estimates[i, 1], N = N.predict, Th = estimates[i, 2]) # populate 
  # predictions matrix by applying type2 function 1000 times in every row.
}


# Plot histogram of parameter estimates.
par(mfrow=c(1,2))
hist(estimates[ ,1],
     main = "",
     xlab = "alpha estimate",
     col = "mediumseagreen")

hist(estimates[ ,2], 
     xlab = "Th estimate",
     main = "",
     col = "mediumseagreen")

predictions.t <- t(predictions)

predictions.t<- apply(predictions.t, MARGIN = 1 , FUN = sort)

lower_cl <- predictions.t[26, ]
upper_cl <- predictions.t[975, ]
lower_cl[5]
upper_cl[5]

predictions3 <- (apply(predictions, MARGIN = 1, FUN = sort))
lower_cl_3 <- predictions3[26, ]
upper_cl_3 <- predictions3[975, ]
upper_cl_3[5]
lower_cl_3[5]
