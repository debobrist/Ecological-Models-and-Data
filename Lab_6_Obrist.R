rm(list = ls())
set.seed(123)
#### sim.predation function ####

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

# Randomly choose 1000 numbers between 1 and 1000.  
N <- runif(1000, 1, 1000)

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

#### Q2: ####

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
  nls.fit <- nls(re.prey.eaten ~ (100*alpha*re.N)/(1+(Th*alpha*re.N)), # fit NLS using resampled data
                 start=list(alpha=0.0001, Th=5), 
                 algorithm="port", 
                 lower=c(0,0),
                 upper=c(10,100))
  estimates[i,1] <- summary(nls.fit)$coefficients[1,1] # populate estimates column for alpha
  estimates[i,2] <- summary(nls.fit)$coefficients[2,1] # populate estimates column for Th
  predictions[i, ]<- type2(alpha = estimates[i, 1], N = N.predict, Th = estimates[i, 2]) # populate 
  # predictions matrix by applying type2 function 1000 times in every row.
}

# Get parameter estimates along with confidence interval.

Th.sorted <- sort(estimates[ ,2])
Th.sorted[26] # 4.171795
Th.sorted[975] #  4.347275
median(Th.sorted) # 4.257248

alpha.sorted <- sort(estimates[ ,1])
alpha.sorted[26] # 0.000805924
alpha.sorted[975] # 0.0008876334
median (alpha.sorted) # 0.0008450862
# Plot histogram of parameter estimates.
par(mfrow=c(1,2))

# Plot histogram of the estimates, with CIs.
hist(estimates[ ,1],
     main = "",
     xlab = "alpha estimate",
     col = "mediumseagreen")
abline(v = alpha.sorted[26] )
abline(v = alpha.sorted[975])

hist(estimates[ ,2], 
     xlab = "Th estimate",
     main = "",
     col = "mediumseagreen")
abline(v = Th.sorted[26] )
abline(v = Th.sorted[975])

# Get best fit line to put on functional response curve: 
# Start by obtaining line of best fit. Each row in the prediction matrix represents one
# iteration of the loop, ie, one resampling of the population of size N.predict (1:1000).
# This means that each value in the matrix represents prey eaten (f(N)), calculated using 
# the alpha and Th estimates for that iteration, and N.predict (prey abundance) for that 
# column. We want to sort each column (which would be f(N)s calculated for the same 
# N.predict but in a different iteration), so that we can plot a line of all the means 
# across N.predict (prey abundances), as well as a line for the 95 % CIs.

?apply # margin = 1 applies to rows, margin = 2 applies to columns. We want to sort by columns.

prediction.sorted <- (apply(predictions, MARGIN = 2, FUN = sort))


plot(prey.eaten ~ N, 
     pch = 16,
     col = "mediumseagreen", 
     cex = 0.5,
     xlab = "Prey Abundance",
     ylab = "Prey Eaten")

lines(prediction.sorted[500, ])

lines(prediction.sorted[26, ],
       col = "steelblue",
      lwd = 1.5 )

lines(prediction.sorted[975, ],
       col = "steelblue")

# Can also make best-fit line by inserting estimated parameters into 
# funtional response equation: 
best.fit <- type2(median(alpha.sorted), N.predict, 
                  median(Th.sorted))
plot(prey.eaten ~ N, 
     pch = 16,
     col = "mediumseagreen", 
     cex = 0.5,
     xlab = "Prey Abundance",
     ylab = "Prey Eaten")

lines(best.fit,
       col = "black")

lines(prediction.sorted[26, ],
      col = "steelblue",
      lwd = 1.5 )

lines(prediction.sorted[975, ],
      col = "steelblue")


#### Q3: ####

# Alter sim.predation such that prey individuals are not replaced.
sim.predation2 = function(size =30, n=100, time=100, handling.time=5, draw.plot=F) {
  
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
          # prey[nrow(prey)+1,] = c(floor(runif(1,1,1+size)),floor(runif(1,1,1+size)), t, NA)
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

# Prey density: 
# the dataframe printed in each sim.predation gives you the x and y position of each
# prey item, which time step it was born in, and which time step it died in NA if it is
# still alive).

# Once you have a dataset (i.e., the simulation function outputs) for each of two prey 
# abundances, create a new column in each dataframe with the prey abundance 
# (call it "group"). 
prey.150 <- sim.predation2(size = 30, n = 150, time = 4000, handling.time = 5, draw.plot = F)
prey.150$group <- 150
prey.500 <- sim.predation2(size = 30, n = 500, time = 4000, handling.time = 5, draw.plot = F)
prey.500$group <- 500

# Take a random subset of the output from the simulation with more abundant prey, 
# so that you have an equal number of data points for each (use sample again, 
# without replacement). 

prey.500.s <- prey.500[sample(nrow(prey.500), 150, replace = FALSE, prob = NULL), ]

# Combine the dataframes into one called "mort" (look up function rbind for that),
# and then generate another new column, "status," that equals 1 if the individual 
# died and 0 if the individual was censored (i.e. didn't die by the end of the
# simulation). Also, set the values of “died” for all censored individuals to 
# the simulation duration (4,000), and turn your group column into a factor with 
# as.factor().

mort <- rbind(prey.150, prey.500.s)

mort$status <- 9999 # Fill new column called status with 9999s so can tell if adding
# 0s and 1s works. 

mort[ ,6][is.na(mort[ ,4])] <- 0 # If column 4 (died) is na, put a 0 in column 6 (status)
mort[ ,6][!is.na(mort[ ,4])] <- 1 # If column 4 is not NA ie, prey is dead, put a 1 in 
# column 6.

mort$group <- as.factor(mort$group)

# Set values of died for all censored individuals to 4000 (simulation duration).
mort[ ,4][is.na(mort[ ,4])] <- 4000

# Survival analysis: 

install.packages("survival")
library(survival)

# Survival analysis in R makes use of Surv objects that combine information about 
# event times with information about censoring.
Sdata <-  Surv(mort$died, mort$status)

# The solid line gives the Kaplan-Meier curve, showing the proportion of individuals
# that remained as the simulation progressed.The dashed lines give confidence regions 
# for the Kaplan-Meier estimates.
plot(survfit(Sdata~1))

# This plot is fairly linear, as is consistent with a Type 2 survivorship curve
# because it has constant hazard. 
plot(survfit(Sdata~1), log=TRUE)

#
# We want to compare survival in our two groups (different prey abundances). 
# To get a picture of this, type:
plot(survfit(Sdata ~ mort$group),
     xlab = "Time", 
     ylab = "Survivorship",
     col = c("mediumseagreen", "mediumpurple4"))
legend("topright",
       legend = c("n = 150", "n = 500"),
       col = c("mediumseagreen", "mediumpurple4"),
       lty = c(1,1),
       bty = "n")


# Fit the model:
model <- coxph(Sdata~mort$group)

summary(model)
model
1/.715


#### Q4: ####

# 3 Add one or more additional predators into the simulation and either
# a) generate data and re-fit the appropriate functional response, or
# b) conduct a two-factor survival analysis (prey abundance and predator abundance).
sim.predation3 = function(size =30, n=100, n.pred = 5, time=100, handling.time=5, draw.plot=F) {
  
  #set up a data frame to represent the prey 
  prey = data.frame(x.pos=floor(runif(n,1,1+size)),
                    y.pos=floor(runif(n,1,1+size)),
                    born = 0, #when were the prey individuals created
                    died = NA)
  
  #another for the predators
  pred = data.frame(x.pos=floor(runif(n.pred,1,1+size)), 
                    y.pos=floor(runif(n.pred,1,1+size)), 
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
          #prey[nrow(prey)+1,] = c(floor(runif(1,1,1+size)),floor(runif(1,1,1+size)), t, NA)
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

# Check to see if simulation works: Success!
sim.predation3(size =30, n=100, n.pred = 5, time=100, handling.time=5, draw.plot=F)

# b) conduct a two-factor survival analysis (prey abundance and predator abundance).

pred.2.prey.150 <- sim.predation3(size = 30, n = 150, n.pred = 2,
                                  time = 500, handling.time = 5, 
                                  draw.plot = F)

pred.2.prey.500 <- sim.predation3(size = 30, n = 500, n.pred = 2,
                                  time = 500, handling.time = 5, 
                                  draw.plot = F)


pred.4.prey.150 <- sim.predation3(size = 30, n = 150, n.pred = 4,
                                  time = 500, handling.time = 5, 
                                  draw.plot = F)

pred.4.prey.500 <- sim.predation3(size = 30, n = 500, n.pred = 4,
                                  time = 500, handling.time = 5, 
                                  draw.plot = F)
pred.2.prey.150$group <- 150
pred.2.prey.500$group <- 500
pred.4.prey.150$group <- 150
pred.4.prey.500$group <- 500

pred.2.prey.150$n.pred <- 2
pred.2.prey.500$n.pred <- 2
pred.4.prey.150$n.pred <- 4
pred.4.prey.500$n.pred <- 4

# Randomly sample 150 prey from each group of n = 500 prey.
pred.2.prey.500.s <- pred.2.prey.500[sample(nrow(pred.2.prey.500), 
                                            150, replace = FALSE, prob = NULL), ]
pred.4.prey.500.s <- pred.4.prey.500[sample(nrow(pred.4.prey.500), 
                                            150, replace = FALSE, prob = NULL), ]


# Combine the dataframes into one called "mort" (look up function rbind for that),
# and then generate another new column, "status," that equals 1 if the individual 
# died and 0 if the individual was censored (i.e. didn't die by the end of the
# simulation). Also, set the values of “died” for all censored individuals to 
# the simulation duration (4,000), and turn your group column into a factor with 
# as.factor().

mort2 <- rbind(pred.2.prey.150, pred.4.prey.150, pred.2.prey.500.s, pred.4.prey.500.s)

mort2$status <- 9999 # Fill new column called status with 9999s so can tell if adding
# 0s and 1s works. 

mort2[ ,7][is.na(mort2[ ,4])] <- 0 # If column 4 (died) is na, put a 0 in column 7 (status)
mort2[ ,7][!is.na(mort2[ ,4])] <- 1 # If column 4 is not NA ie, prey is dead, put a 1 in 
# column 7.

# Make group into a factor.
mort2$group <- as.factor(mort2$group)

# Make n.pred into a factor.
mort2$n.pred <- as.factor(mort2$n.pred)

# Set values of died for all censored individuals to 500 (t = 500).
mort2[ ,4][is.na(mort2[ ,4])] <- 500

# The data is ready for a survival analysis! 

# Survival analysis: 

library(survival)

# Survival analysis in R makes use of Surv objects that combine information about 
# event times with information about censoring.
Sdata2 <-  Surv(mort2$died, mort2$status)

# The solid line gives the Kaplan-Meier curve, showing the proportion of individuals
# that remained as the simulation progressed.The dashed lines give confidence regions 
# for the Kaplan-Meier estimates.
plot(survfit(Sdata2~1))

# This plot linear, as is consistent with a Type 2 survivorship curve
# because it has constant hazard. 
plot(survfit(Sdata2~1), log=TRUE)

#
# We want to compare survival in our two groups (different prey abundances). 
# To get a picture of this, type:

plot(survfit(Sdata2 ~ mort2$group + mort2$n.pred),
     xlab = "Time", 
     ylab = "Survivorship",
     lwd = 2)

# Figure out which line is which: 
lines(survfit(Sdata2[1:150] ~ mort2$group[1:150] + mort2$n.pred[1:150]),
      col = "red") # Prey = 150, Pred = 2. 2nd from top

lines(survfit(Sdata2[151:300] ~ mort2$group[151:300] + mort2$n.pred[151:300]),
      col = "blue") # Prey = 150, Pred = 4. 4th.

lines(survfit(Sdata2[301:450] ~ mort2$group[301:450] + mort2$n.pred[301:450]),
      col = "green") # Prey = 500, Pred = 2. 1st! 

lines(survfit(Sdata2[451:600] ~ mort2$group[451:600] + mort2$n.pred[451:600]),
      col = "yellow") # Prey = 500, Pred = 4. 3rd. 

# Make plot! Label all 4 lines properly. 

plot(survfit(Sdata2 ~ mort2$group + mort2$n.pred),
     xlab = "Time", 
     ylab = "Survivorship",
     col = c("#a6611a", "#dfc27d", "#80cdc1", "#018571"),
     lwd = 2)

legend("bottomleft", 
       legend = c("Prey=500, Pred=2", "Prey=150, Pred=2", "Prey=500, Pred=4", "Prey=150, Pred=4"),
       col = c("#80cdc1", "#a6611a", "#018571", "#dfc27d"),
       lwd = 2,
       bty = "n")


# Fit the model:
model2 <- coxph(Sdata2 ~ mort2$group + mort2$n.pred)

summary(model2)$coefficients
model2


1/.665