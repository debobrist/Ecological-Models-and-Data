# Part 1: Model Comparison
BH.fit$value
dBH.fit$value

# negative log-likelihoods are already negative and log-transformed.

D <- (2*BH.fit$value) - (2*dBH.fit$value)
D

pchisq(D, df=1, lower.tail=FALSE)

#### Q1: ####

# Give the value of the test statistic and the associated p-value in 
# your table. What does this p-value mean, in terms of probabilities? 
# What do you conclude? Which model is 'better'?

# D = 0.9858
# p-value = 0.3207711

AIC.BH <- 2*3 + 2*BH.fit$value
AIC.BH

AIC.dBH <- 2*4 + 2*dBH.fit$value
AIC.dBH

deltaAIC <- AIC.dBH - AIC.BH
deltaAIC

#### Q2: ####
# Discuss AIC stuff

#### Q3: ####

# Parameter estimates! 

boot <- sample(c(1:nrow(sardines)), replace=T)
boot

deltas <- NULL

for (i in c(1:1000)){
  boot <- sample(c(1:nrow(sardines)), replace=T)
  sardines$resample.SSB <- sardines$SSB[boot]
  sardines$resample.R <- sardines$R[boot]
  dBH.fit <- optim(par=p.dBH, fn=negLL.dBH, recruits=sardines$resample.R, 
        stock=sardines$resample.SSB, method="Nelder-Mead", 
        control=list(parscale=p.dBH, maxit=500000))
  deltas[i] <- dBH.fit$par[4]
}

hist(deltas, xlab = "Deltas")

deltas <- sort(deltas)
deltas[c(26, 975)] #CI: 0.7388837 to 3.0177816

#### Q4: ####

alpha <- BH.fit$par[1]
K <- BH.fit$par[2]
sigma <- BH.fit$par[3]
delta <- 2

# Define the predictions - this is model (prediction) line, using the parameters for a more
# depensatory version of the Beverton-Holt model 
predictions <- dBH(alpha*K^(1-delta), K^delta, sardines$SSB, delta)

# Choose random data points with log-normally distributed errors, using the prediction
# line as the mean.
sim1 <- rlnorm(length(sardines$SSB), meanlog = log(predictions)-sigma^2/2, 
         sdlog = sigma)

# Transpose this data frame into one row with 29 columns.
sims <- data.frame(t(sim1))

# Create a for loop to create the next 999 rows of simulated data.
for(i in c(2:1000)) {
  sims[i,] <- rlnorm(length(sardines$SSB), meanlog = log(predictions)-sigma^2/2, 
                     sdlog = sigma)
}

# Plot the newly simulated data.

plot(sardines$SSB, sims[1,], ylim=c(0,75000))

for(i in 2:1000) {
  points(sardines$SSB,sims[i,])
}

# Fitting the model to all 1000 simulated datasets.
# Create new columns to store the negative log-likelihood for the BH and dBH models, 
# as well as for the delta value for each simulated dataset.

sims$BH.nLL = 0
sims$dBH.nLL = 0
sims$delta = -999999 # We don't want to confuse this with an estimated delta.

head(sims)


