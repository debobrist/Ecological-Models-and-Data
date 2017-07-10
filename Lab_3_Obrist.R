# Continuing on from Lab_2_Obrist.R

# Part 1: Model Comparison

# Compare two fitted models made in lab 2. 
# Their negative log-likelihoods are already negative and log-transformed.


#### Q1: ####

D <- (2*BH.fit$value) - (2*dBH.fit$value)
D

pchisq(D, df=1, lower.tail=FALSE)

# D = 0.9858
# p-value = 0.3207711

#### Q2: ####

AIC.BH <- 2*3 + 2*BH.fit$value
AIC.BH # 507.2444

AIC.dBH <- 2*4 + 2*dBH.fit$value
AIC.dBH # 508.2585

deltaAIC <- AIC.dBH - AIC.BH
deltaAIC # 1.01417


#### Q3: ####

# Parameter estimates! 

# Make empty vector to store generated delta values.
deltas <- NULL

# Resample data (with replacement), store results of new delta values in deltas 
# vector. 

for (i in c(1:1000)){
  boot <- sample(c(1:nrow(sardines)), replace=T)
  sardines$resample.SSB <- sardines$SSB[boot]
  sardines$resample.R <- sardines$R[boot]
  dBH.fit <- optim(par=p.dBH, fn=negLL.dBH, recruits=sardines$resample.R, 
                   stock=sardines$resample.SSB, method="Nelder-Mead", 
                   control=list(parscale=p.dBH, maxit=500000))
  deltas[i] <- dBH.fit$par[4]
}
?sample
# Histogram of deltas: 

hist(deltas, 
     xlab = "Delta", 
     col = "grey", 
     main = "")

# Deltas are in random order - sort the deltas
deltas <- sort(deltas)
deltas[c(26, 975)] #CI: 0.7388837 to 3.0177816



#### Q4: ####

# Define parameters
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

# Create loop to fit 2 new models, store -log(L) values for each model in new column
# in sims data frame.

# Also calculate the t-statistic and the p-value for both models for each set of 
# data points.

for (i in 1:nrow(sims)) {
  BH.sfit <- optim(par=p.BH, fn=negLL.BH, 
                   recruits= as.numeric(sims[i,1:29]),
                   stock=sardines$SSB, method="Nelder-Mead", 
                   control=list(parscale=p.BH, maxit=500000))
  dBH.sfit <- optim(par=p.dBH, fn=negLL.dBH, 
                    recruits= as.numeric(sims[i,1:29]),
                    stock=sardines$SSB, method="Nelder-Mead", 
                    control=list(parscale=p.dBH, maxit=500000))
  sims$BH.nLL[i] <- BH.sfit$value
  sims$dBH.nLL[i] <- dBH.sfit$value
  sims$delta[i] <- dBH.sfit$par[4]
  sims$t[i] <- (2*BH.sfit$value) - (2*dBH.sfit$value)
  sims$p[i] <- pchisq(sims$t[i], df=1, lower.tail=FALSE)
}

#### Q5: ####

length(sims$p[sims$p < 0.05]) / 1000 * 100
# 99.9% of simulations had statistically significant p-values, which means that because
# more complex model (the depensatory model) was significantly better than the simpler 
# one.

range(sims$delta) # All simulations had best-fit predictions of delta over 1, so the 
# proportion of simulations in which the depensatory model was significantly better AND 
# the best-fit predictors of delta over 1 is .999. (99.9/100)
