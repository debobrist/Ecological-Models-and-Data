sardines <- read.csv("data-raw/sardine_S-R.csv")

#### Q1: ####

# Define the deterministic core: Beverton-Holt model.

BH <- function(alpha, K, S) {
  R <-  alpha*S / (1 + S/K)
  return(R)
}

# Plot showing predicted recruitment for chosen alpha and K values, 
# and a reasonable range of S values. I overlaid points from the sardines
# data to figure out whether or not my estimates of alpha, K, and S values
# were reasonable.

plot(BH(5, 2000, c(2:4000)) ~ c(2:4000), type = "l", 
     xlab = "Stock (thousand metric tonnes)",
     ylab = "Recruits (million fish)",
     xlim = c(0, max(sardines$SSB)),
     ylim = c(0, max(sardines$R)),
     lwd = 2)

points(sardines$R ~ sardines$SSB, 
       pch = 16)

#### Q2: ####

# Make depensatory Beverton-Holt model:
dBH <- function (alpha, K, S, delta) {
  R <-  alpha * S ^ delta / (1 + S ^ delta / K)
  return(R)
}

# Plot Beverton-Holt depensatory model using same parameters as 
# in Q1. 

# This plot shows depensatory Beverton-Holt with delta of 2.
plot(dBH(5, 2000, c(2:4000), 2) ~ c(2:4000), type = "l", 
     xlab = "Stock (thousand metric tonnes)",
     ylab = "Recruits (million fish)",
     col = "seagreen2",
     lwd = 2)

# This plot shows depensatory Beverton-Holt with delta of 1 ie. same as 
# Beverton-Holt model without depensation.
lines(dBH(5, 2000, c(2:4000), 1) ~ c(2:4000), type = "l", 
      xlab = "Stock (thousand metric tonnes)",
      ylab = "Recruits (million fish)",
      col = "steelblue",
      lwd = 2)

legend('bottomright', c("delta = 2", "delta = 1"), 
       col = c("seagreen2", "steelblue"), lty = 1,
       bty = "n", lwd = 2)


#### Q3: ####

# Setting up the negative log-likelihood for the Beverton-Holt model.
negLL.BH <- function(params, recruits, stock) {
  alpha <- params[1]
  K <- params[2]
  sigma <- params[3]
  predictions <- BH(alpha, K, stock)
  LL <- dlnorm(recruits, 
               meanlog = log (predictions) - sigma ^ 2 / 2, 
               sdlog = sigma, log = TRUE) 
  return(-sum(LL))
}

negLL.BH(c(5, 2000, 1), sardines$R, sardines$SSB) # returns 251.1249 


# Setting up the negative log-likelihood for the depensatory model of 
# the Beverton-Holt model.

negLL.dBH <- function(params, recruits, stock) {
  alpha <- params[1]
  K <- params[2]
  sigma <- params[3]
  delta <- params [4]
  predictions <- dBH(alpha, K, stock, delta)
  LL <- dlnorm(recruits, 
               meanlog = log (predictions) - sigma ^ 2 / 2, 
               sdlog = sigma, log = TRUE) 
  return(-sum(LL))
}

negLL.dBH(c(5, 2000, 1, 1.05), sardines$R, sardines$SSB) # returns 251.4078

#### Q4: ####

p.BH <- c(alpha=5, K=2000, sigma=1)
p.BH

# Fit the model to Beverton-Holt.
BH.fit <- optim(par=p.BH, fn=negLL.BH, recruits=sardines$R, 
                stock=sardines$SSB, method="Nelder-Mead", 
                control=list(parscale=p.BH, maxit=500000))

BH.fit

# Fit the model to the depensatory Beverton-Holt model.

p.dBH <- c(alpha=5, K=2000, sigma=1, delta = 1.05)
p.dBH

dBH.fit <- optim(par=p.dBH, fn=negLL.dBH, recruits=sardines$R, 
                 stock=sardines$SSB, method="Nelder-Mead", 
                 control=list(parscale=p.dBH, maxit=500000))

dBH.fit

# Plot the data, overlay the models! 

plot(sardines$R ~ sardines$SSB,
     xlab = "Stock (thousand metric tonnes)",
     ylab = "Recruits (million fish)",
     pch = 16,
     col = "black")

lines(BH(BH.fit$par[1],BH.fit$par[2], c(2:4000)) ~ c(2:4000), type = "l", 
      xlab = "Stock (thousand metric tonnes)",
      ylab = "Recruits (million fish)",
      col = "steelblue",
      lwd = 2)

lines(dBH(dBH.fit$par[1],dBH.fit$par[2], c(2:4000), dBH.fit$par[4]) ~ c(2:4000), 
      type = "l", 
      xlab = "Stock (thousand metric tonnes)",
      ylab = "Recruits (million fish)",
      col = "seagreen3",
      lwd = 2)
legend('topright', c("BH", "dBH"), col = c("steelblue", "seagreen3"), lty = 1,
       bty = "n", lwd = 2)
