sardines <- read.csv("data-raw/sardine_S-R.csv")

plot(sardines$SSB, sardines$R, xlab="stock (thousand metric tonnes)", 
     ylab="recruits (million fish)")

#### Q1: ####

# Make Beverton-Holt model:
BH <- function(alpha, K, S) {
  R <-  alpha*S / (1 + S/K)
  return(R)
}

plot(BH(5, 2000, c(2:4000)) ~ c(2:4000), type = "l", 
     xlab = "Stock (thousand metric tonnes)",
     ylab = "Recruits (million fish)",
     xlim = c(0, max(sardines$SSB)),
     ylim = c(0, max(sardines$R)))

points(sardines$R ~ sardines$SSB)

#### Q2: ####

# Make depensatory Beverton-Holt model:
dBH <- function (alpha, K, S, delta) {
  R <-  alpha * S ^ delta / (1 + S ^ delta / K)
  return(R)
}

# Plot Beverton-Holt depensatory model using same parameters as 
# in Q1.
plot(dBH(5, 2000, c(2:4000), 1.05) ~ c(2:4000), type = "l", 
     xlab = "Stock (thousand metric tonnes)",
     ylab = "Recruits (million fish)")

# Figure out better values for parameters to fit the line to 
# sardine data given.

plot(sardines$R ~ sardines$SSB,
     xlab = "Stock (thousand metric tonnes)",
     ylab = "Recruits (million fish)")

lines(dBH(5, 2000, c(2:4000), 0.05) ~ c(2:4000), type = "l", 
      xlab = "Stock (thousand metric tonnes)",
      ylab = "Recruits (million fish)")


#### Q3: ####

x <- c(0:100)
y <- dlnorm(c(0:100), meanlog = log(30)-((1.1^2)/2), 
       sdlog = 1.1, log = FALSE) 

plot(y ~ x, type = "l")

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

negLL.BH(c(5, 2000, 1), sardines$R, sardines$SSB)

# negLL.BH = 251.1249

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

negLL.dBH(c(5, 2000, 1, 1.05), sardines$R, sardines$SSB)

# negLL.dBH = 251.4078



