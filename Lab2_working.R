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

legend('bottomright', c("BH", "dBH"), col = c("steelblue", "seagreen3"), lty = 1,
       bty = "n", lwd = 2)

# Figure out better values for parameters to fit the line to 
# sardine data given.

plot(sardines$R ~ sardines$SSB,
     xlab = "Stock (thousand metric tonnes)",
     ylab = "Recruits (million fish)")

lines(dBH(5, 2000, c(2:4000), 1.05) ~ c(2:4000), type = "l", 
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

dBH.fit
