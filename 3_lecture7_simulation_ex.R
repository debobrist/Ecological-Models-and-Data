# Ricker Simulation
# Simulating a data set, accounting for observational error.

r <- 3
b <- 1/4000
N0 <- 1000

Nt <- numeric(51)
Nt[1] <- N0

for (i in 1:50){
  Nt[i+1] <- Nt[i]*exp(r-b*Nt[i])
}

plot(1:51, Nt, type = "l")

# Log normal distribution of errors - always positive.
Nt.obs <- rlnorm(n=50, meanlog = log(Nt[2:51]), sd = 0.08)

points(2:51, Nt.obs)
