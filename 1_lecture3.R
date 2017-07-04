# Ricker model example:

K <- 25
lambda <- 2
r <- log(lambda)
S <- seq(0, 400, 0.1)
R <- S * exp(r * (1 - S/K))

plot(S, R, type = "l", col = "blue", lwd = 2 )


# Holling Type II Functional Response example:
# Limiting factor is how fast predator can eat its prey.

alpha <- 0.2
Th <- 1
x <- 0:100
h <- alpha*x/(1+alpha*x*Th)

plot (x, h, type = "l", col = "blue", lwd = 2)