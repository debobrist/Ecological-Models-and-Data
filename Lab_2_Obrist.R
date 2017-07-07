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
