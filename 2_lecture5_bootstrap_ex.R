# July 5th, 2017
# Example of bootstrapping algorithm

x <- rpois(n = 20, lambda = 2.3)
xi <- sample (x, replace = TRUE)
x.means <- numeric(1000)

bootstrap <- function (x) {
  for (i in 1: 1000){
    xi <- sample(x, replace = TRUE)
    x.means[i] <- mean(xi)
  }
  CI <- quantile (x.means, c(0.025, 0.0975))
  return(CI)
}

bootstrap(x)
