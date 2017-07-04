# 2.3 Plotting and functions

plot(data$diam, data$mass)
diameters <- seq(0, 20, 0.25)
spheres <- pi*4/3*(diameters/2)^3
lines(diameters, spheres, col = "red")

plot(diameters, spheres, type = "l", col = "red")
hemispheres <- (pi*4/3*(diameters/2)^3)/2
lines(diameters, hemispheres, col = "red")

par(mfrow = c(1,3))
plot(data[data$species == "L. pictus", ]$diam,
     data[data$species == "L. pictus", ]$mass)

plotMvsD = function (spp.name) {
  plot(mass ~ diam, data = data[data$species == spp.name, ])
}

CubicLine = function (coeff = 1, x.vals = c(0:100), ...) {
  lines (x.vals, coeff*x.vals^3, ...)
}

plotMvsD("L. pictus")
CubicLine(coeff = 0.5, x.vals=seq(0,5, 0.1), col = "green")
