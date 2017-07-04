# Q1: Include a histogram of urchin masses for L. pictus individuals with 
# diameter less than 3 (cm) and height less than 2 (cm).


hist(data[data$species == "L. pictus" & data$diam < 3 & data$height < 2 , ]$mass, 
     main = "Histogram of L. pictus urchin masses", 
     xlab = "Urchin Mass", col = "grey")


#Q2: In your own words, describe what the plotMvsD and CubicLine functions do. 
# As part of explanation, submit commented R code that explains the function 
# definition and function call line-by-line

plotMvsD <- function (spp.name) {
  plot(mass ~ diam, data = data[data$species == spp.name, ])
}

# This function plots urchin mass vs diameter for whichever species you specify.

# Line 1: Creating a function, calling it plotMvsD. Call the function argument 
# "spp.name" because later, when you use the function, you will type in 
# "plotMvsD (the species name you are interested in). 
# Line 2: plot mass as a function of diameter using data from the dataframe called
# data, subsetting from rows containing the species name given.

CubicLine = function (coeff = 1, x.vals = c(0:100), ...) {
  lines (x.vals, coeff*x.vals^3, ...)
}

# CubicLine
# This function draws a cubic line (y = c*x^3) for any given x values.

#Line 1: Creating a function, assigning it the name "CubicLine". Setting some defaults.
# By default, we are setting the scaling coefficient (c in the above equation) to 1, 
# and considering x values between 0 and 100. The ... allows us to modify and add more 
# graphical parameters later without telling R right now what they will be.

#Line 2: The function will be a modification of the already-existent "lines" function, 
# which takes given coordinates and joins the points on a plot with lines. In this case,
# we are telling R to use the specified x.vals as the x values, and the scaling coefficient 
# (coeff) multiplied by the cubed x.vals as the y values to use as coordinates, and 
# join all these points with line segments. 

