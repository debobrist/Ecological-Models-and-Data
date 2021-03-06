data <- read.csv("data-raw/dissections.csv", 
                 stringsAsFactors = FALSE)

data <- data.frame(species = as.factor(data$Species),
                   country = as.factor(data$Country),
                   region = as.factor(data$State.Province),
                   site = as.factor(data$Site),
                   date = as.Date(data$date, format = "%d-%b-%y"),
                   diam = as.numeric(data$dia..cm.),
                   height = as.numeric(data$ht.cm.),
                   mass = as.numeric(data$total..g.),
                   gonad.mass = as.numeric(data$gonad.wt..g.))

#### Q1: #### 

hist(data[data$species == "L. pictus" & data$diam < 3 & data$height < 2 , ]$mass, 
     main = "Histogram of L. pictus urchin masses", 
     xlab = "Urchin Mass", col = "grey")


#### Q2: #### 

plotMvsD <- function (spp.name) {
  plot(mass ~ diam, data = data[data$species == spp.name, ])
}

# This function plots urchin mass vs diameter for whichever species you specify.

# Line 1: Creating a function, calling it plotMvsD. Call the function argument 
# "spp.name" because later, when you use the function, you will type in 
# "plotMvsD (the species name you are interested in). 
# Line 2: plot mass as a function of diameter using data from the dataframe called
# data, subsetting from rows containing the species name given.

CubicLine <- function (coeff = 1, x.vals = c(0:100), ...) {
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


#### Q3: #### 

plotMvsD2 <- function (spp.name, coeff = 0.06, x.vals = c(0:100), ... ) {
  plot (mass ~ diam, data = data[data$species == spp.name, ], 
        col = "gray21", main = spp.name, ... )
  points (gonad.mass ~ diam, data = data[data$species == spp.name, ], 
          col = "gray47", ...)
  lines (x.vals, coeff*x.vals^3, col = "springgreen4", lwd = 2)
}

par(mfrow = c(1,3))

plotMvsD2("L. pictus", coeff = 0.075)
plotMvsD2("S. purpuratus", coeff = 0.05)
plotMvsD2("S. franciscanus", coeff = 0.05)

graphics.off()


#### Q4: #### 

#Gonad mass over total mass.
data$ratio <- data$gonad.mass/data$mass
boxplot(ratio ~ species, data = data, na.action = NULL)

# Determine the value of the outlier - it is 1.885
max(data$ratio[!is.na(data$ratio)])

# Subset to exclude data$ratio == 1.885. 
boxplot(ratio ~ species, data = data[data$ratio != 1.885, ], na.action = NULL, 
        ylab = "Gonad mass / Total mass")

# Looks good - permanently remove the outlier.
# First, figure out which row the outlier is in.
outlier <- data[data$ratio == 1.885 & !is.na(data$ratio), ] 

# Remove the row with the outlier.
data1 <- data[-10395, ]

# Remove all rows that have NAs in the mass column.
data2 <- data1[!is.na(data1$mass), ]

# Boxplot of total mass alone.
boxplot(data2$mass ~ data2$species, na.action = NULL, ylab = "Total mass")

# All three species of urchin have comparable gonad mass to body mass ratios,
# even though they very in body weights.

# The coefficients for eyeballed cubic curves from the previous question are very
# similar to the medians of the boxplots for each species, with L. pictus having the highest,
# and S. franciscanus and S. purpuratus being lower and very similar.

# How do they compare to the mean value of the gonad.mass/mass ratio for 
# each species (use tapply to find the mean ratios)?


tapply(data2$ratio, data2[ , c("species")], function(x){mean(x, na.rm=TRUE)})

# Both the mean and median value of gonad.mass/mass for each species are pretty 
# close to the value of the scaling coefficient that we estimated in Q2. 

#### Q5: ####

# Start by creating a vector of the sum of masses, and a vector for the length of 
# the vector, for each of the 3 different species.

Sf.sum <- 0
Sf.tot <- 0
Sp.sum <- 0
Sp.tot <- 0
Lp.sum <- 0
Lp.tot <- 0
  
# Make a for loop that will loop from 1 to the nth row of data1, which includes 
# NA values but doesn't include the row that had the outlier in Q4.
# If the ith row's species column has character "S. fransciscanus", take the vector 
# Sf.sum (which is set to start at 0), and add the number in the mass column to the 
# vector Sf.sum. Then, taking vector Sf.tot, add the previous value for Sf.tot 
# (which also starts at 0), and add 1. This works as a counter. For each iteration, it 
# will add "1". Finally, for Sf.mean, divide the value of vector Sf.sum by 
# Sf.tot. If the species in the row of the ith iteration is "S. purpuratus", do the same, 
# but with vectors Sp.sum, Sp.tot, and Sp.mean. If it's L. pictus, do the same with 
# vectors Lp.sum, Lp.tot, and Lp.mean.


for (i in c(1:nrow(data1))) {
  if(!is.na(data1$mass[i])){
    if(data1$species[i]=="S. franciscanus") {
      Sf.sum <- Sf.sum + data1$mass[i]
      Sf.tot <- Sf.tot + 1
      Sf.mean <- Sf.sum / Sf.tot
    } else if(data1$species[i]=="S. purpuratus") {
      Sp.sum <- Sp.sum + data1$mass[i]
      Sp.tot <- Sp.tot + 1
      Sp.mean <- Sp.sum / Sp.tot
    } else if(data1$species[i]=="L. pictus") {
      Lp.sum <- Lp.sum + data1$mass[i]
      Lp.tot <- Lp.tot + 1 
      Lp.mean <- Lp.sum / Lp.tot
    }
  }
}

Sf.mean # 206.30
Sp.mean # 53.09
Lp.mean # 9.82
unique(data$species)

#### Q6 ####

plotRvsD <- function (spp.name, coeff = 0.06, x.vals = c(0:100), ... ) {
  plot (ratio ~ diam, data = data[data$species == spp.name, ], 
        main = spp.name, xlab = "Urchin Diameter", 
        ylab = "Gonad mass / Total mass", pch = 16, ... )
  lines (x.vals, coeff*x.vals^3, col = "black", lwd = 2)
}

par(mfrow = c(1,3))

plotRvsD("L. pictus", coeff = 0.005, col = "paleturquoise2")
plotRvsD("S. purpuratus", coeff = 0.00075, col = "paleturquoise3")
plotRvsD("S. franciscanus", coeff = 0.0002, col = "paleturquoise4")
