#### Pre-Question Data Cleaning and Prep ####

# Install required package to read in data.
# install.packages ("repmis")
library("repmis")

# Load fish data from git repository.
FishData.all <- source_data(url = "https://raw.githubusercontent.com/sjpeacock/Sea-lice-database/master/Data/BroughtonSeaLice_fishData.csv")

# Subset data for after 2010, and exclude sockeye.

FishData <- subset(FishData.all,
                   FishData.all$year > 2010 & FishData.all$species != "sockeye")

# Load site data from git repository, subset for after 2010.
SiteData.all <- source_data(url = "https://raw.githubusercontent.com/sjpeacock/Sea-lice-database/master/Data/BroughtonSeaLice_siteData.csv")
SiteData <- subset(SiteData.all, SiteData.all$year > 2010)

# Make new columns matching salinity and temperature data from site data to individual
# fish data. 
FishData$temp <- SiteData$temp[match(FishData$site_id, SiteData$site_id)]
FishData$sal <- SiteData$salt[match(FishData$site_id, SiteData$site_id)]

# Create 2 new variables in fish data: 

names(FishData)[c(11:25)]

# p.total is total number of parasites per fish.
FishData$p.total<-apply(FishData[,c(11:25)], 1, sum, na.rm=TRUE)
# infested indicates that a fish is infested with lice (1) or not (0)
FishData$infested<-as.numeric(FishData$p.total>0)



# Part II: Linear Models in R


#### Q1: ####

#### b.) ####
# To look at whether height (condition) changes with infestation, model height 
# as a function of length and infestation state, including interaction between
# length and infestation. 
fit1<-lm(height ~ length * infested, data=FishData)

# Create null model without infestation explanatory variable.
fit1b <- lm(height ~ length, data=FishData)

# Calculate -log(L) for each model:
f1LL <- -logLik(fit1) # 28678.44
f1bLL <- -logLik(fit1b) # 28682.39

# Calculate the test statistic: 
t_stat <- 2*f1bLL - 2*f1LL # 7.906919

# Calculate the p-value:
pchisq(t_stat, df=1, lower.tail=FALSE) #0.004924607

#### c.) ####

plot(FishData$height[which(FishData$infested == 1)] ~ 
       FishData$length[which(FishData$infested == 1)], 
     col = "steelblue",
     pch = 1, 
     cex = 1, 
     xlab = "Fish Length (mm)",
     ylab = "Fish Height (mm)")

points(FishData$height[which(FishData$infested == 0)] ~ 
         FishData$length[which(FishData$infested == 0)],
       col = "grey41",
       pch = 16, 
       cex = 0.5)

f1coef <- summary(fit1)$coefficients
f1coef

# Add regression line for model fit1 (including effect of infestation)
x.vals <- seq(0, 150, 10)
lines(x = x.vals, 
      y = f1coef[1,1] + f1coef[2,1]*x.vals + f1coef[3,1] + f1coef[4,1]*x.vals, 
      lwd = 3,
      col = "black")

# Add regression line for model fit1b (not including effect of infestation)
f1bcoef <- summary(fit1b)$coefficients
f1bcoef

x.vals <- seq(0, 150, 10)
lines(x = x.vals, 
      y = f1bcoef[1,1] + f1bcoef[2,1]*x.vals, 
      lwd = 3,
      lty = 3,
      col = "green1")

legend("topleft", 
       legend = c("Infested", "Not infested"), 
       col = c("black", "green1"),
       lty = c(1, 3),
       bty = "n",
       lwd = 3)


#### Q2: ####

#### a.) #### 

# Fit Poisson GLM to p.total using species as explanatory variable!
fit2 <- glm(FishData$p.total ~ FishData$species, family = poisson(link = "log"))
summary(fit2)

fit2b <- glm(FishData$p.total ~ 1, family = poisson ( link = "log"))
summary

# Calculate -log(L) for each model:
f2LL <- -logLik(fit2) 
f2bLL <- -logLik(fit2b) 

# Calculate the test statistic: 
t_stat_2 <- 2*f2bLL - 2*f2LL # 104.0804

# Calculate the p-value:
pchisq(t_stat_2, df=1, lower.tail=FALSE) #1.94 x 10^-24

#### c.) ####
# What is the average number of sea lice 
# (± 95% confidence limits) on pink salmon and chum salmon?

f2coef <- summary(fit2)$coefficients
f2coef

exp(f2coef[1,1]) # Each chum on average has 0.9224443 sea lice. 
exp(f2coef[1,1] + f2coef[2,1]) # Each pink on average has 0.7835677 sea lice.

# Confidence interval = Estimate ± 1.96*Std. Error

# Chum min and max

chumCI_max <- exp(f2coef[1,1] + 1.96*f2coef[1,2]) # 0.9015115
chumCI_min <- exp(f2coef[1,1] - 1.96*f2coef[1,2]) # 0.9438632
chumCI <- c(chumCI_min, chumCI_max)
chumCI


# Pink max and min ie.) Not chum max and min. - Must add chum intercept term.
vcov(fit2) # gives the covariance between int (chum) and pink salmon.

# Calculate the SE for pink salmon using standard errors: 
SE.pink.se <- sqrt((f2coef[1,2])^2 + (f2coef[2,2])^2 + 2*vcov(fit2)[2,1])

# Calculate the SE for pink salmon using variance:
SE.pink.v <- sqrt((vcov(fit2)[1,1]) + (vcov(fit2)[2,2]) + 2*vcov(fit2)[2,1])

# Do you get the same CI? 
pinkCI_max <- exp(f2coef[1,1] + (f2coef[2,1] + 1.96*SE.pink.se)) # 0.7671104
pinkCI_min <- exp(f2coef[1,1] + (f2coef[2,1] - 1.96*SE.pink.se)) # 0.8003781

pinkCI_max <- exp(f2coef[1,1] + (f2coef[2,1] + 1.96*SE.pink.v))  # 0.7671104
pinkCI_min <- exp(f2coef[1,1] + (f2coef[2,1] - 1.96*SE.pink.v))  # 0.8003781
pinkCI <- c(pinkCI_min, pinkCI_max)
pinkCI
# Yay they are the same! I was making a subsetting error earlier but it all makes sense now! :)

# Confirm whether these are correct: 

fit2c <- glm(FishData$p.total ~ FishData$species - 1, family = poisson(link = "log"))
f2ccoef <- summary(fit2c)$coefficients
summary(fit2c)
pinkCI_max_b <- exp(f2ccoef[2,1] + 1.96*f2ccoef[2,2])
pinkCI_min_b <- exp(f2ccoef[2,1] - 1.96*f2ccoef[2,2])
pinkCI_b <- c(pinkCI_min_b, pinkCI_max_b)
pinkCI_b #0.7671104 0.8003781 Yay!


#### d.) ####
# Test for over-dispersion: take ratio of residual deviance / degrees of freedom.

ratio <- 33381 / 18783
ratio # 1.777192
hist(FishData$p.total, 
     xlab = "Number of parasites", 
     ylab = "Frequency",
     col = "maroon",
     labels = TRUE,
     main = "",
     ylim = c(0, 18000))

#### e.) ####

# Fit a model of number of parasites as a function of species, length, and the interaction
# between species and length. 
fit3 <- glm(FishData$p.total ~ FishData$species * FishData$length,
            family = poisson(link = "log"))

# Is the effect of length and/or the interaction with species statistically significant?

# To answer the question of effect of length AND the interaction, first look at 
# p.total ~ species + length + species*length vs just p.total $ species

f2LL # Calculated in question 2 - 26593.2, df = 2
f3LL <- -logLik(fit3) # 26483.61, df = 1

# Calculate the test statistic: 
t_stat_3 <- 2*f2LL - 2*f3LL # 219.1794

# Calculate the p-value:
pchisq(t_stat_3, df=2, lower.tail=FALSE) #2.54 x 10^-48 

# Next, answer question, is the effect of length on species statistically significant?
fit3b <- glm(FishData$p.total ~ FishData$species + FishData$length, 
             family = poisson(link = "log"))

f3bLL <- -logLik(fit3b)
t_stat_4 <- 2*f2LL - 2*f3bLL # 127.8619

pchisq(t_stat_4, df=1, lower.tail=FALSE) #1.20 x 10^-29

# Does the interaction term explain a significant amount of extra variation? 

# Compare fit3 vs fit3b
t_stat_5 <- 2*f3bLL - 2*f3LL #91.3175
pchisq(t_stat_5, df=1, lower.tail=FALSE) #1.22 x 10^-21 so yes, the interaction term
# explains a significant amount of additional variation.


# Is this biologically signficant? Make a plot and see! 

plot(FishData$p.total[which(FishData$species == "chum")] ~ 
       FishData$length[which(FishData$species == "chum")], 
     col = "steelblue",
     pch = 1, 
     cex = 1, 
     xlab = "Fish Length (mm)",
     ylab = "Number of Parasites (n)")

points(FishData$p.total[which(FishData$species == "pink")] ~ 
         FishData$length[which(FishData$species == "pink")],
       col = "grey41",
       pch = 16, 
       cex = 0.5)

legend ("topright",
        legend = c("chum", "pink"), 
        col = c("steelblue", "grey41"),
        pch = c(1,16),
        bty = "n")

# Add regression lines:
x.vals3 <- c(20, 140, 0.1)

f3coef <- summary(fit3)$coefficient

# Chum line: 
lines (x = x.vals3, 
       y = exp(f3coef[1,1] + f3coef[3,1] * x.vals3),
       lwd = 2)
# Pink line: 
lines (x = x.vals3,
       y = exp(f3coef[1,1] + f3coef[3,1] * x.vals3 + 
                 f3coef[2,1]  + f3coef[4,1] * x.vals3), 
       lwd = 2, 
       col = "hotpink")

## Side note - Ignore while marking! Just wanted to learn how to use length for a dataframe.
# You have to make a new data frame subsetting for what you want the length of.
# df1 <- FishData[FishData$species == "pink", ]
#length(df1$species)

#### Q3: ####


#### a.) ####

# Get rid of NA values in blue_blotches column. 
FishData$blue_blotches[is.na(FishData$blue_blotches)]<-0

# Fit a glm with binomial distribution to presence of blue blotches 
# as a function of total number of parasites: 
fit4 <- glm(FishData$blue_blotches ~ FishData$p.total, 
            family = binomial (link = "logit"))
f4coef <- summary(fit4)$coefficients

summary(fit4) 

# Test for statistical significance: 

# Start by fitting intercept model.
fit4b <- glm(FishData$blue_blotches ~ 1, family = binomial (link = "logit"))

# Calculate -log likelihoods:
f4LL <- -logLik(fit4)
f4bLL <- -logLik(fit4b)

# Calculate test statistic: 
t_stat_6 <- 2*f4bLL - 2*f4LL # 222.7049

# Calculate the p-value:
pchisq(t_stat_6, df=1, lower.tail=FALSE) # 2.32 x 10^-50

# Plot data! Jitter so can see real distribution of points.

plot(jitter(FishData$blue_blotches) ~ FishData$p.total,
     xlab = "Number of Lice",
     ylab = "Presence of Blue Blotches", 
     col = "maroon", 
     cex = 0.5, 
     pch = 16)

# Create x for making line. 
x.vals2 <- seq(0, 27, 0.1)

# Add the line - have to convert coefficients for the line back into real 
# world. plogis is a function that lets you do this - enters the values you 
# feed it back into 1 /( 1 + e ^ (-LP)) to convert back to real world.


points(x = x.vals2, 
       y = plogis(f4coef[1,1] + f4coef[2,1]*x.vals2),
       type = "l", 
       lwd = 2)
?plogis # read about plogis

# This plot is the same as the one above but without jitter.
plot(FishData$blue_blotches ~ FishData$p.total,
     xlab = "Number of Lice",
     ylab = "Presence of Blue Blotches", 
     col = "maroon", 
     cex = 1, 
     pch = 16)

points(x = x.vals2, 
       y = plogis(f4coef[1,1] + f4coef[2,1]*x.vals2),
       type = "l", 
       lwd = 2)
# Check to see if this is biologically relevant? 
boxplot(p.total~blue_blotches, data = FishData,
        names = c("No Blue Blotches", "Blue Blotches"), 
        ylab = "Number of Parasites")

#### b.) ####

# Test to see if temperature is associated to blue blotches in fish: 

# First, make new dataframe for FishData because I'm messing
# around with it and don't want to mess up FishData! 

FishData2 <- FishData

# Remove rows that have NAs for temperature: 
FishData2 <- FishData2[!is.na(FishData2$temp),]
range(FishData2$temp) # 7 - 16.2. Removing NAs was a success!

names(FishData2)
fit5 <- glm(FishData2$blue_blotches ~ FishData2$temp, 
            family = binomial (link = "logit"))
f5coef <- summary(fit5)$coefficients

# Test for statistical significance: Compare fit 5 (blue_blotches ~ temp) to the intercept
# model for blue_blotches (fit4b)


# Calculate -log likelihood for fit5:
f5LL <- -logLik(fit5)

# Calculate test statistic: 
t_stat_7 <- 2*f4bLL - 2*f5LL # 688.6385

# Calculate the p-value:
pchisq(t_stat_7, df=1, lower.tail=FALSE) # 8.84 x 10^-152

# Make a plot! 
plot(jitter(FishData2$blue_blotches) ~ FishData2$temp,
     xlab = "Temperature",
     ylab = "Presence of Blue Blotches", 
     col = "blue", 
     cex = 0.5, 
     pch = 16)

x.vals2 <- seq(0, 17, 0.01)

points(x = x.vals2, 
       y = plogis(f5coef[1,1] + f5coef[2,1]*x.vals2),
       type = "l", 
       lwd = 2)

# Explore blob years! 
pre.blob <- FishData2[FishData2$year < 2013, ]
blob <- FishData2[FishData2$year >= 2013, ]

# 
plot(jitter(pre.blob$blue_blotches) ~ pre.blob$temp,
     xlab = "Temperature",
     ylab = "Presence of Blue Blotches", 
     col = "blue", 
     cex = 0.25, 
     pch = 16,
     xlim = c(7,17))

points(jitter(blob$blue_blotches) ~ blob$temp,
       col = "red",
       cex = 0.25)

x.vals2 <- seq(0, 25, 0.01)
points(x = x.vals2, 
       y = plogis(f5coef[1,1] + f5coef[2,1]*x.vals2),
       type = "l", 
       lwd = 2)

legend("left", 
       legend = c("Pre-Blob Years", "Blob Years"), 
       col = c("blue", "red"),
       pch = 16,
       bty = "n")