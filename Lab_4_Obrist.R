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

# b.) 
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
graphics.off()

# Plot regression lines: 

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

# Fit GLM !
fit2 <- glm(FishData$p.total ~ FishData$species, family = poisson(link = "log"))
summary(fit2)

2e-16


# c.) What is the average number of sea lice 
# (± 95% confidence limits) on pink salmon and chum salmon?

f2coef <- summary(fit2)$coefficients
f2coef

exp(f2coef[1,1]) # Each chum on average has 0.9224443 sea lice. 
exp(f2coef[1,1] + f2coef[2,1]) # Each pink on average has 0.7835677 sea lice.

# Estimate ± 1.96*Std. Error

# Chum min and max
chumCI_max <- exp(f2coef[1,1] + 1.96*f2coef[1,2]) # 0.9015115
chumCI_min <- exp(f2coef[1,1] - 1.96*f2coef[1,2]) # 0.9438632
chumCI <- c(chumCI_min, chumCI_max)
chumCI

# Pink max and min ie.) Not chum max and min.
pinkCI_max <- exp((f2coef[1,1] + (f2coef[2,1] + 1.96*f2coef[2,2]))) # 0.8084525
pinkCI_min <- exp((f2coef[1,1] + (f2coef[2,1] - 1.96*f2coef[2,2]))) # 0.7594488
pinkCI <- c(pinkCI_min, pinkCI_max)
pinkCI

summary(fit2)
ratio <- 33381 / 18783
ratio # 1.777192

# d.) 
fit3 <- glm(FishData$p.total ~ FishData$species * FishData$length,
            family = poisson(link = "log"))
summary(fit3)


# Is this biologically signficant? 

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
        bty = "n",
        cex = c(1, 0.5))

#### Q3: ####

FishData$blue_blotches[is.na(FishData$blue_blotches)]<-0

fit4 <- glm(FishData$blue_blotches ~ FishData$p.total, 
            family = binomial (link = "logit"))
f4coef <- summary(fit4)$coefficients

plot(jitter(FishData$blue_blotches) ~ FishData$p.total,
     xlab = "Number of Lice",
     ylab = "Presence of Blue Blotches", 
     col = "maroon", 
     cex = 0.5, 
     pch = 16)

x.vals2 <- seq(0, 27, 0.1)

points(x = x.vals2, 
       y = plogis(f4coef[1,1] + f4coef[2,1]*x.vals2),
       type = "l", 
       lwd = 2)


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

# c.) 

# Test to see if eroded gills are related to sea louse infection: 

# Remove NAs:
FishData$hem[is.na(FishData$hem)]<-0

fit5 <- glm(FishData$hem ~ FishData$p.total, 
            family = binomial (link = "logit"))
f5coef <- summary(fit5)$coefficients

plot(jitter(FishData$hem) ~ FishData$p.total,
     xlab = "Number of Lice",
     ylab = "Presence of Hemorrhaging", 
     col = "maroon", 
     cex = 0.5, 
     pch = 16)

x.vals2 <- seq(0, 27, 0.1)

points(x = x.vals2, 
       y = plogis(f5coef[1,1] + f5coef[2,1]*x.vals2),
       type = "l", 
       lwd = 2)


