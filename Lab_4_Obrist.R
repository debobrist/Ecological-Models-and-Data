# Install required package to read in data.
install.packages ("repmis")
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

FishData$p.total<-apply(FishData[,c(11:25)], 1, sum, na.rm=TRUE)

FishData$infested<-as.numeric(FishData$p.total>0)

# Part II: 

#### Q1: ####

# Models height as a function of length and infestation state, including interaction between
# length and infestation. 
fit1<-lm(height ~ length * infested, data=FishData)

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



