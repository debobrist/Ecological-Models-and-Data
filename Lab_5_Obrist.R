#### Data load & clean #### 

# Install lme4: 
# install.packages("lme4")
library(lme4)
 
# Load fish data from git repository.
FishData.all <- source_data(url = "https://raw.githubusercontent.com/sjpeacock/Sea-lice-database/master/Data/BroughtonSeaLice_fishData.csv")

# Subset data to exclude sockeye.
FishData <- subset(FishData.all, FishData.all$species != "sockeye")

# Load site data from git repository:
SiteData <- source_data(url = "https://raw.githubusercontent.com/sjpeacock/Sea-lice-database/master/Data/BroughtonSeaLice_siteData.csv")

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

#### Q1: ####

# fit a GLM to p.total data, including year as a fixed effect.
fit1 <- glm(p.total ~ as.factor(year), data = FishData, family = poisson(link = log))
summary(fit1)

f1coef <- summary(fit1)$coefficients

# Make some empty vectors
est <- 0 # imaginaryland intercept
lice.m <- 0 # converted mean in real land
year <- c(2001:2016) 
lice.se <- 0 # standard error 
ci.lower <- 0 # real lower confidence level
ci.upper <- 0 # real lower confidence level 
# New data frame with these vectors: 
new.dat <- data.frame(year, est, lice.m, lice.se, ci.lower, ci.upper)

# Confidence interval = Estimate ± 1.96*Std. Error
# lice.se <- sqrt((vcov(fit1)[1,1]) + (vcov(fit1)[i,i]) + 2*vcov(fit1)[i,1])

for(i in 1:nrow(f1coef)) {
  if(new.dat$year[i] == 2001) { # for years = to 2001 (the reference year, beta-0)
    new.dat$est[i] <- f1coef[i,1] # this will be the intercept estimate
    new.dat$lice.m[i] <- exp(f1coef[i,1]) # transfrom it back to real-world (mean lice)
    new.dat$ci.upper[i] <- exp(f1coef[i,1] + 1.96*f1coef[i,2]) # estimate + 1.96*se
    new.dat$ci.lower[i] <- exp(f1coef[i,1] - 1.96*f1coef[i,2]) # estimate - 1.96*se
  } else if(new.dat$year[i] > 2001) { # for every year after 2001, 
    new.dat$est[i] <- f1coef[1,1] + (f1coef[i,1]) # beta-0 plus estimate for ith year
    new.dat$lice.m[i] <- exp(f1coef[1,1] + (f1coef[i,1])) # beta-0 plus estimate for ith year,
    # transformed back into real world
    new.dat$lice.se[i] <- sqrt((vcov(fit1)[1,1]) + (vcov(fit1)[i,i]) + 2*vcov(fit1)[i,1])
    # calculate st error using sqrt(var(intercept) + var(year) + 2*cov(year, intercept))
    new.dat$ci.upper[i] <- exp(new.dat$est[i] + 1.96*new.dat$lice.se[i])  
    new.dat$ci.lower[i] <- exp(new.dat$est[i] - 1.96*new.dat$lice.se[i])  
    # transform (beta-0 plus estimate for ith year) +/- 1.96*st error for that year
  }
}

# Make it into a new data frame to delete unnecessary columns
new.dat2 <- new.dat
new.dat2$est <- NULL
new.dat2$lice.se <- NULL

# Write to csv to make table.
write.csv(new.dat2, "data-generated/lab5-q1")
