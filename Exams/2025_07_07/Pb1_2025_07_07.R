library(MVN)
library(car)
library(heplots)

# The files reference.txt, wristband.txt and smartwatch.txt contain the results of a physiological monitoring
# test in which three devices – a reference sensor, a smart watch, and a wristband – record heart rate (hr) and skin
# temperature (temp) from the same 100 volunteers (considered as randomly sampled) during a 5-minute standardized
# activity.

reference <-read.table("reference.txt", h=TRUE)

wristband <- read.table("wristband.txt", h=TRUE)

smartwatch <- read.table("smartwatch.txt", h=TRUE)


# a) Do you believe that the rows of the consolidated data matrix
# (i.e., heart rate and skin temperature measurements from the three devices
# for each subject) are independent realizations from the same 6-dimensional Gaussian
# distribution? Justify your answer.

data_all <- data.frame(
  hr_ref = reference$hr,
  temp_ref = reference$temp,
  hr_smart = smartwatch$hr,
  temp_smart = smartwatch$temp,
  hr_wr = wristband$hr,
  temp_wr = wristband$temp
)

mvn(data_all)

# Yes, all common data came from 6-dimension Gaussian distribution, I checked it my MVN test:

# multivariateNormality
# Test        HZ   p value MVN
# 1 Henze-Zirkler 0.8898043 0.6298937 YES
# 
# $univariateNormality
# Test   Variable Statistic   p value Normality
# 1 Anderson-Darling   hr_ref      0.2879    0.6121    YES   
# 2 Anderson-Darling  temp_ref     0.5591    0.1450    YES   
# 3 Anderson-Darling  hr_smart     0.1803    0.9134    YES   
# 4 Anderson-Darling temp_smart    0.5132    0.1893    YES   
# 5 Anderson-Darling   hr_wr       0.3222    0.5237    YES   
# 6 Anderson-Darling  temp_wr      0.5750    0.1320    YES  


# b) Test at the exact 95% confidence level whether either of the two wearables (smartwatch and wristband)
# systematically over-or under-estimates any of heart rate and skin temperature compared to the reference
# device.

# Comment from Guillaume:
# Here you have to work on the 4-dim contrasted observations (difference between 
# the reference sensor and the two other sensors for each variable)

data_difference <- data.frame(
  smart_ref = smartwatch - reference,
  wristband_ref = wristband - reference
)

summary(data_difference)

boxplot(data_difference)

mu0      <- c(0, 0, 0, 0) # H0, that difference is zero
x.mean   <- colMeans(data_difference)
x.cov    <- cov(data_difference)
x.invcov <- solve(x.cov)
n <- 100 # each group has 100 observations
p <- 4 # check 4 means on zero

x.T2 <- n * (x.mean-mu0) %*% x.invcov %*% (x.mean-mu0) 
Pb <- 1-pf(x.T2*(n-p)/(p*(n-1)), p, n-p)
Pb # 0.0002267289 < 0.05 (95%), so we can reject, that mean difference = 0

# Still open question
t.test(data_difference$wristband_ref.hr) #p-value = 0.000608 ~ 0.00061
t.test(data_difference$wristband_ref.temp)
t.test(data_difference$smart_ref.hr)
t.test(data_difference$smart_ref.temp)

# c) Characterize the confidence region of the previous test.
# ???
alpha <- 0.05
cfr.fisher <- (p*(n-1)/(n-p))*qf(1-alpha,p,n-p)
cfr.fisher

# d) Construct Bonferroni simultaneous confidence intervals with an overall 95% confidence level for the mean
# di!erences in heart rate and temperature between the ref device and each of the two wearables. What do
# these intervals tell you about the accuracy of the devices?

k <- 4 # we check 4 means => k = 4, if we also wanted to plot for variances => k = 8
alpha <- 0.05


ICmean <- cbind(inf=x.mean - sqrt(diag(x.cov)/n) * qt(1 - alpha/(2*k), n-1),
                center= x.mean,
                sup= x.mean + sqrt(diag(x.cov)/n) * qt(1 - alpha/(2*k), n-1))


ICmean

#                         inf    center     sup
# smart_ref.hr       -0.54380288  0.300  1.14380288 ~ [-0.544,  1.144]
# smart_ref.temp     -0.08259826  0.020  0.12259826 ~ [-0.083,  0.123]
# wristband_ref.hr   -0.94509247 -0.550 -0.15490753 ~ [-0.946, -0.155]
# wristband_ref.temp -0.07590001 -0.014  0.04790001 ~ [-0.076,  0.048]
# As we see from CI wristband underestimates temperature compared to reference
# Also we may see that smart watch more accurate than wristband

# e) Overall, which of the wristband and smartwatch devices would you recommend?
# I would use smartwatch as they more accurate