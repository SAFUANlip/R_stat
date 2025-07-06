# The files wheelworks.txt and cyclecraft.txt contain the prices (in €) and the condition (a real number from
# 0 to 12) of 20 models of vintage bikes sold from two second-hand bike shops WheelWorks and CycleCraft.

wheelworks <- read.table("2023_09_04/wheelworks.txt", h=TRUE)
cyclecraft <- read.table("2023_09_04/cyclecraft.txt", h=TRUE)

boxplot(wheelworks[,2:3])
boxplot(cyclecraft[,2:3])

data_difference <- data.frame(
  delta = wheelworks[,2:3] - cyclecraft[,2:3]
)

boxplot(data_difference) 
plot(data_difference)
mvn(data_difference) # differences 

mu0      <- c(0, 0) # H0, that difference is zero
x.mean   <- colMeans(data_difference)
x.cov    <- cov(data_difference)
x.invcov <- solve(x.cov)
n <- 20 # each group has 10 observations
p <- 2 # check two means on zero

x.T2 <- n * (x.mean-mu0) %*% x.invcov %*% (x.mean-mu0) 
Pb <- 1-pf(x.T2*(n-p)/(p*(n-1)), p, n-p)
Pb # 0.1234051 > 0.05 (95%), so we can not reject, that mean difference = 0

# b) Which are the assumptions of the previous test? Are they met?
# Delta features have to be MVN
mvn(data_difference)

# $multivariateNormality
# Test        HZ   p value MVN
# 1 Henze-Zirkler 0.3356331 0.6245718 YES
# 
# $univariateNormality
# Test        Variable Statistic   p value Normality
# 1 Anderson-Darling   delta.price      0.2681    0.6456    YES   
# 2 Anderson-Darling delta.condition    0.2891    0.5781    YES   

# c) Provide the plot of the confidence region at level 95% for the mean diference in price and condition between
# the two shops.

# mean under H0 (blue)
plot(data_difference)
points(mu0[1], mu0[2], col='blue', pch=16)

# sample mean (black)
points(x.mean[1], x.mean[2], col='black', pch=16)

# we represent the confidence region of level 95%: where does mu0 fall?
alpha <- .05
cfr.fisher <- (p*(n-1)/(n-p))*qf(1-alpha,p,n-p)
ellipse(center=x.mean, shape=x.cov/n, radius=sqrt(cfr.fisher), lwd=2)
# as we see, CI  contain 0, so we can reject H0 of zero mean

# d) Provide four Bonferroni simultaneous confidence intervals of global level 95% for the mean and the variance of
# the di↵erence in price and condition. Interpret the results of the test at point (a) through the intervals.

k <- 4 # if we also wanted to plot for variances => k = 4
alpha <- 0.05


ICmean <- cbind(inf=x.mean - sqrt(diag(x.cov)/n) * qt(1 - alpha/(2*k), n-1),
                center= x.mean,
                sup= x.mean + sqrt(diag(x.cov)/n) * qt(1 - alpha/(2*k), n-1))

ICvar <- cbind(inf=diag(x.cov)*(n-1) / qchisq(1 - alpha/(2*k), n-1),
               center=diag(x.cov),
               sup=diag(x.cov)*(n-1) / qchisq(alpha/(2*k), n-1))

ICmean
#                     inf  center       sup
# delta.price     -1.0018786 -0.3945 0.2128786
# delta.condition -0.9012848 -0.0860 0.7292848

# So we don't see any difference among two groups 

ICvar
#                     inf    center      sup
# delta.price     0.4870497 0.9695629 2.600352
# delta.condition 0.8775521 1.7469305 4.685240

