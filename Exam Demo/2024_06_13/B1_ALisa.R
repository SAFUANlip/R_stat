# Problem 1: Whisky bottles price estimation
# The file whisky.txt contains the outcome of a whisky tasting challenge where 
# three whisky tasters - named t1, t2 and t3, respectively - have to evaluate 
# the price of 10 di↵erent bottles of whisky, randomly picked in a whisky shop.
# Each column corresponds to the ten price evaluations of a single taster. 
# Each row corresponds to the three price evaluations of a single bottle.

whisky <- read.table("2024_06_13//whisky.txt", h=TRUE)
whisky

# a) Do you believe the rows of the data matrix have been independently generated 
# by the same tri-variate Gaussian distribution? Justify your answer.

# we need to check mvn 
mvn(data = whisky)$multivariateNormality$`p value`  # Yes, I believe 

# b) Do you think that one of the tasters tends to over/under-price the bottles 
# compared to the two others? After having verified any required assumptions, 
# support your answer with a 95% level test.

boxplot(whisky)
colMeans(whisky)
# t1 t2 t3 
# 77 76 69 

# I assume that the taster 3 tends to underprice the bottles 
# we need to prove that mean for t3 is less than mean of t1 and t2
# -> difference between t1 and t3, t2 and t3 is not zero

whisky$diff13 <- whisky$t1 - whisky$t3
whisky$diff23 <- whisky$t2 - whisky$t3

whisky_diff <- cbind(whisky$diff13,whisky$diff23)
whisky_diff

# we first need to verify the Gaussian assumption
mvn(data = whisky_diff)$multivariateNormality$`p value`  # mvn
plot(whisky_diff, asp=1, pch=1)

mu0      <- c(0, 0)
x.mean   <- colMeans(whisky_diff)
x.cov    <- cov(whisky_diff)
x.invcov <- solve(x.cov)
n <- 10
p <- 2

x.T2       <- n * (x.mean-mu0) %*% x.invcov %*% (x.mean-mu0)  # 23.00637
Pb <- 1-pf(x.T2*(n-p)/(p*(n-1)), p, n-p)
Pb # 0.006252078-> the mean is not zero!

# mean under H0 (blue)
points(mu0[1], mu0[2], col='blue', pch=16)

# sample mean (black)
points(x.mean[1], x.mean[2], col='black', pch=16)

# we represent the confidence region of level 95%: where does mu0 fall?
alpha <- .05
cfr.fisher <- (p*(n-1)/(n-p))*qf(1-alpha,p,n-p)
ellipse(center=x.mean, shape=x.cov/n, radius=sqrt(cfr.fisher), lwd=2)

# what about the region of level 99%?
alpha <- .01
cfr.fisher <- (p*(n-1)/(n-p))*qf(1-alpha,p,n-p)
ellipse(center=x.mean, shape=x.cov/n, radius=sqrt(cfr.fisher), lwd=2, col='orange', add=TRUE)
dev.off()

# c) Provide a plot of the confidence region for the mean vector of the di↵erences
# in price evaluation between t1 and each of the two other tasters.

whisky$diff12 <- whisky$t1 - whisky$t2
whisky_diff_1 <- cbind(whisky$diff12,whisky$diff13)

mvn(data = whisky_diff_1)$multivariateNormality$`p value`  # mvn
plot(whisky_diff_1, asp=1, pch=1)

mu0      <- c(0, 0)
x.mean.1   <- colMeans(whisky_diff_1)
x.cov.1    <- cov(whisky_diff_1)
x.invcov.1 <- solve(x.cov.1)
n <- 10
p <- 2

x.T2.1       <- n * (x.mean.1-mu0) %*% x.invcov.1 %*% (x.mean.1-mu0) 
Pb <- 1-pf(x.T2.1*(n-p)/(p*(n-1)), p, n-p)
Pb # 0.006252078 -> the mean is not zero!

# mean under H0 (blue)
points(mu0[1], mu0[2], col='blue', pch=16)

# sample mean (black)
points(x.mean[1], x.mean[2], col='black', pch=16)

# we represent the confidence region of level 95%: where does mu0 fall?
alpha <- .05
cfr.fisher <- (p*(n-1)/(n-p))*qf(1-alpha,p,n-p)
ellipse(center=x.mean, shape=x.cov/n, radius=sqrt(cfr.fisher), lwd=2)

# what about the region of level 99%?
alpha <- .01
cfr.fisher <- (p*(n-1)/(n-p))*qf(1-alpha,p,n-p)
ellipse(center=x.mean, shape=x.cov/n, radius=sqrt(cfr.fisher), lwd=2, col='orange', add=TRUE)
dev.off()

# d) Provide two Bonferroni simultaneous confidence intervals of global level 
# 95% for the mean of the difference in price evaluation between t1 and each
# of the two other tasters.

k <- 2
alpha <- 0.05
n
ICmean <- cbind(inf=x.mean.1 - sqrt(diag(x.cov.1)/n) * qt(1 - alpha/(2*k), n-1),
                center= x.mean.1,
                sup= x.mean.1 + sqrt(diag(x.cov.1)/n) * qt(1 - alpha/(2*k), n-1))
ICmean

# inf                 center       sup
# [1,] -3.819742      1         5.819742
# [2,]  2.269181      8         13.730819