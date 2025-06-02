library(mvtnorm)
library(MVN)
library(rgl)
library(car)
library(dbscan)
library(cluster)
library(fields)


## Exercise 1 ---------------------------------------------------------------------------------
# The Veritatis Diagram is a mysterious work attributed to Galileo. Some
# semiologists believe that some pages of the book hide a coded message;
# they also believe that these pages are characterized by an abnormal
# numerosity of some letters of the alphabet. The veritatis.txt file
# lists, for 132 pages of the book, the absolute frequencies of the five
# vowels of the Latin alphabet.
# a) By using an agglomerative clustering algorithm (Manhattan distance,
#    average linkage), identify two clusters and report suspicious pages;
# b) assuming that, for each of the two clusters, the five absolute 
#    frequencies are (approximately) normally distributed with the same 
#    covariance matrix perform a test to prove the existence of a difference
#    in the mean value of the two distributions;
# c) using five Bonferroni intervals of global confidence 90%, comment the
#    results of the test at point (b).

veritatis <- read.table("veritatis.txt", h=TRUE)

# a)
help(dist)

m.dist <- dist(veritatis, method="manhattan")
veritatis.hclust <- hclust(m.dist, method='average')
veritatis.clust <- cutree(veritatis.hclust, k=2)

plot(veritatis.hclust)

table(veritatis.clust)

# b) 5 features => MANOVA, to check mean differences 
# but lets check assumtions 
mvn(veritatis[which(veritatis.clust == 2), ])
mvn(veritatis[which(veritatis.clust == 1), ])

cov1 <- cov(veritatis[which(veritatis.clust == 1), ])
cov2 <- cov(veritatis[which(veritatis.clust == 2), ])

image(cov1)
image(cov2)

# factor less than ~10
max(cov1/cov2)
min(cov1/cov2)

fit <- manova(as.matrix(veritatis) ~ veritatis.clust)
summary.manova(fit)
summary.aov(fit)

# c) Bonferroni CI
alpha <- 0.1
W <- summary.manova(fit)$SS$Residuals 
# degree of freedom taken from manova summary (132 - 2 (n-g))

k <- 5 # 5 Bonferroni CI
cfr.t <- qt(1-alpha/(2*k), 130) # Student quantile

m1 <- sapply(veritatis[veritatis.clust == 1,], mean)
m2 <- sapply(veritatis[veritatis.clust == 2,], mean)

n1 <- sum(veritatis[veritatis.clust == 1,])
n2 <- sum(veritatis[veritatis.clust == 2,])

inf12 <- m2 - m1 - cfr.t * sqrt(diag(W)/(130) * (1/n1+1/n2))
sup12 <- m2 - m1 + cfr.t * sqrt(diag(W)/(130) * (1/n1+1/n2))

CI <- list(group12    = cbind(inf12, m2 - m1, sup12))
CI

# from Bonferroni CI we may say, that all features have effect of differences among
# groups, and all features ahs differet means among groups

## Exercise 4 ------------------------------------------------------------------
# The dataset Pb3.txt reports Length (cm) Width (cm) of the chest of
# 50 sparrows. The biologist who has collected the measurements aims to 
# demonstrate that the sparrows can be divided into two distinct groups
# in terms of length and width of the chest. Help him to prove his theory 
# by implementing and commenting on the following analyses:
# (a) By means of an agglomerative hierarchical clustering algorithm that
#     uses the Manhattan distance and the Single linkage state if it is 
#     reasonable to cluster the data into two groups.
# (b) Implement a test to prove the difference of the means of the two groups.
# (c) Identify and comment the four Bonferroni intervals with global confidence
#     90% (lower bound, central value, upper bound) for:
#     - The difference of the mean of the variable length.
#     - The difference of the mean of the variable width.
#     - The difference of the mean of the sum of the variables length and width.
#     - The difference of the mean of the difference of variable length and
#       width.
# (d) verify the assumptions necessary to the implementation of the test.


Pb3 <- read.table("Pb3.txt", h=TRUE)

# a) 
plot(Pb3)

m.dist <- dist(Pb3, method="manhattan")
Pb3.hclust <- hclust(m.dist, method='single')
Pb3.clust <- cutree(Pb3.hclust, k=2)

plot(Pb3, pch=16, col = as.vector(Pb3.clust)+1) # good clustering

# b) 
fit <- manova(as.matrix(Pb3) ~ Pb3.clust)
summary.manova(fit) # so there are signifficant difference among clusters 
summary.aov(fit) 

# c)
alpha <- 0.1
W <- summary.manova(fit)$SS$Residuals 
# degree of freedom taken from manova summary (50 - 2 (n-g))

DF <- 48

k <- 4 # 4 Bonferroni CI
cfr.t <- qt(1-alpha/(2*k), DF) # Student quantile

m1 <- sapply(Pb3[Pb3.clust == 1,], mean)
m2 <- sapply(Pb3[Pb3.clust == 2,], mean)

n1 <- sum(Pb3[Pb3.clust == 1,])
n2 <- sum(Pb3[Pb3.clust == 2,])


C <- matrix(
  c(1, 0,
    0, 1,
    1, 1,
    1, -1
    ),
  4, 2, byrow=T)

mean_comb <- (m1 - m2) %*% t(C)

WC <- C %*% W %*% t(C)

inf12 <- mean_comb - cfr.t * sqrt(diag(WC)/(DF) * (1/n1+1/n2))
sup12 <- mean_comb + cfr.t * sqrt(diag(WC)/(DF) * (1/n1+1/n2))

CI <- list(group12    = cbind(t(inf12), t(mean_comb), t(sup12)))
CI




