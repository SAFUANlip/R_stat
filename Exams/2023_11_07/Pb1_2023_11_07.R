library(mvtnorm)
library(MVN)
library(rgl)
library(car)
library(dbscan)
library(cluster)
library(fields)

# In the remote wilderness of the Amazon Rainforest, there is a region known as the Jungle Sanctuary that
# has witnessed a long history of natural disturbances, including massive wildfires. The file kapok.txt contains
# information on 294 ancient Kapok trees, specifically recording their heights (in meters) and trunk diameters (in                                                                                               meters).

kapok <- read.table("2023_11_07/kapok.txt", h=TRUE)

# a) Conduct a cluster analysis of the Kapok trees using a hierarchical clustering method (Euclidean distance and                                                                                       Ward linkage). Report the number of clusters you deem appropriate to represent the data, the centroids of
# the clusters, and their size. Additionally, provide a possible interpretation regarding the possible number of
# historical wildfires that a↵ected the growth of the Jungle Sanctuary.

kapok.e <- dist(kapok, method='euclidean')
kapok.hclust_ward <- hclust(kapok.e, method='ward.D2')
plot(kapok.hclust_ward)

# Looking on plot, I would take 4 clusters 

cluster.ec <- cutree(kapok.hclust_ward, k=4) # euclidean-complete:
cluster.ec


# centroids and size:
m1 <- sapply(kapok[cluster.ec==1,], mean)
m1
# (71.831585  7.382927)
n1 <- sum(cluster.ec==1) # 82


m2 <- sapply(kapok[cluster.ec==2,], mean)
m2
# (53.744694  6.111735)
n2 <- sum(cluster.ec==2) # 98


m3 <- sapply(kapok[cluster.ec==3,], mean)
m3
# (62.750909  6.951558)
n3 <- sum(cluster.ec==3) # 77


m4 <- sapply(kapok[cluster.ec==4,], mean)
m4
# (87.184865  9.315405)
n4 <- sum(cluster.ec==4) # 37

# Additionally, provide a possible interpretation regarding the possible number of
# historical wildfires that a↵ected the growth of the Jungle Sanctuary.

# maybe as we have 4 clusters, there were 4 historical wildfires that a↵ected the growth of the Jungle Sanctuary

# b) Calculate Bonferroni intervals at a global significance level of 90% for both the mean and variances of the trunk
# diameters within each of the clusters identified in (a). Introduce and verify the appropriate assumptions.

alpha <- 0.1  # глобальный уровень значимости
k <- 2 * 2 * 4  # 2 статистики (mean + var) × 2 переменные × 4 кластера
# alpha.bf <- alpha / k  # уровень для одного интервала

# Group 1 ----------------------------------------------------------------------
cov1 <- cov(kapok[cluster.ec==1,])

ICmean1 <- cbind(inf=m1 - sqrt(diag(cov1)/n1) * qt(1 - alpha/(2*k), n1-1),
                 center= m1,
                 sup= m1 + sqrt(diag(cov1)/n1) * qt(1 - alpha/(2*k), n1-1))
ICmean1

#                   inf   center      sup
# height   71.369631 71.831585 72.293540
# diameter  7.157096  7.382927  7.608757


ICvar1 <- cbind(inf=diag(cov1)*(n1-1) / qchisq(1 - alpha/(2*k), n1-1),
                center=diag(cov1),
                sup=diag(cov1)*(n1-1) / qchisq(alpha/(2*k), n1-1))
ICvar1

#                  inf       center       sup
# height   1.4968628 2.219804 3.5611804
# diameter 0.3577246 0.530495 0.8510611