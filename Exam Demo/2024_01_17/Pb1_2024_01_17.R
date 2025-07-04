library(mvtnorm)
library(MVN)
library(rgl)
library(car)
library(dbscan)
library(cluster)
library(fields)


sharks <- read.table("2024_01_17/sharks.txt", h=TRUE)

matplot(sharks)

# a) Conduct a cluster analysis of the Blueland Sharks using a hierarchical clustering method
# (Euclidean distance and Average linkage). 
# Report the number of clusters you deem appropriate to represent the data, the centroids
# of the clusters, and their size.

# Additionally, provide a possible interpretation regarding the possible number of seismic events that a↵ected the
# Abyssal Haven in the last four centuries. - Чего, я про это ничего не знаю

sharks.e <- dist(sharks, method='euclidean')
sharks.hclust_average <- hclust(sharks.e, method='average')
plot(sharks.hclust_average)

# I would take 4 clusters as appropriate one 

cluster.ec <- cutree(sharks.hclust_average, k=4) # euclidean-complete:
cluster.ec

# centroids and size:
mean(sharks[cluster.ec==1,][,1])
mean(sharks[cluster.ec==1,][,2])
m1 <- sapply(sharks[cluster.ec==1,], mean)
# (1.001071, 1.468214)
n1 <- sum(cluster.ec==1) # 28

mean(sharks[cluster.ec==2,][,1])
mean(sharks[cluster.ec==2,][,2])
m2 <- sapply(sharks[cluster.ec==2,], mean)
# (2.214286, 3.332857)
n2 <- sum(cluster.ec==2) # 35

mean(sharks[cluster.ec==3,][,1])
mean(sharks[cluster.ec==3,][,2])
m3 <- sapply(sharks[cluster.ec==3,], mean)
# (3.062791, 5.064186)
n3 <- sum(cluster.ec==3) # 43

mean(sharks[cluster.ec==4,][,1])
mean(sharks[cluster.ec==4,][,2])
m4 <- sapply(sharks[cluster.ec==4,], mean)
# (3.9075, 6.619583)
n4 <- sum(cluster.ec==4) # 24


# b) Calculate Bonferroni intervals at a global significance level of 90% for both the mean and the variances of
# the body circumferences within each of the clusters identified in (a). Introduce and verify the appropriate
# assumptions.

alpha <- 0.1  # глобальный уровень значимости
k <- 2 * 2 * 4  # 2 статистики (mean + var) × 2 переменные × 4 кластера
# alpha.bf <- alpha / k  # уровень для одного интервала

# Group 1 ----------------------------------------------------------------------
cov1 <- cov(sharks[cluster.ec==1,])

ICmean1 <- cbind(inf=m1 - sqrt(diag(cov1)/n1) * qt(1 - alpha/(2*k), n1-1),
                center= m1,
                sup= m1 + sqrt(diag(cov1)/n1) * qt(1 - alpha/(2*k), n1-1))
ICmean1

#                   inf   center      sup
# circumference 0.916496 1.001071 1.085647
# length        1.338416 1.468214 1.598013


ICvar1 <- cbind(inf=diag(cov1)*(n1-1) / qchisq(1 - alpha/(2*k), n1-1),
               center=diag(cov1),
               sup=diag(cov1)*(n1-1) / qchisq(alpha/(2*k), n1-1))
ICvar1

#                  inf       center       sup
# circumference 0.01196274 0.02277288 0.0550907
# length        0.02817608 0.05363743 0.1297562


# Group 2 ----------------------------------------------------------------------
cov2 <- cov(sharks[cluster.ec==2,])

ICmean2 <- cbind(inf=m2 - sqrt(diag(cov2)/n2) * qt(1 - alpha/(2*k), n2-1),
                 center= m2,
                 sup= m2 + sqrt(diag(cov2)/n2) * qt(1 - alpha/(2*k), n2-1))
ICmean2

#                   inf   center      sup
# circumference 2.125725 2.214286 2.302846
# length        3.219792 3.332857 3.445923


ICvar2 <- cbind(inf=diag(cov2)*(n2-1) / qchisq(1 - alpha/(2*k), n2-1),
                center=diag(cov2),
                sup=diag(cov2)*(n2-1) / qchisq(alpha/(2*k), n2-1))
ICvar2

#                  inf       center       sup
# circumference 0.01804637 0.03230168 0.06987256
# length        0.02941485 0.05265042 0.11388942

# Group 3 ----------------------------------------------------------------------
cov3 <- cov(sharks[cluster.ec==3,])

ICmean3 <- cbind(inf=m3 - sqrt(diag(cov3)/n3) * qt(1 - alpha/(2*k), n3-1),
                 center= m3,
                 sup= m3 + sqrt(diag(cov3)/n3) * qt(1 - alpha/(2*k), n3-1))
ICmean3

#                   inf   center      sup
# circumference 2.985884 3.062791 3.139697
# length        4.951374 5.064186 5.176998


ICvar3 <- cbind(inf=diag(cov3)*(n3-1) / qchisq(1 - alpha/(2*k), n3-1),
                center=diag(cov3),
                sup=diag(cov3)*(n3-1) / qchisq(alpha/(2*k), n3-1))
ICvar3

#                  inf       center       sup
# circumference 0.01805532 0.03068250 0.06074616
# length        0.03884998 0.06602016 0.13070874

# Group 4 ----------------------------------------------------------------------
cov4 <- cov(sharks[cluster.ec==4,])

ICmean4 <- cbind(inf=m4 - sqrt(diag(cov4)/n4) * qt(1 - alpha/(2*k), n4-1),
                 center= m4,
                 sup= m4 + sqrt(diag(cov4)/n4) * qt(1 - alpha/(2*k), n4-1))
ICmean4

#                   inf   center      sup
# circumference 3.761417 3.907500 4.053583
# length        6.424629 6.619583 6.814538


ICvar4 <- cbind(inf=diag(cov4)*(n4-1) / qchisq(1 - alpha/(2*k), n4-1),
                center=diag(cov4),
                sup=diag(cov4)*(n4-1) / qchisq(alpha/(2*k), n4-1))
ICvar4

#                  inf       center       sup
# circumference 0.02836937 0.0565500 0.1495155
# length        0.05052668 0.1007172 0.2662915


















