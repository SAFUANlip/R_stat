library(mvtnorm) # to deal with multivariate normal distributions
library(car) # "Companion to Applied Regression" for regression analysis
library(pheatmap) # pheatmap

# The dataset asteroids.txt contains information on 150 di↵erent asteroids (assumed to be ellipsoidal), each
# characterized by eight quantitative features obtained through advanced astronomical observations.
# These features include surface area, volume, major axis length, minor axis length, eccentricity, convex volume
# (volume of the 3D convex hull), equivalent diameter (diameter of the sphere of the same volume), and roundness
# (calculated with the formula 6p⇡· area/perimeter^(3/2) ,
# equal to 1 for a perfect sphere). The dataset also includes
# details about the type of asteroid.

asteroids <- read.table("2024_01_17/asteroids.txt", h=TRUE)

asteroids_data <- asteroids[, 1:8]

# a) Conduct a Principal Component Analysis (PCA) of the dataset, focusing on the quantitative variables only.
# Determine whether it is more appropriate to use the original variables or the standardized ones and proceed
# accordingly.

n <- dim(asteroids_data)[1]
p <- dim(asteroids_data)[2]

# Exploration
# I would  standartise variabels, due to each feature have different sample variance
boxplot(asteroids_data, col = 'gold', main = 'Original Variables')
asteroids_data.sd <- scale(asteroids_data, center=FALSE, scale=TRUE)
asteroids_data.sd <- data.frame(asteroids_data.sd)
boxplot(asteroids_data.sd, col = 'gold', main = 'Standartised Variables')

asteroids_data.sd

pc.asteroids_data.sd <- princomp(asteroids_data.sd, scores=T)
pc.asteroids_data.sd
summary(pc.asteroids_data.sd)

# Importance of components:
#   Comp.1     Comp.2     Comp.3      Comp.4      Comp.5       Comp.6
# Standard deviation     0.3176319 0.10193845 0.05498212 0.015708325 0.011378849 0.0101583597
# Proportion of Variance 0.8775817 0.09038893 0.02629554 0.002146342 0.001126253 0.0008976073
# Cumulative Proportion  0.8775817 0.96797061 0.99426616 0.996412498 0.997538751 0.9984363584
# Comp.7       Comp.8
# Standard deviation     0.0099822004 0.0089508503
# Proportion of Variance 0.0008667459 0.0006968957
# Cumulative Proportion  0.9993031043 1.0000000000

# b) Report a plot of the loadings of the first two principal components and provide an interpretation. Report the
# scatter plot of the data along the first two principal components. Considering the categorical variable Type,
# interpret the results.

n_lod_show <- 2
load.asteroids_data.sd <- pc.asteroids_data.sd$loadings
par(mar = c(1, n_lod_show, 0, 2), mfrow = c(n_lod_show, 1))
for (i in 1:n_lod_show) {
  barplot(load.asteroids_data.sd[, i], ylim = c(-1, 1))
}
# first components represents general effect of variabels, as:
# SurfArea, Volume, MajAxis, MinAxis, ConvVol, EqDiam, Round
# while second component represents contrast of:
# MajAxis and Ecc, with MinAxis and Round

cols <- as.factor(asteroids$Type)

plot(pc.asteroids_data.sd$scores[,1], pc.asteroids_data.sd$scores[,2],
     col = cols, pch = 19, xlab = "PC1", ylab = "PC2", main = "PCA of Asteroids")

text(pc.asteroids_data.sd$scores[,1], pc.asteroids_data.sd$scores[,2],
     labels = asteroids$Type, pos = 3, cex = 0.7)

# c) For asteroids of the metallic variety, construct a 95% confidence region for the mean of the vector whose
# components are the first two principal components. Describe this region by providing its center coordinates,
# axis directions, and semi-axes lengths. Provide a plot of the region. Introduce and assess the hypothesis of
# normality, which will here be tested at the 1% significance level.

metalic_data <- data.frame(
  pc1=pc.asteroids_data.sd$scores[,1][asteroids$Type == "metallic"],
  pc2=pc.asteroids_data.sd$scores[,2][asteroids$Type == "metallic"]
)

mvn(metalic_data) # 1% significance level => if p-value > 0.01 => normal

metalic.m <- sapply(metalic_data, mean)
# 0.06248007 -0.08163318 

metalic.cov <- cov(metalic_data)
#.       pc1         pc2
# pc1 0.009653561 0.001505507
# pc2 0.001505507 0.007900650

# Directions and axis length ---------------------------------------------------
n_metalic <- nrow(metalic_data)
p <- 2
alpha <- 0.05

cfr.fisher <- ((n_metalic - 1) * p / (n_metalic - p)) * qf(1 - alpha, p, n_metalic - p)
cfr.fisher

eig <- eigen(metalic.cov / n_metalic)
directions <- eig$vectors      # направления (ось эллипса)
lengths <- sqrt(eig$values * cfr.fisher)  # длины полуосей

# Результаты:
directions  # Each column — direction of axis 
#         [,1]       [,2]
# [1,] -0.8669251  0.4984385
# [2,] -0.4984385 -0.8669251

lengths     # длины полуосей (вдоль направлений) 
# 0.03702053 0.03027514


# Ellipsoidal confidence region with confidence level 95%
plot(metalic_data, asp=1, pch=1, main='Metallic data')
# plottin ellipse, don't forget to divide cov/n
ellipse(center=metalic.m, shape=metalic.cov/n_metalic, radius=sqrt(cfr.fisher), lwd=2)

arrows(metalic.m[1], metalic.m[2],
       metalic.m[1] + lengths[1]*directions[1,1],
       metalic.m[2] + lengths[1]*directions[2,1],
       col="red", lwd=2)

arrows(metalic.m[1], metalic.m[2],
       metalic.m[1] + lengths[2]*directions[1,2],
       metalic.m[2] + lengths[2]*directions[2,2],
       col="darkgreen", lwd=2)


abline(a = metalic.m[2] - eigen(metalic.cov)$vectors[2, 1] / eigen(metalic.cov)$vectors[1, 1] * metalic.m[1], 
       b = eigen(metalic.cov)$vectors[2, 1] / eigen(metalic.cov)$vectors[1, 1], 
       lty = 2, col = 'dark red', lwd = 2)
abline(a = metalic.m[2] - eigen(metalic.cov)$vectors[2, 2] / eigen(metalic.cov)$vectors[1, 2] * metalic.m[1], 
       b = eigen(metalic.cov)$vectors[2, 2] / eigen(metalic.cov)$vectors[1, 2], 
       lty = 2, col = 'red', lwd = 2)



