library(mvtnorm) # to deal with multivariate normal distributions
library(car) # "Companion to Applied Regression" for regression analysis
library(pheatmap) 


gemstones <- read.table("2023_11_07/gemstones.txt", h=TRUE)
str(gemstones)
gemstones_data <- gemstones[, 1:8]

n <- dim(gemstones_data)[1]
p <- dim(gemstones_data)[2]

# a) Conduct a Principal Component Analysis (PCA) of the dataset, focusing on the quantitative variables only.
# Determine whether it is more appropriate to use the original variables or the standardized ones and proceed
# accordingly.

boxplot(gemstones_data) # need to standartise data

gemstones_data.sd <- scale(gemstones_data, center=FALSE, scale=TRUE)
boxplot(gemstones_data.sd)

pc.gemstones_data.sd <- princomp(gemstones_data.sd, scores=T)
pc.gemstones_data.sd
summary(pc.gemstones_data.sd)

# b) Report a plot of the loadings of the first two principal components and provide an interpretation. Report the
# scatter plot of the data along the first two principal components. Considering the categorical variable variety,
# interpret the results.

n_lod_show <- 2
load.gemstones_data.sd <- pc.gemstones_data.sd$loadings
par(mar = c(1, n_lod_show, 0, 2), mfrow = c(n_lod_show, 1))
for (i in 1:n_lod_show) {
  barplot(load.gemstones_data.sd[, i], ylim = c(-1, 1))
}
# first components represents general effect of variabels
# while second component represents contrast of:
# MinorAxisLEngth, MajorAxisLength, Eccentricity 

cols <- as.factor(gemstones$Type)

plot(pc.gemstones_data.sd$scores[,1], pc.gemstones_data.sd$scores[,2],
     col = cols, pch = 19, xlab = "PC1", ylab = "PC2", main = "PCA of Asteroids")

text(pc.gemstones_data.sd$scores[,1], pc.gemstones_data.sd$scores[,2],
     labels = gemstones$Type, pos = 3, cex = 0.7)

# c) For gemstones of the ruby variety, construct a 95% confidence region for the mean of the vector whose
# components are the first two principal components. Describe this region by providing its center coordinates,
# axis directions, and semi-axes lengths. Provide a plot of the region. Introduce and assess the hypothesis of
# normality, which will here be tested at the 1% significance level..

rubby_data <- data.frame(
  pc1=pc.gemstones_data.sd$scores[,1][gemstones$Type == "ruby"],
  pc2=pc.gemstones_data.sd$scores[,2][gemstones$Type == "ruby"]
)

mvn(rubby_data) # 1% significance level => if p-value > 0.01 => normal

rubby.m <- sapply(rubby_data, mean)
rubby.m
# 0.06460798 -0.07997023 

rubby.cov <- cov(rubby_data)
rubby.cov
#.       pc1         pc2
# pc1 0.009472973 0.001570472
# pc2 0.001570472 0.007305535

# Directions and axis length ---------------------------------------------------
n_rubby <- sum(gemstones$Type == "ruby")
p <- 2
alpha <- 0.05

cfr.fisher <- ((n_rubby - 1) * p / (n_rubby - p)) * qf(1 - alpha, p, n_rubby - p)
cfr.fisher

eig <- eigen(rubby.cov / n_rubby)
directions <- eig$vectors      # направления (ось эллипса)
lengths <- sqrt(eig$values * cfr.fisher)  # длины полуосей

# Результаты:
directions  # Each column — direction of axis 
#         [,1]       [,2]
# [1,] -0.8854260  0.4647804
# [2,] -0.4647804 -0.8854260

lengths     # длины полуосей (вдоль направлений) 
# 0.03662815 0.02905886

# Ellipsoidal confidence region with confidence level 95%
plot(rubby_data, asp=1, pch=1, main='Metallic data')
# plottin ellipse, don't forget to divide cov/n
ellipse(center=rubby.m, shape=rubby.cov/n_rubby, radius=sqrt(cfr.fisher), lwd=2)

arrows(rubby.m[1], rubby.m[2],
       rubby.m[1] + lengths[1]*directions[1,1],
       rubby.m[2] + lengths[1]*directions[2,1],
       col="red", lwd=2)

arrows(rubby.m[1], rubby.m[2],
       rubby.m[1] + lengths[2]*directions[1,2],
       rubby.m[2] + lengths[2]*directions[2,2],
       col="darkgreen", lwd=2)
