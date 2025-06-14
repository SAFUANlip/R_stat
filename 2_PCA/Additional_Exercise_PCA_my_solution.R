library(mvtnorm) # to deal with multivariate normal distributions
library(car) # "Companion to Applied Regression" for regression analysis
library(pheatmap) # pheatmap

##  Additional exercise 1 --------------------------------------------------------------------

# Question of a problem of a past exam #
## The file scotland.txt collects the number of residents in Scotland, according
## to the last census of 2001, divided by age and county. Assume the data 
## associated with different counties to be independent and identically distributed,
## and assume the data corresponding to different age ranges to be dependent.
## Perform a dimensionality reduction of the dataset through a principal component
## analysis and interpret the obtained components

scotland <- read.table('scotland.txt', header = TRUE)

n <- dim(scotland)[1]
p <- dim(scotland)[2]

# Exploration
boxplot(scotland, col = 'gold', main = 'Original Variables') 
# boxes has different sizes, so we need scale data, to have same effect from each
# source variable to principal components 

boxplot(scale(x = scotland, center = TRUE, scale = TRUE), col = 'gold')
scotland.sd <- scale(scotland,center=TRUE, scale=TRUE)
scotland.sd <- data.frame(scotland.sd)

boxplot(scotland.sd, col = 'gold', main = 'Standartised Variables')

sapply(scotland.sd, sd)
sapply(scotland.sd, mean)

pc.scotland.sd <- princomp(scotland.sd, scores=T)
pc.scotland.sd
summary(pc.scotland.sd)

# Explained variance
layout(matrix(c(2, 3, 1, 3), 2, byrow = TRUE))
barplot(pc.scotland.sd$sdev^2, las = 2, main = 'Principal Components', ylim = c(0, 4), ylab = 'Variances')
abline(h = 1, col = 'blue')
barplot(sapply(scotland.sd, sd)^2, las = 2, main = 'Original Variables', ylim = c(0, 4),
        ylab = 'Variances')
plot(cumsum(pc.scotland.sd$sdev^2) / sum(pc.scotland.sd$sde^2), type = 'b', axes = FALSE,
     xlab = 'Number of Components', ylab = 'Contribution to the Total Variance', ylim = c(0, 1))
box()
axis(2, at = 0:10/10, labels = 0:10/10)
axis(1, at = 1:ncol(scotland.sd), labels = 1:ncol(scotland.sd), las = 2)

# As we see from graphics and PCA summary first two componets explain more than 99% of variability
# Also it can be seen from scores box-plot, where first two components have highest variability

# Scores
scores.scotland.sd <- pc.scotland.sd$scores

# Variability of the original variables / scores
layout(matrix(c(1, 2), 2))
boxplot(scotland.sd, las = 2, col = 'gold', main = 'Original Variables')
scores.scotland.sd <- data.frame(scores.scotland.sd)
boxplot(scores.scotland.sd, las = 2, col = 'gold', main = 'Principal Components')

# Loadings
load.scotland.sd <- pc.scotland.sd$loadings
par(mar = c(1, 4, 0, 2), mfrow = c(4, 1))
for (i in 1:4) {
  barplot(load.scotland.sd[, i], ylim = c(-1, 1))
}
# From Loadings graphics, we may notice that first component presents average of all
# people of all ages
# Second components based on contrast of yang with mid age people (0-40) and old people (61 +)
# Third component represents contrast between residence of (41-60 years old) and (61-80) correspondingly 


# Let's plot only the most significant loadings
par(mar = c(1, 4, 0, 2), mfrow = c(4, 1))
for (i in 1:4) {
  barplot(ifelse(abs(load.scotland.sd[, i]) < 0.3, 0, load.scotland.sd[, i]),
          ylim = c(-1, 1))
  abline(h = 0)
}

# Scores
par(mfrow = c(1, 1))
plot(scores.scotland.sd[, 1], scores.scotland.sd[, 2], type = "n", xlab = "PC1", ylab = "PC2", asp = 1,
     xlim = c(-4, 3))
text(scores.scotland.sd[, 1], scores.scotland.sd[, 2], dimnames(scotland)[[1]], cex = 0.7)

biplot(pc.scotland.sd)
# Here we may notice where live younger people (Peeblesshire, Sutherland)
# And where live older people (Ayrshire, Selkirshire)

boxplot(scotland, col = 'gold', main = 'Original Variables') 

scotland[rownames(scotland) == "Sutherland", ]
scotland[rownames(scotland) == "Ayrshire", ]

# as we see, in Sutherland more yong people, and in Ayrshire more older ones

##  Additional exercise 2 --------------------------------------------------------------------

# Along the ringroads of Milan four control units measure the concentration of the 
# pollutant NO in the air.
# The measures collected during the last year are reported in the file NO.txt.
# Perform a principal component analysis of the available data. In particular:
# (a) Compute the loadings
# (b) Compute the variances along the PCs
# (c) Comment and interpret the results at points (a) and (b)
# (d) On the 3rd July the control unites registered the values (13, 10, 11, 13).
#     Compute the corresponding scores

NO <- read.table("NO.txt", header = TRUE)

n <- dim(NO)[1]
p <- dim(NO)[2]

boxplot(NO, col = 'gold', main = 'Original Variables') 

S <- cov(NO)

var.gen <- det(S)
var.tot <- sum( diag(S) )
S

pheatmap(
  mat = S,
  main = "Covariance Matrix",
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  display_numbers = TRUE,
  fontsize_number = 8,
  border_color = NA
)

# Exploration
boxplot(NO, col = 'gold', main = 'Original Variables') 

boxplot(scale(x = NO, center = TRUE, scale = FALSE), col = 'gold')
NO.sd <- scale(NO,center=TRUE, scale=FALSE)
NO.sd <- data.frame(NO.sd)

boxplot(NO.sd, col = 'gold', main = 'Standartised Variables')

pc.NO.sd <- princomp(NO.sd, scores=T)
pc.NO <- princomp(NO, scores=T)

summary(pc.NO)
summary(pc.NO.sd)

# a)
loadings.pc.NO.sd <- pc.NO.sd$loadings
scores.pc.NO.sd <- pc.NO.sd$scores
scores.pc.NO.sd 

# b)
variances.pc.sd <- pc.NO.sd$sdev^2
variances.pc.sd

# c)

# Explained variance
layout(matrix(c(2, 3, 1, 3), 2, byrow = TRUE))
barplot(pc.NO.sd$sdev^2, las = 2, main = 'Principal Components', ylim = c(0, 4), ylab = 'Variances')
abline(h = 1, col = 'blue')
barplot(sapply(NO.sd, sd)^2, las = 2, main = 'Original Variables', ylim = c(0, 4),
        ylab = 'Variances')
plot(cumsum(pc.NO.sd$sdev^2) / sum(pc.NO.sd$sde^2), type = 'b', axes = FALSE,
     xlab = 'Number of Components', ylab = 'Contribution to the Total Variance', ylim = c(0, 1))
box()
axis(2, at = 0:10/10, labels = 0:10/10)
axis(1, at = 1:ncol(NO.sd), labels = 1:ncol(NO.sd), las = 2)
abline(h = 0.8, v = 0, lty = 2, col = 'red')


# Scores
# Variability of the original variables / scores
layout(matrix(c(1, 2), 2))
boxplot(NO.sd, las = 2, col = 'gold', main = 'Original Variables')
scores.pc.NO.sd <- data.frame(scores.pc.NO.sd)
boxplot(scores.pc.NO.sd, las = 2, col = 'gold', main = 'Principal Components')
# scores still spread among all components, therefor first component take less than 50% of variability
# 

# Loadings

par(mar = c(1, 4, 0, 2), mfrow = c(4, 1))
for (i in 1:4) {
  barplot(loadings.pc.NO.sd[, i], ylim = c(-1, 1))
}
# From Loadings graphics, we may notice that first component presents average of first 3 variabels
# second component full explained by 4-th variable


# d) prediction
new_obs <- c(13, 10, 11, 13)
new_obs_df <- as.data.frame(t(new_obs))
colnames(new_obs_df) <- colnames(NO)

pc.NO$loadings[,]
pc.NO.sd$loadings[,]

# what happen if don't centralise input
t(as.matrix(new_obs)) %*% as.matrix(pc.NO$loadings)
t(as.matrix(new_obs)) %*% as.matrix(pc.NO.sd$loadings)

predict(pc.NO, new_obs_df) # model knowing, that data has not zero mean, centralise input by automatically 
predict(pc.NO.sd, new_obs_df-colMeans(NO)) # if we use pca, which trained on сentralised data - we have to centralise by ourself

scores.pc.NO.sd

# Check Total and Generalised Variance after PCA -------------------------------
# Загружаем данные
data(iris)
X <- iris[, 1:4]

boxplot(X, col = 'gold', main = 'Original Variables')

# Стандартизация данных
X_scaled <- scale(X)
cov(X_scaled)
boxplot(X_scaled, col = 'gold', main = 'Standartised Variabels')

# --- Исходная ковариационная матрица ---
cov_orig <- cov(X_scaled)
cov_orig
# --- Total Variance (сумма дисперсий) ---
total_var_orig <- sum(diag(cov_orig))
cat("Total variance (original):", total_var_orig, "\n")

# --- Generalized Variance (определитель) ---
gen_var_orig <- det(cov_orig)
cat("Generalized variance (original):", gen_var_orig, "\n")

# --- PCA через princomp ---
pca_model <- princomp(X_scaled, cor = FALSE, scores = TRUE)

# Преобразованные данные
X_pca <- pca_model$scores

# --- Ковариационная матрица после PCA ---
cov_pca <- cov(X_pca)

cov_pca
# --- Total Variance после PCA ---
total_var_pca <- sum(diag(cov_pca))
cat("Total variance (PCA):", total_var_pca, "\n")

# --- Generalized Variance после PCA ---
gen_var_pca <- det(cov_pca)
cat("Generalized variance (PCA):", gen_var_pca, "\n")

# what if cetralise 
# PCA без стандартизации
p1 <- princomp(X, cor = FALSE)

# PCA со стандартизацией
p2 <- princomp(X, cor = TRUE)

X_centred <- scale(X,scale = FALSE, center = TRUE)
boxplot(X_centred, col = 'gold', main = 'Centered Variabels')
p3 <- princomp(X_centred, cor = FALSE)

# Сравним долю объяснённой дисперсии
summary(p1)
summary(p2)
summary(p3)
