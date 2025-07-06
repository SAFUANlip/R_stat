###-------------------------------------------###
### Lab 2: Principal Component Analysis (PCA) ###
###-------------------------------------------###

# Password: KLIbPAaVo7QbN66

# Load necessary libraries
library(mvtnorm) # to deal with multivariate normal distributions
library(car) # "Companion to Applied Regression" for regression analysis

# Empirical search of the direction with maximum variance -------------------------------------

## Simulate data from a multivariate normal ---------------------------------------------------
mu  <-  c(1, 2) # mean
sig <-  cbind(c(1, 1), c(1, 4)) # covariance matrix
n   <-  100 # number of observations

set.seed(05032024) # Fix the random state to have reproducible results
X <- rmvnorm(n, mu, sig) # sample n observations from N(mu, sig)

# Plot the data
plot(X, asp = 1, xlab = 'Var 1', ylab = 'Var 2', pch = 19, xlim = c(-6, 6), ylim = c(-5, 10))

# Plot the sample mean
points(colMeans(X)[1], colMeans(X)[2], col = 'red', pch = 19, lwd = 3)



## Compute the variance along specific directions ---------------------------------------------
# Compute and plot variance of the projection of the data along the x-axis
abline(h = colMeans(X)[2], lty = 2, col = 'grey')
points(X[, 1], rep(colMeans(X)[2], n), col = 'red')
var(X[, 1])
# Compute and plot variance of the projection of the data along the y-axis
abline(v = colMeans(X)[1], lty = 2, col = 'grey')
points(rep(colMeans(X)[1], n), X[, 2], col = 'red')
var(X[, 2])

# Example: along the direction with angle pi / 6
theta <- pi / 6

# Plot the line corresponding to pi / 6
abline(a = colMeans(X)[2] - tan(theta) * colMeans(X)[1], 
       b = tan(theta), lty = 2)

# Compute unit vector a in direction theta[30]
a <- c(cos(theta), sin(theta))

# Project data onto direction defined by vector a
proj30 <- a %*% (t(X) - colMeans(X))

# Plot the projected points
points(colMeans(X)[1] + cos(theta) * proj30, 
       colMeans(X)[2] + sin(theta) * proj30, col = 'red')

# Compute variance along the direction
var(X %*% a)


## Compute variance along all directions ------------------------------------------------------
theta <- seq(0, pi, by = 2 * pi / 360)
Var <- NULL

for (i in 1:length(theta)) {
  a <- c(cos(theta[i]), sin(theta[i]))  # unit vector in direction theta[i]
  v <- var(X %*% a)  # sample variance of the projection of X along direction defined by vector a
  Var <- c(Var, v)
}

# Compute min and max variance
max.var <- max(Var)
max.theta <- theta[which.max(Var)]
max.a <- c(cos(max.theta), sin(max.theta))

min.var <- min(Var)
min.theta <- theta[which.min(Var)]
min.a <- c(cos(min.theta), sin(min.theta))

# Graphical representation
par(mfrow = c(1, 2))
plot(X, asp = 1, xlab = 'Var 1', ylab = 'Var 2', pch = 20)

abline(a = colMeans(X)[2] - tan(max.theta) * colMeans(X)[1], b = tan(max.theta),
       lty = 4, col = 'navyblue', lwd = 2)
abline(a = colMeans(X)[2] - tan(min.theta) * colMeans(X)[1], b = tan(min.theta),
       lty = 4, col = 'blue', lwd = 2)

plot(theta, Var, type = 'l', col = 'dark grey', lwd = 2, ylab = 'Variance')
points(max.theta, max.var, pch = 16, col = 'navyblue')
points(min.theta, min.var, pch = 16, col = 'blue')
par(mfrow = c(1, 1))


## Confrontation with the theory --------------------------------------------------------------
# Compute sample covariance matrix
M <- colMeans(X)
S <- cov(X)

# Compute eigenvectors and eigenvalues
eigen(S) # eigen(S)$vectors returns a matrix whose columns are the eigenvectors of S

# Compare with empirically found values and directions of max/min variability
max.var
min.var
max.a
min.a

# Plot directions of max/min variability
par(mfrow = c(1, 2))
plot(X, asp = 1, xlab = 'Var 1', ylab = 'Var 2', pch = 20)

ellipse(M, S, 1, add = TRUE, lwd = 3, col = 'red')

abline(a = M[2] - eigen(S)$vectors[2, 1] / eigen(S)$vectors[1, 1] * M[1], 
       b = eigen(S)$vectors[2, 1] / eigen(S)$vectors[1, 1], 
       lty = 2, col = 'dark red', lwd = 2)
abline(a = M[2] - eigen(S)$vectors[2, 2] / eigen(S)$vectors[1, 2] * M[1], 
       b = eigen(S)$vectors[2, 2] / eigen(S)$vectors[1, 2], 
       lty = 2, col = 'red', lwd = 2)
abline(a = M[2] - tan(max.theta) * M[1], b = tan(max.theta), 
       lty = 4, col = 'navyblue', lwd = 2)
abline(a = M[2] - tan(min.theta) * M[1], b = tan(min.theta), 
       lty = 4, col = 'blue', lwd = 2)

plot(theta, Var, type = 'l', col = 'dark grey', lwd = 2, ylab = 'Variance')
points(max.theta, max.var, pch = 20, col = 'navyblue')
points(min.theta, min.var, pch = 20, col = 'blue')
points(atan(eigen(S)$vector[2, 1] / eigen(S)$vector[1, 1]), max.var, pch = 3, col = 'dark red')
points(atan(eigen(S)$vector[2, 2] / eigen(S)$vector[1, 2]) + pi, min.var, pch = 3, col = 'red')

par(mfrow = c(1, 1))



# 3-dimensional geometrical interpretation of the principal components ------------------------


library(rgl)
options(rgl.printRglwidget = TRUE)

# theoretical mean and covariance matrix of the model
mu  <- c(0, 2, 3)
sig <- rbind(c(9, 1, 1), c(1, 4, 1), c(1, 1, 1))
nobs <- 100

X <- rmvnorm(nobs, mu, sig)

# sample mean and covariance matrix
M <- colMeans(X)
M
S <- cov(X)
S

points3d(X, asp=1, size=4)  # plot the points
axes3d()                    # add the axes
plot3d(ellipse3d(S, centre=M, level= 9/10), alpha=0.15, add=TRUE) # add the ellipsoid

# Principal components
PC <- princomp(X)
summary(PC)

# "0" principal component: the best approximation of dimension 0 (a point)
open3d()
points3d(X, asp=1, size=4)
axes3d()

points3d(t(M), col='red', size=6)

for(i in 1:100)
  lines3d(rbind(X[i,], M))

## Space spanned by the first PC --------------------------------------------------------------
# I principal component: the best approximation of dimension 1 (a line)
open3d()
points3d(X, asp=1, size=4)
axes3d()

PC1 <- NULL
for(i in 1:nobs) {
  PC1 <- rbind(PC1, PC$loadings[,1]*PC$scores[i,1] + M)
}
points3d(PC1, col='red', size=6)

for(i in 1:nobs) {
  lines3d(rbind(X[i,], PC1[i,]), col='blue')
}

lines3d(rbind(M + 2*PC$sdev[1] * PC$loadings[,1], M - 2*PC$sdev[1] * PC$loadings[,1]),
        col='forestgreen',lwd=2)

## Space spanned by the first two PCs ----------------------------------------------------------
# I and II principal components: the best approximation of dimension 2 (a plane)
open3d()
points3d(X, asp=1, size=4)
axes3d()

PC12 <- NULL
for(i in 1:nobs) {
  PC12 <- rbind(PC12, PC$loadings[,1]*PC$scores[i,1] + PC$loadings[,2]*PC$scores[i,2] + M)
}
points3d(PC12, col='red', size=6)

for(i in 1:nobs) {
  lines3d(rbind(X[i,], PC12[i,]),col='blue')
}

lines3d(rbind(M + 2*PC$sdev[1] * PC$loadings[,1], M - 2*PC$sdev[1] * PC$loadings[,1]),
        col='forestgreen',lwd=2) 
lines3d(rbind(M + 2*PC$sdev[2] * PC$loadings[,2], M - 2*PC$sdev[2] * PC$loadings[,2]),
        col='forestgreen',lwd=2)


## Space spanned by all PCs -------------------------------------------------------------------
# I, II and III principal components: the best approximation of dimension 3 (the entire space)
open3d()
points3d(X, asp=1, size=4)
axes3d()

PC123 <- NULL
for(i in 1:nobs) {
  PC123 <- rbind(PC123, PC$loadings[,1]*PC$scores[i,1] + PC$loadings[,2]*PC$scores[i,2]
                 + PC$loadings[,3]*PC$scores[i,3] + M)
}
points3d(PC123, col='red', size=6)

for(i in 1:nobs) {
  lines3d(rbind(X[i,], PC123[i,]),col='blue')
}

lines3d(rbind(M + 2*PC$sdev[1] * PC$loadings[,1], M - 2*PC$sdev[1] * PC$loadings[,1]),
        col='forestgreen',lwd=2) 
lines3d(rbind(M + 2*PC$sdev[2] * PC$loadings[,2], M - 2*PC$sdev[2] * PC$loadings[,2]),
        col='forestgreen',lwd=2) 
lines3d(rbind(M + 2*PC$sdev[3] * PC$loadings[,3], M - 2*PC$sdev[3] * PC$loadings[,3]),
        col='forestgreen',lwd=2)


# PCA on the Tourists dataset -----------------------------------------------------------------


# The dataset "tourists.txt" collects the data on the flow of Italian tourism
# from outside Lombardy to Milan for the year 2015. Each statistical unit 
# corresponds to a Region of origin and a month of observation. For each unit,
# the tourist flow is quantified through the number of nights spent by clients
# in: '5 stars hotels', '4 stars hotels', '3 stars hotels', '2 stars hotels', 
# '1 star hotels', 'residences', 'B&B' and 'rented flats'.

tourists <- read.table('tourists.txt', header=T)
head(tourists)
dim(tourists)

tourists.label <- tourists[, 1:2]
tourists <- tourists[, -(1:2)]

n <- dim(tourists)[1]
p <- dim(tourists)[2]

# Visualize the original data using boxplots
# We observe that the variability of the number of nights in 3, 4-star hotels, and residences
# is higher than that of the others. This may influence the PCA.
boxplot(tourists, las = 2, col = 'gold')

# Note: PCA is not about the mean, it is about the variability
boxplot(scale(x = tourists, center = T, scale = F), las = 2, col = 'gold')


## PCA on the original data -------------------------------------------------------------------
pc.tourists <- princomp(tourists, scores = T)
pc.tourists
summary(pc.tourists)

# To obtain the rows of the summary:
# Standard deviation of the components
pc.tourists$sd
# Proportion of variance explained by each PC
pc.tourists$sd^2 / sum(pc.tourists$sd^2)
# Cumulative proportion of explained variance
cumsum(pc.tourists$sd^2) / sum(pc.tourists$sd^2)

# Get loadings (coefficients of the linear combination of the original variables that defines each
# principal component)
load.tour <- pc.tourists$loadings
load.tour
load.tour[, 1:8]

# graphical representation of the loadings of the first three principal components
par(mfrow = c(3,1))
for(i in 1:3) barplot(load.tour[,i], ylim = c(-1, 1))

# Interpretation of the loadings:
# First PC: weighted average of the number of nights in 3,4 stars hotel and residences
# Second PC: contrast between the number of nights in 3 and 4 stars hotel
# Third PC: residences

# The loadings reflect the previous observation: the first 3 PCs are (yea, look again on boxplots)
# driven by the variables displaying the highest variability

layout(matrix(c(2, 3, 1, 3), 2, byrow = TRUE))

# Variance explained by the principal components
plot(pc.tourists, las = 2, main = 'Principal components', ylim = c(0, 4.5e7))

# Variances of original variables
barplot(sapply(tourists, sd)^2, las = 2, main = 'Original Variables', ylim = c(0, 4.5e7),
        ylab = 'Variances')

# Plot contribution to the total variance by number of components
plot(cumsum(pc.tourists$sd^2) / sum(pc.tourists$sd^2), type = 'b', axes = FALSE,
     xlab = 'Number of components', ylab = 'Contribution to the total variance', ylim = c(0, 1))
abline(h = 1, col = 'blue')
abline(h = 0.8, lty = 2, col = 'blue')
box()
axis(2, at = 0:10/10, labels = 0:10/10)
axis(1, at = 1:ncol(tourists), labels = 1:ncol(tourists), las = 2)

# The first PC explains more than 98% of the total variability. 
# This is due to the masking effect of those 3 variables over the others

## PCA on the standardized variables ----------------------------------------------------------
tourists.sd <- scale(tourists)
tourists.sd <- data.frame(tourists.sd)

head(tourists.sd)

sapply(tourists.sd, mean)

# Boxplot
par(mfrow = c(1, 1))
boxplot(tourists.sd, las = 2, col = 'gold')

pc.tourists <- princomp(tourists.sd, scores = T)
pc.tourists
summary(pc.tourists)

layout(matrix(c(2, 3, 1, 3), 2, byrow = TRUE))

# Variance explained by the principal components
plot(pc.tourists, las = 2, main = 'Principal Components', ylim = c(0, 7))
abline(h = 1, col = 'blue')

# Variances of original variables
barplot(sapply(tourists.sd, sd)^2, las = 2, main = 'Original Variables', ylim = c(0, 7),
        ylab = 'Variances')

# Plot contribution to the total variance by number of components
plot(cumsum(pc.tourists$sde^2) / sum(pc.tourists$sde^2), type = 'b', axes = FALSE, 
     xlab = 'Number of components', ylab = 'Contribution to the total variance', ylim = c(0, 1))
abline(h = 1, col = 'blue')
abline(h = 0.8, lty = 2, col = 'blue')
box()
axis(2, at = 0:10/10, labels = 0:10/10)
axis(1, at = 1:ncol(tourists.sd), labels = 1:ncol(tourists.sd), las = 2)

# If we wanted to perform dimensionality reduction, we could keep 1 or 2 PCs

# Loadings
load.tour <- pc.tourists$loadings
load.tour

# Graphical representation
par(mar = c(2,2,2,1), mfrow=c(3,1))
for(i in 1:3)barplot(load.tour[,i], ylim = c(-1, 1), main=paste('Loadings PC ',i,sep=''))

# Interpretation of the loadings:
# In this case, the first PC represents an average of the number of nights spent in 
# all the types of hotels and residences, taken with very similar weights.
# The second PC contrasts the more expensive solutions (4,5 stars hotels and residences)
# against the cheap solutions (1,2 stars hotels and B&B)

# High PC1: general high flow of tourists
# Low PC1: general low flow of tourists 
# High PC2: high flow for expensive solutions, low flow for cheap solutions
# Low PC2: low flow for expensive solutions, high flow for cheap solutions


## Scores -------------------------------------------------------------------------------------
scores.tourists <- pc.tourists$scores
scores.tourists

# Scatter plot of the scores
par(mfrow = c(1,1))
plot(scores.tourists[, 1:2])
abline(h=0, v=0, lty=2, col='grey')

# loadings - components of vector to represent (coefficients, that used in linear combination of source data to represents PCA data)
# scores - coodrinates of data points of obesrvations on principal component space (projections) 


loadings_matrix <- as.matrix(pc.tourists$loadings)
first_observation <- tourists.sd[1, , drop = FALSE]  # Убеждаемся, что это матрица
first_scores <- as.matrix(first_observation) %*% loadings_matrix  

# same as we get before
scores.tourists[1,]
first_scores

# Biplot
par(mfrow = c(1, 1))
biplot(pc.tourists)

# Let's use the categorical variables to further interpret the results
head(tourists.label)

# Color according to Month
# We order the labels according to time order
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct",
            "Nov", "Dec")
tourists.label[, 1] <- factor(tourists.label[, 1], levels = months)

# Define a cyclic palette
colors <- c("#2D1857", "#4D2C8C", "#6C47C7", "#8B63FF", "#7A8CDD", "#68B5C5",
            "#5ECABA", "#46AF89", "#108963", "#047138", "#025A26", "#013814")

col.lab1 <- rep(NA, n)
for(i in 1:n)
  col.lab1[i] <- colors[which(tourists.label[i, 1] == levels(tourists.label[, 1]))]

plot(scores.tourists[, 1:2], col = col.lab1, pch = 19, xlim = c(-8, 25), ylim = c(-3, 3.2))
abline(h = -3, v = -8, col = 1)
points(scores.tourists[, 1], rep(-3, n), col = col.lab1, pch = 19)
points(rep(-8, n), scores.tourists[, 2], col = col.lab1, pch = 19)
abline(h = 0, v = 0, lty = 2, col = 'grey')
legend('topright', levels(tourists.label[, 1]), fill = colors, bty = 'n')

# Months of Expo 2015: May to Oct
expo.label <- factor(ifelse(tourists.label[, 1] %in% c("May", "Jun", "Jul", "Aug", "Sept", "Oct"),
                            'Expo', 'Non Expo'))
col.expo <- ifelse(tourists.label[, 1] %in% c("May", "Jun", "Jul", "Aug", "Sept", "Oct"),
                   'red', 'blue')

layout(cbind(c(2, 4), c(1, 3)), widths = c(1, 4), heights = c(4, 1))
par(mar = rep(3, 4))
plot(scores.tourists[, 1:2], col = col.expo, pch = 19, xlim = c(-8, 20), ylim = c(-3, 3.2), las = 2)
abline(h = -3, v = -8, col = 1)
points(scores.tourists[, 1], rep(-3, n), col = col.expo, pch = 19)
points(rep(-8, n), scores.tourists[, 2], col = col.expo, pch = 19)
abline(h = 0, v = 0, lty = 2, col = 'grey')
boxplot(scores.tourists[, 2] ~ expo.label, col = c('red', 'blue'), ylim = c(-3, 3.2), las = 2)
boxplot(scores.tourists[, 1] ~ expo.label, col = c('red', 'blue'), ylim = c(-8, 20), horizontal = T,
        las = 2)

# Color according to Region of Origin
col.lab2 <- rainbow(length(unique(tourists.label[, 2])))
par(mfrow = c(1, 1))
plot(
  scores.tourists[, 1:2],
  col = col.lab2,
  pch = 19,
  xlim = c(-8, 30),
  ylim = c(-3, 3.2)
)
abline(h = -3, v = -8, col = 1)
points(scores.tourists[, 1], rep(-3, n), col = col.lab2, pch = 19)
points(rep(-8, n), scores.tourists[, 2], col = col.lab2, pch = 19)
abline(h = 0, v = 0, lty = 2, col = 'grey')
legend('topright', levels(factor(tourists.label[, 2])), fill = col.lab2, bty = 'n')


graphics.off()

##### Homework: try to perform the PCA on the log-transformed data -------------
tourists.mod = tourists
for(i in 1:8) tourists.mod[which(tourists[,i]==0),i] = 1
tourists.log = log(tourists.mod)
head(tourists.log)

pc.tourists_log <- princomp(tourists.log, scores = T)
pc.tourists_log
summary(pc.tourists_log)

# To obtain the rows of the summary:
# Standard deviation of the components
pc.tourists_log$sd
# Proportion of variance explained by each PC
pc.tourists_log$sd^2 / sum(pc.tourists_log$sd^2)
# Cumulative proportion of explained variance
cumsum(pc.tourists_log$sd^2) / sum(pc.tourists_log$sd^2)

# Get loadings (coefficients of the linear combination of the original variables that defines each
# principal component)
load.tour_log <- pc.tourists_log$loadings
load.tour_log
load.tour_log[, 1:8]

# graphical representation of the loadings of the first three principal components
par(mfrow = c(3,1))
for(i in 1:3) barplot(load.tour_log[,i], ylim = c(-1, 1))


# Here we may notice that frst component based on avergae of source variabels 
# (probably it means overall number of visited tourists all type of hotels)
# Second component based on contrast of hotels providing breakfast in bed
# and Residence
# third componets - contrasst of cheapest hotels (star 1) and most expensive 
# ones (star 5)


# PCA on the Food dataset ---------------------------------------------------------------------
food <- read.table('Food.txt', header = TRUE)
head(food)
dim(food)

n <- dim(food)[1]
p <- dim(food)[2]

# Exploration
boxplot(food, col = 'gold')
boxplot(scale(x = food, center = TRUE, scale = FALSE), col = 'gold')

# Principal component analysis of the dataset 'food', based on the correlation matrix
food.sd <- scale(food)
food.sd <- data.frame(food.sd)

head(food.sd)
sapply(food.sd, mean)
sapply(food.sd, sd)
cov(food.sd)
cor(food)

# PC on correlation matrix
pc.food <- princomp(food.sd, scores=T)
pc.food
summary(pc.food)

# Explained variance
layout(matrix(c(2, 3, 1, 3), 2, byrow = TRUE))
barplot(pc.food$sdev^2, las = 2, main = 'Principal Components', ylim = c(0, 4), ylab = 'Variances')
abline(h = 1, col = 'blue')
barplot(sapply(food.sd, sd)^2, las = 2, main = 'Original Variables', ylim = c(0, 4),
        ylab = 'Variances')
plot(cumsum(pc.food$sdev^2) / sum(pc.food$sde^2), type = 'b', axes = FALSE,
     xlab = 'Number of Components', ylab = 'Contribution to the Total Variance', ylim = c(0, 1))
box()
axis(2, at = 0:10/10, labels = 0:10/10)
axis(1, at = 1:ncol(food.sd), labels = 1:ncol(food.sd), las = 2)

# Scores
scores.food <- pc.food$scores

# Variability of the original variables / scores
layout(matrix(c(1, 2), 2))
boxplot(food.sd, las = 2, col = 'gold', main = 'Original Variables')
scores.food <- data.frame(scores.food)
boxplot(scores.food, las = 2, col = 'gold', main = 'Principal Components')

# Loadings
load.food <- pc.food$loadings
par(mar = c(1, 4, 0, 2), mfrow = c(4, 1))
for (i in 1:4) {
  barplot(load.food[, i], ylim = c(-1, 1))
}

# Let's plot only the most significant loadings
par(mar = c(1, 4, 0, 2), mfrow = c(4, 1))
for (i in 1:4) {
  barplot(ifelse(abs(load.food[, i]) < 0.3, 0, load.food[, i]),
          ylim = c(-1, 1))
  abline(h = 0)
}

# Scores
par(mfrow = c(1, 1))
plot(scores.food[, 1], scores.food[, 2], type = "n", xlab = "PC1", ylab = "PC2", asp = 1,
     xlim = c(-4, 3))
text(scores.food[, 1], scores.food[, 2], dimnames(food)[[1]], cex = 0.7)

biplot(pc.food)

# Projection on the space generated by the first k principal components
par(mfrow = c(2, 5))
matplot(t(food.sd), type = 'l', main = 'Data', ylim = range(food.sd))
meanF <- colMeans(food.sd)
matplot(meanF, type = 'l', main = 'First 0 PCs', lwd = 2, ylim = range(food.sd))
projection <- matrix(meanF, dim(food.sd)[[1]], dim(food.sd)[[2]], byrow = T)
for (i in 1:8) {
  projection <- projection + scores.food[, i] %*% t(load.food[, i])
  matplot(t(projection), type = 'l', main = paste('First', i, 'PCs'), ylim = range(food.sd))
  matplot(meanF, type = 'l', lwd = 2, add = T)
}


# Additional exercises -----------------------------------------------------------------------

##  Additional exercise 1 --------------------------------------------------------------------

# Question of a problem of a past exam #
## The file scotland.txt collects the number of residents in Scotland, according
## to the last census of 2001, divided by age and county. Assume the data 
## associated with different counties to be independent and identically distributed,
## and assume the data corresponding to different age ranges to be dependent.
## Perform a dimensionality reduction of the dataset through a principal component
## analysis and interpret the obtained components

age <- read.table('scotland.txt', header=T)
head(age)
dim(age)

quartz()
pairs(age, pch=19)
boxplot(age)
matplot(t(age), type='l', xlab='Age', ylab='Number of Residents', lty=1, col=rainbow(33), las=1)

S <- cov(age)
image(S, asp=1)

var.gen <- det(S)
var.tot <- sum( diag(S) )

# PCA (on the covariance matrix)
pc.age <- princomp(age, scores=T)
pc.age
summary(pc.age)

# Explained variance
quartz()
layout(matrix(c(2,3,1,3),2,byrow=T))
barplot(pc.age$sdev^2, las=2, main='Principal Components', ylab='Variances')
barplot(sapply(age,sd)^2, las=2, main='Original variables', ylab='Variances')
plot(cumsum(pc.age$sdev^2)/sum(pc.age$sde^2), type='b', axes=F, xlab='number of components', ylab='contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.8, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(age),labels=1:ncol(age),las=2)

# Scores
scores.age <- pc.age$scores
scores.age

layout(matrix(c(1,2),2))
boxplot(age, las=2, col='gold', main='Original variables')
scores.age <- data.frame(scores.age)
boxplot(scores.age, las=2, col='gold', main='Principal components')

load.age    <- pc.age$loadings
load.age

quartz()
par(mar = c(1,4,0,2), mfrow = c(3,1))
for(i in 1:3)barplot(load.age[,i], ylim = c(-1, 1))

quartz()
plot(scores.age[,1],scores.age[,2],type="n",xlab="pc1",ylab="pc2", asp=1)
text(scores.age[,1],scores.age[,2],dimnames(age)[[1]], cex=0.7)

biplot(pc.age)

# Projection on the space generated by the k-th principal component
x11(width=18, height=7)
par(mfrow=c(2,3))
#matplot(t(age), type='l', main = 'Data', ylim=range(age))
meanA <- colMeans(age)
matplot(meanA, type='l', main = '0 PC', lwd=2, ylim=range(age))
for(i in 1:5)
{
  projection <- matrix(meanA, dim(age)[[1]], dim(age)[[2]], byrow=T) + scores.age[,i] %*% t(load.age[,i])
  matplot(t(projection), type='l', main = paste(i, 'PC'), ylim=range(age))
  matplot(meanA, type='l', lwd=2, add=T)
}

# Projection on the space generated by the first k principal components
x11(width=18, height=7)
par(mfrow=c(2,3))
#matplot(t(age), type='l', main = 'Data', ylim=range(age))
meanA <- colMeans(age)
matplot(meanA, type='l', main = '0 PC', lwd=2, ylim=range(age))
projection <- matrix(meanA, dim(age)[[1]], dim(age)[[2]], byrow=T)
for(i in 1:5)
{
  projection <- projection + scores.age[,i] %*% t(load.age[,i])
  matplot(t(projection), type='l', main = paste('First', i, 'PCs'), ylim=range(age))
  matplot(meanA, type='l', lwd=2, add=T)
}

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

pollution <- read.table('NO.txt', header=T)
head(pollution)
dim(pollution)

quartz()
pairs(pollution, pch=19)
boxplot(pollution)
matplot(t(pollution), type='l', xlab='Control units', ylab='days', lty=1, col=rainbow(33), las=1)

S <- cov(pollution)
image(S, asp=1)

var.gen <- det(S)
var.tot <- sum( diag(S) )

pc.pollution <- princomp(pollution, scores=T)
pc.pollution
summary(pc.pollution)

pc.pollution$loading


# Additional material: Theoretical & sample PCA -----------------------------------------------

# Let's generate a sample of size 100 from a Gaussian
set.seed(14032022)
mu  <-  c(1,2)
sig <-  cbind(c(1,1), c(1,4))
n   <-  100
X <- rmvnorm(n, mu, sig)

M <- colMeans(X)
S <- cov(X)

x11(width=14, height=7)
par(mfrow=c(1,3))
plot(X, asp=1, xlab='Var 1', ylab='Var 2',pch=1)
ellipse(M, S, 1, add=T,lwd=3, col = 'red')
ellipse(mu, sig, 1, add=T,lwd=3, col='springgreen')

abline(a = M[2] - eigen(S)$vectors[2,1]/eigen(S)$vectors[1,1]*M[1], b = eigen(S)$vectors[2,1]/eigen(S)$vectors[1,1], lty = 2, col = 'red', lwd = 2)
abline(a = M[2] - eigen(S)$vectors[2,2]/eigen(S)$vectors[1,2]*M[1], b = eigen(S)$vectors[2,2]/eigen(S)$vectors[1,2], lty = 2, col = 'red', lwd = 2)
abline(a = mu[2] - eigen(sig)$vectors[2,1]/eigen(sig)$vectors[1,1]*mu[1], b = eigen(sig)$vectors[2,1]/eigen(sig)$vectors[1,1], lty = 2, col = 'springgreen', lwd = 2)
abline(a = mu[2] - eigen(sig)$vectors[2,2]/eigen(sig)$vectors[1,2]*mu[1], b = eigen(sig)$vectors[2,2]/eigen(sig)$vectors[1,2], lty = 2, col = 'springgreen', lwd = 2)

legend('topleft',c('True','Estimated'),col=c('springgreen','red'),lty=c(1,1),lwd=2)

### let's now increase n...
n <- 1000

X <- rmvnorm(n, mu, sig)
M <- colMeans(X)
S <- cov(X)

plot(X, asp=1, xlab='Var 1', ylab='Var 2',pch=1)
ellipse(M, S, 1, add=T,lwd=3, col = 'red')
ellipse(mu, sig, 1, add=T,lwd=3, col='springgreen')

abline(a = M[2] - eigen(S)$vectors[2,1]/eigen(S)$vectors[1,1]*M[1], b = eigen(S)$vectors[2,1]/eigen(S)$vectors[1,1], lty = 2, col = 'red', lwd = 2)
abline(a = M[2] - eigen(S)$vectors[2,2]/eigen(S)$vectors[1,2]*M[1], b = eigen(S)$vectors[2,2]/eigen(S)$vectors[1,2], lty = 2, col = 'red', lwd = 2)
abline(a = mu[2] - eigen(sig)$vectors[2,1]/eigen(sig)$vectors[1,1]*mu[1], b = eigen(sig)$vectors[2,1]/eigen(sig)$vectors[1,1], lty = 2, col = 'springgreen', lwd = 2)
abline(a = mu[2] - eigen(sig)$vectors[2,2]/eigen(sig)$vectors[1,2]*mu[1], b = eigen(sig)$vectors[2,2]/eigen(sig)$vectors[1,2], lty = 2, col = 'springgreen', lwd = 2)

legend('topleft',c('True','Estimated'),col=c('springgreen','red'),lty=c(1,1),lwd=2)

###
n <- 5000

X <- rmvnorm(n, mu, sig)
M <- colMeans(X)
S <- cov(X)

plot(X, asp=1, xlab='Var 1', ylab='Var 2',pch=1)
ellipse(M, S, 1, add=T,lwd=3, col = 'red')
ellipse(mu, sig, 1, add=T,lwd=3, col='springgreen')

abline(a = M[2] - eigen(S)$vectors[2,1]/eigen(S)$vectors[1,1]*M[1], b = eigen(S)$vectors[2,1]/eigen(S)$vectors[1,1], lty = 2, col = 'red', lwd = 2)
abline(a = M[2] - eigen(S)$vectors[2,2]/eigen(S)$vectors[1,2]*M[1], b = eigen(S)$vectors[2,2]/eigen(S)$vectors[1,2], lty = 2, col = 'red', lwd = 2)
abline(a = mu[2] - eigen(sig)$vectors[2,1]/eigen(sig)$vectors[1,1]*mu[1], b = eigen(sig)$vectors[2,1]/eigen(sig)$vectors[1,1], lty = 2, col = 'springgreen', lwd = 2)
abline(a = mu[2] - eigen(sig)$vectors[2,2]/eigen(sig)$vectors[1,2]*mu[1], b = eigen(sig)$vectors[2,2]/eigen(sig)$vectors[1,2], lty = 2, col = 'springgreen', lwd = 2)

legend('topleft',c('True','Estimated'),col=c('springgreen','red'),lty=c(1,1),lwd=2)

