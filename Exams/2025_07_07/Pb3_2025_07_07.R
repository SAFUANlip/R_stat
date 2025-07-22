library(fda)

# The file temperature.txt contains hourly temperature measurements (in degrees Celsius) recorded in Milan
# throughout the year 2023. Each row corresponds to one of the 365 days, and each column represents one of
# the 25 hourly time points from 00:00 to 24:00 (h0 to h24). Additionally,
# each day is labeled according to the season
# (Winter, Spring, Summer, or Autumn) through the variable season.
# We take a functional data analysis perspective, treating the hourly temperature profiles as discretized evaluations
# of smooth functions defined over the domain [0, 24].

# a) Apply penalized smoothing to the temperature data using a basis of quadratic B-splines, with knots placed at
# each observed time point (i.e., every hour). Penalize the first-order derivative and use a smoothing parameter
# of ω = 1. Report the number of splines used and the mean generalized cross-validation (GCV) error. Provide
# a plot of the smoothed temperature curves. What is the approximate dimension of the space in which the
# smoothed curves live?

temperature <- read.table("temperature.txt",h=TRUE)
season <-temperature$season
data <- t(temperature[, 1:25])

help(create.bspline.basis)

m <- 3 # "using a basis of QUADRATIC B-splines", then we have to use norder = 4

nrow(data) # 25 points 

abscissa <- seq(0,24,1)

basis <- create.bspline.basis(rangeval=c(0,24), norder=m, breaks=abscissa)

functionalPar <- fdPar(fdobj=basis, Lfdobj=1, lambda=1) # Lfdobj=1  penalise second derivative ? not sure, maybe m-1?

Xobs0 <- data
Xss <- smooth.basis(abscissa, Xobs0, functionalPar)

Xss0 <- eval.fd(abscissa, Xss$fd, Lfd=0) # zero derivative (fit to data)
Xss1 <- eval.fd(abscissa, Xss$fd, Lfd=1) # first derivative (fit to first derivative)
Xss2 <- eval.fd(abscissa, Xss$fd, Lfd=2)

matplot(data, type='l',main='Temperature source data', xlab='Hours', ylab='365 days')
plot(Xss$fd, main='Smoothed data', xlab='Hours', ylab='365 days') # expected picture (I guess)


matplot(Xss0, type='l',main='Fit to data', xlab='Hours', ylab='365 days')
# plot.fd - does't work here!!!

# matplot(Xss1, type='l',main='Fit to data 1-st derivative', xlab='Hours', ylab='365 days')
# matplot(Xss2, type='l',main='Fit to data 2-nd derivative', xlab='Hours', ylab='365 days')


# Number of splines
basis$nbasis # 26

# GCV
gcv <- Xss$gcv
mean(gcv) # 1.174517

# Approximate dimension of the space in which the smoothed curves live
df <- Xss$df   #  number of parameters (so dimension of space)
df  # 9.833603 ~ 10

# b) Conduct a functional principal component analysis (FPCA) on the smoothed functions. What proportion of the
# total variance is explained by the second principal component? From a dimensionality reduction perspective,
# how many principal components would you retain? Justify your choice.

pca_W <- pca.fd(Xss$fd, nharm=5, centerfns=TRUE)

plot(pca_W$values[1:26],xlab='j',ylab='Eigenvalues')
plot(cumsum(pca_W$values)[1:26]/sum(pca_W$values), xlab='j', ylab='CPV', ylim=c(0.8,1))

# we have 26 data, (features - nbasis), so we can not plot more principal components
pca_W$values
cumsum(pca_W$values)[1:26]/sum(pca_W$values) # we have 13 PC, because we used basis of 13 function
# I would take first 2 components, because they are explains more than 98% of variance 
# and PC2 is elbow of explained variance

pca_W$values[2]/sum(pca_W$values) # 0.04872728 ~ 0.049
# second PC exaplins 0.9820061 - 0.9332788 = 0.04872728 part of variance (took from cumsum)


# c) Provide a plot showing the effect of the second principal component.
# plot of the FPCs as perturbation of the mean
media <- mean.fd(Xss$fd)

plot(media,lwd=2,ylim=c(0,60),ylab='temperature',main='FPC1')
lines(media+pca_W$harmonics[1,]*sqrt(pca_W$values[1]), col=2)
lines(media-pca_W$harmonics[1,]*sqrt(pca_W$values[1]), col=3)

plot(media,lwd=2,ylim=c(0,60),ylab='temperature',main='FPC2')
lines(media+pca_W$harmonics[2,]*sqrt(pca_W$values[2]), col=2)
lines(media-pca_W$harmonics[2,]*sqrt(pca_W$values[2]), col=3)
# temperate climate or not

# USE THIS TO EXPLAIN COMPONENTS
# Command of the library fda that automatically does these plots
par(mfrow=c(1,2))
plot(pca_W, nx=100, pointplot=TRUE, harm=c(1,2), expand=0, cycle=FALSE)
# second component more shows difference of temperature in first and second half of the day

# d) Is the representation given by the first principal component satisfying for distinguishing seasons? Support your
# answer with a plot.

scores <- pca_W$scores
plot(scores[, 1], scores[, 2], col = as.factor(season),
     pch = 19, xlab = "PC1", ylab = "PC2", main = "Scores of First Two Principal Components")
legend("topright", legend = c("Autumn", "Spring", "Summer", "Winter"), col = as.factor(levels(as.factor(season))), pch = 19)

plot(scores[, 1] ~ as.factor(season)) # by PC1 we see difference among Summer, Winter and Autumn wiht Spring
plot(scores[, 2] ~ as.factor(season)) # by PC2 it's difficlut to ifnd difference among seasons

# e) Could we successfully classify seasons based on the representation given by the second principal component?
# Justify your answer.

# No we can not sucesssfully classify seasons based on PC2 because there are almost no differnece among seasons in that PC
# we can see in from plot of PC1-PC2 or from Plot of PC2
