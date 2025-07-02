#FDA -----------------------
# robe fda
library(fda)
library(fields)
library(KernSmooth) # per locpoly in fda

# 1) Bpsline 12 basis 2 degree, PCA, plot of PC as perturbation of the mean, plot and comment day1 --------------
rm(list=ls())
graphics.off()
data <- read.table('wind.txt',header=T)
head(data)
matplot(t(data), type='l' , xlab ='x', ylab = 'y')
data <- t(data)
time <- 1:24 
basis <- create.bspline.basis(range(time),
                              nbasis=12, 3) # 4 è order, degree +1 || FOURIER SE CAMBIA
data_L.fd <- Data2fd(y=data, argvals= time , basisobj=basis) 
plot.fd(data_L.fd, main="B-splines")

data_L.fd
data_L.fd$coefs[1:3,1]
# bspl3.1  bspl3.2  bspl3.3 
# 17.04582 17.63498 31.07959 


#b) PCA

pca_W <- pca.fd(data_L.fd,nharm=5,centerfns=TRUE)
# scree plot

par(mfrow=c(1,1))
plot(pca_W$values[1:3],xlab='j',ylab='Eigenvalues')# 1:3 se vuoi per i primi 3, altrimenti togli 1:3
plot(cumsum(pca_W$values)/sum(pca_W$values),xlab='j',ylab='CPV',ylim=c(0.5,1)) 
abline(h=0.9, lty=2, col='blue')

# First three FPCs variance explained
cumsum(pca_W$values[1:3])/sum(pca_W$values)
# 0.9585892

#first three eigenfunctions plot
par(mfrow = c(1,3))
plot(pca_W.1$harmonics[1,],col=1,ylab='FPC1')
abline(h=0,lty=2)
plot(pca_W.1$harmonics[2,],col=2,ylab='FPC2')
plot(pca_W.1$harmonics[3,],col=3,ylab='FPC3')
#c) find a possible reduction and plot the new pc as perturbation of the mean and interpret
#keep first 2 pc


media <- mean.fd(data_L.fd)

par(mfrow=c(1,2))
plot(media,lwd=2,ylim=c(5,35),main='FPC1')
lines(media+pca_W$harmonics[1,]*sqrt(pca_W$values[1]), col=2)
lines(media-pca_W$harmonics[1,]*sqrt(pca_W$values[1]), col=3)
# first - weighted average with heavier weights in the central part of the day 

plot(media,lwd=2,ylim=c(5,35),main='FPC2')
lines(media+pca_W$harmonics[2,]*sqrt(pca_W$values[2]), col=2)
lines(media-pca_W$harmonics[2,]*sqrt(pca_W$values[2]), col=3)
# second - contrast between first 15 hour of the day and the rest of the evening  


# d) plot and comment characteristics of Day 1.

par(mfrow=c(1, 1))
plot(pca_W$scores[, 1], pca_W$scores[, 2], xlab="Scores FPC1", ylab="Scores FPC2", lwd=2)
points(pca_W$scores[1, 1], pca_W$scores[1, 2], col=3, lwd=4) # Day1

# Day1
# FCP1 positive: above average intensity throughout all day
# FCP2 positive: slighly above average in the first part of the day until 15 and then slighly below average

day1 <- t(as.matrix(data[1,]))
plot.fd(data.fd.1[1,])
points(time,day1)
lines(media,lwd=2)


#2) Bpsline basis curva unica, with general cv criterior for basis, plot approx pointwise CI, approximation of the first derivative   --------
rm(list=ls())
graphics.off()

# Load data
d <- read.table("tide.txt", header=T)
data_W <- d[,2]

# Define time range
time <- 1:48

# a) Smoothing using B-splines

library(fda) # Load necessary package

# Define spline order and degree
m <- 4
degree <- m - 1

# Choose the number of basis functions using GCV
nbasis <- 6:30
gcv <- numeric(length(nbasis))
for (i in 1:length(nbasis)) {
  basis <- create.bspline.basis(range(time), nbasis[i], m)
  gcv[i] <- smooth.basis(time, data_W, basis)$gcv
}
plot(nbasis, gcv, type='b', xlab='Number of Basis Functions', ylab='GCV')
optimal_nbasis <- nbasis[which.min(gcv)]
abline(v = optimal_nbasis, col = 2)

# Smoothing with optimal number of basis functions
nbasis <- optimal_nbasis
basis <- create.bspline.basis(rangeval=range(time), nbasis=nbasis, norder=m)
basismat <- eval.basis(time, basis)
est_coef <- lsfit(basismat, data_W, intercept=FALSE)$coef
Xsp0 <- basismat %*% est_coef

# Plot smoothed data
matplot(time, cbind(data_W, Xsp0), type='l', col=c("black", "blue"), lty=c(1,1), lwd=c(1,2),
        xlab='Day', ylab='Temperature', main='Smoothed Data with B-Splines')
legend("topright", legend=c("Observed Data", "Smoothed Data"), col=c("black", "blue"), lwd=c(1,2))



# b) Compute and plot approximate pointwise confidence intervals
S <- basismat %*% solve(t(basismat) %*% basismat) %*% t(basismat) # Projection operator
sigmahat <- sqrt(sum((Xsp0 - data_W)^2) / (length(time) - degree)) # Estimate of sigma
lb <- Xsp0 - qnorm(0.975) * sigmahat * sqrt(diag(S))
ub <- Xsp0 + qnorm(0.975) * sigmahat * sqrt(diag(S))

plot(time, Xsp0, type='l', col='blue', lwd=2, ylim=range(c(data_W, lb, ub)),
     xlab='Day', ylab='Temperature', main='Smoothed Data with Confidence Intervals')
lines(time, lb, col='blue', lty='dashed')
lines(time, ub, col='blue', lty='dashed')
lines(time, data_W, col='black')
legend("topright", legend=c("Smoothed Data", "95% CI", "Observed Data"), col=c("blue", "blue", "black"), lty=c(1, 2, 1), lwd=c(2, 1, 1))



# c) Compute and compare first derivatives

# Derivative of smoothed curve
basismat1 <- eval.basis(time, basis, Lfdobj=1)
Xsp1 <- basismat1 %*% est_coef

# Approximate first differences of observed data
rappincX1 <- (data_W[3:length(time)] - data_W[1:(length(time) - 2)]) / (time[3:length(time)] - time[1:(length(time) - 2)])

# Plot smoothed curve and its first derivative
par(mfrow=c(1,2))
plot(time, data_W, type='l', xlab='Day', ylab='Temperature', main='Observed and Smoothed Curves')
lines(time, Xsp0, col='blue', lwd=2)
legend("topright", legend=c("Observed Data", "Smoothed Data"), col=c("black", "blue"), lwd=c(1,2))

plot(time[2:(length(time)-1)], rappincX1, type='l', xlab='Day', ylab='First Differences', main='First Derivative Comparison')
lines(time, Xsp1, col='blue', lwd=2)
legend("topright", legend=c("First Differences (Observed)", "First Derivative (Smoothed)"), col=c("black", "blue"), lwd=c(1,2))



# d) Consider alternative basis system: Fourier basis

# Fourier basis
nbasis_fourier <- 12
basis_fourier <- create.fourier.basis(rangeval=range(time), nbasis=nbasis_fourier)
basismat_fourier <- eval.basis(time, basis_fourier)
est_coef_fourier <- lsfit(basismat_fourier, data_W, intercept=FALSE)$coef
Xsp0_fourier <- basismat_fourier %*% est_coef_fourier

# Plot with Fourier basis
par(mfrow=c(1,2))
plot(time, data_W, type='l', xlab='Day', ylab='Temperature', main='Fourier Basis Smoothing')
lines(time, Xsp0_fourier, col='red')
legend("topright", legend=c("Observed Data", "Smoothed Data (Fourier)"), col=c("black", "red"), lwd=2)









#3) Fourier smoothing curva unica + nbasis chosen with GCV + approx of first derivative from data vs from the smoothed curve + show effect of over/under smoothing ---------------------

rm(list=ls())
graphics.off()
data <- read.table('power.txt', header=TRUE)
head(data)

matplot(data, type='l', xlab='Day', ylab='Power Consumption', main='Raw Power Data')
library(fda)
abscissa <- 1:365
Xobs0 <- as.matrix(data)

nbasis <- 3:50
gcv <- numeric(length(nbasis))

# Loop attraverso i valori di nbasis per calcolare il GCV
for (i in 1:length(nbasis)) {
  basis <- create.fourier.basis(rangeval=c(1, 365), nbasis=nbasis[i]) 
  gcv[i] <- smooth.basis(abscissa, Xobs0, basis)$gcv
}

# Plot dei valori GCV in funzione del numero di basi
par(mfrow=c(1,1))
plot(nbasis, gcv, type='b', xlab='Number of Basis Functions', ylab='GCV', main='GCV vs Number of Basis Functions')
nbasis_chosen <- nbasis[which.min(gcv)]
nbasis_chosen
abline(v=nbasis_chosen, col=2, lty=2)

# Creazione della base di Fourier con il numero ottimale di funzioni di base
basis_opt <- create.fourier.basis(rangeval=c(1, 365), nbasis=nbasis_chosen)

# Plot del sistema di basi usato
plot(basis_opt, main='Fourier Basis System')


# Plot dei dati lisciati con base ottimale
Xsp <- smooth.basis(argvals=abscissa, y=Xobs0, fdParobj=basis_opt)
Xsp0bis <- eval.fd(abscissa, Xsp$fd) #  the curve smoothing the data
plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa,Xsp0bis ,type="l",col="red",lwd=2)



#b)  approximation of the first derivative from data vs from the smoothed curve 

Xsp <- smooth.basis(argvals=abscissa, y=Xobs0, fdParobj=basis_opt) 
smth_curve <- eval.fd(abscissa, Xsp$fd) #  the curve smoothing the data
fst_smth_der <- eval.fd(abscissa, Xsp$fd, Lfd=1) # first derivative


NT <- length(abscissa)

#modo uno
fst_approx_der <- (Xobs0[3:NT]-Xobs0[1:(NT-2)])/(abscissa[3:NT]-abscissa[1:(NT-2)])
abscissa_der <- abscissa[2:(NT-1)]
par(mfrow = c(2, 1))
matplot(abscissa, fst_smth_der, type='l', xlab='Day', ylab='First Derivative', main='First Derivative of Smoothed Curve')
matplot(abscissa_der, fst_approx_der, type='l', xlab='Day', ylab='First Derivative', main='Approximation of First Derivative')

#oppure
par(mfrow = c(1, 1))
rappincX1 <- (Xobs0[3:NT]-Xobs0[1:(NT-2)])/(abscissa[3:NT]-abscissa[1:(NT-2)])
Xsp1bis <- eval.fd(abscissa, Xsp$fd, Lfd=1) # first derivative
plot(abscissa[2:(NT-1)],rappincX1,xlab="t",ylab="first differences x",type="l")
points(abscissa,Xsp1bis,type='l',col="orange",lwd=3)

# The approximate derivative obtained from the raw data tends to be noisier compared to the derivative of the smoothed curve. 
# This is expected because the central difference method uses finite differences that amplify the noise present in the original data. 
# The derivative of the smoothed curve, on the other hand, is more regular and better reflects the overall trend of the data due to noise reduction through smoothing with the Fourier basis




#c) oversmoothing

#oversmoothing when i over simplify the function using less number of basis :

# nbasis = 2
nbasis <- 2
basis <- create.fourier.basis(rangeval=range(abscissa), nbasis=nbasis)

Xsp <- smooth.basis(argvals=abscissa, y=Xobs0, fdParobj=basis)
Xsp0bis <- eval.fd(abscissa, Xsp$fd) #  the curve smoothing the data

par(mfrow = c(1, 1))
plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa,Xsp0bis ,type="l",col="red",lwd=2)



#d) overfitting

#overfitting when i overfit the function using more number of basis :

# nbasis = 50
nbasis <- 50
basis <- create.fourier.basis(rangeval=range(abscissa), nbasis=nbasis)

Xsp <- smooth.basis(argvals=abscissa, y=Xobs0, fdParobj=basis)
Xsp0bis <- eval.fd(abscissa, Xsp$fd) #  the curve smoothing the data

par(mfrow = c(1, 1))
plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa,Xsp0bis ,type="l",col="red",lwd=2)


#4) Fourier 21 basis, PCA, variance explained, plot of 3 eigenfunctions, the screeplot, use categorical data for further pc analysis and plot the scores ----------------------
rm(list=ls())
graphics.off()
data <- read.table('temperature.txt',header=T)
head(data)
#dim(data)
#[1] 151 366  <- devo trasporre! voglio nelle x(righe) 366

head(t(data))

data <- t(data)

matplot(data, type='l' , xlab ='x', ylab = 'y')


time <- 1:365
basis <- create.fourier.basis(rangeval=c(1, 365), nbasis=21)
data_matrix <- as.matrix(data)
data_L.fd <- Data2fd(y=data_matrix, argvals= time , basisobj=basis) 
plot.fd(data_L.fd, main="FOURIER")

data_L.fd$coefs[1:3,1:2]
#       station1  station2
#const 467.62612 493.09532
#sin1  -95.75819 -90.15895
#cos1  -45.39373 -40.70578

# b) 
pca_W <- pca.fd(data_L.fd,nharm=5,centerfns=TRUE)

# scree plot

par(mfrow=c(1,1))
plot(pca_W$values[1:3],xlab='j',ylab='Eigenvalues')# 1:3 se vuoi per i primi 3, altrimenti togli 1:3
plot(cumsum(pca_W$values)/sum(pca_W$values),xlab='j',ylab='CPV',ylim=c(0.5,1)) 
abline(h=0.9, lty=2, col='blue')

# First five FPCs variance explained
cumsum(pca_W$values[1:5])/sum(pca_W$values)
pca_W$varprop
#0.9992837

#first three eigenfunctions plot
# eigenfunctions
par(mfrow = c(1,3))
plot(pca_W.1$harmonics[1,],col=1,ylab='FPC1')
abline(h=0,lty=2)
plot(pca_W.1$harmonics[2,],col=2,ylab='FPC2')
plot(pca_W.1$harmonics[3,],col=3,ylab='FPC3')



#c) plot of the scores of the first 2 PC, use the categorical variable country to further PC analysis 

par(mfrow=c(1,1))

# Estrazione della categorica
zones <- read.table('temperature.txt')
zones <- zones$country
# Estrazione dei punteggi lungo le prime 2 componenti principali
scores <- pca_W$scores

# Plot of scores along first 2 pc
plot(scores[,1], scores[,2], col=as.factor(zones), pch=16,
     xlab="Prima Componente Principale", ylab="Seconda Componente Principale",
     main="Plot qualitativo dei punteggi")
legend("topright", legend=levels(as.factor(zones)), col=1:3, pch=16)

# Germany and italy has different temperatures, and PC1 and PC2 has a linear dependence between them

#c)   Propose a possible dimensionality reduction

par(mfrow=c(2,1))
plot(pca_W$values[1:10],xlab='j',ylab='Eigenvalues')# 1:10 se vuoi per i primi 10, altrimenti togli 1:3
plot(cumsum(pca_W$values)/sum(pca_W$values),xlab='j',ylab='CPV',ylim=c(0.5,1)) 
abline(h=0.9, lty=2, col='blue')
# we can use first two pc since they explain 0.98 of the total variability









#5) Bpsline basis con lambda dato, number of nodes of the splines, plot of smoothed data, 3 coeff of 1 obs, PCA, screeplot,  -------
#variance explained, plot the retained PC, plot PC as perturbation of the mean, plot of the scores along the first 2 PC -----------

rm(list=ls())
graphics.off()
data <- read.table('listening.txt',header=T)
head(data)
matplot(t(data), type='l' , xlab ='x', ylab = 'y')
data <- t(data)

Xobs0 <- data
abscissa <- 1:365

#Specify the choice for the nodes of the splines
# 365 ovvero abscissa

#then order and degree
norder <- 3     # spline order (3th order polynomials)
degree <- norder-1    # spline degree
lambda1 = 100

breaks <- abscissa 
basis <- create.bspline.basis(breaks, norder=norder)
functionalPar <- fdPar(fdobj=basis, Lfdobj=1, lambda=lambda1) #Lfdobj=2 se non funziona lo abbassi
Xss <- smooth.basis(abscissa, Xobs0, functionalPar)

# Plot the smoothed data
plot.fd(Xss$fd, main="Smoothed Data using B-Splines")

# Extract the first three coefficients for the first song
Xss$fd$coefs[1:3, 1]
#  bspl4.1  bspl4.2  bspl4.3 
#17.83256 17.75425 17.59763


#b) PCA
data_L.fd <- Xss$fd
pca_W <- pca.fd(data_L.fd,nharm=5,centerfns=TRUE)
# scree plot

par(mfrow=c(1,1))
plot(pca_W$values[1:15],xlab='j',ylab='Eigenvalues')# 1:15 se vuoi per i primi 15, altrimenti togli 1:15
plot(cumsum(pca_W$values)/sum(pca_W$values),xlab='j',ylab='CPV',ylim=c(0.5,1)) 
abline(h=0.9, lty=2, col='blue')

# First five FPCs variance explained
cumsum(pca_W$values[1:5])/sum(pca_W$values)
# 0.9899074

#c) Propose a possible dimensionality reduction for the data and justify your choice.
# we can keep the first 3 pc since they cover 94% of the variance explained


#Plot the retained principal components.
par(mfrow=c(1,3))
plot(pca_W$harmonics[1,],col=1,ylab='FPC1') # harmonics sarebbero i loadings nella pca normale
plot(pca_W$harmonics[2,],col=2,ylab='FPC2')
plot(pca_W$harmonics[3,],col=3,ylab='FPC3')
# we can use first two pc since they explain 0.98 of the total variability



#d) plot  the retained principal components as perturbation of the mean, and interpret them.

media <- mean.fd(data_L.fd)

par(mfrow=c(1,3))
plot(media,lwd=2,ylim=c(25,65),main='FPC1')
lines(media+pca_W$harmonics[1,]*sqrt(pca_W$values[1]), col=2) #positive PC
lines(media-pca_W$harmonics[1,]*sqrt(pca_W$values[1]), col=3)
# first -  higher PC1 corrispondes to higher music listening on the first part of the year from the mean, and lower at the end of the year

plot(media,lwd=2,ylim=c(25,65),main='FPC2')
lines(media+pca_W$harmonics[2,]*sqrt(pca_W$values[2]), col=2)
lines(media-pca_W$harmonics[2,]*sqrt(pca_W$values[2]), col=3)
# second - higher PC2 corrispondes to accentuate the depth of the curve (both positive and negative curves), lower pc2 is more constant through the year

plot(media,lwd=2,ylim=c(25,65),main='FPC2')
lines(media+pca_W$harmonics[3,]*sqrt(pca_W$values[3]), col=2)
lines(media-pca_W$harmonics[3,]*sqrt(pca_W$values[3]), col=3)
# third - higher PC2 corrispondes to higher music listening on the of the mean in general

#oppure meglio!!!
par(mfrow=c(1,3)) # c(1,3) 
plot(pca_W, nx=100, pointplot=TRUE, harm=c(1,2,3), expand=0, cycle=FALSE) #c(1,2,3)


#e) plot of the scores along the first 2 PC and comment the results

par(mfrow=c(1,1))

# Estrazione dei punteggi lungo le prime 2 componenti principali
scores <- pca_W$scores

# Plot of scores along first 2 pc
plot(scores[,1], scores[,2], pch=16,
     xlab="Prima Componente Principale", ylab="Seconda Componente Principale",
     main="Plot qualitativo dei punteggi")

labels <- colnames(data) # etichette dei punti
text(scores[,1], scores[,2], labels=labels, pos=4, cex=0.8)

# we observe that we identify 3 different types of music.
# on the left we have low pc1, so low music listening from the mean on the first part of the year from the mean, and higer at the end of the year and high pc2, so we accentuate the curve
# on the middle we have pc1 null, so we follow the mean, but with low pc2, so we have a constant listening trought all the year
# on the rightwe have high pc1, so high music listening from the mean on the first part of the year from the mean, and lower at the end of the year and high pc2, so we accentuate the curve




















#6) Bpsline basis, plot of smoothed data, 3 coeff of 1 obs, PCA, screeplot, variance explained, plot PC as perturbation of the mean, plot of the scores along the first 2 PC -----------

rm(list=ls())
graphics.off()
data <- read.table('listening.txt',header=T)
head(data)
matplot(t(data), type='l' , xlab ='x', ylab = 'y')
data <- t(data)
time <- 1:365 
nbasis <- 12 #if he don't specify anything, put how many basis you want
basis <- create.bspline.basis(range(time),  
                              nbasis=nbasis, 3) # 4 è order, degree +1 || FOURIER SE CAMBIA
data_L.fd <- Data2fd(y=data, argvals= time , basisobj=basis)

#plot of the smoothed data and report the
plot.fd(data_L.fd, main="B-splines")

#first 3 coefficients for the first song.
data_L.fd
data_L.fd$coefs[1:3,1]
#  bspl3.1  bspl3.2  bspl3.3 
# 17.38683 19.24652 18.57496

#b) PCA

pca_W <- pca.fd(data_L.fd,nharm=5,centerfns=TRUE)
# scree plot

par(mfrow=c(1,1))
plot(pca_W$values[1:15],xlab='j',ylab='Eigenvalues')# 1:15 se vuoi per i primi 3, altrimenti togli 1:3
plot(cumsum(pca_W$values)/sum(pca_W$values),xlab='j',ylab='CPV',ylim=c(0.5,1)) 
abline(h=0.9, lty=2, col='blue')

# First five FPCs variance explained
cumsum(pca_W$values[1:5])/sum(pca_W$values)
# 0.9899074

#c) Propose a possible dimensionality reduction for the data and justify your choice.
# we can keep the first 3 pc since they cover 94% of the variance explained


#Plot the retained principal components.
par(mfrow=c(2,1))
plot(pca_W$values[1:10],xlab='j',ylab='Eigenvalues')# 1:10 se vuoi per i primi 10, altrimenti togli 1:3
plot(cumsum(pca_W$values)/sum(pca_W$values),xlab='j',ylab='CPV',ylim=c(0.5,1)) 
abline(h=0.9, lty=2, col='blue')
# we can use first two pc since they explain 0.98 of the total variability



#d) plot  the retained principal components as perturbation of the mean, and interpret them.

media <- mean.fd(data_L.fd)

par(mfrow=c(1,3))
plot(media,lwd=2,ylim=c(25,65),main='FPC1')
lines(media+pca_W$harmonics[1,]*sqrt(pca_W$values[1]), col=2) #positive PC
lines(media-pca_W$harmonics[1,]*sqrt(pca_W$values[1]), col=3)
# first -  higher PC1 corrispondes to higher music listening on the first part of the year from the mean, and lower at the end of the year

plot(media,lwd=2,ylim=c(25,65),main='FPC2')
lines(media+pca_W$harmonics[2,]*sqrt(pca_W$values[2]), col=2)
lines(media-pca_W$harmonics[2,]*sqrt(pca_W$values[2]), col=3)
# second - higher PC2 corrispondes to accentuate the depth of the curve (both positive and negative curves), lower pc2 is more constant through the year

plot(media,lwd=2,ylim=c(25,65),main='FPC2')
lines(media+pca_W$harmonics[3,]*sqrt(pca_W$values[3]), col=2)
lines(media-pca_W$harmonics[3,]*sqrt(pca_W$values[3]), col=3)
# third - higher PC2 corrispondes to higher music listening on the of the mean in general



par(mfrow=c(1,3)) # c(1,2) 
plot(pca_W, nx=100, pointplot=TRUE, harm=c(1,2,3), expand=0, cycle=FALSE) #harm= c(1,2)


#e) plot of the scores along the first 2 PC and comment the results

par(mfrow=c(1,1))

# Estrazione dei punteggi lungo le prime 2 componenti principali
scores <- pca_W$scores

# Plot of scores along first 2 pc
plot(scores[,1], scores[,2], pch=16,
     xlab="Prima Componente Principale", ylab="Seconda Componente Principale",
     main="Plot qualitativo dei punteggi")

# we observe that we identify 3 different types of music.
# on the left we have low pc1, so low music listening from the mean on the first part of the year from the mean, and higer at the end of the year and high pc2, so we accentuate the curve
# on the middle we have pc1 null, so we follow the mean, but with low pc2, so we have a constant listening trought all the year
# on the rightwe have high pc1, so high music listening from the mean on the first part of the year from the mean, and lower at the end of the year and high pc2, so we accentuate the curve











#7) bpsline curva unica with lambda scelto, number of splines used and value of gcv error,degrees of freedom, plot of first derivative, lambda who minimize gcv error, slope at the steepest part    -------------------------
rm(list=ls())
graphics.off()
data <- read.table('izoard.txt',header=T)
head(data)

#point A:  Report the number of splines used and the GCV error
#first set the data (dividing for the time)
Xobs0 <- data[,2]
abscissa <- data[,1]

#then order and degree
norder <- 4     # spline order (4th order polynomials)
degree <- norder-1    # spline degree
lambda1 = 0.1

breaks <- abscissa 
basis <- create.bspline.basis(breaks, norder=norder)
functionalPar <- fdPar(fdobj=basis, Lfdobj=2, lambda=lambda1)
Xss <- smooth.basis(abscissa, Xobs0, functionalPar)
gcvbest.n <- Xss$gcv  #  the value of the gcv statistic error
gcvbest.n


number_of_splines <- length(abscissa) + norder - 2
print(number_of_splines) #103



#point b

df <- Xss$df    #  the degrees of freedom in the smoothing curve
#approximate dimension of the space in which the fitted curve lives
df #20

Xss0 <- eval.fd(abscissa, Xss$fd, Lfd=0)
Xss1 <- eval.fd(abscissa, Xss$fd, Lfd=1)
rappincX1 <- (Xobs0[3:NT]-Xobs0[1:(NT-2)])/(abscissa[3:NT]-abscissa[1:(NT-2)])

#plot of fitted curve and and its first derivative
NT <- length(abscissa)
par(mfrow=c(1, 2))
plot(abscissa, Xobs0, xlab="t", ylab="observed data")
points(abscissa, Xss0, type="l", col="blue", lwd=2)
plot(abscissa[2:(NT-1)], rappincX1, xlab="t", ylab="first differences x", type="l")
points(abscissa, Xss1, type="l", col="blue", lwd=2)


#point c
#lambda who minimize gcv error 

lambda <- 10^seq(-1, 3, by=0.5)
gcv <- numeric(length(lambda))
for (i in 1:length(lambda)){
  functionalPar <- fdPar(fdobj=basis, Lfdobj=2, lambda=lambda[i])
  gcv[i] <- smooth.basis(abscissa, Xobs0, functionalPar)$gcv
}
par(mfrow=c(1, 1))
plot(log10(lambda), gcv)

lambda2 <- lambda[which.min(gcv)]

functionalParbis <- fdPar(fdobj=basis, Lfdobj=2, lambda=lambda2)
Xss2 <- smooth.basis(abscissa, Xobs0, functionalParbis)


gcvbestlamb <- Xss2$gcv  #  the value of the gcv statistic error
gcvbestlamb #842.0294 

#plot
Xss0.2 <- eval.fd(abscissa, Xss2$fd, Lfd=0)
Xss1.2 <- eval.fd(abscissa, Xss2$fd, Lfd=1)
rappincX1 <- (Xobs0[3:NT]-Xobs0[1:(NT-2)])/(abscissa[3:NT]-abscissa[1:(NT-2)])

NT <- length(abscissa)
par(mfrow=c(1, 2))
plot(abscissa, Xobs0, xlab="t", ylab="observed data")
points(abscissa, Xss0.2, type="l", col="blue", lwd=2)
plot(abscissa[2:(NT-1)], rappincX1, xlab="t", ylab="first differences x", type="l")
points(abscissa, Xss1.2, type="l", col="blue", lwd=2)


#point d
#slope at the steepest part
slope <- eval.fd(abscissa, Xss2$fd, 1)
data$distance[which.max(slope)] #17













#8) bpsline  curva unica, scegli lambda con gcv , plot the GCV statistic as a function of log(lambda), approximation of the 1st derivative of curve and smoothing curve, choose lambda for oversmoothing/overfitting -------------

rm(list=ls())
graphics.off()
# Load the data
data <- read.table("10km.txt", sep =" ", header = TRUE, stringsAsFactors = TRUE)

# Extract the relevant data
Xobs0 <- data$velocity  # Assuming 'velocity' is the column name for the observed data
abscissa <- 1:length(Xobs0)  # X-values (assuming they are in the range 1 to length of Xobs0)
NT <- length(abscissa) # Number of observations

# Set the spline parameters
m <- 4  # Spline order (order = degree + 1)
degree <- m - 1  # Spline degree

# Define the lambda grid (range of lambda values)
lambda <- 10^seq(-5, 1, length=200)  # Logarithmic grid from 10^-5 to 10^1
gcv <- numeric(length(lambda))  # Initialize GCV vector

# Create a B-spline basis
basis <- create.bspline.basis(rangeval = range(abscissa), norder=m)

# Compute GCV for each lambda
for (i in 1:length(lambda)){
  functionalPar <- fdPar(fdobj=basis, Lfdobj=2, lambda=lambda[i])  # Penalty on second derivative
  gcv[i] <- smooth.basis(abscissa, Xobs0, functionalPar)$gcv
}

# Plot the GCV statistic as a function of log(lambda)
#CONTROLLA SE CHIEDE LOG O NO 
par(mfrow=c(1,1))
plot(log(lambda), gcv, type='l', xlab="log(lambda)", ylab="GCV", main="GCV vs log(lambda)")

# Find the optimal lambda
optimal_lambda <- lambda[which.min(gcv)]
abline(v = log/no(optimal_lambda), col = 2, lty = 2)

# Report the chosen lambda
cat("Chosen lambda: ", optimal_lambda, "\n")

# Perform smoothing using the optimal lambda
functionalPar_opt <- fdPar(fdobj=basis, Lfdobj=2, lambda=optimal_lambda)
smooth_result <- smooth.basis(abscissa, Xobs0, functionalPar_opt)


Xss <- smooth_result
Xss0 <- eval.fd(abscissa, Xss$fd, Lfd=0) # derviatives 0,1,2
Xss1 <- eval.fd(abscissa, Xss$fd, Lfd=1)
Xss2 <- eval.fd(abscissa, Xss$fd, Lfd=2)

df <- Xss$df   #  the degrees of freedom in the smoothing curve
df

# Plot the smoothed data
plot(abscissa, Xobs0,  col='grey', lwd=1.5, xlab="Index", ylab="Velocity", main="Smoothed Data")
lines(smooth_result$fd, col='blue', lwd=2)


#b)
# Compute the central finite differences for the first derivative
rappincX1 <- (Xobs0[3:NT] - Xobs0[1:(NT-2)]) / (abscissa[3:NT] - abscissa[1:(NT-2)])


# Plot the results
par(mfrow=c(1,1))

# Plot the central finite differences (approximation of the first derivative of observed data)
plot(abscissa[2:(NT-1)], rappincX1, type="l", col="red", lwd=2, 
     xlab="t", ylab="First derivative", main="Finite Differences (Observed Data)")
legend("topright", legend = c("Finite Differences"), col = c("red"), lwd = c(2))

# Plot the first derivative of the smoothed curve
lines(abscissa, Xss1, type="l", col="blue", lwd=2, 
      xlab="t", ylab="First derivative", main="First Derivative (Smoothed Curve)")
legend("topright", legend = c("Smoothed Derivative"), col = c("blue"), lwd = c(2))


#c)
# Large lambda value for oversmoothing
lambda_large <- 10  # Choose a large value for lambda (you can experiment with this)

# Set up the functional parameter object with the large lambda
functionalPar_oversmooth <- fdPar(fdobj=basis, Lfdobj=2, lambda=lambda_large)

# Perform the smoothing with the large lambda
smooth_result_oversmooth <- smooth.basis(abscissa, Xobs0, functionalPar_oversmooth)

# Evaluate the smoothed curve
Xss_oversmooth <- eval.fd(abscissa, smooth_result_oversmooth$fd, Lfd=0)

# Plot the original data and the oversmoothed curve
plot(abscissa, Xobs0, type="p", col="black", xlab="t", ylab="velocity", 
     main="Oversmoothing Effect (Large Lambda)")
lines(abscissa, Xss_oversmooth, col="red", lwd=2)

legend("topright", legend = c("Observed Data", "Oversmoothed Curve"), 
       col = c("black", "red"), lwd = c(1, 2))


#d)
#overfitting
lambda_small <- 1e-10 # Choose a very small value for lambda for overfitting

# Set up the functional parameter object with the small lambda
functionalPar_overfit <- fdPar(fdobj=basis, Lfdobj=2, lambda=lambda_small)

# Perform the smoothing with the small lambda
smooth_result_overfit <- smooth.basis(abscissa, Xobs0, functionalPar_overfit)

# Evaluate the smoothed curve
Xss_overfit <- eval.fd(abscissa, smooth_result_overfit$fd, Lfd=0)

# Plot the original data and the overfitted curve
plot(abscissa, Xobs0, type="p", col="black", xlab="t", ylab="velocity", 
     main="Overfitting Effect (Small Lambda)")
lines(abscissa, Xss_overfit, col="blue", lwd=2)

legend("topright", legend = c("Observed Data", "Overfitted Curve"), 
       col = c("black", "blue"), lwd = c(1, 2))


















#9) Fourier 13 basis, PCA, screeplot for dimensionality reduction, categorical ispection (holiday/nonHoliday) confrontando PC, scegliere un migliore divisore di PC1 per suddividere meglio i dati? ------------------
rm(list=ls())
graphics.off()
dataor <- read.table('load.txt',header=T)
data <- data.frame(dataor[,1:25])
head(data)
dim(data)
data<- t(data)

matplot(data, type='l' , xlab ='x', ylab = 'y')


time <- 0:24
basis <- create.fourier.basis(rangeval=c(0, 24), nbasis=13)
data_matrix <- as.matrix(data)
data_L.fd <- Data2fd(y=data_matrix, argvals= time , basisobj=basis) 
plot.fd(data_L.fd, main="FOURIER")


# b) 
pca_W <- pca.fd(data_L.fd,nharm=5,centerfns=TRUE)


# First five FPCs variance explained
cumsum(pca_W$values[1:5])/sum(pca_W$values)
pca_W$varprop
#0.039847287  

# screeplot for dimensionality reduction
par(mfrow=c(1,1))
plot(pca_W$values[1:3],xlab='j',ylab='Eigenvalues')# 1:3 se vuoi per i primi 3, altrimenti togli 1:3
plot(cumsum(pca_W$values)/sum(pca_W$values),xlab='j',ylab='CPV',ylim=c(0.5,1)) 
abline(h=0.9, lty=2, col='blue')


#c)
# provide an explanation of first pc

par(mfrow=c(1,2)) # c(1,3) 
plot(pca_W, nx=100, pointplot=TRUE, harm=c(1,2), expand=0, cycle=FALSE) #c(1,2,3)


#d)
par(mfrow=c(1,1))

# Estrazione dei punteggi lungo le prime 2 componenti principali
scores <- pca_W$scores

# Estrazione della categorica
zones <- dataor$daytype

# Estrazione dei punteggi lungo le prime 2 componenti principali
scores <- pca_W$scores

# Plot of scores along first 2 pc
plot(scores[,1], scores[,2], col=as.factor(zones), pch=16,
     xlab="Prima Componente Principale", ylab="Seconda Componente Principale",
     main="Plot qualitativo dei punteggi")
legend("topright", legend=levels(as.factor(zones)), col=1:3, pch=16)






#e) 1-dimensional representation of the data allowing for a better discrimination than the pc1 discriminant analysis
# i use LDA function

# Load necessary library
library(MASS)  # For LDA

# Perform LDA using the scores obtained from PCA
lda_result <- lda(zones ~ scores[, 1:5])  # Use the first 5 PCA scores for LDA

# Extract LDA scores (discriminant functions)
lda_scores <- predict(lda_result)$x

# Plot the LDA scores along the first discriminant function
plot(lda_scores[, 1], col=as.factor(zones), pch=16,
     xlab="LDA 1", ylab="Density",
     main="LDA - 1st Discriminant Function")
legend("topright", legend=levels(as.factor(zones)), col=1:3, pch=16)

# Compare with the first PCA component
plot(scores[,1], col=as.factor(zones), pch=16,
     xlab="PCA 1", ylab="Density",
     main="PCA - 1st Principal Component")
legend("topright", legend=levels(as.factor(zones)), col=1:3, pch=16)

