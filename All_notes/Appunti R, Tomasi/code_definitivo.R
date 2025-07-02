# First rows of code ----

rm(list=ls())
graphics.off()
data <- read.table('...',header=T)
head(data)

par(mfrow = c(1,1))

# Libraries ---
library(car)                 # plotting ellipses, Box-Cox transformation, linearHypothesis()
library(MASS)                # lda(), qda() function, lm.ridge()
library(class)               # knn function
library(glmnet)              # glmnet() function (Ridge, Lasso, Elastic Net)
library(leaps)               # regsubsets()
library(tree)                # tree() function (classification and regression tree
library(mvtnorm)
library(mvnormtest)
library(MVN)
library(nlmeU) ## --> for the dataset, 
library(nlme)  ## --> for models implementation, LMM per eteroschedastici
library(lme4)  ## --> LMM per omosch.
library(e1071) ## --> support vector machine
library(dbscan)
load('C:/Users/frenc/OneDrive/Desktop/mcshapiro.test.RData')

# robe in lmm
library(corrplot)
library(lattice)
library(plot.matrix)
library(nlmeU)
library(nlme)
library(lme4)
library(insight)

# robe fda
library(fda)
library(fields)
library(KernSmooth) # per locpoly in fda

# robe spatial
library(sp)           ## Data management
library(lattice)      ## Data management
library(geoR)         ## Geostatistics
library(gstat)        ## Geostatistics

#### PCA #### 

head(data)
n <- dim(data)[1]
p <- dim(data)[2]

data.originalmean <- sapply(data, mean)
data.originalsd <- sapply(data, sd)

# checking variability: if there are some variables with a very larger variance, 
# they could driven the analysis of Principal Components:

boxplot(scale(x=data, center = FALSE, scale = FALSE), las = 2, col = 'gold')  ##boxplot dati originali
boxplot(scale(x=data, center = TRUE, scale = FALSE), las = 2, col = 'gold')  ##boxplot dati centrati (mu=0, tengono la stessa varianza)
boxplot(scale(x=data, center = TRUE, scale = TRUE), las = 2, col = 'gold')   ##boxplot dati standardizzati (mu=0, deviazione standard a 1)

# if you want to standardize data:
data <- scale(data)
data <- data.frame(data)

# performing PCA:
pc.data <- princomp(data, scores=T)
summary(pc.data)

# standard deviation of the components (square root of eigenvalues):
pc.data$sd
# proportion of variance explained by each Principal Component: 
pc.data$sd^2/sum(pc.data$sd^2)
# cumulative proportion of explained variance: 
cumsum(pc.data$sd^2)/sum(pc.data$sd^2)

load.data <- pc.data$loadings

# plotting all loadings
n_bar <- ceiling(p/2)
par(mfrow = c(2,n_bar))
for(i in 1:p) barplot(load.data[,i], ylim = c(-1, 1), main=paste('PC',i))

# plotting first 3 loadings
par(mfrow = c(3,1))
for(i in 1:3) barplot(load.data[,i], ylim = c(-1, 1), main = paste('PC', i))


# plotting results (Screeplot on the right)
varmax <- max(var(data[,1:dim(data)[2]]))
varmax_pc <- max(pc.data$sd)
layout(matrix(c(2,3,1,3),2,byrow=T))
plot(pc.data, las=2, main='Principal components', ylim=c(0,varmax_pc^2))
barplot(sapply(data,sd)^2, las=2, main='Original Variables', ylim=c(0,varmax),
        ylab='Variances')
plot(cumsum(pc.data$sd^2)/sum(pc.data$sd^2), type='b', axes=F, 
     xlab='number of components', ylab='contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.8, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(data),labels=1:ncol(data),las=2)

# Looking at the screeplot, I can notice that the first ### PC's explain about ### of the total variability.
# Moreover, there is an elbow after the ### PC's.  
# These signs suggest me to keep only the first ### PC's to greatly reduce the dimensionality 
# of the data without loosing so much information.


### Scatterplot of the data along the first two principal components 
par(mfrow = c(1,1))
biplot(pc.data)





scores.data <- pc.dataset$scores
# Variability of the original variables / scores
layout(matrix(c(1, 2), 2))
boxplot(dataset.sd, las = 2, col = 'gold', main = 'Original Variables')
scores.data<- data.frame(scores.data)
boxplot(scores.data, las = 2, col = 'gold', main = 'Principal Components')

# keep only k components
k <- #
  # Variance explained along the first k PC
  variances = c(0,0,0,0)
for (k in 1:4) {
  variances[k] = (cumsum(pc.data$sd^2)/sum(pc.data$sd^2))[k]
}

# Dimensionality reduction: the first two components explain 73.6%<80% of the total
# variability. However, we do not see an elbow in the proportion of explained variability
# in correspondence of the III or IV PC. It could be thus sufficient to analyse the 
# sample through the first 2 PCs.
# analyze the results 
par(mfrow = c(1,1))
biplot(pc.data)


# Biplot usando la seconda e la terza componente principale
par(mfrow = c(1,1))
biplot(pc.data, choices = c(2, 3))























##### new point in PC -----------------------------------------
new.datum <- c(, , , ) #+

# if I have scaled my data, I have to scale even the new datum!
new.datum <- (new.datum - data.originalmean)/data.originalsd

proj <- new.datum %*% load.data

# keep only the first k components:
proj[1:k]




# Normality of the principal components
PCs <- data.frame(princomp(data)$scores)
plot(PCs, asp=1, pch=19)

par(mfcol=c(2,4))
for(i in 1:4)
{
  hist(PCs[,i], prob=T, main=paste('Histogram of PC', i, sep=''))
  lines(seq(min(PCs[,i]), max(PCs[,i]), length=2000), dnorm(seq(min(PCs[,i]), max(PCs[,i]), length=2000),mean(PCs[,i]),sd(PCs[,i])), col='blue', lty=2)
  qqnorm(PCs[,i], main=paste('QQplot of PC', i, sep=''))
  qqline(PCs[,i])
  print(shapiro.test(PCs[,i])$p)
}


# Mahalanobis distances of the data from the sample mean
M <- colMeans(data)
S <- cov(data)

d2 <- matrix(mahalanobis(data, M, S))

par(mfrow=c(1,2))

hist(d2, prob=T)
lines(0:2000/100, dchisq(0:2000/100,4), col='blue', lty=2)

qqplot(qchisq(seq(0.5/30, 1 - 0.5/30 ,by = 1/30), df = 4), d2,  main='QQplot di d2')
abline(0, 1)

d2.class <- cut(d2, qchisq((0:6)/6, df = 4))
d2.freq  <- table(d2.class)

chisq.test(x = d2.freq, p = rep(1/6, 6), simulate.p.value = T)

# test of all the directions simultaneously
mcshapiro.test(data)


### If the data don't seem Gaussian. What can we do?
### Identify clusters 
### Identify (and possibly remove) outliers
### Transform the data (e.g., Box-Cox transformations, see Johnson-Wichern Chap.4.8,
###                     R functions powerTransform(); bcPower())
### Work without the Gaussian assumption (e.g., permutation tests)

### Let's try to identify and remove outliers:
# We remove the data too far (in the sense of the Mahalanobis distance) 
# from the center of the distribution





##### PCA categoricals ----------------------------------------------------


#DATASET NUOVO CON PRINCIPAL COMPONENT CON UN SOLO FACTOR
data_c1<- which(dataor$...=='factor') #cambia il factor
scores <- as.data.frame(pc.data$scores)
data <- scores[data_c1,1:2] #cambia se vuoi pi? principal component 

# PLOT
dataor <- #dataset originale mai manomesso 
  
  k <- c()
for( i in 1:n){ 
  if(dataor[i,9] == "cannellini")  #SOSTITUISCI A 9 IL NUM DELLA COLONNA CON LA VARIABILE FACTOR 
    k <- cbind(k,1)
  if(dataor[i,9] == "adzuki")  #SOSTITUISCI A 9 IL NUM DELLA COLONNA CON LA VARIABILE FACTOR + factor  
    k <- cbind(k,2)
  if(dataor[i,9] == "black-eyed") #SOSTITUISCI A 9 IL NUM DELLA COLONNA CON LA VARIABILE FACTOR + factor 
    k <- cbind(k,3)
}
plot(pc.data$scores[,1],pc.data$scores[,2], col = k , pch = 16, xlab = 'PC1', ylab = 'PC2')
legend('topleft',c('ex', 'ad', 'factor3'), col = c('black','red','green'), pch = 16) 


#### GAUSSIANITY ####

# univariate case: 
shapiro.test(data)

# multivariate case: MVN package
# Different multivariate normality tests are implemented but the default one is the Henze-Zirkler's
result <- mvn(data = X)
# Royston’s test (multivariate extension of the Shapiro-Wilk test)
result <- mvn(data = X, mvnTest = "royston")
# with Q-Q plot of the squared mahalanobis distance over chi-square
result <- mvn(data = X, mvnTest = "hz", multivariatePlot = "qq")
# result
result$multivariateNormality

# if pvalue is large, I have no evidence to reject the null hypothesis of Gaussianity.
# if pvalue is small, I reject the hypothesis of Gaussianity.

#Prediction ellipse that contains alpha% of data (you need assumption of Gaussianity): (Ellisse contenente aplha% dei dati )

n <- dim(data)[1]
p <- dim(data)[2]

data.mean <- sapply(data, mean) # mean
data.cov <- cov(data) # var/cov matrix
data.invcov <- solve(data.cov) # inverse of var/cov matrix

alpha <- 0.01
cfr.fisher <- qchisq(1-alpha,p)

# Center:
center <- data.mean
colnames(center) <- colnames(data.mean)
center

# Directions of the principal axes:
direction <- eigen(data.cov)$vectors
colnames(direction) <- colnames(data)
direction

# # Length of the semi-axes of the ellipse:
semi.axes <- sqrt(cfr.fisher)*sqrt(eigen(data.cov/n)$values)
semi.axes

plot(data, asp = 1,xlim = c(-40,40),ylim = c(-40,40)) # scatterplot of data
points(data.mean[1], data.mean[2], pch = 16, col ='red', cex = 1.5) # sample mean
# plotting Prediction region for data (centered in data.mean)
ellipse(data.mean, data.cov, sqrt(cfr.fisher), col = 'red', lty = 2, lwd=2, center.cex=1)
#dataEllipse(as.matrix(data), levels=0.99, add =T)   Questa ? quella del lab

# Plot variabile e residui
par(mfrow=c(2,2))
plot(data$age, residuals(fm))
plot( data$perc_taxes, residuals(fm))
plot( data$income, residuals(fm))
data$owns_house <- factor(data$owns_house)
plot( data$owns_house, residuals(fm))

##### Mahalanobis, BoxCox ------------------------------------------------------

## Retrieving Gaussianity:
# • Identify clusters, and split the analysis in different clusters;
# • Identify (and possibly remove) outliers;
# • Transform the data (e.g., Box-Cox transformations, see Johnson-Wichern Chap.4.8, R functions pow-
#                         erTransform(); bcPower());
# • Work without the Gaussian assumption (e.g., permutation tests);
# • Asymptotic results (if we have a large dataset and if you need Gaussianity for the sample mean)

## example: identifying outliers
# Mahalanobis distances of the data from the sample mean
d2 <- matrix(mahalanobis(data, colMeans(data), cov(data)))
threshold <- 7.5        # SET THE THRESHOLD 
plot(d2, pch = ifelse(d2 < threshold, 1, 19)) # plotting the distances to set threshold 
plot(data, pch=ifelse(d2 < threshold, 1, 19)) # plotting original data highlighting outliers
data <- data[which(d2<threshold),]            # removing outliers 
mcshapiro.test(data)                          # checking Gaussianity

## example: UNIVARIATE Box-Cox transformations -----
# remember that you can use it only for POSITIVE data, and that:
# • For lambda<1: observations <1 are “spread”, observations >1 are “shrinked” 
# • For lambda>1: observations <1 are “shrinked”, observations >1 are “spread”
# • For lambda=1: no transformation made
# • For lambda=0: log transformation
shapiro.test(data)
lambda.data <- powerTransform(data)           # optimal lambda
bc.data <- bcPower(data, lambda.data$lambda)  # transforming data
shapiro.test(bc.data)


## example: MULTIVARIATE Box-Cox transformations -----
# I perform Box-Cox simultaneously on each variable
lambda.data <- powerTransform(data)
lambda.data
# note that if there are some values of lambda that are 
# close to 0 you can approximate them to 0, 
# and similarly with lambda close to 1 for a better interpretation.
# # id_1: id_1-the lambda is close to 1
# lambda.data[id_1] <- 1
# # id_0: id_1-the lambda is close to 0
# lambda.data[id_0] <- 0
bc.data <- data
for (i in 1:dim(data)[2]){
  bc.data[,i] <- bcPower(data[,i], lambda.data$lambda[i])
}
mvn(bc.data)

## log trasformazione
log_length=log(data$length) 


#zeno
# how to fix normality: 
# box-cox transofrmation

lambda.data <- powerTransform(data$length)           # optimal lambda
bc.length <- bcPower(data$length, lambda.data$lambda)  # transforming data

x <- bc.length #era la x di anova














#### TEST FOR THE MEAN - MULTIVARIATE GAUSSIAN ####

# Test on the mean of level $alpha$ 
# We don't have a large number of data so I need Gaussianity assumption on X. In this way I can use the Hotelling's Theorem
# that assume that T0 is distributed as a Fischer distribution. (Gaussianity assumption is needed)

#  H_0: mu = mu_0  vs   H_1: mu != mu_0]
# where $mu_0 = (, , , ...)$  

n <- dim(data)[1]
p <- dim(data)[2]
data.mean <- sapply(data, mean)  # mean
data.cov <- cov(data)            # var/cov matrix
data.invcov <- solve(data.cov)   # inverse of var/cov matrix

alpha <- 0.05 # SET ALPHA
mu0 <- c(0,0) # SET MU_0
xdataT2       <- n * (data.mean-mu0) %*% data.invcov %*% (data.mean-mu0) 

# Comando automatico
var_1 <- data[[1]]
var_2 <- data[[2]]
t.test(var_1, var_2, var.eq=T)

plot(data, main = "Scatter Plot with Confidence Ellipse")
# mean under H0 (blue)
points(mu0[1], mu0[2], col='blue', pch=16)

# sample mean (black)
points(data.mean[1], data.mean[2], col='black', pch=16)

# we represent the confidence region of level 95%: where does mu0 fall?
alpha <- .05
cfr.fisher <- (p*(n-1)/(n-p))*qf(1-alpha,p,n-p)
ellipse(center=data.mean, shape=data.cov/n, radius=sqrt(cfr.fisher), lwd=2)

# what about the region of level 99%?
# plot(data)    se non va senza!
alpha <- .01
cfr.fisher <- (p*(n-1)/(n-p))*qf(1-alpha,p,n-p)
ellipse(center=data.mean, shape=data.cov/n, radius=sqrt(cfr.fisher), lwd=2, col='orange', add=TRUE)
radius <- sqrt(cfr.fisher)

# Checking Gaussianity assumption of data: 
mvn(data)$multivariateNormality
# High (> 0.1): there is no statistical evidence to reject H0 (I can assume Gaussianity of data)
# Very low (< 0.05): there is statistical evidence to reject H0 (I can NOT assume Gaussianity of data)

# Squared malhanobis distance between the sample mean and the values of the hypothesis mu0
data.T2 <- n * (data.mean-mu0) %*% data.invcov %*% (data.mean-mu0) # T2 statistics

# Radius of the ellipsoid: 
# quantile of the Fisher distribution with p and n-p degrees of freedom at level 1-alpha
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
radius <- sqrt(cfr.fisher)

# Check if mu0 is outside the rejection region {data.T2>cfr.fisher}:
data.T2 < cfr.fisher

# True: there is no statistical evidence to reject H0 at level alpha (the statistic is outside the rejection region).
# False: mu0 is inside the rejection region so there is statistical evidence to reject H0 at level alpha

# Compute the p-value (if alpha not defined)
pvalue <- 1 - pf(data.T2*(n-p)/((n-1)*p), p, n-p)
pvalue
# High (> 0.1): there is no statistical evidence to reject H0 
# Very low (< 0.05): there is statistical evidence to reject H0 

#CONFIDENCE REGION (NO NEED OF mu0):
#Center
data.mean
# Directions of the principal axes:
eigen(data.cov/n)$vectors
# Length of the semi-axes of the ellipse:
sqrt(cfr.fisher)*sqrt(eigen(data.cov/n)$values)

#REJECTION REGION:
# Center:  
print(paste('Center: ', mu0)) #se non abbiamo mu_0 usare data.mean
# Directions of the principal axes:
print(paste('Directions: ', eigen(data.cov/n)$vectors))
# Length of the semi-axes of the ellipse:
print(paste('Length of the semi-axes: ', sqrt(cfr.fisher)*sqrt(eigen(data.cov/n)$values)))




###### MULTIVARIATE GAUSSIAN: PLOT DI REJECTION E CONFIDENCE REGION #puoi anche scegliere una delle due  ------------

# plotting: only if dim=2
plot(data, asp = 1) # scatterplot of data
# plotting the Rejection Region (centered in mu0)
ellipse(mu0, shape=data.cov/n, sqrt(cfr.fisher), col = 'blue', lty = 2, center.pch = 16)
points(data.mean[1], data.mean[2], pch = 16, col ='red', cex = 1.5) # sample mean 
# plotting Confidence region (centered in data.mean)
ellipse(data.mean, data.cov/n, sqrt(cfr.fisher), col = 'red', lty = 2, lwd=2, center.cex=1)

##### Elliptical REGION THAT CONTAINS 99% OF DATA

#CONFIDENCE REGION (NO NEED OF mu0):

alpha <- 0.01 # SET ALPHA: (0.01 for 99% of data)
cfr.chisq <- qchisq(1-alpha,p)
# Characterize the ellipse:
# Axes directions:
eigen(data.cov)$vectors
# Center:
data.mean
# Radius of the ellipse:
r <- sqrt(cfr.chisq)
# Length of the semi-axes:
r*sqrt(eigen(data.cov)$values)
# Plot
plot(data, asp = 1, col='forestgreen', pch=19)
points(data.mean[1], data.mean[2], pch = 4, cex = 1.5, lwd = 2)
ellipse(center=data.mean, shape=data.cov, radius=sqrt(cfr.chisq), col = 'black', lty = 2, center.pch = 4)

##### MULTIVARIATE GAUSSIAN: Test on linear combination -----------------------------------------------


D <- data.frame(cbind(data11[,], data22[,]))  # il tuo dataset su cui vuoi fare la linear combination

n <- dim(data)[1]
a <- c(a_1,a_2)
delta.0 <- 0 #NON CAMBIARE

data <- as.matrix(D)
t.stat <- (mean(data %*% a) - delta.0) / sqrt(var(data %*% a) / n ) # t-statistics (statistica 1!)

## UNILATERAL
# H0: a'*mu <= delta.0  vs H1: a'*mu > delta.0  (aggiunto frenci)
# Reject for large values of t 
# => compute the p-value as the probability of the right tail (i.e., of values >tstat)
P <- 1-pt(t.stat, n-1)
P

## UNILATERAL (aggiunto frenci)
# H0: a'*mu >= delta.0  vs H1: a'*mu < delta.0
# Reject for large values of t 
# => compute the p-value as the probability of the left tail (i.e., of values >tstat)
P <- pt(t.stat, n-1)
P

## BILATERAL
# H0: a'*mu = delta.0  vs H1: a'*mu != delta.0
P <- (1-pt(abs(t.stat),n-1))*2
P


##### MULTIVARIATE GAUSSIAN - CI of level 1-alpha% for the mean of the sum  --------
a2 <- c(1,1)
alpha <- ...
cfr.t <- qt(1-alpha/2, n-1)

c(inf = mean(as.matrix(data) %*% a2) - cfr.t * sqrt( var(as.matrix(data) %*% a2) / n ),
  center = mean(as.matrix(data) %*% a2),
  sup = mean(as.matrix(data) %*% a2) + cfr.t * sqrt( var(as.matrix(data) %*% a2) / n ))


# Otherwise one can use the function t.test() 
lc <- data[,1] + data[,2]
t.test(lc, alternative = 'two.sided', mu = 0, conf.level = 0.90)
# funzione t.test è univariata: se hai diverse colonne te le mette automaticamente in riga


# t test: se hai diverse colonne te le mette automaticamente in riga---------------------
# funzione t.test è univariata: se hai diverse colonne te le mette automaticamente in riga
t.test(data, alternative = 'two.sided', mu = 0, conf.level = 0.90) #H0: mean_data=mu0
t.test(data, alternative = 'less', mu = 0, conf.level = 0.90) #H0: mean_data<mu0
t.test(data, alternative = 'greater', mu = 0, conf.level = 0.90) #H0: mean_data>mu0



##### MULTIVARIATE GAUSSIAN - simultaneous T2 intervals ----------------------------
## If there are two variables and we want a test for each one and for the sum
a1<-c(1,0)
a2<-c(0,1)
a3<-c(1,1)
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
data.mean <- sapply(data, mean)  # mean
data.cov <- cov(data)            # var/cov matrix

ICT21<-data.frame(L=t(a1)%*%data.mean-sqrt(t(a1)%*%data.cov%*%a1/n)*sqrt(cfr.fisher),C=t(a1)%*%data.mean,U=t(a1)%*%data.mean+sqrt(t(a1)%*%data.cov%*%a1/n)*sqrt(cfr.fisher))
ICT22<-data.frame(L=t(a2)%*%data.mean-sqrt(t(a2)%*%data.cov%*%a2/n)*sqrt(cfr.fisher),C=t(a2)%*%data.mean,U=t(a2)%*%data.mean+sqrt(t(a2)%*%data.cov%*%a2/n)*sqrt(cfr.fisher))
ICT23<-data.frame(L=t(a3)%*%data.mean-sqrt(t(a3)%*%data.cov%*%a3/n)*sqrt(cfr.fisher),C=t(a3)%*%data.mean,U=t(a3)%*%data.mean+sqrt(t(a3)%*%data.cov%*%a3/n)*sqrt(cfr.fisher))
ICT2<-data.frame(rbind(ICT21,ICT22,ICT23))
ICT2
# Add the comment
for (i in 1:3)
  print(paste('Reject H0 for a',i,': ', !(0>ICT2[i,1] & 0<ICT2[i,3]),sep=''))



#### TEST FOR THE VARIANCE - MULTIVARIATE GAUSSIAN ####
#Prima di eseguire un test t per confrontare le medie di due gruppi (dove si assume varianze uguali)

var_1 <- data[[1]]
var_2 <- data[[2]]

var.test(var_1, var_2)



#### TEST FOR THE MEAN - ASYMPTOTIC ####

# No Gaussianity assumption are needed in this case since we use the Central Limit Theorem.
#So TCL and LGN assure that the Mahalanobis distance of the sample mean to the real mean is distributed as a Chi Squared.
# [H_0: mu = mu_0  vs  H_1: mu != mu_0 ]
# with in this case mu_0 = c(,)

mu0 <- c(, , , ...)
alpha <- 0.05

n <- dim(data)[1]
p <- dim(data)[2]
data.mean <- sapply(data, mean)  # mean
data.cov <- cov(data)            # var/cov matrix
data.invcov <- solve(data.cov)   # inverse of var/cov matrix

data.T2A <- n * (data.mean-mu0) %*%  data.invcov  %*% (data.mean-mu0)  
cfr.chisq <- qchisq(1-alpha,p)   # radius of the ellipsoid

# Check if mu0 is outside the rejection region {data.T2A>cfr.chisq}:
data.T2A < cfr.chisq 
ifelse(data.T2A < cfr.chisq , 
       "TRUE: there is no statistical evidence to reject H0", 
       "FALSE: there is statistical evidence to reject H0")
# True: there is no statistical evidence to reject H0 at level alpha (the statistic is outside the rejection region).
# False: mu0 is inside the rejection region so there is statistical evidence to reject H0 at level alpha

# Compute the p-value (if alpha not defined)
pvalue <- 1-pchisq(data.T2A, p)
# High (> 0.1): there is no statistical evidence to reject H0 
# Very low (< 0.05): there is statistical evidence to reject H0 


#### MULTIVARIATE GAUSSIAN - SIMULTANEOUS CI ####

# basically they are the projections on specific directions of the ellipsoidal confident region

n <- dim(data)[1]
p <- dim(data)[2]
alpha <- 
  data.mean <- sapply(data, mean)  # mean
data.cov <- cov(data)            # var/cov matrix
data.invcov <- solve(data.cov)   # inverse of var/cov matrix
cfr.fish ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)

mu0 <- c(, , , ...)

# Checking Gaussianity assumption of data: 
mvn(data)

# Simultaneous T2 confidence intervals on the coordinate directions:
T2 <- cbind(inf = data.mean - sqrt(cfr.fisher*diag(data.cov)/n),
            center = data.mean, 
            sup = data.mean + sqrt(cfr.fisher*diag(data.cov)/n))
T2
# if mu0 is contained in each interval, we cannot reject H0. 
# if mu0 is NOT contained in each interval, there is evidence to reject H0. 


# if p=2, you can plot the Sim-CI and the confidence region:
plot(data, asp = 1,main='Confidence and rejection regions')
# rejection region
ellipse(mu0, shape=data.cov/n, sqrt(cfr.fisher), col = 'blue', lty = 2, center.pch = 16)
# add mean 
points(data.mean[1], data.mean[2], pch = 16, col = 'red', cex=1.5)
# confidence interval
ellipse(data.mean, shape=data.cov/n, sqrt(cfr.fisher), col = 'red', lty = 2, center.pch = 16)
# Sim-CI 
rect(T2[1,1],T2[2,1],T2[1,3],T2[2,3], border='red', lwd=2)


# if p>2 we can plot the Sim-CI as:
matplot(1:p,1:p,pch='',ylim=range(data),
        xlab='Variables',ylab='T2 for a component', 
        main='Simultaneous T2 conf. int. for the components')
for(i in 1:p) segments(i,T2[i,1],i,T2[i,3],lwd=3,col=i)
points(1:p, T2[,2], pch=16, col=1:p)
# Is mu0 inside the rectangular region? We add it to the plot
points(1:p, mu0, lwd=3, col='orange')




# Sim-CI on the worst direction: 
# direction along which the T2 statistics (univariate) is maximized (from Maximum Lemma)
# La direzione peggiore è la direzione lungo la quale il test T2 risulta più sensibile, e quindi il test è più potente per identificare deviazioni dalla media ipotizzata.

worst <- data.invcov %*% (data.mean-mu0)
worst <- worst/sqrt(sum(worst^2))         # normalization of the vector to have a direction
theta.worst <- atan(worst[2]/worst[1])+pi # angle with the x-axis

# Confidence Interval along this worst direction, and compare it with mu0: 
IC.worst  <- c( data %*% worst - sqrt(cfr.fisher(t(worst)%*%D.cov%*%worst)/n),
                data %*% worst,
                data %*% worst + sqrt(cfr.fisher(t(worst)%*%D.cov%*%worst)/n) )
IC.worst
# mu0%*%worst     # projection on mu0 on the worst direction
(IC.worst[1] < mu0%*%worst) & (mu0%*%worst < IC.worst[2])  
# True: there is no statistical evidence to reject H0 at level alpha (the statistic is inside the Confidence Region).
# False: mu0 is outside the Confidence Region so there is statistical evidence to reject H0 at level alpha


# if p=2 you can plot it
plot(data, asp=1, pch=1, main='scatterplot of data',ylim=range(data))
ellipse(center=data.mean, shape=data.cov/n, radius=sqrt(cfr.fisher), lwd=2, col='red')
abline(v = T2[1,1], col='red', lwd=1, lty=2)
abline(v = T2[1,3], col='red', lwd=1, lty=2)
abline(h = T2[2,1], col='red', lwd=1, lty=2)
abline(h = T2[2,3], col='red', lwd=1, lty=2)
# add mu0
points(mu0[1], mu0[2], pch=16, col='blue', cex=1.5)
# Extremes of IC.worst in the coordinate system (x,y):
x.min <- IC.worst[1]*worst
x.max <- IC.worst[3]*worst
m1.ort <- -worst[1]/worst[2]
q.min.ort <- x.min[2] - m1.ort*x.min[1]
q.max.ort <- x.max[2] - m1.ort*x.max[1]
abline(q.min.ort, m1.ort, col='forestgreen', lty=2,lwd=1)
abline(q.max.ort, m1.ort, col='forestgreen', lty=2,lwd=1)
m1=worst[2]/worst[1] # worst direction
abline(0, m1, col='grey35')
segments(x.min[1],x.min[2],x.max[1],x.max[2],lty=1,lwd=2, col='forestgreen')




# Sim-CI on all the directions:
data.matrix <- as.matrix(data)
theta    <- seq(0, pi - pi/180, by = pi/180)
T2.d     <- NULL
Centerf  <- NULL
Maxf     <- NULL
Minf     <- NULL
for(i in 1:length(theta)) {
  a   <- c(cos(theta[i]), sin(theta[i])) # direction of the current projection
  proj <- mu0%*%a         # projection of mu on the current direction
  mu0_a <- c(mu0_a, proj) # collection of all the projections
  t2  <- (mean(data.matrix %*% a) - (mu0 %*% a) )^2 / ( var(data.matrix %*% a) / n )
  T2.d  <- c(T2.d, t2)
  centerf  <- data.mean %*% a
  maxf     <- data.mean %*% a + sqrt( t(a) %*% data.cov%*% a / n) * sqrt(cfr.fisher)
  minf     <- data.mean %*% a - sqrt( t(a) %*% data.cov%*% a / n) * sqrt(cfr.fisher)
  Centerf  <- c(Centerf, centerf)
  Maxf     <- c(Maxf, maxf)
  Minf     <- c(Minf, minf)
}

# plot the Sim-CI on all the direction
plot(theta, Centerf, main = 'Simultaneous T2 confidence intervals', ylim = range(data), col = 'grey25', type='l',ylab='IC')
for(i in 1:length(theta)) {
  lines(c(theta[i], theta[i]), c(Minf[i], Maxf[i]), col = 'grey75')
}
lines(c(theta[1], theta[1]), c(Minf[1], Maxf[1]), col = 'red', lwd=2) 
lines(c(theta[91], theta[91]), c(Minf[91], Maxf[91]), col = 'red', lwd=2)
lines(c(theta[which.max(T2.d)], theta[which.max(T2.d)]), c(Minf[which.max(T2.d)], Maxf[which.max(T2.d)]), col = 'forestgreen', lwd=2)
lines(theta, mu_a)
abline(h=mu0, col='black')
lines(theta, Minf, col = 'red', lty = 2)
lines(theta, Maxf, col = 'red', lty = 2)



#### MULTIVARIATE GAUSSIAN - SIMULTANEOUS CI with BONFERRONI CORRECTION ####

# As before, you need Gaussianity assumption on your data:
mvn(data)


n <- dim(data)[1]
p <- dim(data)[2]
data.mean <- sapply(data, mean)  # mean
data.cov <- cov(data)            # var/cov matrix
data.invcov <- solve(data.cov)   # inverse of var/cov matrix

mu0 <- c(, , , ...)


# Bonferroni CI for the MEAN at level (1-alpha):
k <- p # number of intervals/2 I want to compute (set in advance)
alpha <- ...
  cfr.t <- qt(1-alpha/(2*k),n-1)
Bf <- cbind(inf = data.mean - cfr.t*sqrt(diag(data.cov)/n),
            center = data.mean, 
            sup = data.mean + cfr.t*sqrt(diag(data.cov)/n))
Bf
# if mu0 is contained in each interval, we cannot reject H0. 
# if mu0 is NOT contained in each interval, there is evidence to reject H0. 

# Bonferroni CI for the VARIANCE at level (1-alpha)
k <- p
BFvar <- cbind(inf = ((n1-1)*diag(data.cov))/qchisq(1-alpha/(2*k), n1-1), 
               center =((((n1-1)*diag(data.cov))/qchisq(alpha/(2*k), n1-1)+(n1-1)*diag(data.cov)/qchisq( 1-alpha/(2*k),n1-1)))/2, 
               sup =(n1-1)*diag(data.cov)/qchisq( alpha/(2*k),n1-1))


# if p=2, you can plot the Sim-CI, the Bonferroni intervals and the confidence region:
plot(data, asp = 1,main='Confidence and rejection regions')
# rejection region
ellipse(mu0, shape=data.cov/n, sqrt(cfr.fisher), col = 'blue', lty = 2, center.pch = 16)
# add mean
points(data.mean[1], data.mean[2], pch = 16, col = 'red', cex=1.5)
# confidence region
ellipse(data.mean, shape=data.cov/n, sqrt(cfr.fisher), col = 'red', lty = 2, center.pch = 16)
# Sim-CI
rect(T2[1,1],T2[2,1],T2[1,3],T2[2,3], border='red', lwd=2)
# Bonferroni intervals
rect(Bf[1,1],Bf[2,1],Bf[1,3],Bf[2,3], border='orange', lwd=2)
legend('topleft', c('Rej. Reg.', 'Conf. Reg','T2-sim', 'Bonferroni'),
       col=c('blue','red','red','orange'), lty=c(2,2,1,1), lwd=1, cex=0.7)

legend('topright', 
       legend = c('Rej. Reg.', 'Conf. Reg'), 
       col = c('blue', 'red'), #
       lty = 2, 
       lwd = c(1, 2), 
       cex = 0.7)


#### TEST FOR THE MEAN - PAIRED MULTIVARIATE GAUSSIAN, CI (pre vs post di una stessa popolazione)####

n <- dim(data)[1]  
p <- dim(data)[2]
# sample of differences (post-pre for each variable)
data1 <- data[,c( which(colnames(data)=='...') ,which(colnames(data)=='...') , , ,)] #post
data2 <- data[,c(, , , ,)] #pre
data3 <- data[,c(, , , ,)] 

D <- data.frame(cbind(data1[,1:p]-data2[,1:p])) # imposta qui di quali data vuoi fare la differenza
                                                # non è p trovato in p <- dim(data)[2]
# Checking Gaussianity assumption of sample of differences: 
# we are not interested in the Gaussianity of the original data.
mvn(D)$multivariateNormality

n <- dim(D)[1]  
p <- dim(D)[2]  
D.mean   <- sapply(D,mean)
D.cov    <- cov(D)
D.invcov <- solve(D.cov)
alpha   <- ...
delta.0 <- c(..., ...) #

# Compute the T2 statistics:
D.T2 <- n * (D.mean-delta.0) %*% D.invcov %*% (D.mean-delta.0)
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p) # p-value that defines the rejection region

# Check if mu0 is outside the rejection region {D.T2>cfr.fisher}:
ifelse(D.T2 < cfr.fisher,
       print("TRUE: there is no statistical evidence to reject H0"), 
       print("FALSE: there is statistical evidence to reject H0"))
# True: there is no statistical evidence to reject H0 at level alpha (the statistic is outside the rejection region).
# False: mu0 is inside the rejection region so there is statistical evidence to reject H0 at level alpha

# compute the pvalue 
pvalue <- 1-pf(D.T2*(n-p)/(p*(n-1)), p, n-p)
pvalue
# High (> 0.1): there is no statistical evidence to reject H0 
# Very low (< 0.05): there is statistical evidence to reject H0 


#CASO UNIVARIATO

#OPPURE USO (Solo nel caso in cui la differenza è univariata):
#SE NON HO LA DIFFERENZA DEVO MODIFICARE DATA1 E DATA2 MOLTIPLICANDO DATA2 per -a2!! (RICORDATI IL -)!
data1 <- data[,c(which(colnames(data)=='...'))]  #post, semplicemente ho una colonna (quella di prima avevo piu misurazione)
data2 <- data[,c(which(colnames(data)=='...'))]  #pre
data1 <- as.data.frame(data1)
data2 <- as.data.frame(data2)
n <- dim(data1)[1]  
p <- dim(data1)[2] 
D <- data.frame(cbind(data1[,1:p]- data2[,1:p])) # scegli espressione, pu? essere per esempio: D <- data.frame(cbind(data1[,1:p]- 1.2 * data2[,1:p])) 
colnames(D) <- paste('DV', 1:p, sep='')
D
# Checking Gaussianity before performing t test
mvn(D)
t.test(D, mu=0, alternative="two.sided", paired=TRUE,conf.level = 0.9)  #Mettere "greater" se ho H0 <= , "l" se HO>=
# esempio t.test(D, mu=90, alternative="less",  conf.level=0.99)


# SE CHIEDE ELLIPTICLA REGION THAT CONTAINS tot% of the data go to the chapter GAUSSIANITY

# if p=2 we can plot the ellipsoidal region
plot(D, asp=1, pch=1, main='Dataset of the Differences',ylim=range(D))
# Ellipsoidal confidence region with confidence level 1-alpha
ellipse(center=D.mean, shape=D.cov/n, radius=sqrt((n-1)*p/(n-p)*qf(1-alpha,p,n-p)),
        col='red', lwd=2)
# add sample mean
points(D.mean[1], D.mean[2], pch=16, col='red', cex=1.5)
# Ellipsoidal Rejection Region of level 1-alpha
ellipse(center=delta.0, shape=D.cov/n, radius=sqrt((n-1)*p/(n-p)*qf(1-alpha,p,n-p)),
        col='blue', lwd=2)
# add delta.0
points(delta.0[1], delta.0[2], pch=16, col='blue', cex=1.5)






#### TEST FOR THE MEAN - PAIRED MULTIVARIATE GAUSSIAN, Sim-CI ####

# sample of differences (lab1-lab2 for each variable)
data1 <- data[,c(, , , ,)]
data2 <- data[,c(, , , ,)]
data3 <- data[,c(, , , ,)]

D <- data.frame(cbind(data1[,1:p]-data2[,1:p])) # imposta qui di quali data vuoi fare la differenza 
colnames(D) <- paste('DV', 1:p, sep='')


# Checking Gaussianity assumption of sample of differences: 
# we are not interested in the Gaussianity of the original data.
mvn(D)


data0 <- data # save original data
data <- D     # now data is the sample of the differences!

### Simultaneous T2 confidence intervals on the coordinate directions:
p<-
alpha<-
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p) 
T2 <- cbind(inf = D.mean - sqrt(cfr.fisher*diag(D.cov)/n),
            center = D.mean , 
            sup = D.mean + sqrt(cfr.fisher*diag(D.cov)/n))
T2

# WORST DIRECTION
worst <- D.invcov %*% (D.mean - delta.0)
worst <- worst / sqrt(sum(worst ^ 2)) # Normalization
worst

# Confidence interval along the worst direction:
IC.worst  <- c(D.mean %*% worst - sqrt(cfr.fisher(t(worst) %*% D.cov %*% worst) / n),
               D.mean %*% worst,
               D.mean %*% worst + sqrt(cfr.fisher(t(worst) %*% D.cov %*% worst) / n))
IC.worst
delta.0 %*% worst
(IC.worst[1] < delta.0 %*% worst) & (delta.0 %*% worst < IC.worst[3])   
# Reject H0: a'mu == a'delta.0 in direction a = worst







#### TEST FOR THE MEAN - PAIRED MULTIVARIATE GAUSSIAN, Sim-CI with Bonferroni ####

# sample of differences (lab1-lab2 for each variable)
data1 <- data[,c(, , , ,)
data2 <- data[,c(, , , ,)]
data3 <- data[,c(, , , ,)]

D <- data.frame(cbind(data1[,1:p]-data2[,1:p])) # imposta qui di quali data vuoi fare la differenza 
colnames(D) <- paste('DV', 1:p, sep='')


# Checking Gaussianity assumption of sample of differences: 
# we are not interested in the Gaussianity of the original data.
mvn(D)


data0 <- data # save original data
data <- D     # now data is the sample of the differences!

# Bonferroni CI for the MEAN at level (1-alpha):
k <- p # number of intervals I want to compute (set in advance)
alpha <-
  cfr.t <- qt(1-alpha/(2*k),n-1)
Bf <- cbind(inf = D.mean - cfr.t*sqrt(diag(D.cov)/n),
            center = D.mean, 
            sup = D.mean + cfr.t*sqrt(diag(D.cov)/n))
Bf

## PLOT INTERVALS
# Adding the 95% confidence region for the true mean of the differences
plot(D, asp=1, pch=1, main='Dataset of the Differences',ylim=range(D))
ellipse(center=D.mean, shape=D.cov/n, radius=sqrt(cfr.fisher), lwd=2, col='grey',
        center.cex=1.25)

# Adding quadrant lines and delta.0
abline(h=0, v=0, col='grey', lty=1, lwd=2)
points(delta.0[1], delta.0[2], pch=16, col='grey35', cex=1.25)

# Simultaneous confidence intervals T2
abline(v = T2[1,1], col='red', lwd=1, lty=2)
abline(v = T2[1,3], col='red', lwd=1, lty=2)
abline(h = T2[2,1], col='red', lwd=1, lty=2)
abline(h = T2[2,3], col='red', lwd=1, lty=2)
segments(T2[1,1], 0, T2[1,3], 0, lty=1, lwd=2, col='red')
segments(0, T2[2,1], 0, T2[2,3], lty=1, lwd=2, col='red')

# Bonferroni intervals 
abline(v = Bf[1,1], col='blue', lwd=1, lty=2)
abline(v = Bf[1,3], col='blue', lwd=1, lty=2)
abline(h = Bf[2,1], col='blue', lwd=1, lty=2)
abline(h = Bf[2,3], col='blue', lwd=1, lty=2)
segments(Bf[1,1], 0, Bf[1,3], 0, lty=1, lwd=2, col='blue')
segments(0, Bf[2,1], 0, Bf[2,3], lty=1, lwd=2, col='blue')

legend('topright', c('Bonf. IC', 'Sim-T2 IC'), col=c('blue', 'red'), lty=1)





#### TEST FOR THE MEAN - REPEATED MEASURES ####

# First representation of our data:
matplot(t(data))


n <- dim(data)[1]
q <- dim(data)[2]

# We need Gaussianity assumption on the increments: I perform mcshapiro.test()
# on original data:
mvn(data)$multivariateNormality
# se abbiamo i dati gaussiani anche la loro combinazione lineare è gaussiana

# We build the contrast matrix
# differences with baseline  
C <- matrix(c(-1, 1, 0, 0, 
              -1, 0, 1, 0,
              -1, 0, 0, 1), q-1, q, byrow=T)
# consecutive differences 
C <- matrix(c(-1, 1, 0, 0, 
              0, -1, 1, 0,
              0, 0, -1, 1), q-1, q, byrow=T)  #q-1 rows
#altro es
C <- matrix(c(-1, 0, 1, 0, 0, 0,
              0, 0, -1, 0, 1, 0,
              0, -1, 0, 1, 0, 0,
              0, 0, 0, -1, 0, 1), 4, 6, byrow=T) #4 rows, 6 cols


# We want to perform the following test at level $alpha$ : #
# [ H_0: mu = delta_0 vs H_1: mu != delta_0 ]
# nel primo caso
# h0: media1= media2
# h0: media1= media3
# where delta_0 = c(0,0,0,0)

# Compute mean and covariance matrix:
alpha = 0.05
delta.0 <- c(0,0,0) #dim=q-1 
M <- sapply(data, mean)
S <- cov(data)
Md <- C %*% M            # mean of transformed data (we apply C to the matrix of the mean M)
Sd <- C %*% S %*% t(C)   # variance/covariance matrix of transformed data
Sdinv <- solve(Sd)

# compute the test statistics and the radius of the ellipsoid:
T2 <- n * t( Md - delta.0 ) %*% Sdinv %*% ( Md - delta.0 ) 
cfr.fisher <- ((q-1)*(n-1)/(n-(q-1)))*qf(1-alpha,(q-1),n-(q-1))

# Check if mu0 is outside the rejection region {T2>cfr.fisher}:
ifelse(T2 < cfr.fisher,
       "TRUE: there is no statistical evidence to reject H0", 
       "FALSE: there is statistical evidence to reject H0")
# True: there is no statistical evidence to reject H0 at level alpha (the statistic is outside the rejection region).
# False: mu0 is inside the rejection region so there is statistical evidence to reject H0 at level alpha

# compute the pvalue:
pvalue <- 1-pf(T2*(n-(q-1))/((q-1)*(n-1)),(q-1),n-(q-1))
pvalue 
# High (> 0.1): there is no statistical evidence to reject H0 
# Very low (< 0.05): there is statistical evidence to reject H0 
# p value alto significa che hanno media simile


## SIMULTANEOUS CI:
# Simultaneous T2 intervals to check each difference simultaneously:
IC.T2 <- cbind( Md - sqrt(cfr.fisher*diag(Sd)/n) , Md, Md + sqrt(cfr.fisher*diag(Sd)/n) ) 
IC.T2
# check if the intervals contain the components of delta0.
k <- q - 1
## Graphical representation of the intervals:
matplot(matrix(1:3,k,3, byrow = T), t(IC.T2), type='b', pch='',xlim=c(0,4),xlab='', ylab='', main='Confidence intervals')
segments(matrix(1:k,k,1), IC.T2[,1], matrix(1:k,k,1),IC.T2[,3], col='orange', lwd=2) 
points(1:k, IC.T2[,2], col='orange', pch=16)
points(1:k+.05, delta.0, col='black', pch=16) 


# Confidence intervals of global level 1-alpha% for the daily increments
k <- q-1
ICmean <- cbind(Md - sqrt(diag(Sd)/n) * qt(1 - alpha/(2*k), n-1),
                Md,
                Md + sqrt(diag(Sd)/n) * qt(1 - alpha/(2*k), n-1))
ICmean
# comment 
# for (i in 1:(q-1))
# print(paste('Reject H0 in direction ',i,': ', !(delta.0[i]>ICmean[i,1] & delta.0[i]<ICmean[i,3]),sep=''))


## BONFERRONI CI: (differenze)
k <- q - 1 # number of increments (i.e., dim(C)[1]) 
cfr.t <- qt(1-alpha/(2*k), n-1)
IC.BF <- cbind( Md - cfr.t*sqrt(diag(Sd)/n) , Md, Md + cfr.t*sqrt(diag(Sd)/n) ) 
IC.BF
# check if the intervals contain the components of delta0.

## Graphical representation of the intervals:
matplot(matrix(1:3,k,3, byrow = T), t(IC.BF), type='b', pch='',xlim=c(0,4),xlab='', ylab='', main='Confidence intervals')
segments(matrix(1:k,k,1), IC.BF[,1], matrix(1:k,k,1),IC.BF[,3], col='orange', lwd=2) 
points(1:k, IC.BF[,2], col='orange', pch=16)
points(1:k+.05, delta.0, col='black', pch=16) 
#Se voglio anche i Simultaneous
# segments(matrix(1:k+.1,k,1),IC.T2[,1],matrix(1:k+.1,k,1),IC.T2[,3], col='blue', lwd=2) 
# points(1:k+.1,IC.T2[,2], col='blue', pch=16)   #T2= Simultaneous

#??? tde 10-07-18
#IC.T2 <- cbind( Md - sqrt(cfr.fisher*diag(Sd)/n) , Md, Md + sqrt(cfr.fisher*diag(Sd)/n) ) 
#IC.T2


## Se chiede solo dei bonferroni sulle variabili andare al capitolo degli intervalli simultanei con
## bonferroni correction


              
#### TEST FOR THE MEAN - ONE AT THE TIME #### 
              
n <- dim(data)[1]
p <- 1
data.mean <- mean('colonna di data che si vuole considerare')  # mean
data.cov <- var('colonna di data che si vuole considerare')            # var/cov matrix
data.invcov <- solve(data.cov)   # inverse of var/cov matrix


# Bonferroni CI for the mean at level (1-alpha):
k <- 1 # number of intervals I want to compute (set in advance)
cfr.t <- qt(1-alpha/(2*k),n-1)
Bf <- cbind(inf = data.mean - cfr.t*sqrt(data.cov/n),
            center = data.mean, 
            sup = data.mean + cfr.t*sqrt(data.cov/n))
Bf



#### TEST FOR THE MEAN - INDEPENDENT GAUSSIAN POPULATIONS (male vs female) ####

# We want to perform this test at level alpha = 0.05: 
# [ H_0: mu_1 = mu_2 vs H_1: mu_1 != mu_2 ]
# namely: 
# [ H_0: mu_1 - mu_2 = c(0,0) vs H_1: mu_1 - mu_2 != c(0,0) ]

data0 <- read.table('luggage.txt',header=T)
data1<- data.frame(cbind(data[,1], data[,3]) ) #male
data2<-   #female

n1 <- dim(data1)[1] # statistical units of the first group
n2 <- dim(data2)[1] # statistical units of the second group
p  <- dim(data1)[2] 

# sample mean, covariance matrices and matrix Spooled:
data1.mean <- sapply(data1, mean) 
data2.mean <- sapply(data2, mean) 
data1.cov <- cov(data1)
data2.cov <- cov(data2)
Sp <- ((n1-1)*data1.cov + (n2-1)*data2.cov)/(n1+n2-2) # Spooled matrix 
Spinv <- solve(Sp)


# check assumption of the test:

# 1. Gaussianity of each population (oppure vedere da pacchetto mvn)
mvn(data1)$multivariateNormality
mvn(data2)$multivariateNormality
# High (> 0.1): there is no statistical evidence to reject H0 (I can assume Gaussianity of data)
# Very low (< 0.05): there is statistical evidence to reject H0 (I can NOT assume Gaussianity of data)
#
# 2. Homogeneity of Covariances
cov1 <- cov(data1)
cov2 <- cov(data2)
#  I need similar covariances matrix
par(mfrow=c(1,2))
image(cov1)
image(cov2)
#oppure
bartlett.test(data1, data2) 


# perform the test (of level 1- alpha) (H0: data1.mean-data2.mean = delta0 VS H1: data1.mean-data2.mean != delta0)
alpha <- 0.05
delta.0 <- c(0,0, ...) 
T2 <- n1*n2/(n1+n2) * (data1.mean-data2.mean-delta.0) %*% Spinv %*% (data1.mean-data2.mean-delta.0)

cfr.fisher <- (p*(n1+n2-2)/(n1+n2-1-p))*qf(1-alpha,p,n1+n2-1-p)

# Check if mu0 is outside the rejection region {T2>cfr.fisher}:
ifelse(T2 < cfr.fisher,
       "TRUE: there is no statistical evidence to reject H0", 
       "FALSE: there is statistical evidence to reject H0")
# True: there is no statistical evidence to reject H0 at level alpha (the statistic is outside the rejection region).
# False: mu0 is inside the rejection region so there is statistical evidence to reject H0 at level alpha


# compute pvalue:
pvalue <- 1 - pf(T2/(p*(n1+n2-2)/(n1+n2-1-p)), p, n1+n2-1-p)
pvalue
# High (> 0.1): there is no statistical evidence to reject H0 
# Very low (< 0.05): there is statistical evidence to reject H0 


##CONFIDENCE REGION FOR THE DIFFERENCE IN MEAN OF THE COMPONENTS (RELATIVE)
# Characterize the ellipse:
eigen(Sp)$vector

# Radius
r <- sqrt(cfr.fisher)

# Length of the semi-axes
r*sqrt(eigen(Sp)$values*(1/n1+1/n2))

par(mfrow=c(1,2))
plot(data1 , col=rainbow(2)[1], asp=1, pch=16, main='Original data and groups')
points(data2, col=rainbow(2)[2],asp=1, pch=16)

xlimit <- c((data1.mean-data2.mean-sqrt(cfr.fisher))[1], (data1.mean-data2.mean+sqrt(cfr.fisher))[1])
ylimit <- c((data1.mean-data2.mean-sqrt(cfr.fisher))[2], (data1.mean-data2.mean+sqrt(cfr.fisher))[2])

par(mfrow=c(1,2))
plot(satellite , col=sata+1, asp=1, pch=16, main='Original data and groups')
plot(satellite, xlim=xlimit, ylim=ylimit, pch='', asp=1, 
     main='Elliptic region for the mean diff. (data1 - data2)')
# confidence region and sample mean in blue
ellipse(center=data1.mean-data2.mean, shape=Sp*(1/n1+1/n2), radius=sqrt(cfr.fisher), 
        lwd=2, col='blue')

# t2 simultaneous confidence intervals 
alpha <- ...
p <- ...
cfr.fisher <- (p*(n1+n2-2)/(n1+n2-1-p))*qf(1-alpha,p,n1+n2-1-p)
IC.T2.X1 <- c(data1.mean[1]-data2.mean[1]-sqrt(cfr.fisher*Sp[1,1]*(1/n1+1/n2)),data1.mean[1]-data2.mean[1], data1.mean[1]-data2.mean[1]+sqrt(cfr.fisher*Sp[1,1]*(1/n1+1/n2)) )
IC.T2.X2 <- c(data1.mean[2]-data2.mean[2]-sqrt(cfr.fisher*Sp[2,2]*(1/n1+1/n2)),data1.mean[2]-data2.mean[2], data1.mean[2]-data2.mean[2]+sqrt(cfr.fisher*Sp[2,2]*(1/n1+1/n2)) )
IC.T2 <- rbind(IC.T2.X1, IC.T2.X2)


# Bonferroni intervals for the mean (not equal variances)
k <- p # number of intervals I want to compute (set in advance) 
cfr.t <- qt(1-alpha/(2*k), n1+n2-2)
Bf <- cbind(inf = data1.mean-data2.mean - cfr.t*sqrt(diag(data1.cov/n1 + data2.cov/n2)),
            center = data1.mean-data2.mean, 
            sup = data1.mean-data2.mean + cfr.t*sqrt(diag(data1.cov/n1 + data2.cov/n2)))
Bf

# Bonferroni intervals for the mean (equal variances, we use Sp) -> potentially more stable 
k <- p # number of intervals I want to compute (set in advance)  
cfr.t <- qt(1-alpha/(2*k), n1+n2-2)
Bf <- cbind(inf = data1.mean-data2.mean - cfr.t*sqrt(diag(Sp)*(1/n1 + 1/n2)),
            center = data1.mean-data2.mean, 
            sup =  data1.mean-data2.mean + cfr.t*sqrt(diag(Sp)*(1/n1 + 1/n2)))
Bf




#### TEST FOR THE MEAN - INDEPENDENT BERNOULLI POPULATIONS - one-at-the-time ####

# Suppose you have two Independent Bernoulli Populations 
# $X_1 sim Be(mu_1), X_2 sim Be(mu_2)$. 

# We want to perform this test at level $alpha=0.05$
# [ H_{0i}: mu_{1i} = mu_{2i}  vs  H_{1i}: mu_{1i} neq mu_2i   forall i in 1,dots,p]

p <- dim(data)[2]

data1 <- data[,c(, , , , )] #
data2 <- data[,c(, , , , )] #

n1 <- dim(data1)[1]
n2 <- dim(data2)[1]

# compute the sample mean of the two populations:
x.mean1 <- sapply(data1, mean) 
x.mean2 <- sapply(data2, mean)

# set alpha (significance level for each of the test)
alpha <- 0.05 #

p.hat <- (x.mean1*n1+x.mean2*n2)/(n1+n2) 
x.var <- (p.hat*(1-p.hat))

z.i <- (x.mean1-x.mean2)/sqrt(x.var*(1/n1+1/n2))
p.i <- ifelse(z.i<0, 2*pnorm(z.i),2*(1-pnorm(z.i))) 

# Check if alpha is greater than the pvalue
for (i in 1:p) {
  ifelse(p.i < alpha,
         "TRUE: there is no statistical evidence to reject H0", 
         "FALSE: there is statistical evidence to reject H0")
}
# True: there is no statistical evidence to reject H0 at level alpha (the statistic is outside the rejection region).
# False: mu0 is inside the rejection region so there is statistical evidence to reject H0 at level alpha

# for which components I cannot reject the null hypothesis:
which(p.i<alpha) 



#### TEST FOR THE MEAN - INDEPENDENT BERNOULLI POPULATIONS - Simultaneous Bonferroni CI####

# Suppose you have two Independent Bernoulli Populations 
# $X_1 ~ Be(mu_1), X_2 ~ Be(mu_2). 

# We want to perform this test at level $alpha=0.05$
# [ H_{0i}: mu_{i} = mu_{2i} vs H_{1i}: mu_{1i} != mu_2i for all i in 1:p]

p <- dim(data)[2]
data1 <- data[,c(, , , , )] #
data2 <- data[,c(, , , , )] #
k <- p #

n1 <- dim(data1)[1]
n2 <- dim(data2)[1]

# compute the sample mean of the two populations:
x.mean1 <- sapply(data1, mean) 
x.mean2 <- sapply(data2, mean)

# set alpha (significance level for each of the test)
alpha <- 0.05 #
alpha <- alpha/k 

p.hat <- (x.mean1*n1+x.mean2*n2)/(n1+n2) 
x.var <- (p.hat*(1-p.hat))

z.i <- (x.mean1-x.mean2)/sqrt(x.var*(1/n1+1/n2))
p.i <- ifelse(z.i<0, 2*pnorm(z.i),2*(1-pnorm(z.i))) 

# Check if alpha is greater than the pvalue
for (i in 1:p) {
  ifelse(p.i < alpha,
         "TRUE: there is no statistical evidence to reject H0", 
         "FALSE: there is statistical evidence to reject H0")
}
# True: there is no statistical evidence to reject H0 at level alpha (the statistic is outside the rejection region).
# False: mu0 is inside the rejection region so there is statistical evidence to reject H0 at level alpha

# for which components I cannot reject the null hypothesis:
which(p.i<alpha) 


# if you have already computed the one-at-the-time CI, you can simply adjust the pvalues by:
p.Bf <- p.adjust(p.i, method = 'bonferroni') 


#### SIMULTANEOUS WHEN P>2 ####

## SIMULTANEOUS T2 CI for the difference in mean (of level 1- alpha):
cfr.fisher <- (p*(n1+n2-2)/(n1+n2-1-p))*qf(1-alpha,p,n1+n2-1-p)
IC.T2 <- NULL
for (i in 1:p) {
  IC.T2.temp <- c(data1.mean[i]-data2.mean[i]-sqrt(cfr.fisher*Sp[i,i]*(1/n1+1/n2)), data1.mean[i]-data2.mean[i]+sqrt(cfr.fisher*Sp[i,i]*(1/n1+1/n2)) )
  IC.T2 <- rbind(IC.T2, IC.T2.temp)
}
dimnames(IC.T2)[[2]] <- c('inf','sup')     
IC.T2
# check if the CI's contain delta.0


## BONFERRONI CI (of level 1-alpha) for the difference of each component
k <- p
cfr.t <- qt(1-alpha/(2*k), n1+n2-2)
IC.T2 <- NULL
for (i in 1:p) {
  IC.T2.temp <- c(data1.mean[i]-data2.mean[i]-cfr.t*sqrt(Sp[i,i]*(1/n1+1/n2)), data1.mean[i]-data2.mean[i]+cfr.t*sqrt(Sp[i,i]*(1/n1+1/n2))) 
  IC.T2 <- rbind(IC.T2, IC.T2.temp)
}
dimnames(IC.T2)[[2]] <- c('inf','sup')     
IC.T2
# check if the Bonf-CI's contain delta.0

## if p>2 we can plot the Sim-CI with Bonferroni correction as: 
matplot(1:k,1:k,pch='',ylim=range(data),
        xlab='Variables',ylab='T2 for a component', 
        main='Simultaneous T2 conf. int. for the components')
for(i in 1:k) segments(i,T2[i,1],i,T2[i,3],lwd=3,col=i) # Sim-CI
points(1:k, T2[,2], pch=16, col=1:k)
for(i in 1:k) segments(i,Bf[i,1],i,Bf[i,3],lwd=2,col=i) # Bonferroni
points(1:k, Bf[,2], pch=16, col=1:k)
points(1:k, Bf[,1], pch='-', col=1:k)
points(1:k, Bf[,3], pch='-', col=1:k)
# Is mu0 inside the rectangular region? We add it to the plot
points(1:k, mu0, lwd=3, col='orange')






























#### (M)ANOVA scheme: ####

# p = number of characteristics (X)
# g = number of different types of factor.1
# b = number of different types of factor.2 (if it exists)

# ANOVA:      p = 1
# MANOVA:     p > 1

# One-Way: 	  only factor.1
# Two-Ways:   factor.1 and factor.2




#### One-Way ANOVA ####
# L'analisi della varianza a una via è un metodo statistico per testare 
# l'ipotesi nulla (H0) in base alla quale le medie di tre o più popolazioni 
# sarebbero uguali, contro l'ipotesi alternativa (Ha)



p <- 1
# setting dataset:
# labels:
factor1 <- factor(data$......) #
# var numerica:
x <- data$.....        #

# boxplot of data divided by labels:
plot(factor1, x, xlab='labels', ylab='x', col='grey85', main='Dataset')
# look at if there are some differences in terms of variability between the different groups. 

# set important quantities:
n <- dim(data)[1]         # number of observations
ng <- table(factor1)      # number of observations in each group
treat <- levels(factor1)  # levels of the treatment
g <- length(treat)        # number of levels/groups/labels


# We are building this model:
# [ x_{ij} = mu + tau_i + varepsilon_{ij} with varepsilon_{ij} ~ N(0, sigma^2) ]
# where
# mu : overall mean of the x;
# tau_i: effect of the treatment i;
# varepsilon_{ij}: additive Gaussian random noise.
# 
# We want to perform this test to see if the treatment given by the labels has an effect on x:
# [ H_0: tau_1=tau_2=tau_g=0 vs H_1: (H_0)^C ]
# namely:
# $H_0$: the treatment has no effect;
# $H_1$: at least one treatment has an effect;
# 
# Model assumptions:
# - Gaussian distribution of the error;
# - Homoschedasticity.


# Verify assumption of the model:
# 
# - Normality in each group:
# 
# we are in ANOVA setting, so we perform g shapiro tests, one for each group:
pvalue <- NULL
for (i in 1:g) {
  pval <- shapiro.test(x[factor1==treat[i]])$p
  pvalue <- c(pvalue, pval)
}
pvalue
# If pvalues are large, I can accept the hypothesis of Gaussianity of data.

# - same covariance structure
#
# I can perform the Bartlett test (that relies on Gaussianity assumption) to check homogeneity of variances. 
# Namely, the test I'm performing is the following:
# [ H_0: sigma_1 = sigma_2 = dots = sigma_g  vs  H_1: exists i,j s.t. sigma_i neq sigma_j]

bartlett.test(x, factor1)
# if pvalue is high, I don't have enough evidence to reject the null hypothesis 
# of homoschedasticity of data, so I can assume the homogeneity of variances.
# pvalue VERY SMALL (<alpha): we can reject the null hypothesis, I can NOT assume Homoschedasticity
# pvalue VERY LARGE (>alpha): we cannot reject the null hypothesis, I can assume Homoschedasticity


# Now I can perform the One-Way ANOVA:
fit <- aov(x ~ factor1)     # aov( variable of interest ~ treatment )
summary(fit)

# NOTE: HOW TO READ THE SUMMARY:
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ AOV summary ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#              Df   Sum Sq      Mean Sq      F value     Pr(>F)
#  treat      (g-1) SStreat  SStreat/(g-1)  Fstatistic  p-value [H0: tau.i=0 for every i]
#  Residuals  (n-g) SSres     SSres/(n-g)
# ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# ##
#
# SStreat: component of the variance, between variability (g-1 degrees of freedom).
# SSres: component of the variance, within variability (n-g degrees of freedom).
# Fstat: SStreat*(n-g) / SSres*(g-1)
# To see if the treatment has an effect on x you have to look at the pvalue of the test Pr(>F)
#
# if pvalue is small: we reject H0, so we have evidence to say that the treatment has an effect on x;
# if pvalue is large: we cannot reject H0, so we don't have evidence to say that the treatment has an effect on x;


##### stime ---------------------------------
# Estimate variances
W <- sum(fit$residuals^2)  # SS_res
var <- W/(n-g)     # SS_res/gdl(res)   # controlla n!!
var
# Estimate the great mean mu:
m <- mean(data[,1])
m
# Estimate tau.i:
tau1  <- mean(data[factor1=='GRUPPO1',1]) - m  # tau.1
tau2  <- mean(data[factor1=='GRUPPO2',1]) - m  # tau.2
# tau3  <- mean(data[factor1=='GRUPPO3',1]) - m  # tau.3 #solo se hai 3 gruppi nel factor
tau <- cbind(tau1,tau2,tau3)
tau 
# point-wise estimate of the mean:
m1 <- m + tau1
m2  <- m + tau2
# m3  <- m + tau3
mu <- cbind(m1,m2,m3)
mu 

##### Bonf stuffs with anova ---------------


# ONE CASE PARTICOLARE: quando ho factor1 numerico e non categorico
# IC sia per le medie dei gruppi sia per la varianza pooled

# Using BONFERRONI's INEQUALITY estimate through BILATERAL confidence
# intervals (with global confidence 95%) the means and variances of the
# subpopulations associated with the chosen model.

N <- dim(data)[1]
### length(levels(factor1)) NON FUNZIONA lab 7
g <- length(levels(factor1)) # inserire factor1 se si parte da una one way o il factor
# rimasto dal modello ridotto 
DF <- N-g
alpha <- 0.05
k <- g+1 # il +1 si riferisce a quando c'è da fare CI per la varianza
qT <- qt(1-alpha/(2*k), DF)
qCinf <- qchisq(1 - alpha / (2*k), DF)
qCsup <- qchisq(alpha / (2*k), DF)
Spooled <- (t(fit$res) %*% fit$res)/DF  
Spooled
# Tre gruppi
m1 <- mean(data[which(factor1==levels(factor1)[1]),1])
m2 <- mean(data[which(factor1==levels(factor1)[2]),1])
m3 <- mean(data[which(factor1==levels(factor1)[3]),1])
medie <- c(m1,m2,m3)
ng <- c(length(which(factor1==levels(factor1)[1])),length(which(factor1==levels(factor1)[2])),length(which(factor1==levels(factor1)[3])))

BF    <- rbind(cbind(inf=medie - sqrt(as.vector(Spooled) / ng) * qT,
                     sup=medie + sqrt(as.vector(Spooled) / ng) * qT),
               c(inf=Spooled * DF / qCinf,
                 sup=Spooled * DF / qCsup))
BF








#SECOND CASE: Bonferroni by my code!!
# CI for all the difference in mean 

# Now we want to see which treatment is responsible for this effect. 
# So we perform g*(g-1)/2 tests simultaneously, one for each couple of treatments.
# We use BONFERRONI approach.

n=274  #n <- dim(data)[1] 
g=3  #quanti label ha il factor    ovvero  g <- length(treat) # number of levels/groups/labels

k <- g*(g-1)/2    # +1 se chiede sia media che varianza number of comparisons
alpha <- 0.05     # overall level #

Mediag <- tapply(x, factor1, mean) 
SSres <- sum(residuals(fit)^2)
S <- SSres/(n-g)
ng <- table(factor1)      # number of observations in each group
treat=levels(factor1)


# CONFIDENCE INTERVALS for all the differences in mean at level 1-alpha - BONFERRONI
ICrange=NULL
for(i in 1:(g-1)) {
  for(j in (i+1):g) {
    print(paste(treat[i],"-",treat[j]))        
    print(as.numeric(c(Mediag[i]-Mediag[j] - qt(1-alpha/(2*k), n-g) * 
                         sqrt( S * ( 1/ng[i] + 1/ng[j] )),
                       Mediag[i]-Mediag[j] + qt(1-alpha/(2*k), n-g) * 
                         sqrt( S * ( 1/ng[i] + 1/ng[j] )))))
    ICrange=rbind(ICrange,as.numeric(c(Mediag[i]-Mediag[j] - qt(1-alpha/(2*k), n-g) * 
                                         sqrt( S * ( 1/ng[i] + 1/ng[j] )),
                                       Mediag[i]-Mediag[j] + qt(1-alpha/(2*k), n-g) * 
                                         sqrt( S * ( 1/ng[i] + 1/ng[j] )))))
  }}

#I have to check for which couple if 0 is inside the interval, in this case i can assume
#that the treatment has no effect of the respective variable


# plot CI for all the differences in mean - BONFERRONI
par(mfrow=c(1,2))
plot(factor1, x, xlab='treatment', ylab='x', col = rainbow(g), las=2)

h <- 1
plot(c(1,g*(g-1)/2),range(ICrange), pch='',
     xlab='pairs treat', ylab='Conf. Int. tau x')
for(i in 1:(g-1)) {
  for(j in (i+1):g) {
    ind <- (i-1)*g-i*(i-1)/2+(j-i)
    lines (c(h,h), c(ICrange[ind,1],ICrange[ind,2]), col='grey55'); 
    points(h, Mediag[i]-Mediag[j], pch=16, col='grey55'); 
    points(h, ICrange[ind,1], col=rainbow(g)[j], pch=16); 
    points(h, ICrange[ind,2], col=rainbow(g)[i], pch=16); 
    h <- h+1
  }}
abline(h=0)
# Note: if the interval do not contains 0, 
# there is a strong statistical difference between the effects of the two treatments.









#THIRD CASE: ANOVA BONFERRONI PER LA MEDIA nel caso 1-way:   (k = #gruppi)
# IC solo per le medie dei gruppi , senza considerare la varianza pooled

k <- g
S <- sum(residuals(fit)^2)/(n-g)

alpha<- ... #(1-livello!)    # overall level

M1  <- mean(x[data$GRUPPO=='...'])# cambia GRUPPO
M2  <- mean(x[data$GRUPPO=='...'])# cambia GRUPPO
M3  <- mean(x[data$GRUPPO=='...'])# cambia GRUPPO
#FINO Al g-esimo
label <- levels(factor1)
n1 <- length(x[data$GRUPPO=='...']) #cambia GRUPPO
n2 <- length(x[data$GRUPPO=='...']) #cambia GRUPPO
n3 <- length(x[data$GRUPPO=='...']) #cambia GRUPPO
#FINO Al g-esimo
t <- qt(1-alpha/(2*k),n-g)

# Conf int for the means
ICB1<-data.frame(L=M1-sqrt(S*(1/n1))*t,C=M1,U=M1+sqrt(S/n1)*t)
ICB2<-data.frame(L=M2-sqrt(S*(1/n2))*t,C=M2,U=M2+sqrt(S/n2)*t)
ICB3<-data.frame(L=M3-sqrt(S*(1/n3))*t,C=M3,U=M3+sqrt(S/n3)*t)
ICB<-data.frame(rbind(ICB1,ICB2,ICB3))
ICB






# CONFIDENCE INTERVALS for variances (è sempre la stessa)

chi_u <- qchisq(alpha/(2*k),n-g)
chi_l <- qchisq(1-alpha/(2*k),n-g)
ICBV <- data.frame(L=(n-g)*S/chi_l, C=S,U=(n-g)*S/chi_u)
ICBV






# BONFERRONI AND BENJAMINI CORRECTIONS ON PVALUES
# One-at-the-time CI:
# lower triangular matrix: lower bound of CI (i treat - j treat)
# upper triangular matrix: upper bound of CI (i treat - j treat)
# diagonal is 0 (i treat - i treat = 0)

Auni <- matrix(0,g,g)
for(i in 1:g) {
  for(j in i:g) {
    Auni[i,j] <- Mediag[i]-Mediag[j] + qt(1-alpha/2, n-g) * 
      sqrt( S * ( 1/ng[i] + 1/ng[j] ) )}
  for(j in 1:i) {
    Auni[i,j] <- Mediag[j]-Mediag[i] - qt(1-alpha/2, n-g) * 
      sqrt( S * ( 1/ng[i] + 1/ng[j] ) )}
  Auni[i,i]     <- 0
}
#Plot CI of the differences WITHOUT corrections
x11( width=14, height=7)
par(mfrow=c(1,2))
h <- 1
plot(c(1,g*(g-1)/2),range(Auni), pch='', xlab='pairs treat', 
     ylab='CI delta x', main='Univariate Conf. Int.', col='grey55')

for(i in 1:g) {
  for(j in (i+1):g) {lines (c(h,h), c(Auni[i,j],Auni[j,i])); 
    points(h, Mediag[i]-Mediag[j], pch=16, col='grey55'); 
    points(h, Auni[i,j], col=rainbow(g)[i], pch=16); 
    points(h, Auni[j,i], col=rainbow(g)[j], pch=16); 
    h <- h+1
  }}
abline(h=0)





# One-at-the-time CI pvalue (NO CORRECTIONS): 
P <- matrix(0,g,g)
for(i in 1:g) {
  for(j in i:g) {
    P[i,j] <- (1-pt(abs((Mediag[i]-Mediag[j]) / 
                          sqrt( S * ( 1/ng[i] + 1/ng[j] ) ) ), n-g))*2}
  for(j in 1:i) {
    P[i,j] <- (1-pt(abs((Mediag[i]-Mediag[j]) / 
                          sqrt( S * ( 1/ng[i] + 1/ng[j] ) ) ), n-g))*2}
  P[i,i]     <- 0
}

p <- NULL
for (i in 1:g){
  temp <- P[i,(i+1):g]
  p <- c(p, temp)
}
p # vector of pvalues





# Bonferroni corrections on pvalues:
p.bonf <- p.adjust(p, 'bonf')
# Indexes of the couples for which Bonf correction tells us that there is a 
# significant difference at level alpha=5%:
which(p.bonf<alpha) 





# Benjamini-Hockberg corrections on pvalues:
p.fdr <- p.adjust(p, 'fdr')
# Indexes of the couples for which BH correction tells us that there is a 
# significant difference at level alpha=5%:
which(p.fdr<alpha)




# PLOTTARE I VARI P-VALUES
#
# plot(1:15, p, ylim=c(0,1), type='b', pch=16, col='grey55', xlab='pairs treat',
#      main='P-values')
# abline(h=alpha, lty=2)
# 
# # Bonferroni correction
# p.bonf <- p.adjust(p, 'bonf') 
# lines(1:15, p.bonf, col='blue', pch=16, type='b')
# 
# # Correction according to the false discovery rate (Benjamini-Hockberg)
# p.fdr <- p.adjust(p, 'fdr')
# lines(1:15, p.fdr, col='red', pch=16, type='b')
# 
# legend('topleft', c('Not corr.', 'Bonf.', 'BH'), col=c('grey55', 'blue', 'red'), pch=16)
# 
# which(p.bonf<alpha)
# which(p.fdr<alpha)








#### One-Way MANOVA ####
# In statistica, l'analisi multivariata della varianza (MANOVA) 
# è una procedura per confrontare le medie campionarie multivariate.

# setting dataset:
# labels:
factor1 <- factor(data$...) #
# var numerica:
x <- data[, c(2, 3, 4)]  # Sostituisci 2, 3, 4 con gli indici effettivi delle colonne delle variabili numeriche

p <- dim(x)[2]

# set important variables: 
treat <- levels(factor1)
g <- length(treat)
n <- dim(data)[1]         # number of observations
ng <- table(factor1)      # number of observations in each group

n1=ng[1]
n2=ng[2]

#id of measurement with treatment i-th
i1=which(factor1==treat[1])
i2=which(factor1==treat[2])



## graphical exploration:

# scatterplot

colore <- rep(rainbow(g), ng)
pairs(x, col = colore, pch=16)

#BoxPlot
par(mfrow=c(1,dim(x)[2]))
for(i in 1:dim(x)[2]){
  boxplot(x[,i]~factor1, main=colnames(x)[i], ylim=c(min(x[,i]),max(x[,i])), col = rainbow(g))
}


# We are building this model:
# [ x_{ij} = mu + tau_i + varepsilon_{ij} with varepsilon_{ij} ~ N(0,sigma^2), x_{ij}, mu, tau_i in {R}^p ]
# where
# mu : overall mean of the x;
# tau_i: treatment effect of the i-th group;
# varepsilon_{ij}: additive Gaussian random noise.

# We want to perform this test to see if the treatment given by the labels has an effect on x: 
# [ H_0: tau_1=tau_2=...=tau_g=(0, ..., 0)  vs  H_1: (H_0)^C ]
# namely: 
# H_0 The membership to a label hasn't any significant effect on the mean of x_{ij} (in any direction of {R}^p) 
# H_1: There exists at least one direction in {R}^p along which at least two labels
###  have some feature significantly different


# First of all I have to verify the Hypothesis

# 1) normality (multivariate) in each group (g tests)
Ps <- NULL
for (i in 1:g){
  Ps <- c(Ps, mvn(x[factor1==levels(factor1)[i],],)$multivariateNormality$`p value`)
}
Ps
# High (> 0.1): there is no statistical evidence to reject H0 (So I can assume Gaussianity)
# Very low (< 0.05): there is statistical evidence to reject H0 


# 2) same covariance structure (= same covariance matrix Sigma)
S <- cov(x)
S1 <- cov(x[i1,])
S2 <- cov(x[i2,])

# I can only check qualitatively:
par(mfrow=c(1,g))
image(S1, col=heat.colors(100),main='Cov. S1', asp=1, axes = FALSE,
      breaks = quantile(rbind(S1,S2,S3), (0:100)/100, na.rm=TRUE))
image(S2, col=heat.colors(100),main='Cov. S2', asp=1, axes = FALSE,
      breaks = quantile(rbind(S1,S2,S3), (0:100)/100, na.rm=TRUE))
#... image(S3, ...)

# dani vois round(S1, digits=2)

# Fitting the model:
fit <- manova(as.matrix(x) ~ factor1)
summary.manova(fit,test="Wilks") # you can choose different types of lambda

summary.manova(fit,test="Wilks")$p

# if pvalue is small: we reject H0, so we have evidence to say that the treatment has an effect on x;
# if pvalue is large: we cannot reject H0, so we don't have evidence to say that the treatment has an effect on x;


#If I want to see which variable is affected by the treatments:
summary.aov(fit)

#Note that this analysis does NOT say:
#a) which group differ
#b) with respect to which variables the groups in (a) differ.






##### Bonferroni stuff in manova ---------------

#ONE CASE: BF for the difference in mean
#To see which treatment has an effect on which variables I perform MANY Bonferroni
alpha <- 0.05
k <- p*g*(g-1)/2 # number of possible couples between the g levels
qT <- qt(1-alpha/(2*k), n-g)
W <- summary.manova(fit)$SS$Residuals
m <- sapply(x,mean) # estimates mu
m1 <- sapply(x[i1,],mean) # estimates mu.1=mu+tau.1
m2 <- sapply(x[i2,],mean) # estimates mu.2=mu+tau.2
#m3 <- sapply(x[i3,],mean) # estimates mu.3=mu+tau.3

inf12 <- m1-m2 - qT * sqrt( diag(W)/(n-g) * (1/n1+1/n2) )
sup12 <- m1-m2 + qT * sqrt( diag(W)/(n-g) * (1/n1+1/n2) )

inf13 <- m1-m3 - qT * sqrt( diag(W)/(n-g) * (1/n2+1/n3) )
sup13 <- m1-m3 + qT * sqrt( diag(W)/(n-g) * (1/n1+1/n3) )

inf23 <- m2-m3 - qT * sqrt( diag(W)/(n-g) * (1/n2+1/n3) )
sup23 <- m2-m3 + qT * sqrt( diag(W)/(n-g) * (1/n2+1/n3) )

CI <- list(Treat1vs2=cbind(inf12, sup12),
           Treat1vs3=cbind(inf13, sup13),
           Treat2vs3=cbind(inf23, sup23))
CI

#I have to check for which couple if 0 is inside the interval, in this case i can assume
#that the treatment has no effect of the respective variable

#Plotting the CI
par(mfrow=c(2,p))
for(i in 1:p) {
  boxplot(x[,i]~factor1, main=colnames(x)[i], ylim=range(x[,i]), col = rainbow(g)[i])
}

mg <- rbind(m1,m2,...)
temp.min <- min(inf12, inf13, inf23, ...)
temp.max <- min(sup12, sup13, sup23, ...)


for(k in 1:p){
  plot(c(1,g*(g-1)/2),ylim=c(temp.min, temp.max), xlim=c(1,g), pch='', 
       xlab='pairs treat', ylab=paste('CI tau',k), 
       main=paste('CI tau',colnames(x)[k]))
  
  lines (c(1,1), c(CI[[1]][k,1],CI[[1]][k,2])); 
  points(1, mg[1,k]-mg[2,k], pch=16); 
  points(1, CI[[1]][k,1], col=rainbow(g)[2], pch=16); 
  points(1, CI[[1]][k,2], col=rainbow(g)[1], pch=16);  
  lines (c(2,2), c(CI[[2]][k,1],CI[[2]][k,2])); 
  points(2, mg[1,k]-mg[3,k], pch=16);
  points(2, CI[[2]][k,1], col=rainbow(g)[3], pch=16); 
  points(2, CI[[2]][k,2], col=rainbow(g)[1], pch=16);
  lines (c(3,3), c(CI[[3]][k,1],CI[[3]][k,2])); 
  points(3, mg[2,k]-mg[3,k], pch=16);
  points(3, CI[[3]][k,1], col=rainbow(g)[3], pch=16); 
  points(3, CI[[3]][k,2], col=rainbow(g)[2], pch=16);  
  abline(h=0) 
}
# INTERVALLI T2 per la differenza delle medie
qF <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
inf12 <- m1-m2 - qF * sqrt( diag(W)/(n-g) * (1/n1+1/n2) )
sup12 <- m1-m2 + qF * sqrt( diag(W)/(n-g) * (1/n1+1/n2) )

CIt <- cbind(inf12, sup12)  
CIt







# SECOND CASE: BF for the mean
# MANOVA BONFERRONI PER LA MEDIA nel caso 1-way:   (k = #gruppi)

treat <- levels(factor1)
g <- length(treat)
k <- p*g
W <- summary.manova(fit)$SS$Residuals


alpha<- ... #(1-livello!)    # overall level #*

m1 <- sapply(x[i1,],mean)
m2 <- sapply(x[i2,],mean)
m3 <- sapply(x[i3,],mean)

#FINO Al g-esimo

label <- levels(factor1)
n1 <- dim(x[i1,])[1]
n2 <- dim(x[i2,])[1]
n3 <- dim(x[i3,])[1]
#FINO Al g-esimo
t <- qt(1-alpha/(2*k),n-g)

# Conf int for the means
ICB11<-data.frame(L=m1[1]-sqrt(diag(W)[1]*(1/n1))*t,C=m1[1],U=M1+sqrt(diag(W)[1]/n1)*t)
ICB12<-data.frame(L=m1[2]-sqrt(diag(W)[2]*(1/n1))*t,C=m1[2],U=M1+sqrt(diag(W)[2]/n1)*t)
#ICB13...
ICB21<-data.frame(L=m2[1]-sqrt(diag(W)[1]*(1/n2))*t,C=m2[1],U=M1+sqrt(diag(W)[1]/n2)*t)
ICB22<-data.frame(L=m2[2]-sqrt(diag(W)[2]*(1/n2))*t,C=m2[2],U=M1+sqrt(diag(W)[2]/n2)*t)
#ICB23...
ICB<-data.frame(rbind(ICB11, ICB12,ICB21, ICB22 , ...))
ICB






# BONFERRONI FOR VARIANCES WITHIN EACH GROUP
chi_u <- qchisq(alpha/(2*k),n-g)
chi_l <- qchisq(1-alpha/(2*k),n-g)
ICBV1 <- data.frame(L=(n-g)*diag(W)[1]/chi_l,C=S,U=(n-g)*diag(W)[1]/chi_u)  #Var covariata 1
ICBV2 <- data.frame(L=(n-g)*diag(W)[2]/chi_l,C=S,U=(n-g)*diag(W)[2]/chi_u)  #Var covariata 2
ICBV <- data.frame(rbind(ICBV1, ICBV2, ...))
ICBV





#### Two-Ways ANOVA ####

p <- 1

# setting dataset:
# labels:
factor1 <- factor(data$......) #
factor2 <- factor(data$......) #
factor12 <- factor(paste(factor1, factor2, sep='-'))

# var numerica:
x <- data$.....        #

g <- length(levels(factor1))   # number of factor.1 
b <- length(levels(factor2))   # number of factor.2 
gb <- length(levels(factor12)) # number of factor.12 
n <- length(x)/(g*b)           # number of data

M         <- mean(x)                     # overall mean
Mfactor1  <- tapply(x, factor1, mean)    # mean for the types of factor1
Mfactor2  <- tapply(x, factor2, mean)    # mean for the types of factor2
Mfactor12 <- tapply(x, factor12, mean)   # mean for the all possible combinations factor1+factor2


# The complete model (with interaction) is: 
#   [ X_{ijk} = mu + tau_i + beta_j + gamma_{ij} + varepsilon_{ijk};  varepsilon_{ijk} ~ N(0,sigma^2) ]
# where: i=1,2,.....  effect factor1,  and j=1,2,..... effect factor2 


# Check the ASSUMPTIONS of the model:

# 1) Gaussianity of each interaction/combination of factors:

Ps12 <- NULL
for (i in 1:gb){
  Ps12 <- c(Ps12, shapiro.test(x[factor12 == levels(factor12)[i]])$p.value)
}
Ps12



# 2) Homogeneity of variances 
bartlett.test(x, factor12)
pvalue.bartlett <- bartlett.test(x, factor12)$p.value
pvalue.bartlett 
# pvalue VERY SMALL (<alpha): we can reject the null hypothesis, I can NOT assume Homoschedasticity
# pvalue VERY LARGE (>alpha): we cannot reject the null hypothesis, I can assume Homoschedasticity

## TEST for the significance of the interaction factor, namely:
## H0: gamma_(i,j) =0 vs H1: gamma(i,j) =/ 0

SSinter <- 0 
for(i in 1:g){
  for(j in 1:b){
    SSinter <- SSinter + (Mfactor12[g*(i-1)+j] - Mfactor1[i] - Mfactor2[j] + M)^2
  }
}
SSinter <- as.numeric(n*SSinter)
SSfactor1 <- sum(n*b*(Mfactor1 - M)^2)            # or from the summary   
SSfactor2  <- sum(n*g*(Mfactor2  - M)^2) 
SSres   <- sum((x - M)^2) - (SSfactor1+SSfactor2+SSinter)
Fcomp <- (SSinter/((g-1)*(b-1)))/(SSres/(g*b*(n-1)))
Pcomp      <- 1 - pf(Fcomp, (g-1)*(b-1), g*b*(n-1)) 
Pcomp
# or using the command:
attach(data)
# fit.aov.int <- aov("prediction"~ "group1" + "group2" + "group1":"group2")
fit.aov.int <- aov(x ~ factor1 + factor2 + factor1:factor2)
summary.aov(fit.aov.int)

## TEST for the global significance of the two factors:
SSfactor1 <- sum(n*b*(Mfactor1 - M)^2)            # or from the summary   
SSfactor2  <- sum(n*g*(Mfactor2  - M)^2)          # or from the summary
SSres   <- sum((x - M)^2) - (SSfactor1+SSfactor2)   # or from the summary

Ftot      <- ( (SSfactor1 + SSfactor2) / ((g-1)+(b-1)))/(SSres / (n*g*b-g-b+1))
Ptot      <- 1 - pf(Ftot, (g-1)+(b-1), n*g*b-g-b+1) # attention to the dgf!
Ptot
# pvalue VERY SMALL (<alpha): we can reject the null hypothesis, globally the factor effects ARE significant.
# pvalue VERY LARGE (>alpha): we cannot reject the null hypothesis, globally the factor effects are NOT significant.


##### stime ----------------


# Estimate variances
W <- sum(fit.aov.int$residuals^2)  # SS_res
var <- W/(n-g-b+1)     # SS_res/gdl(res)
var
# These are the coefficients of the model
fit.aov$coefficients

# Estimate the great mean mu:
m <- mean(x)
# Estimate tau.i, beta.j:
tau1  <- mean(data[factor1=='GRUPPO11',1]) - m  # tau.1
tau2  <- mean(data[factor1=='GRUPPO12',1]) - m  # tau.2
beta1 <- mean(data[factor2=='GRUPPO21',1]) - m  # beta.1
beta2  <- mean(data[factor2=='GRUPPO22',1]) - m  # beta.2
# Point-wise estimates of mean duration of travels (model without interaction!)
m11 <- m + tau1 + beta1
m12  <- m + tau1 + beta2
m21 <- m + tau2 + beta1
m22  <- m + tau2 + beta2

## Now we can perform ANOVA WITH INTERACTION (COMPLETE MODEL)
# X.ijk = mu + tau.i + beta.j + gamma.ij + eps.ijk; eps.ijk~N(0,sigma^2), 
#     i=1,2 (effect city), j=1,2,3 (effect type)
# Two-ways ANOVA: model with interaction
fit.aov.int <- aov(x ~ factor1 + factor2 + factor1:factor2)
summary.aov(fit.aov.int)




# We have performed 3 tests:
# 
# 
# 1) Test1:
#   [ H_0: tau_1 = tau_2 = 0  vs  H_1: (H_0)^c ]
# namely: 
#   
# H0: The effect factor1 doesn't significantly influence x 
# 
# H1: The effect factor1" significantly influences x
# 
# 
# 2) Test2
# [ H_0: beta_1 = beta_2 = 0  vs  H_1: (H_0)^c ]
# namely: 
# 
# H0: The effect factor2 doesn't significantly influence x
# 
# H1: The effect factor2 significantly influences x
#
#
# 3) Test3:
#   [ H_0: gamma_{11} = gamma_{12} = gamma_{21} = gamma_{22} = 0  vs  H_1: (H_0)^c ] 
# namely: 
#
# H0: There is no significant interaction between the factor1 and factor2 in terms of x
#
# H1: There exists a significant interaction between the factor1 and factor2 in terms of x
# 
# For each test I have to look at the pvalue:
# pvalue VERY SMALL (<alpha): we can reject the null hypothesis, the factor/interaction IS significant.
# pvalue VERY LARGE (>alpha): we cannot reject the null hypothesis, the factor/interaction is NOT significant.
#
# If there are some factors/interactions that are NOT significant, 
# we can remove them from the model.

# Note: if only 1 factor is important, you can perform One-Way ANOVA 
# considering only this factor.






##### Now we can perform ANOVA WITHOUT INTERACTION (ADDITIVE MODEL)
# Additive model: WITHOUT interaction
# X.ijk = mu + tau.i + beta.j + eps.ijk; eps.ijk~N(0,sigma^2), 
#     i=1,2 (effect city), j=1,2,3 (effect type)
fit.aov.ad <- aov(x ~ factor1 + factor2) 
summary.aov(fit.aov.ad)











#### Two-Ways MANOVA ####

# setting dataset
# labels:
factor1 <- factor(data$......) #
factor2 <- factor(data$......) #
factor12 <- factor(paste(factor1, factor2, sep='-'))

# note: if factors are dummy variables, you can set labels manually as:
# levels(factor(data$......)) # check the order of the labels
# factor1 <- factor(data$......, labels = c('L','H')) # set labels manually (they must refer to levels above in same order)

# var numerica:
x <- data[, c( , , , , )]       #
p <- dim(x)[2]
n <- dim(x)[1]

g <- length(levels(factor1))
b <- length(levels(factor2))
gb <- length(levels(factor12))


### Model WITH interaction (complete model): 
### X.ijk = mu + tau.i + beta.j + gamma.ij + eps.ijk; eps.ijk~N_p(0,Sigma), [p=2]
###     i=1,2 (effect REL), j=1,2 (effect HPV),
###     X.ijk, mu, tau.i, beta.j, gamma.ij in R^2

### We don't verify the assumptions if we have very few data

# Check Assumptions:

# 1) normality (multivariate) in each interaction of factors (gb test)

Ps12 <- NULL
for (i in 1:gb){
  result <- mvn(data = x[factor12 == levels(factor12)[i],],
                # multivariatePlot = "qq",
                covariance = FALSE)
  result$multivariateNormality$`p value`
  Ps12 <- c(Ps12, result$multivariateNormality$`p value`)
}
Ps12

# 2) homogeneity of the variance (qualitatively)
S1 <-  cov(x[ factor12==levels(factor12)[1], ])
S2 <-  cov(x[ factor12==levels(factor12)[2], ])
S3 <-  cov(x[ factor12==levels(factor12)[3], ])
S4 <-  cov(x[ factor12==levels(factor12)[4], ])

# they must be gb 

par(mfrow=c(1,g*b))
image(S1, col=heat.colors(100),main='Cov. S1', asp=1, axes = FALSE, 
     breaks = quantile(rbind(S1,S2,S3,S4), (0:100)/100, na.rm=TRUE))
image(S2, col=heat.colors(100),main='Cov. S2', asp=1, axes = FALSE, 
     breaks = quantile(rbind(S1,S2,S3,S4), (0:100)/100, na.rm=TRUE))
image(S3, col=heat.colors(100),main='Cov. S3', asp=1, axes = FALSE, 
     breaks = quantile(rbind(S1,S2,S3,S4), (0:100)/100, na.rm=TRUE))
image(S4, col=heat.colors(100),main='Cov. S4', asp=1, axes = FALSE, 
     breaks = quantile(rbind(S1,S2,S3,S4), (0:100)/100, na.rm=TRUE))



##### Two-Ways MANOVA (COMPLETE model WITH interaction): ----------------
fit <- manova( as.matrix(x) ~ factor1 + factor2 + factor1:factor2) 
summary.manova(fit, test="Wilks")

# For each test I have to look at the pvalue:
# pvalue VERY SMALL (<alpha): we can reject the null hypothesis, the factor/interaction IS significant.
# pvalue VERY LARGE (>alpha): we cannot reject the null hypothesis, the factor/interaction is NOT significant.

# If there are some factors/interactions that are NOT significant, 
# we can remove them from the model.












#####  Two-Ways MANOVA (ADDITIVE model WITHOUT interaction): ------------------
### Model without interaction (additive model): 
### X.ijk = mu + tau.i + beta.j + eps.ijk; eps.ijk~N_p(0,Sigma), [p=2]
###     i=1,2 (effect REL), j=1,2 (effect HPV),
###     X.ijk, mu, tau.i, beta.j in R^2

fit2 <- manova( as.matrix(x) ~ factor1 + factor2) 
summary.manova(fit2, test="Wilks")

# For each test I have to look at the pvalue:
# pvalue VERY SMALL (<alpha): we can reject the null hypothesis, the factor IS significant.
# pvalue VERY LARGE (>alpha): we cannot reject the null hypothesis, the factor

# If there are some factors that are NOT significant, 
# we can remove them from the model.




##### Bonferroni  ------------------------
# può essere utilizzato per verificare come un fattore influenza un label o un altro fattore influenza l'altro label

alpha <- ... 

N <- n*g*b 

W <- summary.manova(fit2)$SS$Residuals

# how many comparisons?
k <- g*(g-1)/2*p + b*(b-1)/2*p

qT <- qt(1 - alpha / (2 * k), g*b*n-g-b+1)
# the degrees of freedom of the residuals on the additive model are
# g*b*n-g-b+1

n1.1 <- table(factor1)[1] # number of observations in level 1 of factor1
n1.2 <- table(factor1)[2] # number of observations in level 2 of factor1


mfactor1.1  <- sapply(x[factor1==levels(factor1)[1],],mean)
mfactor1.2  <- sapply(x[factor1==levels(factor1)[2],],mean)
inffactor1 <- mfactor1.2-mfactor1.1 - qT * sqrt( diag(W)/(g*b*n-g-b+1) * (1/n1.1+1/n1.2) )
supfactor1 <- mfactor1.2-mfactor1.1 + qT * sqrt( diag(W)/(g*b*n-g-b+1) * (1/n1.1+1/n1.2) )
# note that if you have more than 2 levels for each factor you should do:
# inffactor1.12 <- mfactor1.2-mfactor1.1 - qT * sqrt( diag(W)/(g*b*n-g-b+1) * (1/n1.1+1/n1.2) )
# inffactor1.13 <- mfactor1.3-mfactor1.1 - qT * sqrt( diag(W)/(g*b*n-g-b+1) * (1/n1.1+1/n1.3) )
# and so on........ # *

n2.1 <- table(factor2)[1] # number of observations in level 1 of factor2
n2.2 <- table(factor2)[2] # number of observations in level 2 of factor2

mfactor2.1  <- sapply(x[factor2==levels(factor2)[1],],mean)
mfactor2.2  <- sapply(x[factor2==levels(factor2)[2],],mean)
inffactor2 <- mfactor2.2-mfactor2.1 - qT * sqrt( diag(W)/(g*b*n-g-b+1) * (1/n2.1+1/n2.2))
supfactor2 <- mfactor2.2-mfactor2.1 + qT * sqrt( diag(W)/(g*b*n-g-b+1) * (1/n2.1+1/n2.2))
# note that if you have more than 2 levels for each factor you should do as before... *

IC2 <- list(factor1.1_factor1.2=cbind(inffactor1, supfactor1), 
           factor2.1_factor2.2=cbind(inffactor2, supfactor2))
IC2
# You have to check if 0 is inside/outside the Bonferroni CI. 
# If 0 is inside the Bonferroni CI, the effect of the correspondent factor
# has NO effect on that variable. 




# You can plot the CI's
# chatgpt
library(ggplot2)

# Convert the results into a data frame for plotting
df <- data.frame(
  Comparison = rep(c("Factor1: Level 1 vs 2", "Factor2: Level 1 vs 2"), each = length(inffactor1)),
  Variable = rep(names(inffactor1), times = 2),
  Lower = c(inffactor1, inffactor2),
  Upper = c(supfactor1, supfactor2)
)

# Plotting the confidence intervals
ggplot(df, aes(x = Variable, y = (Lower + Upper) / 2, ymin = Lower, ymax = Upper)) +
  geom_pointrange() +
  facet_wrap(~Comparison) +
  theme_minimal() +
  labs(
    title = "Bonferroni Confidence Intervals for Factor Levels",
    x = "Response Variables",
    y = "Mean Difference"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


                     
                     
                     
                     



### inizio dataset particolari: LDA/QDA  -------------------------------

# CASO I: ho due colonne separate
data <- read.table('sardine.txt', header=T)
head(data)
data.new <- data.frame(
  data = c(data$Atlantica, data$Iberica),   #qui metti il nome delle colonne
  Region = factor(rep(c("Atlantica", "Iberica"), each = nrow(data)))
)
data<- data.new
head(data)
dim(data)

# CASO II: ho due dataset
true <- read.table('true.txt', header=T)
false <- read.table('false.txt', header=T)
head(true)
head(false)
data <- data.frame(
  data = rbind(true, false) ,    #qui metti dataset
  Region = factor(rep(c('true', 'false'), each = nrow(true)))
)

# fine dataset particolare       fine        fine        fine      fine        fine


#### LDA: Univariate Linear Discriminant Analysis ####

n <- dim(data)[1]
p <- dim(data)[2] - 1 # p = 1 in this case!

names(data)[names(data) == 'vecchio nome/Region'] <- 'GRUPPO'
groups <- levels(factor(data$GRUPPO)) 
g <- length(groups)


id1 <- which(data$GRUPPO==groups[1])  
id2 <- which(data$GRUPPO==groups[2])
n1 <- length(id1)
n2 <- length(id2)
n <- n1+n2

data2 <- data
data2$GRUPPO <- NULL  

colori <- rep(0, times = n)
for (i in 1:g){
  colori[data$GRUPPO == groups[i]] <- rainbow(g)[i]
}

# pay attention to which variables you are plotting (data[,1]):
plot(data[,1], rep(0.5, times = n), pch=19, col=colori, ylim = c(0,1), 
     xlab=colnames(data)[1], ylab='')


# LDA assumptions 
#
# 1. if L=i, X_i ~ N(mu_i, sigma_i^2), i = A, B #
# 2. sigma_A = sigma_B #

# Check Assumptions:
#
# 1. Gaussianity in each group:
#
pval1 <- mvn(data2[id1,])$multivariateNormality
pval2 <- mvn(data2[id2,])$multivariateNormality
c(pval1, pval2, ....)
#oppure 
mvn(data)
shapiro.test(data[,1])
# pvalue VERY SMALL : we cannot reject the null hypothesis, I can NOT assume Gaussianity
# pvalue VERY LARGE : we can reject the null hypothesis, I can assume Gaussianity


# 2. Homoschedasticity between the groups:
var.test(data[id1,1], data[id2,1])$p.value
bartlett.test(data2, groups)
# pvalue VERY SMALL (<alpha): we cannot reject the null hypothesis, I can NOT assume Homoschedasticity
# pvalue VERY LARGE (>alpha): we can reject the null hypothesis, I can assume Homoschedasticity
# A <- which(data$GRUPPO=='A') #
# B <- which(data$GRUPPO=='B') #
# var.test(data[A,1],data[B,1])
# p-value LARGE: homoschedasticity OK
# p-value SMALL: homoschedasticity NO


# setting priors probabilities and classification costs
# p1: prob of being group1
groups
p1 <- n1/n # only if different by default
p2 <- n2/n # only if different by default
priors <- c(p1,p2)
# if you have missclassification costs:
# c12: cost you pay if it's 2 but assigned 1
c12 <- ...
c21 <- ...
priors <- c(p1*c21 / (p1*c21 + p2*c12), p2*c12 / (p1*c21 + p2*c12))
cost = c(c12,c21)

# performing LDA:
groups
priors <- c(, , , ) # only if different by default
data.lda <- lda(data$GRUPPO ~ data[,1] , prior = priors. cost = costs)
data.lda

# plot the classifier:
# you can generalize if more than 2 groups 
P1 <- n1/n
P2 <- n2/n
M1 <- mean(data[id1,1])
M2 <- mean(data[id2,1])
S1 <- var(data[id1,1])
S2 <- var(data[id2,1])
S <- ((n1-1)*S1 + (n2-1)*S2) / (n1+n2-g) # Spooled

x <- seq(range(data[,1]), 0.5)
plot(x, P1*dnorm(x, M1, sqrt(S)) / (P1 * dnorm(x, M1, sqrt(S)) + P2 * dnorm(x, M2, sqrt(S))),
     type = 'l', col = 'blue', ylab = 'estimated posterior')
points(x, P2*dnorm(x, M2, sqrt(S)) / (P1 * dnorm(x, M1, sqrt(S)) + P2 * dnorm(x, M2, sqrt(S))),
       type = 'l', col = 'red')
points(data[id1,1], rep(0, times=length(id1)), pch=16, col='blue')
points(data[id2,1], rep(0, times=length(id2)), pch=16, col='red')
legend(legend=c('P(group1|X=x)', 'P(group2|X=x)'), col = c('blue', 'red'), lty = 1, cex = 0.7)



# classification of a new datum 
new.datum <- c(...,...) # 
# class associated with the highest posterior probability:
predict(data.lda, new.datum)$class
# posterior probabilities for the classes:
predict(data.lda, new.datum)$posterior
# coordinates of the canonical analysis of Fisher (Fisher’s discriminant scores):
predict(data.lda, new.datum)$x


# classification of a grid of data:
new.data <- data.frame(colnames(data)[1] = c(, , , ,))  #
# class associated with the highest posterior probability:
predict(data.lda, new.data)$class
# posterior probabilities for the classes:
predict(data.lda, new.data)$posterior
# coordinates of the canonical analysis of Fisher (Fisher’s discriminant scores):
predict(data.lda, new.data)$x



## APER: Actual Predictor Error Rate - with empirical frequencies
# Namely the total number of mistakes over the total number of data.
Lda.predetto <- predict(data.lda, data[,1])
table(class.true=data$GRUPPO, class.assigned=Lda.predetto$class) # misclassification table
errors <- (data.lda$class != data$GRUPPO)

APER <- sum(errors)/n
APER

## APER: Apparent Error Rate - with given priors
priors <- c(, , , ,) #
misc <- table(class.true=data$GRUPPO, class.assigned=data.lda$class)
APER <- 0
for ( i in 1:g){
  APER <- APER + sum(misc[i,-i])/sum(misc[i,]) * priors[i]
}
APER

# Trivial classifier; classifies always as the most likely class a priori
APER.banale <- 1-(prior più alto)
  APER.banale



## APER: Actual Error Rate - with empirical frequencies
# Compute  via loo-CV.
# set CV=TRUE for Leave-one-out Cross Validation
data.ldaCV <- lda(data2, data$GRUPPO, CV=TRUE), prior=priors)

# misclassification table:
misc <- table(class.true=data$GRUPPO, class.assignedCV=data.ldaCV$class)

# SENZA PRIORS
errorsCV <- (data.ldaCV$class != data$GRUPPO)
CV <- sum(errorsCV)/n
CV

# CON PRIORS
# Errore nel clasificare il gruppo 1 come gruppo 2 
CV_lda <- misc[1,2]/(misc[1,1]+misc[1,2])*priors[1]+
  misc[2,1]/(misc[2,1]+misc[2,2])*priors[2]
CV_lda


#SE g=3
_CV= priors[1]*((misc[1,2]+misc[1,3])/(misc[1,1]+misc[1,2]+misc[1,3]))+
  priors[2]*((misc[2,1]+misc[2,3])/(misc[2,1]+misc[2,2]+misc[2,3]))+
  priors[3]*((misc[3,1]+misc[3,2])/(misc[3,1]+misc[3,2]+misc[3,3]))+
  
  
  
  
  
  
  
  
#### LDA: Multivariate Linear Discriminant Analysis ####
load('C:/Users/frenc/OneDrive/Desktop/mcshapiro.test.RData')

n <- dim(data)[1]
p <- dim(data)[2] - 1 
names(data)[names(data) == 'vecchio nome'] <- 'GRUPPO'
groups <- levels(factor(data$GRUPPO))
groups
g <- length(groups)

id1 <- which(data$GRUPPO==groups[1])
id2 <- which(data$GRUPPO==groups[2])
id <- c(id1, id2, .....) #aggiungi se ci sono più di due fattori
n1 <- length(id1)
n2 <- length(id2)

data2 <- data
data2$GRUPPO <- NULL   


# inizio inutile     inutile    inutile     inutile     inutile     inutile     inutile      inutile      inutile
colori <- rep(0, times = n)
for (i in 1:g){
  colori[data$GRUPPO == groups[i]] <- rainbow(g)[i] 
}

# plotting data (if bivariate)
plot(data2[,1], data2[,2], pch=19, col=colori, 
     xlab=colnames(data2)[1], ylab=colnames(data2)[2])
# fine inutile     inutile    inutile     inutile     inutile     inutile     inutile      inutile      inutile


# Check Assumptions:
#
# 1. Gaussianity in each group:

pval1 <- mvn(data2[id1,])$multivariateNormality
pval2 <- mvn(data2[id2,])$multivariateNormality
# pval3 <- mcshapiro.test(data2[id3,])$p

c(pval1, pval2, ......)

# pvalue VERY SMALL (<alpha): we can reject the null hypothesis, I can NOT assume Gaussianity
# pvalue VERY LARGE (>alpha): we cannot reject the null hypothesis, I can assume Gaussianity

# 2. Homoschedasticity between the groups:
S <- cov(data2)
S1 <- cov(data2[id1,])
S2 <- cov(data2[id2,])
#S3 <- cov(data2[id3,])

# I can only check qualitatively:
par(mfrow=c(1,g))
image(S1, col=heat.colors(100),main='Cov. S1', asp=1, axes = FALSE,
      breaks = quantile(rbind(S1,S2,S3), (0:100)/100, na.rm=TRUE))
image(S2, col=heat.colors(100),main='Cov. S2', asp=1, axes = FALSE,
      breaks = quantile(rbind(S1,S2, S3), (0:100)/100, na.rm=TRUE))
image(S3, col=heat.colors(100),main='Cov. S3', asp=1, axes = FALSE,
      breaks = quantile(rbind(S1,S2, S3), (0:100)/100, na.rm=TRUE))
#... image(S3, ...)
#if they are different, use QDA!!!
bartlett.test(data2, groups)

# setting priors probabilities and classification costs
# p1: prob of being group1
p1 <- n1/n # only if different by default
p2 <- n2/n # only if different by default
priors <- c(p1,p2)

# SUBCASE: if you have missclassification costs:
# c12: cost you pay if it's 2 but assigned 1
c12 <- ... 
c21 <- ... 
priors <- c(p1*c21 / (p1*c21 + p2*c12), p2*c12 / (p1*c21 + p2*c12))
# END SUBCASE;

# performing LDA 
data.lda <- lda(data2, data$GRUPPO) , prior = priors) 
data.lda


#Group Priors:
data.lda$prior
#Group Mean:
data.lda$means
lda.pred <- predict(data.lda, data2)
id1_new <- which(lda.pred$class==groups[1])
id2_new <- which(lda.pred$class==groups[2])
cov(data2[id1_new,])
cov(data2[id2_new,])

# inizio inutile     inutile    inutile     inutile     inutile     inutile     inutile      inutile      inutile


# graphical representation: plot the partition induced by LDA
plot(data2, xlab=colnames(data2)[1], ylab=colnames(data2)[2], pch=20)
points(data2[id1,], col=rainbow(g)[1], pch=20)
points(data2[id2,], col=rainbow(g)[2], pch=20)
#points(data2[id3,], col=rainbow(g)[3], pch=20) #Se i gruppi sono 3
legend("topright", legend=groups, fill=rainbow(g), cex=.7, bty='n')
points(data.lda$means, pch=4, col=rainbow(g) , lwd=2, cex=1.5)

x <- seq(min(data2[,1]), max(data2[,1]), length=200) 
y <- seq(min(data2[,2]), max(data2[,2]), length=200) 
xy <- expand.grid("metti_nome_colonna_data2[,1]"=x, "metti_nome_colonna_data2[,2]"=y) #*
z <- predict(data.lda, xy)$post   # these are P_i*f_i(x,y)

#Se i gruppi sono 2
z1 <- z[,1] - pmax(z[,2]) # P_1*f_1(x,y)-max{P_j*f_j(x,y)} 
z2 <- z[,2] - pmax(z[,1]) # P_2*f_2(x,y)-max{P_j*f_j(x,y)} 

#Se i gruppi sono 3
# z1 <- z[,1] - pmax(z[,2], z[,3]) # P_1*f_1(x,y)-max{P_j*f_j(x,y)} 
# z2 <- z[,2] - pmax(z[,1], z[,3]) # P_2*f_2(x,y)-max{P_j*f_j(x,y)} 
# z3 <- z[,3] - pmax(z[,1], z[,2]) # P_3*f_3(x,y)-max{P_j*f_j(x,y)}

contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T) 
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T) 
#contour(x, y, matrix(z3, 200), levels=0, drawlabels=F, add=T) #Se i gruppi sono 3

# fine inutile     inutile    inutile     inutile     inutile     inutile     inutile      inutile      inutile



# classification of a new datum 
new.datum <- c(...,...)
# class associated with the highest posterior probability:
predict(data.lda, new.datum)$class
# posterior probabilities for the classes:
predict(data.lda, new.datum)$posterior
# coordinates of the canonical analysis of Fisher (Fisher’s discriminant scores):
predict(data.lda, new.datum)$x


# classification of a grid of data:
new.data <- data.frame(colnames(data)[1] = c(, , , ,))  
# class associated with the highest posterior probability:
predict(data.lda, new.data)$class
# posterior probabilities for the classes:
predict(data.lda, new.data)$posterior
# coordinates of the canonical analysis of Fisher (Fisher’s discriminant scores):
predict(data.lda, new.data)$x



## APER: Actual Predictor Error Rate - with empirical frequencies
# Namely the total number of mistakes over the total number of data.
# Calcola il tasso di errore usando solo i dati osservati, senza considerare priori specifici.
lda.pred <- predict(data.lda, data2)
table(class.true=data$GRUPPO, class.assigned=lda.pred$class) # misclassification table
errors <- (lda.pred$class != data$GRUPPO)

APER <- sum(errors)/length(data$GRUPPO) 
APER


## APER: Apparent Error Rate - WITH GIVEN PRIORS
priors <- c(, , , ,) 
misc <- table(class.true=data$GRUPPO, class.assigned=data.lda$class)  
APER <- 0
for ( i in 1:g){
  APER <- APER + sum(misc[i,-i])/sum(misc[i,]) * priors[i]
}
APER

# Trivial classifier; classifies always as the most likely class a priori
APER.banale <- # 1-(prior più alto)
  APER.banale


## AER : Actual Error Rate - with empirical frequencies
# Compute  via loo-CV.
# set CV=TRUE for Leave-one-out Cross Validation
data.ldaCV <- lda(data2, data$GRUPPO, CV=TRUE, prior=priors) 

# misclassification table:
misc <- table(class.true=data$GRUPPO, class.assignedCV=data.ldaCV$class)

# SENZA PRIORS
errorsCV <- (data.ldaCV$class != data$GRUPPO)
CV <- sum(errorsCV)/length(data$GRUPPO) 
CV

# CON PRIORS
# Errore nel clasificare il gruppo 1 come gruppo 2 
CV <- misc[1,2]/(misc[1,1]+misc[1,2])*priors[1]+
  misc[2,1]/(misc[2,1]+misc[2,2])*priors[2]


#SE g=3
_CV= priors[1]*((misc[1,2]+misc[1,3])/(misc[1,1]+misc[1,2]+misc[1,3]))+
  priors[2]*((misc[2,1]+misc[2,3])/(misc[2,1]+misc[2,2]+misc[2,3]))+
  priors[3]*((misc[3,1]+misc[3,2])/(misc[3,1]+misc[3,2]+misc[3,3]))+
  
  









#### QDA: Univariate Quadratic Discriminant Analysis ####


n <- dim(data)[1]
p <- dim(data)[2] - 1 
names(data)[names(data) == 'vecchio nome'] <- 'GRUPPO' *
groups <- levels(factor(data$GRUPPO)) 
g <- length(groups)

id1 <- which(data$GRUPPO==groups[1]) #
id2 <- which(data$GRUPPO==groups[2]) #
id <- cbind(id1, id2, .....) # se non funziona usa c(,)
n1 <- length(id1)
n2 <- length(id2)

data2 <- data # solo numerici
data2$GRUPPO <- NULL #*


colori <- rep(0, times = n)
for (i in 1:g){
  colori[data$GRUPPO == groups[i]] <- rainbow(g)[i]
}

# plotting data (if bivariate)
plot(data2[,1], data2[,2], pch=19, col=colori, 
     xlab=colnames(data2)[1], ylab=colnames(data2)[2])


# QDA assumptions 
#
# 1. L=i, X_i ~ N(mu_i, sigma_i^2), i = A, B #

# Check Assumptions:
#
# 1. Gaussianity in each group:
#
pval1 <- shapiro.test(data[id1,1])$p
pval2 <- shapiro.test(data[id2,1])$p
c(pval1, pval2, ....)
# pvalue VERY SMALL : we can reject the null hypothesis, I can NOT assume Gaussianity
# pvalue VERY LARGE : we cannot reject the null hypothesis, I can assume Gaussianity

# setting priors probabilities and classification costs
# p1: prob of being group1
p1 <- n1/n # only if different by default #*
p2 <- n2/n # only if different by default #*
# if you have missclassification costs:
# c12: cost you pay if it's 2 but assigned 1
c12 <- ... #
c21 <- ... #
priors <- c(p1*c21 / (p1*c21 + p2*c12), p2*c12 / (p1*c21 + p2*c12))


# performing QDA:
priors <- c(, , , ) # only if different by default #
data.qda <- qda(data2, data$GRUPPO) #, prior = priors) #NON SONO SICURA DELLA MODIFICA CHE HO FATTO
data.qda




# plot the classifier:
# NON SI PUO'



## APER: Actual Predictor Error Rate - with empirical frequencies
# Namely the total number of mistakes over the total number of data.
qda.pred <- predict(data.qda, data2) # assigned class to our dataset
table(class.true=data$GRUPPO, class.assigned=qda.pred$class) # misclassification table #
errors <- (qda.pred$class != data$GRUPPO) #

APER <- sum(errors)/n
APER

## APER: APparent Error Rate - with given priors
priors <- c(, , , ,) #
qda.pred <- predict(data.qda, data2) # assigned class to our dataset
misc <- table(class.true=data$GRUPPO, class.assigned=qda.pred$class) #
APER <- 0
for ( i in 1:g){
  APER <- APER + sum(misc[i,-i])/sum(misc[i,]) * priors[i]
}
APER


# Trivial classifier; classifies always as the most likely class a priori
APER.banale <- # 1-(prior più alto)
  APER.banale


## AER: Actual Error Rate - with empirical frequencies
# Compute  via loo-CV.
# set CV=TRUE for Leave-one-out Cross Validation
data.qdaCV <- qda(data2, data$GRUPPO, CV=TRUE) #prior=priors #

# misclassification table:
misc <- table(class.true=data$GRUPPO, class.assignedCV=data.qdaCV$class) #

# SENZA priors
errorsCV <- (data.qdaCV$class != data$GRUPPO) #
AERCV <- sum(errorsCV)/n
AERCV

# CON PRIORS
# Errore nel clasificare il gruppo 1 come gruppo 2 
AERCV <- misc[1,2]/(misc[1,1]+misc[1,2])*priors[1]+
  misc[2,1]/(misc[2,1]+misc[2,2])*priors[2]

#SE g=3
AER_CV= priors[1]*((misc[1,2]+misc[1,3])/(misc[1,1]+misc[1,2]+misc[1,3]))+
  priors[2]*((misc[2,1]+misc[2,3])/(misc[2,1]+misc[2,2]+misc[2,3]))+
  priors[3]*((misc[3,1]+misc[3,2])/(misc[3,1]+misc[3,2]+misc[3,3]))+
  
  
  
### PREDICTION of a new datum
new <- c(...,...) #
predict(data.qda,new)$class
predict(data.qda,new)$posterior[i] # se si vuole accedere al classifier i-esimo







#### QDA: Multivariate Quadratic Discriminant Analysis ####
load('C:/Users/frenc/OneDrive/Desktop/mcshapiro.test.RData')

n <- dim(data)[1]
p <- dim(data)[2] - 1 
names(data)[names(data) == 'vecchio nome'] <- 'GRUPPO'
groups <- levels(factor(data$GRUPPO))
g <- length(groups)

id1 <- which(data$GRUPPO==groups[1])
id2 <- which(data$GRUPPO==groups[2])
id <- c(id1, id2, .....) #aggiungi se ci sono più di due fattori
n1 <- length(id1)
n2 <- length(id2)

data2 <- data
data2$GRUPPO <- NULL   


colori <- rep(0, times = n)
for (i in 1:g){
  colori[data$GRUPPO == groups[i]] <- rainbow(g)[i] 
}

# plotting data (if bivariate)
plot(data2[,1], data2[,2], pch=19, col=colori, 
     xlab=colnames(data2)[1], ylab=colnames(data2)[2])


# Check Assumptions:
#
# 1. Gaussianity in each group:

pval1 <- mvn(data2[id1,])$multivariateNormality
pval2 <- mvn(data2[id2,])$multivariateNormality
# pval3 <- mcshapiro.test(data2[id3,])$p

c(pval1, pval2, ......)

# pvalue VERY SMALL (<alpha): we can reject the null hypothesis, I can NOT assume Gaussianity
# pvalue VERY LARGE (>alpha): we cannot reject the null hypothesis, I can assume Gaussianity

# setting priors probabilities and classification costs
# p1: prob of being group1
groups #per capire gruppo1 e gruppo2
p1 <- n1/n # only if different by default QUASI SEMPRE DIVERSI
p2 <- n2/n # only if different by default
priors <- c(p1,p2)
priors <- c( , ) #se ti dice qualcosa il testo

# SUBCASE: if you have missclassification costs:
# c12: cost you pay if it's 2 but assigned 1
c12 <- ... 
c21 <- ... 
priors <- c(p1*c21 / (p1*c21 + p2*c12), p2*c12 / (p1*c21 + p2*c12))
# END SUBCASE;

# performing qda:
data.qda <- qda(data2, data$GRUPPO , prior = priors)
data.qda



#Group Priors:
data.qda$prior

#estimates of its parameters (means and covariances)
#Group Mean: 
data.qda$means
#Group Covariances:
qda.pred <- predict(data.qda, data2)
id1_new <- which(qda.pred$class==groups[1])
id2_new <- which(qda.pred$class==groups[2])
# id3_new <- which(qda.pred$class==groups[3])

cov(data2[id1_new,])
cov(data2[id2_new,])
# cov(data2[id3_new,])


# plot the classifier
par(mfrow=c(1,1))
plot(data2, main='', xlab=colnames(data2)[1], ylab=colnames(data2)[2], pch=20)
points(data2[id1,], col=rainbow(g)[1], pch=20)
points(data2[id2,], col=rainbow(g)[2], pch=20)
# points(data2[id3,], col=rainbow(g)[3], pch=20)

legend("topright", legend=groups, fill=rainbow(g))

points(data.qda$means, col=rainbow(g), pch=4, lwd=2, cex=1.5)

x  <- seq(min(data2[,1]), max(data2[,1]), length=200)
y  <- seq(min(data2[,2]), max(data2[,2]), length=200)
xy <- expand.grid("metti_nome_colonna_data2[,1]"=x, "metti_nome_colonna_data2[,2]"=y) #

z  <- predict(data.qda, xy)$post  
# Se g = 2
z1 <- z[,1] - z[,2]
z2 <- z[,2] - z[,1]
# SE g > 2
# z1 <- z[,1] - pmax(z[,2], z[,3])    
# z2 <- z[,2] - pmax(z[,1], z[,3])    
# z3 <- z[,3] - pmax(z[,1], z[,2])

contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)
# contour(x, y, matrix(z3, 200), levels=0, drawlabels=F, add=T)



## APER: Actual Predictor Error Rate - with empirical frequencies
# Namely the total number of mistakes over the total number of data.
qda.pred <- predict(data.qda, data2)
table(class.true=data$GRUPPO, class.assigned=qda.pred$class) # misclassification table
errors <- (qda.pred$class != data$GRUPPO)

APER <- sum(errors)/n
APER


## APER: APparent Error Rate - with GIVEN PRIOR
priors <- c(, , , ,) 
qda.pred <- predict(data.qda, data2)
misc <- table(class.true=data$GRUPPO, class.assigned=qda.pred$class)
APER <- 0
for ( i in 1:g){
  APER <- APER + sum(misc[i,-i])/sum(misc[i,]) * priors[i]
}
APER





# Trivial classifier; classifies always as the most likely class a priori
APER.banale <- # 1-(prior più alto)
  APER.banale





#CASO costi di misclassificazione: expected economic loss of the classifier. guardando table
exp_loss <- ( MISC[2,1] * c12 + MISC[1,2] * c21) / n





##AER : Actual Error Rate - with empirical frequencies for MULTIVARIATE QDA
# Compute  via leave one out -CV.
# set CV=TRUE for Leave-one-out Cross Validation
data.qdaCV <- qda(data2, data$GRUPPO, CV=TRUE, prior=priors)

# misclassification table:
misc <- table(class.true=data$GRUPPO, class.assignedCV=data.qdaCV$class)

#SENZA PRIORS
errorsCV <- (data.qdaCV$class != data$GRUPPO)
CV <- sum(errorsCV)/n
CV

# CON PRIORS
# Errore nel clasificare il gruppo 1 come gruppo 2 
CV <- misc[1,2]/(misc[1,1]+misc[1,2])*priors[1]+
  misc[2,1]/(misc[2,1]+misc[2,2])*priors[2]
CV

#SE g=3
_CV= priors[1]*((misc[1,2]+misc[1,3])/(misc[1,1]+misc[1,2]+misc[1,3]))+
  priors[2]*((misc[2,1]+misc[2,3])/(misc[2,1]+misc[2,2]+misc[2,3]))+
  priors[3]*((misc[3,1]+misc[3,2])/(misc[3,1]+misc[3,2]+misc[3,3]))+
  

  
  
# inizio inutile     inutile    inutile     inutile     inutile     inutile     inutile      inutile      inutile
# NO SENSE CONFERMATO DAI DOGS
  
## Considerando dei COSTI # CONTROLLARE SE FUNZIONA fino a APER
g1 <-data[data$VARIABILE_CATEGORICA=='...',1:2] #
g2 <-data[data$VARIABILE_CATEGORICA=='...',1:2] #
mvn(g1)$pvalue
# mvn(g1)
mvn(g2)$pvalue
prior <- c(dim(g2)[1]/(sum(dim(g2)[1],dim(g1)[1])),dim(g1)[1]/(sum(dim(g2)[1],dim(g1)[1])))
p1 <- prior[1]
p2 <- prior[2]
c1 <- # costo g1 
c2 <- # costo g2
# Modified prior to account for the misclassification costs
prior.c <- c(g2=p1*c2/(c1*p2+c2*p1),g1=p2*c1/(c1*p2+c2*p1))
prior.c
# performing qda:
data.qda <- qda(data2, data$GRUPPO, prior = prior.c) 
qda.pred <- predict(data.qda, data2)
misc <- table(class.true=data$GRUPPO, class.assigned=qda.pred$class) #
APER <- 0
for ( i in 1:g){
  APER <- APER + sum(misc[i,-i])/sum(misc[i,]) * priors[i] * costs[i]
}
APER


# fine inutile     inutile    inutile     inutile     inutile     inutile     inutile      inutile      inutile



### PREDICTION of a new datum
new.datum <- c(...,...)
# class associated with the highest posterior probability:
predict(data.qda, new.datum)$class
# posterior probabilities for the classes:
predict(data.qda, new.datum)$posterior


z0.new <- data.frame(data$...=..., data$...=...) #
points(z0.new, pch=4, col='springgreen', lwd=2, cex=1.5)

predict(data.qda,z0.new)$class



# Consideri rilevante una delle variabili ai fini della classificazione?
lab <- factor(data$opera)
data$GRUPPO <- NULL
fitted <- manova(as.matrix(data) ~ lab)
summary.aov(fitted)





#c)quanto devo pagare per i test precisi?

# verranno testati quelli assegnati dopati in CV * 4
(misc[1,2]+misc[2,2])*4  #92 persone
92*1000 #costo totale
#92000

#d) quanto guadagno con punto b?
(200-92)*1000 - misc[2,1]*4*50000  # test che non faccio - falsi negativi 
#-1092000
# non mi conviene usare il classificatore

#d) ???savings???
(200-92)*1000  #???




#### KNN: Univariate k-nearest neighbor classifier ####

# KNN classifier does NOT requires particular assumptions. 


n <- dim(data)[1]
p <- dim(data)[2] - 1 # in this case p = 1, univariate
k <- # #number of neighbours
  groups <- levels(factor(data$GRUPPO)) #
g <- length(groups)
#
id1 <- which(data$GRUPPO==groups[1]) #
id2 <- which(data$GRUPPO==groups[2]) #
n1 <- length(id1)
n2 <- length(id2)

data.knn <- knn(train = data[,1], test = new.data, 
                cl = data$GRUPPO,  #
                k = k,    # number of neighbours 
                prob = T) # if you want to show the proportion of a neighbour in favour of the result 
# (empirical relative frequency of each datum to be classified as group1)

data.knn.class <- (data.knn == groups[1]) + 0 # it contains 1 if datum has been classified as groups[1], 0 otherwise.
# probability of being classified as group[1]:
data.knn.class.1 <- ifelse(data.knn.class==1, 
                           attributes(data.knn)$prob, 
                           1-attributes(data.knn)$prob)

# plot the classifier:
plot(x[,1], data.knn.B, type='l', col='black', lty=1)
abline(h = 0.5)
legend(-10, 0.75, legend='knn', lty=c(2,1), col='black')








#### KNN: Multivariate k-nearest neighbor classifier ####

# Scelta del k con CV se non ti danno k, ma un intervallo
# nel for si fa anche la funzione knn e AER

set.seed(19)  #set the random seed equal to 19
  
AER_cv=NULL
k_grid  <- 10:30  #qua intervallo
for(j in k_grid){
  errors_cv = 0
  for(i in 1:n){
    knn_d <- knn(train = data2[-i,], test = data2[i,], cl = data[-i,]$GRUPPO, k = j , prob = T)
    if(knn_d!=data[i,]$GRUPPO)
      errors_cv <- errors_cv +1
  }
  AER_cv   <- c(AER_cv,sum(errors_cv)/n)
}
rbind(k_grid,AER_cv)
aer_min = min(AER_cv)
k_opt=k_grid[which.min(AER_cv)]
aer_min # 0.128   #error rate associated with the optimal classifier
k_opt # 13





# solo se hai gia dato k
# KNN classifier does NOT requires particular assumptions. 

k<-  ...
n <- dim(data)[1]
p <- dim(data)[2] - 1

data2 <- data
data2$GRUPPO <- NULL #

groups <- levels(factor(data$GRUPPO)) #
g <- length(groups)
#
id1 <- which(data$GRUPPO==groups[1]) #
id2 <- which(data$GRUPPO==groups[2]) #
n1 <- length(id1)
n2 <- length(id2)

data.knn <- knn(train = data2, test = new.data, ## newdata=data2?????
                  cl = data$GRUPPO,  # 
                  k = k,    # number of neighbours 
                  prob = T) # if you want to show the proportion of a neighbour in favour of the result 
# (empirical relative frequency of each datum to be classified as group1)

data.knn.class <- (data.knn == groups[1]) + 0 # it contains 1 if datum has been classified as groups[1], 0 otherwise.
# probability of being classified as group[1]:
data.knn.class.1 <- ifelse(data.knn.class==1, 
                           attributes(data.knn)$prob, 
                           1-attributes(data.knn)$prob)




# plot the classifier:
k<- ...
plot(data2, xlab=colnames(data2)[1], ylab=colnames(data2)[2], pch=20)
points(data2[id1,], col = rainbow(g)[1], pch = 20)
points(data2[id2,], col = rainbow(g)[2], pch = 20)
points(data2[id3,], col = rainbow(g)[3], pch = 20)
legend("topright", legend=groups, fill=rainbow(g))

x <- seq(min(data2[,1]), max(data2[,1]), length=200) 
y <- seq(min(data2[,2]), max(data2[,2]), length=200) 
xy <- expand.grid("metti nome colonna data2[,1]"=x, "metti nome colonna data2[,2]"=y) #*
data.knn.plot <- knn(train = data2, test = xy, cl = data$GRUPPO, k = k) #
z <- as.numeric(data.knn.plot)
contour(x, y, matrix(z, 200), levels=c(1.5, 2.5), drawlabels=F, add=T)



### APER
labels <- factor(data$GRUPPO) #label del groups[1]*
# Senza priors
errors <- n - sum(as.numeric(data.knn.class==labels))
APER <- errors/n
APER
# CON PRIORS
predicted <- knn(train = data2, test = data2, cl = labels, k = k)
misclassified <- table(class.true = labels, class.assinged = predicted)
priors <- c(p1,p2,p3)
# ,p3) se ci sono tre gruppi
APER <- 0
for(i in 1:g) 
  APER = APER + sum(misclassified[i,-i])/sum(misclassified[i,]) * priors[i]
APER




#Compute APER with leave one out Cross Validation (stima dell')
#Choose the best k to optimize the misclassification error, via leave-one-out cross-validation
set.seed(321)
labels <- ifelse(data$GRUPPO=='...label del groups[1]', 1, 0) #label del groups[1]*


kk <- 10:30
APER <- NULL
for (i in 1:length(kk)){
  k <- kk[i]
  
  right <- 0
  for (j in 1:n){
    id.sel <- j
    train <- data2[-id.sel,]
    test <- data2[id.sel,]
    data.knn <- knn(train = train, test = test, 
                    cl = labels[-id.sel], 
                    k = k,    # number of neighbours 
                    prob = T)
    data.knn.class <- (data.knn == 1) + 0 # it contains 1 if datum has been classified as groups[1], 0 otherwise.
    right <- right + as.numeric(data.knn.class==labels[id.sel])
  }
  APER[i] <- 1- right/n
}

plot(kk, APER, type = 'b', pch=19)
k <- kk[which(APER==min(APER))]

APER[which(APER==min(APER))]



#come clasificare un nuovo punto?
new.point <- data.frame(x=1,y=-4)
data.knn <- knn(train = data2, test = new.point, 
                cl = data$GRUPPO, #
                k = k,    # number of neighbours 
                prob = T)
data.knn
points(new.point,col="orange",pch=19)




#### FisherDA: Univariate Fisher Discriminant Analysis ####

#### FisherDA: Multivariate Fisher Discriminant Analysis ####


n <- dim(data)[1]
p <- dim(data)[2] - 1 

groups <- levels(factor(data$GRUPPO)) #
g <- length(groups)
s <- min(g-1, p)
#
id1 <- which(data$GRUPPO==groups[1]) #
id2 <- which(data$GRUPPO==groups[2]) #
id <- cbind(id1, id2, .....)
n1 <- length(id1)
n2 <- length(id2)

data2 <- data
data2$group <- NULL

colori <- rep(0, times = n)
for (i in 1:g){
  colori[data2$group == groups[i]] <- rainbow(g)[i]
}

# plotting data (if bivariate)
plot(data2[,1], data2[,2], pch=19, col=colori, 
     xlab=colnames(data2)[1], ylab=colnames(data2)[2])


# Check Assumptions:
# non c'è normality assumption

# 1. Homoschedasticity between the groups:
#*
bartlett.test(data2[id1,], data2[id2,], data2[id3,],...)$p.value
# pvalue VERY SMALL (<alpha): we can reject the null hypothesis, I can NOT assume Homoschedasticity
# pvalue VERY LARGE (>alpha): we cannot reject the null hypothesis, I can assume Homoschedasticity


# useful parameters
# you can generalize if there are more than 2 groups
m <- colMeans(data2)
m1 <- colMeans(data2[id1,])
m2 <- colMeans(data2[id2,])
S1 <- cov(data2[id1,])
S2 <- cov(data2[id2,])
Sp <- ( (n1-1)*S1 + (n2-1)*S2 ) / (n-g) # Spooled
B <- 1/g * (cbind(m1-m) %*% rbind(m1-m) +
              cbind(m2-m) %*% rbind(m2-m)) # Covariance between groups
Spval <- eigen(Sp)$val
Spvec <- eigen(Sp)$vec
Spinv2 <- 1/sqrt(Spval[1]) * Spvec[,1] %*% t(Spvec[,1]) +
  1/sqrt(Spval[2]) * Spvec[,2] %*% t(Spvec[,2])

# note: you have p number of a_i, NOT g
spec.dec <- eigen(Spinv2 %*% B %*% Spinv2)
a1 <- Spinv2 %*% spec.dec$vec[,1] # first canonical decomposition
a2 <- Spinv2 %*% spec.dec$vec[,2] # second canonical decomposition



# Canonical Coordinates of the data:
cc1.data <- as.matrix(data2) %*% a1
cc2.data <- as.matrix(data2) %*% a2
coord.cc <- cbind(cc1.data, cc2.data) 

# Canonical Coordinates of the mean (g objects):
cc.m1 <- c(m1%*%a1, m1%*%a2) # Canonical Coordinates of group1 mean along canonical directions
cc.m2 <- c(m2%*%a1, m2%*%a2) # Canonical Coordinates of group2 mean along canonical directions

# classification of data looking at Euclidean distance
f.class=rep(0, n)
for(i in 1:n) { # for each datum
  # Compute the Euclidean distance of the i-th datum from mean within the groups (g distances)
  dist.m=c(d1 = sqrt(sum((coord.cc[i,]-cc.m1)^2)),
           d2 = sqrt(sum((coord.cc[i,]-cc.m2)^2)))
  # Assign the datum to the group whose mean is the nearest
  f.class[i] = which.min(dist.m)
}
table(class.true=groups, class.assigned=f.class) # misclassification table


## APER: Actual Predictor Error Rate - with empirical frequencies
# Namely the total number of mistakes over the total number of data.
APER <- ( n - sum(diag(table(class.true=groups, class.assigned=f.class))) ) / n


## APER: APparent Error Rate - with given PRIORS
priors <- c(, , , ,) #
misc <- table(class.true=groups, class.assigned=f.class)
APER <- 0
for ( i in 1:g){
  APER <- APER + sum(misc[i,-i])/sum(misc[i,]) * priors[i]
}
APER



# Classification of a new observation
x.new <- c(,,,,)
# compute the canonical coordinates
cc.new <- c(x.new%*%a1, x.new%*%a2)
# compute the distance from the means
dist.m <- c(d1=sqrt(sum((cc.new-cc.m1)^2)),
            d2=sqrt(sum((cc.new-cc.m2)^2)))
# assign to the nearest mean
which.min(dist.m)



# Plot the partition induced by the classifier
color.groups <- groups
levels(color.groups) <- c('red','blue')
plot(cc1.data, cc2.data, main='Fisher discriminant analysis',
     xlab='first canonical coordinate', ylab='second canonical coordinate',
     pch=20, col=as.character(color.groups))
legend("topleft", legend=levels(groups), fill=c('red','blue'), cex=.7)
points(cc.m1[1], cc.m1[2], pch=4,col='red' , lwd=2, cex=1.5)
points(cc.m2[1], cc.m2[2], pch=4,col='blue' , lwd=2, cex=1.5)
x.cc <- seq(min(cc1.data),max(cc1.data),len=200)
y.cc <- seq(min(cc2.data),max(cc2.data),len=200)
xy.cc <- expand.grid(cc1=x.cc, cc2=y.cc)
z <- cbind( sqrt(rowSums(scale(xy.cc,cc.m1,scale=FALSE)^2)),
            sqrt(rowSums(scale(xy.cc,cc.m2,scale=FALSE)^2)))
# if g = 2
z1.cc <- z[,1] - z[,2]
z2.cc <- z[,2] - z[,1]
# if g >2
z1.cc <- z[,1] - pmin(z[,2], z[,3])
z2.cc <- z[,2] - pmin(z[,1], z[,3])
z3.cc <- z[,3] - pmin(z[,1], z[,2])

contour(x.cc, y.cc, matrix(z1.cc, 200), levels=0, drawlabels=F, add=T)
contour(x.cc, y.cc, matrix(z2.cc, 200), levels=0, drawlabels=F, add=T)










#### Hierarchical Clustering ####

# if you DO know the labels: spesso non ce li danno
data.lab <- data$...
data$... <- NULL
#if you want to standardize the data
#data <- data.frame(scale(data))
#head(data)

n <- dim(data)[1]
p <- dim(data)[2]

# graphical representation of data:
plot(data) #solo se p=2
pairs(data)

# setting distance and linkage:
distance <- 'euclidean' # manhattan, canberra
linkages <- c('single', 'average', 'complete', 'ward.D2')

# distance matrix:
data.dist <- dist(data, method=distance)
# plot:
image(1:n,1:n, as.matrix(data.dist), main=paste('metrics: ', distance), asp=1, xlab='', ylab='')

# perform hierarchical clustering:
data.s <- hclust(data.dist, method=linkages[1])
data.a <- hclust(data.dist, method=linkages[2])
data.c <- hclust(data.dist, method=linkages[3])
data.w <- hclust(data.dist, method=linkages[4])

# plot dendograms:
par(mfrow=c(2,2))
plot(data.s, main=paste(distance, ' - ', linkages[1]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
plot(data.a, main=paste(distance, ' - ', linkages[2]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
plot(data.c, main=paste(distance, ' - ', linkages[3]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
plot(data.w, main=paste(distance, ' - ', linkages[4]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
# note: if you want to set dendogram ylim equal:
#         add: ylim = c(0,VALOREMAX), 
#         take out hang=-0.1, 
#         take out labels=F, 
#         add leaflab='none'

# select the best linkage and best k:
k <- ...
linkage <- 'single' # average, complete, ward.D2
data.hc <- data.SCEGLINEUNO #s, a, c, w
par(mfrow=c(1,1))
plot(data.hc, main=paste(distance, ' - ', linkage), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.hc, k=k)

# cut dendogram:
clusters <- cutree(data.hc, k=k)


# if you know the true labels:
table(label.true = data.lab, label.cluster = clusters)


#Dimension of the Clusters
table(clusters) 
#Mean within the Clusters


#Centers/centroid of the Clusters
center <- matrix(0,k,p)
for (i in 1:k) {
  center[i,] <- colMeans(data[clusters==i, ])
}
colnames(center) <- colnames(data)
center



# graphical representation:
#fai attenzione che il dataset sia formato dataframe, altrimenti non plotta!
colors <- rep(0, times=n)
for (i in 1:k){
  colors[clusters==i] <- rainbow(k)[i]
}
plot(data, col=colors, pch=1, main = paste(distance,'-', linkage))
points(center[1,1], center[1,2], col=rainbow(k)[1], pch=4, lwd=3)  
points(center[2,1], center[2,2], col=rainbow(k)[2] ,pch=4, lwd=3)
#points(center[3,1], center[3,2], col=rainbow(k)[3], pch=4, lwd=3)  
#points(center[4,1], center[4,2], col=rainbow(k)[4] ,pch=4, lwd=3)


# Cophenetic Matrix and Cophenetic Coefficient: (per valutare la classificazione)
coph.hc <- cophenetic(data.hc)
cor.hc <- cor(data.dist, coph.hc)
cor.hc
# The closer it is to 1, the better the clustering structure in the
# Cophenetic Matrix is representing a true structure that is in the Distance matrix!
#vicino a 1: indica che il dendrogramma rappresenta bene le distanze originali tra gli oggetti.


#### NUMEROSITY OF CLUSTERS
i1 <- which(clusters==1)
i2 <- which(clusters==2)
ng <- c(length(i1),length(i2)) 
ng 
N <- sum(ng)


##### #confidence region per i centroidi (mean of data) e plot -----------
# Ellipse centered around our sample mean

d1 <- data[i1,]
d2 <- data[i2,]

n1 <- dim(d1)[1]  # 36
n2 <- dim(d2)[1] # 61
p <- dim(d)[2]  #  2
d1.mean   <- sapply(d1,mean)
d1.cov    <- cov(d1)
d1.invcov <- solve(d1.cov)
d2.mean   <- sapply(d2,mean)
d2.cov    <- cov(d2)
d2.invcov <- solve(d2.cov)

# Significance level
alpha   <- .05

# Fisher quantile
cfr.fisher1 <- ((n1-1)*p/(n1-p))*qf(1-alpha,p,n1-p)
cfr.fisher2 <- ((n2-1)*p/(n2-p))*qf(1-alpha,p,n2-p)

#### CONFIDENCE/REJECTION REGION
# Ellipsoidal confidence region with confidence level (1-alpha)100%
plot(data, asp=1, pch=1, main='scatter plot of data',col=clusters)

# Ellipse centered around our sample mean
ellipse(d1.mean, d1.cov/n1, sqrt(cfr.fisher1), col = 'black', lty = 2, lwd=2, center.cex=1)
ellipse(d2.mean, d2.cov/n2, sqrt(cfr.fisher2), col = 'red', lty = 2, lwd=2, center.cex=1)









##### build a model anova ---------------
#ho costruito il clusters su "long" e "lat "
# voglio capire se questi clusters hanno medie diverse su "DO: dissolved oxygen"

i1 <- which(clusters==1)
i2 <- which(clusters==2)

mvn(data[i1,]) # 0.6228
mvn(data[i2,]) # 0.8584
clust <- rep(0,n)
d.new <- cbind(d,clust) # creating categorical feature with cluster labels
d.new$clust[i2] = 1

bartlett.test(d.new[,1:2], d.new$clust) # 0.5642
d.new$clust_fact <- as.factor(d.new$clust)

fit <- manova(as.matrix(d.new[,1:2]) ~ d.new$clust_fact) # 2.2e-16 ***
summary.manova(fit,test="Wilks")

fit$coefficients


##### build a model manova ---------------

i1 <- which(clusters==1)
i2 <- which(clusters==2)
i3 <- which(clusters==3)
n <- nrow(data)

# creating categorical feature with cluster labels
clust <- rep(0,n)
data.new <- cbind(data,clust) 
data.new$clust[i2] = 1  #se ho 3 clusters aggiungo:  data.new$clust[i3] = 2
data.new$clust <- as.factor(data.new$clust)

#assumption
mvn(data[i1,])$multivariateNormality
mvn(data[i2,])$multivariateNormality

bartlett.test(data.new[,1:2], data.new$clust) # 0.5642

fit <- manova(as.matrix(data.new[,1:2]) ~ data.new$clust) # 2.2e-16 ***
summary.manova(fit,test="Wilks")

fit$coefficients
#                          lon        lat
#(Intercept)       85.9741972 39.4606028
#d.new$clust1  0.5135979  0.5166185






##### bonferroni stuff hierarchical -----------------

######ONE CASE: bonferroni for the mean and the variances on one variable -------------------

alpha <- ...
k <- 10 # number of CI: se voglio sia mean che variance k<- #clusters*2
#               se voglio solo mean  k<- #clusters (FORSE)             
g <- 5 # number of clusters
IC={}
Ps={}
for(i in 1:g){
  X <- data[clusters==i,2] # 2 is the variable sepected
  n <- length(X)
  Ps <- c(Ps,shapiro.test(X)$p)
  x.mean   <- mean(X)
  x.cov    <- var(X)
  
  ICmean <- c(inf    = x.mean - sqrt(x.cov/n) * qt(1 - alpha/(2*k), n-1),
              center = x.mean,
              sup    = x.mean + sqrt(x.cov/n) * qt(1 - alpha/(2*k), n-1))
  
  ICvar <- c(inf     = x.cov*(n-1) / qchisq(1 - alpha/(2*k), n-1),
             center  = x.cov,
             sup     = x.cov*(n-1) / qchisq(alpha/(2*k), n-1))
  
  IC <- rbind(IC,
              ICmean,
              ICvar)
}
Ps #0.4091165 0.9899046 0.8147612 0.1966434 0.7798557
IC
#               inf     center        sup 
#ICmean  7.17075311  7.3829268  7.5951005 #cluster1
#ICvar   0.36562759  0.5304950  0.8268407 #cluster1
#ICmean  5.90812307  6.1117347  6.3153463 #cluster2
#ICvar   0.41784804  0.5885135  0.8798950 #cluster2
#ICmean  6.73439942  6.9515584  7.1687175 #cluster3
#ICvar   0.35457803  0.5201817  0.8236806 #cluster3
#ICmean  7.71385634  8.0752000  8.4365437 #cluster4
#ICvar   0.21981455  0.4172677  1.0129666 #cluster4
#ICmean 11.60553121 11.8991667 12.1928021 #cluster5
#ICvar   0.04409681  0.1072629  0.4532428 #cluster5





#######SECOND CASE: bonferroni between the difference among the variables  ----------------
#diamanti con 2 variabili suddivisi in 2 cluster
i1 <- which(clusters==1)
i2 <- which(clusters==2)
t1 <- data[i1,]
t2 <- data[i2,]
n1 <- dim(t1)[1] # n1=3
n2 <- dim(t2)[1] # n2=4
p  <- dim(t1)[2] # p=2

# we compute the sample mean, covariance matrices and the matrix Spooled
t1.mean <- sapply(t1,mean)
t2.mean <- sapply(t2,mean)
t1.cov  <-  cov(t1)
t2.cov  <-  cov(t2)
Sp      <- ((n1-1)*t1.cov + (n2-1)*t2.cov)/(n1+n2-2)

alpha   <- ...
Spinv   <- solve(Sp)
cfr.fisher <- (p*(n1+n2-2)/(n1+n2-1-p))*qf(1-alpha,p,n1+n2-1-p)

#verify assumption
mvn(t1)
mvn(t2)
cov1 <- cov(t1)
cov2 <- cov(t2)
par(mfrow= c(1,2))
image(cov1)
image(cov2)

#only if variance equal
IC <- cbind(t2.mean-t1.mean - sqrt(diag(Sp)*(1/n1+1/n2)) * qt(1 - alpha/(p*2), n1+n2-2),
            t2.mean-t1.mean,
            t2.mean-t1.mean + sqrt(diag(Sp)*(1/n1+1/n2)) * qt(1 - alpha/(p*2), n1+n2-2))
IC




####### THIRD CASE: univariate, just the mean/variable of one variable in different clusters ------------------
# è come il primo caso ma senza ICvar 
#TDE: 4 Bonferroni intervals (global level 90%) for the mean characteristics of a successful tour.

alpha <- 0.1
k <-...# number of CI
g <- 2 # number of clusters
IC={}
Ps={}
for(i in 1:2){
  X <- data[clusters==1,i] # 1 group, cycle for each feature
  n <- length(X)
  Ps <- c(Ps,shapiro.test(X)$p)
  x.mean   <- mean(X)
  x.cov    <- var(X)
  
  ICmean <- c(inf    = x.mean - sqrt(x.cov/n) * qt(1 - alpha/(2*k), n-1),
              center = x.mean,
              sup    = x.mean + sqrt(x.cov/n) * qt(1 - alpha/(2*k), n-1))
  
  IC <- rbind(IC,
              ICmean)
}
Ps
IC






####### FOURTH CASE: Bonferroni CI for the VARIANCE at level (1-alpha)  ---------------------
# mai usato

# primo cluster
data_c1<- data[clusters==1,] #************** impostare il cluster che si sta analizzando **************** se non funziona metti which(clusters==1) al posto di (clusters==1) 
n_1 <-  dim(data_c1)[1] #*************
mcshapiro.test(data_c1)
data.mean_1 <- sapply(data_c1, mean) #mean(..) se univariato
data.cov_1 <- cov(data_c1) #var se univariato

n <- n_1
data.cov <- data.cov_1
#Qui sotto togli diag se sei nell'univariato

BFvar_1 <- cbind( inf    =  ((n-1)*diag(data.cov))/qchisq(1-alpha/(2*k), n-1), 
                  center =  diag(data.cov),
                  sup    =  (n-1)*diag(data.cov)/qchisq( alpha/(2*k),n-1))
BFvar_1

# secondo cluster
data_c2<- data[clusters==2,] #************** impostare il cluster che si sta analizzando ****************
n_2 <-  dim(data_c2)[1] #*************
mcshapiro.test(data_c2)
data.mean_2 <- sapply(data_c2, mean)
data.cov_2 <- cov(data_c2) 

n <- n_2
data.cov <- data.cov_2
#Qui sotto togli diag se sei nell'univariato

BFvar_2 <- cbind( inf    =  ((n-1)*diag(data.cov))/qchisq(1-alpha/(2*k), n-1), 
                  center =  diag(data.cov),
                  sup    =  (n-1)*diag(data.cov)/qchisq( alpha/(2*k),n-1))

BFvar_2 

# terzo cluster
data_c3<- data[clusters==3,] #************** impostare il cluster che si sta analizzando ****************
n_3 <-  dim(data_c3)[1] #*************
mcshapiro.test(data_c3)
data.mean_3 <- sapply(data_c3, mean)
data.cov_3 <- cov(data_c3) 

n <- n_3
data.cov <- data.cov_3
#Qui sotto togli diag se sei nell'univariato

BFvar_3 <- cbind( inf    =  ((n-1)*diag(data.cov))/qchisq(1-alpha/(2*k), n-1), 
                  center =  diag(data.cov),
                  sup    =  (n-1)*diag(data.cov)/qchisq( alpha/(2*k),n-1))
BFvar_3











#### Kmeans Clustering ####

# if you DO know the labels:
#*
data.lab <- data$...
data$... <- NULL

n <- dim(data)[1]
p <- dim(data)[2]

# graphical representation of data:
pairs(data)


# setting k
k <- 2  #*

# performing kmeans clustering
result.k <- kmeans(data, centers = k) # Centers: fixed number of clusters

# see the results:
result.k$cluster        # labels of clusters
result.k$centers        # centers of the clusters
# result.k$totss          # tot. sum of squares
# result.k$withinss       # sum of squares within clusters
# result.k$tot.withinss   # sum(sum of squares in the cluster)
# result.k$betweenss      # sum of squares between clusters
# result.k$size           # dimension of the clusters
# and other useful features (see names(result.k))

# graphical representation
centers <- as.data.frame(result.k$centers)
plot(rbind(data, centers), 
     col = c(result.k$cluster+1, rep('black', times = k)), 
     pch = c(rep(19, times=n), rep(18, times=k)), 
     cex = c(rep(1, times=n), rep(1.75, times = k)))


## HOW to choose k?
#
# 1) method 1: evaluate the variability between the groups with respect to the variability withing the groups
# 2) method 2: evaluate the result of hierarchical clustering

# 1) method 1:
b <- NULL
w <- NULL
for(k in 1:10){
  result.k <- kmeans(Q, k)
  w <- c(w, sum(result.k$wit)) # within sum of squares
  b <- c(b, result.k$bet)      # between sum of squares
}
matplot(1:10, w/(w+b), pch='', xlab='clusters', ylab='within/tot', main='Choice of k', ylim = c(0,1))
lines(1:10, w/(w+b), type='b', lwd=2)
# look for an elbow etc....

# 2) method 2:
#
# see Hierarchical Clustering




#### DBSCAN clustering ####

data <- data.frame(data)
data <- as.matrix(data)
data <- as.dist(data)
# Rule of thumb, minPts should be at least p + 1 = 3 here
# Can be eventually increased
minpts <- 3
# Plot of the distances to the minPts-1 nearest neighbor
kNNdistplot(data, minPts = minpts)
# Taking eps = 0.05 seems to be a good threshold
abline(h = 0.05, col = "red", lty = 2)

# Run the dbscan
dbs <- dbscan(data, eps = 0.05, minPts = minpts)
dbs

# Plot of the resulting clustering
plot(x, col = dbs$cluster + 1L, pch=19)

# silhouette
clustered_index <- which(dbs$cluster != 0) # Index of non noise points
clustered_points <- x[clustered_index] # only clustered points
clustered_labels <- dbs$cluster[clustered_index] # corresponding labels

sil <- silhouette(clustered_labels, dist(clustered_points))
summary(sil)

#sil_score <- function(labels, dist) {   ATTENZIONE! togli #, era per evitare funzione
  # Compute the average of the silhouette widths 
  sil <- silhouette(labels, dist)
  sil_widths <- sil[,"sil_width"]
  mean(sil_widths)
}

sil_score(clustered_labels, dist(clustered_points))




# Suggest a quantitative method for comparing the quality of the clustering results obtained from this DBSCAN
#run and the hierarchical clustering conducted in b)

library(cluster)

sil <- silhouette(dbs$cluster, dist(dati))
summary(sil)
mean(sil[, "sil_width"])
#0.3451817

sil <- silhouette(cluster.ec, dist(dati))
summary(sil)
mean(sil[, "sil_width"])
#0.5633844

#Values close to 1 indicate that the object is well clustered.
#Values close to 0 indicate that the object lies between two cluste



#### Multidimensional Scaling ####

#Dimension of the space in which i want to represent the data
k <- ...  #* 

# setting distance:
#*
distance <- 'euclidean' # manhattan, canberra
# distance matrix:
data.dist <- dist(data, method=distance)


#chatgpt

data.map <- cmdscale(data.dist, k=k)
# Create a color vector for clusters:
colors <- c("red", "blue", "green")  # Assign more colors if you have more clusters
cluster_colors <- colors[as.factor(clusters)]  # Assign colors based on clusters
# Plot the MDS results with colors according to clusters:
plot(data.map[, 1], data.map[, 2], col = cluster_colors, pch = 16, asp = 1, 
     main = "MDS of Data Colored by Cluster", xlab = "Dimension 1", ylab = "Dimension 2")
# Optionally, add cluster labels:
text(data.map[, 1], data.map[, 2], labels = clusters, col = cluster_colors, cex = 0.75, pos = 3)
# Alternatively, if you want to label with molecule names and color by clusters:
text(data.map[, 1], data.map[, 2], labels = colnames(as.matrix(data.dist)), 
     col = cluster_colors, cex = 0.75, pos = 3)




data.map <- cmdscale(data.dist, k=k)

plot(data.map[,1], data.map[,2], type='n', asp=1, axes=FALSE, main="MDS of data",xlab='',ylab='')
text(data.map[,1], data.map[,2], labels=colnames(as.matrix(data.dist)), cex = 0.75, pos = 3)

# compare the original distance matrix d_ij = d(x_i,x_j) and delta_ij = d(y_i,y_j) 
plot(data.dist, dist(data.map))  # Is good if the data are on the bisector


# visualize the most different distances
p <- dim(data.dist)[2]
par(cex = 0.75, mar = c(10,10,2,2))
image(1:p, 1:p, asp=1, abs(as.matrix(dist(data.map)) - as.matrix(data.dist)), axes = F, xlab = '', ylab ='')
axis(1, at = 1:p, labels = colnames(as.matrix(data.dist)), las = 2, cex = 0.75)
axis(2, at = 1:p, labels = colnames(as.matrix(data.dist)), las = 1, cex = 0.75)
box()

# I Compute the "stress": the higher it is, the worse the matching between original distances and their
# geometrical representation through MDS
Stressk <- NULL
for(kk in 1:5)
{
  data.map.k <- cmdscale(data.dist, kk)
  Stress <- (sum( (as.vector(data.dist) - as.vector(dist(data.map.k)))^2)  /
               sum( as.vector(data.map.k)^2))^(1/2)
  Stressk <- c(Stressk, Stress) 
}

plot(1:5,Stressk,xlab='k',ylab='Stress',lwd=2)
# I choose k = .. since i see an elbow.








#### LM - Multiple Linear Regression ####

# inizio caso particolare     inizio        inizio        inizio    inizio        inizio        inizio 
# CASO I: ho due colonne separate
data <- read.table('Pb2.txt', header=T)
head(data)
data.new <- data.frame(
  mese = c(1:12),
  temp = c(data$Edmonton, data$Resolute),   #qui metti il nome delle colonne
  Region = factor(rep(c("0", "1"), each = nrow(data)))   # al posto di each-> ,c(length(data$Edmonton), length(data$Resolute))
)
data<- data.new
head(data)
dim(data)
# fine caso particolare     fine        fine        fine          fine        fine      fine


pairs(data)
# Control if i have some patterns and i need to transform my data 
#(p.e. if data don't have a clear shape probably i have to y<-log(y), x<-log(x))

n     <- dim(data)[[1]]
y     <- data$...
reg1  <- data$...   #al posto di reg1, se vuoi, il nome che preferisci
reg2  <- data$...   #puoi mettere anche una colonna di dummy variables

## se nel modello proposto nell'esercizio c'è uno dei regressori al quadrato
## mettere un'altra variabile reg, ad esempio reg2 <-reg1^2 oppure nel modello mettere I(reg^2)


## Model:
# [ y = beta_0 + beta_1 * reg1 + beta_2 * reg2 + varepsilon ]

## Model assumptions:
#
# 1) Parameter estimation: E(varepsilon) = 0   and Var(varepsilon) = sigma^2
# 2) Inference:            varepsilon ~ N(0, sigma^2)


fm <- lm(y ~ reg1 + reg2, data)

fm <- lm(y ~  reg1*reg2, data )  #se nel modello si considerano reg1, reg2, e la moltiplicazione tra reg1 e reg2
 #uguale a
fm <- lm(y ~ reg1 + reg2 + reg1:reg2 , data)

fm = lm(yield ~ -1 + as.factor(species) + temp, data)
fm <- lm(price ~ two.bathrooms*. ,data ) 

#per 
#  Y =αg + βg·dim +γg ·weight +δg*dim·weight + ε con g=1,2 processing
fm <- lm(price ~ processing + dimension:processing  + weight:processing + (dimension*weight):processing, data)
# non serve aggiungere la colonna dim*weight a data



anova(fm1, fm2)
#H0 anova: fm1 e fm2 sono modelli simili
#H1 anova:

## H0: bi=0, H1: bi!=0
summary(fm) 

par(mfrow=c(2,2))
plot(fm)

shapiro.test(residuals(fm))
vif(fm)

dev.off()
#Diagnostic Plots
#Residuals vs Fitted: If points appear randomly distributed around the horizontal line with no pattern, the assumptions of linearity and homoscedasticity are respected.
#Normal Q-Q: If points follow the diagonal line, the residuals are normally distributed.
#Scale-Location: If points are homogeneously distributed, there are no heteroscedasticity issues.
#Residuals vs Leverage: Few or no points outside Cook's distance lines , we have no significant influential points.
#Shapiro
#the p-value is greater than 0.05, we conclude that the residuals are normally distributed.
#VIF
#VIF = 1: No correlation between the predictor and other variables.
#1 < VIF < 5: Moderate correlation. Generally, not a cause for concern.
#VIF ≥ 5: High correlation. Indicates potential multicollinearity issues.
#VIF > 10: Very high correlation. Suggests serious multicollinearity problems
#goodness of fit: R^2 is very high, it means that the model can explain 0.9697 of variability, it
# might be too high, overfitting problem => it becomes an interpolation of data, not a useful model that predicts new values


# The parameters are: !!!!!!!!!!!!
beta <- coefficients(fm)
beta

sigmasq <- sum(residuals(fm)^2)/fm$df # estimate of sigma^2
sigmasq
sigma <- sqrt(sigmasq) # estimate of sigma
sigma

AIC(fm)

#fitted(fm)        # y hat
#residuals(fm)     # eps hat   (misfit tra gli yhat calcolati e i veri y)
# 
# coefficients(fm)             # beta_i
# summary(fm)$coefficients[,4] #p-values of beta_i
# vcov(fm)                     # cov(beta_i)
# 
# fm$rank # order of the model [r+1]
# fm$df   # degrees of freedom of the residuals [n-(r+1)]
# 
# hatvalues(fm) # h_ii
# rstandard(fm) # standardized residuals
# # sum(residuals(fm)^2)/fm$df  ###### estimate of sigma^2
#





# Plot the Model:
# (you can plot if you have only one variable, remember to keep all the betas that account for that variable)
plot(reg1,y, las=1, xlim=range(reg1), ylim=range(y))
x <- seq(by=0.1, from=min(reg1), to=max(reg1))
b <- coef(fit)
lines(x, b[1]+b[2]*x)  #+b[3]*x^2)



# Verifying the model assumptions: Grafico residui:

par(mfrow=c(2,2))
plot(fit)     

shapiro.test(residuals(fit))


## Inference on the parameters

# Fisher Test is a test of global significance of the regression, namely:
# [H_0: (beta_1, beta_2) = (0,0)  vs  H_1: (beta_1, beta_2) neq (0,0) ]

# setting the number of covariates you have included in your model 
# (i.e. number of regressors (with intercept))
r <- ...
C   <-  matrix(c(-1, 1, 0, 0, 0, 0, 0,
                 0, -1, 1, 0, 0, 0, 0,
                 0, 0, -1, 1, 0, 0, 0,
                 0, 0, 0, -1, 1, 0, 0,
                 0, 0, 0, 0, -1, 1, 0,
                 0, 0, 0, 0, 0, -1, 1), 6, 6, byrow=T)
d <- c(0,0,..)                        
linearHypothesis(fm, C, d)

#oppure
linearHypothesis(fm,
                 rbind(c(0,0,0,0,0,0,0,1,0),
                       c(0,0,0,0,0,0,0,0,1)),
                 c(0,0))

#oppure
anova(fm, fitmodellocercato)




###### Plotting Confidence Region for the regressors: ------------
p <- r  # number of tested coefficient (in this case, p=r)

# center: point estimate
coefficients(fm)[2:r]
# Direction of the axes
eigen(vcov(fm)[2:r,2:r])$vectors


# if r=2, I can plot the ellipse:
xrange <- c(coefficients(fm)[2]- sqrt(p*qf(1-0.05,p,n-(r))), 
            coefficients(fm)[2] +sqrt(p*qf(1-0.05,p,n-(r))))
yrange <- c(coefficients(fm)[3]- sqrt(p*qf(1-0.05,p,n-(r))), 
            coefficients(fm)[3] +sqrt(p*qf(1-0.05,p,n-(r))))
plot(coefficients(fm)[2], coefficients(fm)[3], xlim = xrange, ylim = yrange, asp=1, 
     xlab='beta1', ylab='beta2')
ellipse(coefficients(fm)[2:3], vcov(fm)[2:3,2:3], sqrt(p*qf(1-0.05,p,n-(r))))
abline(v=0)
abline(h=0)
# note that if the Ellipse is stretched, regressors might be collinear.





# the model explain the dependance between th e variable and the regressor?
points(reg,fitted(fm),col='blue', pch=19) #reg inserire quale
summary(fm)$r.squared







##### Bonferroni intervals (of level 1-alpha) #"Intervallo in cui stanno i regressori" -----
alpha <- ...
confint(fm, level= 1-alpha/p)[2:r,]  # r: number of regressors (minus beta0)
# Note: confint() returns the confidence intervals one-at-a-time;
# to build BonfCI with global level 1-alpha we need to include Bonf-correction (level= 1-alpha/p)


### Test:
# H0: (a1*beta0+b1*beta1+c1beta2, a2beta0+b2beta1+c2beta2, ...) == (d1,d2, ...) vs H1: (a1*beta0+b1*beta1+c1beta2, a2beta0+b2beta1+c2beta2, ...) != (d1,d2, ...)   
# C*[beta0,beta1, beta2,...]= c(d1,d2,...): test linear combination of the coefficients
C <- rbind(c(0,1,1), # beta1 + beta2   #
           c(1,0,0), # beta0
           c(0,1,0), # beta1
           c(0,0,1)) # beta2
d <- c(0,0,..)                        

linearHypothesis(fm, C, d)











# INCREMENTO dipedente dal REGRESSORE
# Waste = A + B * t  + C * (1-cos(2pi / 12 * t)) + eps
# The University of Cantabria considered that the GROWTH attributable to
# residents is quantifiable in an increase of 10 tons per month (REGRESSORE).
fit <- lm(rifiuti ~ mese + I(1 - cos(2*pi/12*mese)))

linearHypothesis(fit,rbind(c(0,1,0)),10)

# or (from the summary)
summary(fit)
t <- (coef(fit)[2]-10)/sqrt(diag(vcov(fit))[2])
t
pval <- 2*(1-pt(t,29-(2+1)))
pval

### question e)
rifiuti.vinc <- rifiuti - 10*mese

fit2 <- lm(rifiuti.vinc ~ I(1 - cos(2*pi/12*mese)))
summary(fit2)





# AGGIORNARE
### Se viene chiesto se il reg_i è m volte il reg_j

C <- rbind(c(0,0,1,-m))  # 1 ed m nelle posizioni dei regressori i e j 
# beta2
d <- c(0)                #

linearHypothesis(fm, C, d)

# Aggiornare i coefficienti
fm$coefficients[i]=m*fm$coefficients[j]

fm <- lm(y ~ reg1 +reg3+ reg4)
summary(fm)






##### CONFIDENCE INTERVALS for the mean & prediction of a new obs --------------------
# A volte lo chiedono come CONFIDENCE INTERVALS of GLOBAL LEVEL for the mean 
#In this case i need the assumpion of Gaussianity. 
new.datum <- data.frame(reg1=..., reg2=...)    # freq =3.2, OS= "Windows"  (se è character)
                                              # shower=TRUE (se è logical)
alpha <- ... 
# Conf. int. for the mean
Conf <- predict(fm****, new.datum, interval='confidence', level=1-alpha)  # ATTENZIONE: SE CHIEDONO CI for the mean fixed cost for y (ovvero non ho un dato specifico) metti tutti regressor = 0  
                                                                      # ATTENZIONE: the mean fixed cost sono i costi fissi e non il costo medio 
Conf
# Note: confint() returns the confidence intervals one-at-a-time;
# to build BonfCI with global level 1-alpha we need to include Bonf-correction (level= 1-alpha/p)

# Pred. int. for a new obs
Pred <- predict(fm, new.datum, interval='prediction', level=1-alpha)  
Pred
# Pointwise prediction
pointwise_prediction <- (Pred[2]+Pred[3])/2 # cioè Pred[1]
pointwise_prediction



plot(data, xlab='x', ylab='y', las=1, xlim=range(reg1), ylim=range(y))
x <- seq(range(reg1),by=0.1)
b <- coef(fm)
lines(x, b[1]+b[2]*x) #+b[3]*x^2)
points(new.datum$reg1,Conf[1], pch=19)
segments(new.datum$reg1,Pred[2], new.datum$reg1,Pred[3], col='gold', lwd=2)
segments(new.datum$reg1,Conf[2], new.datum$reg1,Conf[3], col='red', lwd=2)
points(new.datum$reg1,Conf[2], pch='-', col='red', lwd=2)
points(new.datum$reg1,Conf[3], pch='-', col='red', lwd=2)
points(new.datum$reg1,Pred[2], pch='-', col='gold', lwd=2)
points(new.datum$reg1,Pred[3], pch='-', col='gold', lwd=2)





# We can repeat these for values of speed between 0 and 30
#In this case i need the assumpion of Gaussianity.
# (prediction and confidence one-at-the-time intervals)
# (remember: they are point-wise intervals! NOT bands!!)
# new.data <- data.frame(cbind(reg1=seq(range(reg1), length=100), 
#                          reg2=seq(range(reg2), length=100)))  #if i want a grid
new.data <- data.frame(cbind(reg1=...,  reg2=...))              #for only 1 data
Conf <- predict(fm, new.data, interval='confidence')
Pred <- predict(fm, new.data, interval='prediction')

plot(cars, xlab='x', ylab='y', las=1, xlim=range(reg1), ylim=range(y))
lines(new.data[,1], Conf[,'fit'])
lines(new.data[,1], Conf[,'lwr'], lty=2, col='red', lwd=2)
lines(new.data[,1], Conf[,'upr'], lty=2, col='red', lwd=2)

lines(new.data[,1], Pred[,'lwr'], lty=3, col='gold', lwd=2)
lines(new.data[,1], Pred[,'upr'], lty=3, col='gold', lwd=2)












# ESEMPIO
# Model B: mu(t) = alpha1 + beta1 * t for 1768 <= t <= 1815
#          mu(t) = alpha2 + beta2 * t for t <= 1767 or t> = 1816;
# DIVENTA
# D <- ifelse(Anno>=1768 & Anno<=1815, 1, 0)
# fitB <- lm(Numero ~ D + Anno + D*Anno )
# summary(fitB)

# alpha <- c(alpha1=coef(fitB)[1]+coef(fitB)[2],alpha2=coef(fitB)[1])
# alpha
# beta  <- c( beta1=coef(fitB)[3]+coef(fitB)[4], beta2=coef(fitB)[3])
# beta

# (c)
#    using the model (b) and Bonferroni's inequality, they provide 2 90% 
#    global confidence intervals for the mean and the variance of the number
#    of works included in the index in the year 1800;
# 
# k <- 2
# alpha <- .1
# n <- dim(index)[1]
# r <- 3
# 
# Z0   <- data.frame(D=1, Anno=1800)
# ICBmean <- predict(fitB, Z0, interval='confidence',level=1-alpha/k) 
# ICBmean
# 
# e <- residuals(fitB)
# ICBvar <- data.frame(L=t(e)%*%e/qchisq(1-alpha/(2*k),n-(r+1)),
#                      U=t(e)%*%e/qchisq(alpha/(2*k),n-(r+1)))
# ICBvar

# (d)
# using the model (b), provide a 90% confidence interval for the difference 
# between the average number of works added to the Index in 1816 and average 
# number of works added in 1815.
# a <- c(0,-1,1,-1815)
# Bf <- c('1816-1815_L'= t(a) %*% coefficients(fitB) - sqrt(t(a) %*% vcov(fitB) %*% a) * qt(1 - alpha/2, n-(r+1)),
#         '1816-1815_U'= t(a) %*% coefficients(fitB) + sqrt(t(a) %*% vcov(fitB) %*% a) * qt(1 - alpha/2, n-(r+1)) )
# Bf



##### Bonferroni and simultaneus ci with Fischer ------


beta <- coefficients(fit2) 
beta

sigmasq <- sum(residuals(fit2)^2)/fit2$df # estimate of sigma^2
sigmasq

sigma <- sqrt(sigmasq)

Z <- model.matrix(fit2)
invztz <- solve(t(Z)%*%Z)
Z0 <- cbind("nuovo dato1","nuovo dato 2",...)

alpha <- ....
k <- dim(Z0)[2]

# Bonferroni CI for the prediction of multiple data with global confidence alpha
ICb <- NULL
for(i in 1:k){
  ICb <- cbind(ICb,rbind((t(Z0) %*% beta)[i] - sigma*sqrt(1 + diag(t(Z0)%*%invztz%*%Z0)[i])*qt(1 - alpha/(2*k),fit2$df.residual),(t(Z0) %*% beta)[i],
                         (t(Z0) %*% beta)[i] + sigma*sqrt(1 + diag(t(Z0)%*%invztz%*%Z0)[i])*qt(1 - alpha/(2*k),fit2$df.residual)))
}
ICb                                                                         

# Simultaneous CI for the prediction of multiple data with confidence alpha                                                                            
ICs <- NULL 
for(i in 1:k){
  ICs <- cbind(ICs,rbind((t(Z0) %*% beta)[i] - sigma*sqrt(1 + diag(t(Z0)%*%invztz%*%Z0)[i])*sqrt((n-fit2$df.residual)*qf(1 - alpha,n-fit2$df.residual,fit2$df.residual)),(t(Z0) %*% beta)[i],
                         (t(Z0) %*% beta)[i] + sigma*sqrt(1 + diag(t(Z0)%*%invztz%*%Z0)[i])*sqrt((n-fit2$df.residual)*qf(1 - alpha,n-fit2$df.residual,fit2$df.residual))))
}
ICs


# Bonferroni CI for the mean of multiple data with global confidence alpha
ICb <- NULL
for(i in 1:k){
  ICb <- cbind(ICb,rbind((t(Z0) %*% beta)[i] - sigma*sqrt(diag(t(Z0)%*%invztz%*%Z0)[i])*qt(1 - alpha/(2*k),fit2$df.residual),(t(Z0) %*% beta)[i],
                         (t(Z0) %*% beta)[i] + sigma*sqrt(diag(t(Z0)%*%invztz%*%Z0)[i])*qt(1 - alpha/(2*k),fit2$df.residual)))
}
ICb                                                                         

# Simultaneous CI for the mean of multiple data with confidence alpha                                                                            
ICs <- NULL 
for(i in 1:k){
  ICs <- cbind(ICs,rbind((t(Z0) %*% beta)[i] - sigma*sqrt(diag(t(Z0)%*%invztz%*%Z0)[i])*sqrt((n-fit2$df.residual)*qf(1 - alpha,n-fit2$df.residual,fit2$df.residual)),(t(Z0) %*% beta)[i],
                         (t(Z0) %*% beta)[i] + sigma*sqrt(diag(t(Z0)%*%invztz%*%Z0)[i])*sqrt((n-fit2$df.residual)*qf(1 - alpha,n-fit2$df.residual,fit2$df.residual))))
}
ICs







#### Linear Regression: ANCOVA ####


n     <- dim(data)[[1]]
y     <- data$...   #
reg1  <- data$...   #
reg2  <- data$...   #



## se nel modello proposto nell'esercizio c'è uno dei regressori al quadrato
## mettere un'altra variabile reg, ad esempio reg2 <-reg1^2 oppure nel modello mettere I(reg^2)




# Model:
# (Se ho 2 regressori oltre a Beta0 e i gruppi sono 2)
#  [ Reg = b_0+b_1*reg1+b_2*reg2 +b_3*d1 +b_4*d1*reg1+b_5*d1*reg2 + varepsilon,  varepsilon sim N(0, sigma^2)]
# Indeed:
#   begin{align*}
#  beta_0^{group1}=b_0;         beta_1^{group1}=b_1;    beta_2^{group1}=b_2;
#  beta_0^{group2}=b_0+b_3;     beta_1^{group2}=b_1+b_4  beta_2^{group2}=b_2+b_5;;
# end{align*}
#
#
#(Se ho 1 regressore oltre a Beta0 e i gruppi sono 3)
#I can perform the estimation of the linear model. 
#  [ Reg = b_0+b_1*d1+b_2*d2 +b_3*reg1+b_4*d1*reg1+b_5*d2*reg1 + varepsilon,  varepsilon sim N(0, sigma^2)]  
# Indeed:
#   begin{align*}
#  beta_0^{group1}=b_0;         beta_1^{group1}=b_3; 
#  beta_0^{group2}=b_0+b_1;     beta_1^{group2}=b_3+b_4;
#  beta_0^{group3}=b_0+b_2;    beta_1^{group3}=b_3+b_5
# end{align*}
#
#
# Model:
#(Se ho 1 regressore oltre a Beta0 e i gruppi sono 2)
#  [ Reg = b_0+b_1*reg1+b_2*d1 +b_3*d1*reg1 varepsilon,  varepsilon sim N(0, sigma^2)]
# Indeed:
# begin{align*}
#  beta_0^{group1}=b_0;         beta_1^{group1}=b_1;      
#  beta_0^{group2}=b_0+b_2;     beta_1^{group2}=b_1+b_3   
# end{align*}
#
#


#(se g1=2, r=2 aggiungo reg2 ed elimino d2) )
fit <- lm(y ~ reg1 + reg2 + d1 + reg1:d1 + reg2:d1)



#(Se g1=3, r=1)   
#fit <- lm(y ~ d1 + d2 + reg1 + reg1:d1 + reg1:d2)

summary(fit)

# fitted(fit)        # y hat
# residuals(fit)     # eps hat   (misfit tra gli yhat calcolati e i veri y)
# 
# coefficients(fit)  # beta_i
# summary(fit)$coefficients[,4] #p-values of beta_i
# vcov(fit)          # cov(beta_i)
# 
# fit$rank # order of the model [r+1]
# fit$df   # degrees of freedom of the residuals [n-(r+1)]
# 
# hatvalues(fit) # h_ii
# rstandard(fit) # standardized residuals
# # sum(residuals(fit)^2)/fit$df  ###### estimate of sigma^2
#
# The parameters are: 
coefficients(fit) 
sum(residuals(fit)^2)/fit$df # sigma^2
# 
# PER TROVARE I PARAMETRI SOMMA I RISPETTIVI VALORI CHE ESCONO DAL COEFF(fit) (da cui escono le b)




# Plot the regression line (2 dummy + N regressors ) (only in 2 dimensions)
col=rep(NA, n)
col[which(d1==0 & d2==0)]=rainbow(g1*g2)[1] #If i have 2 dummy (2 cat.l variables with 2 groups each or 1 cat. variable with 3 groups)                            #         i have to provide all the interactions ()
col[which(d1==1 & d2==0)]=rainbow(g1*g2)[2] #I HAVE TO PROVIDE ALL THE INTERACTIONS! (g1*g2)
#col[which(d1==0)& d2==1 )]=rainbow(g1*g2)[3]
#col[which(d1==1)& d2==1 )]=rainbow(g1*g2)[4]


plot(reg1, y, main='Scatterplot of Y vs Reg', lwd=2, 
     xlab='Reg', ylab='Y', col = col)

coef <- fit$coef
#abline(coef beta0 + dummy, coef reg1 + reg1:dummy)  #DEVO FARE g1*g2 plot
abline(coef[1],coef[2],lwd=2,col=...) #
abline(coef[1]+coef[3],coef[2],lwd=2,col=...) #
abline(coef[1]+coef[4],coef[2],lwd=2,col=...) #
abline(coef[1]+coef[3]+coef[4],coef[2],lwd=2,col=...) #



# PLOT THE REGRESSION LINE (1 dummy + 1 reg) (vedi Ex4 120919)

plot(data$..., data$...,xlab="...",ylab="observed data",type="l") #dati originali che voglio plottare

x <- data[which(data$GRUPPO == "...."),] # GRUPPO a cui corrisponde dummy 1*
newdata <- data.frame(cbind(reg1 = x[,2] ,d1 = 1)) # sostituisco a 2 la colonna a cui corrisponde reg1 *
y_t <- predict(fit, newdata)
points(x$..., y_t, xlab="...",ylab="observed data",type="l", col = "blue") #colonna di reg1 

x <- data[which(data$GRUPPO == "..."),]  #GRUPPO a cui corrisponde dummy 0*
newdata2 <- data.frame(cbind(reg1 = x[,2] ,d1 = 0)) # sostituisco a 2 la colonna a cui corrisponde reg1 *
y_t2 <- predict(fit, newdata2) 
points(x$..., y_t2, xlab="...",ylab="observed data",type="l", col = "red") #colonna di reg1 



## Diagnostic of residuals:
par(mfrow=c(2,2))
plot(fit)     

# * Top-Left: scatterplot of residuals against the fitted values 
# ($hat{y}$ on x-axis, $hat{varepsilon}$ on y-axis). 
# The red line corresponds to the general trend, namely the local average of the residuals 
# (to give you an idea if there are specific patterns in your dataset); 
# dashed grey line corresponds to the value 0. 
# Moreover, it points out some possible outliers, enumerated as the number of row 
# corresponding to the original dataset. 
#### We should see homogeneous distribution around 0 with no specific patterns and no particular shapes. 
# 
# * Top-Right: Normal QQ-plot of the quantiles. 
# I have the theoretical quantiles of the Gaussian distribution and the standardized 
# residuals on the y-axis. Again here you see outliers highlighted with numbers. 
#### The scatterplot of the data should be as close as possible to the straight line, 
#### if you see a tail you have the indexes of the extreme observations.
#
# * Bottom-Left: Scale-Location plot. 
# Square root of the standardized residuals against the fitted values. 
#### Here you should see homogeneous distribution with no patterns and no trends.
# 
# * Bottom-Right: Residuals vs Leverage. 
# It provides the standardized residuals against the leverage ($h_{ii}$). 
# You also have the isolines of the Cook's distance represented with red dashed lines. 
#### Using that you can identify the leverage points looking at those points that 
#### lies outside the dashed lines.



shapiro.test(residuals(fit))
# High (> 0.1): there is no statistical evidence to reject H0 (I can assume Gaussianity of data)
# Very low (< 0.05): there is statistical evidence to reject H0 (I can NOT assume Gaussianity of data)
# (this test is a confirmation of the normal QQ-plot: check if there are some heavy tails)





### Test on regressors:
# H0: (a1*beta0+b1*beta1+c1beta2, a2beta0+b2beta1+c2beta2, ...) == (d1,d2, ...) 
# vs H1: (a1*beta0+b1*beta1+c1beta2, a2beta0+b2beta1+c2beta2, ...) != (d1,d2, ...)   
# C*[beta0,beta1, beta2,...]= c(d1,d2,...): test linear combination of the coefficients
C <- rbind(c(0,1,1), # beta1 + beta2   #
           c(1,0,0), # beta0
           c(0,1,0), # beta1
           c(0,0,1)) # beta2
d <- c(0,0,..) 
linearHypothesis(fit, C, d)
#Test per vedere se il gruppo influisce: tutto 0 tranne i bi moltiplicati per dummy (anche l'interazione) 
#Test per vedere se il regressore e' influente: tutto 0 tranne i bi moltiplicati per il regressore
#Test per vedere se il gruppo influisce sul regressore i: tutto 0 tranne i bi moltiplicati per il regressore i E le dummy







# Se viene chiesto se il MASSIMO della variable y è per un valore specifico del reg1
# plot(reg1,y, las=1, xlim=range(reg1), ylim=range(y))
# x <- seq(by=0.1, from=min(reg1), to=max(reg1))
# b <- coef(fit)
# lines(x, b[1]+b[2]*x+b[3]*x^2) # il secondo elemento è l'espressione della funzione
## l'andamento è a parabola quindi posso controllare se la derivata nel valore specifico di reg1
# è nulla

# C <- rbind(.. coefficienti della derivata della funzione ... )  esempio C <- rbind(0,1,2*valore di reg 1(era 61))
# d <- c(0)
# linearHypothesis(fit, C, d)
## Se si bisogna aggiornare il parametro:
# fit <- lm(y ~ I(-2*61*reg1 + reg2))









# MASSIMO uguale a un valore dipendentemente dal GRUPPO
g1 <- lm(waiting.time ~ day.of.the.week + I(1 + cos(4*pi*day/365)))

incremento <- 60
linearHypothesis(g1, c(1,1,2), incremento) # inserire  i coefficienti per avere il massimo della funzione 
#So we propose the reduced model
# y = alpha_0 + beta(1 + cos(4 * pi * day/365)) + I(g = 1)*(60 - 2*beta) + eps
W <- ifelse(day.of.the.week=="weekend",1,0)
wait <- waiting.time
wait[which(W==1)] <- wait[which(W==1)] - 60
g_red <- lm(wait ~ I(1-W) + I((1 + cos(4 * pi * day/365)) - 2*W) - 1)
summary(g_red)
shapiro.test(g_red$residuals)
params_red <- c(alpha_0 = g_red$coefficients[1],
                alpha_1 = 60 - 2*g_red$coefficients[2],
                beta = g_red$coefficients[2])
s_red <- summary(g_red)$sigma
z_new <- data.frame(W = 0, day = 238)
predict(g_red, z_new, interval = "prediction", level = 0.95)+60*(z_new$W==1) # predizione di un nuovo dato



# INCREMENTO DIPENDENTE DAL RGRUPPO
# AGGIORNARE





# there is a significant difference between the groups of the dummy, 
# in the increase of the mean sales along time. 
C <- rbind(c()) # mettere il vettore di 0 e un 1 dove c'è reg:dummy
d <- c(0,0,..) 
linearHypothesis(fit, C, d)









## Confidence intervals for the mean & prediction of a new obs
#In this case i need the assumpion of Gaussianity.
new.datum <- data.frame(reg1=..., reg2=..., d1= ..., d2= ...)  #mettere f(reg1) se reg1 NON ? LINEARE! #*
alpha <- 0.05 #
# Conf. int. for the mean
Conf <- predict(fit, new.datum, interval='confidence', level=1-alpha)  
Conf
# Pred. int. for a new obs
Pred <- predict(fit, new.datum, interval='prediction', level=1-alpha)  
Pred
# Pointwise prediction
pointwise_prediction <- (Pred[2]+Pred[3])/2 # cioè (lwr+upw)/2
pointwise_prediction
# Condifence interval for the maximum
# # Il massimo ? ottenuto ad esempio per t = 365 e wet = 0, sea = 1
# shapiro.test(residuals(fitted))
# new = data.frame(t=365,sea=1,wet=0)
# interval = predict(fitted,new,interval="confidence",level=0.99)






# PLOT
plot(data, xlab='x', ylab='y', las=1, xlim=range(reg1), ylim=range(y))
x <- seq(range(reg1),by=0.1)
b <- coef(fm)
lines(x, b[1]+b[2]*x*d1...) #+b[3]*x^2)  #
points(new.datum$reg1,Conf[1], pch=19)
segments(new.datum$reg1,Pred[2], new.datum$reg1,Pred[3], col='gold', lwd=2)
segments(new.datum$reg1,Conf[2], new.datum$reg1,Conf[3], col='red', lwd=2)
points(new.datum$reg1,Conf[2], pch='-', col='red', lwd=2)
points(new.datum$reg1,Conf[3], pch='-', col='red', lwd=2)
points(new.datum$reg1,Pred[2], pch='-', col='gold', lwd=2)
points(new.datum$reg1,Pred[3], pch='-', col='gold', lwd=2)





# We can repeat these for values of speed between 0 and 30
# (prediction and confidence one-at-the-time intervals)
# (remember: they are point-wise intervals! NOT bands!!)
#In this case i need the assumpion of Gaussianity.
new.data <- data.frame(cbind(reg1=seq(range(reg1), length=100), 
                             reg2=seq(range(reg2), length=100),
                             d1=c(...), d2=c(...)))
Conf <- predict(fm, new.data, interval='confidence')
Pred <- predict(fm, new.data, interval='prediction')

plot(cars, xlab='x', ylab='y', las=1, xlim=range(reg1), ylim=range(y))
lines(new.data[,1], Conf[,'fit'])
lines(new.data[,1], Conf[,'lwr'], lty=2, col='red', lwd=2)
lines(new.data[,1], Conf[,'upr'], lty=2, col='red', lwd=2)

lines(new.data[,1], Pred[,'lwr'], lty=3, col='gold', lwd=2)
lines(new.data[,1], Pred[,'upr'], lty=3, col='gold', lwd=2)









#### Linear Regression: PCA solution ####

n     <- dim(data)[[1]]
y     <- data$...   #
reg1  <- data$...   #
reg2  <- data$...   #
r <- ... # number of regressors (beta0 included) #

fm <- lm(y ~ reg1 + reg2) #

# check collinearity: compute Variance Inflation Factor
vif(fm)
# if reg_i has vif_i>10, it's consider high (collinear)


data.reg <- cbind(reg1,reg2, ...) #
# performing PCA to solve the problem of collinearity:
pc.data <- princomp(data.reg, scores=TRUE) 
summary(pc.data)
pc.data$loadings


# plotting first nr = 2 loadings
nr <- 2    # number of components you want to visualize
par(mfrow = c(nr,1))
for(i in 1:nr) barplot(pc.data$loadings[,i], ylim = c(-1, 1), main = paste('PC', i))


# plotting results (Screeplot on the right)
varmax <- max(var(data.reg[,1:dim(data.reg)[2]]))
varmax_pc <- max(pc.data$sd)
layout(matrix(c(2,3,1,3),2,byrow=T))
plot(pc.data, las=2, main='Principal components', ylim=c(0,varmax_pc^2))
barplot(sapply(data.reg,sd)^2, las=2, main='Original Variables', ylim=c(0,varmax),
        ylab='Variances')
plot(cumsum(pc.data$sd^2)/sum(pc.data$sd^2), type='b', axes=F, 
     xlab='number of components', ylab='contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.8, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(data.reg),labels=1:ncol(data.reg),las=2)

# choose k: number of components you want to keep (elbow, threshold...)
k <- 2 #


# Now we estimate the model by inserting the PCs instead of the original regressors: Model:
# [ y = b_0 + b_1*PC_1 + b_2*PC_2 + ... + b_k*PC_k + varepsilon,  varepsilon sim N(0, sigma^2)]
reg1.pc <- pc.data$scores[,1] # projection 
reg2.pc <- pc.data$scores[,2] # projection 
# and so on for all k reg... #

# Model:
fm.pc <- lm(y ~ reg1.pc + reg2.pc + ....) #
summary(fm.pc)
# NB: look at the p-values and eventually remove them one by one.


# plotting the regression line in space PC1 vs y:
plot(reg1.pc,y, xlab='PC1', ylab='y', las=1, xlim=range(reg1.pc), ylim = range(y)) 
x1 <- seq(range(reg1.pc), by=1)
b <- coef(fm.pc)
lines(x, b[1]+b[2]*x) 


# I can go back to the original variables by using the expressions of the PCs. 
# So I can transform the coefficients I have obtained with PCA to the coefficients in the original system.
m1 <- mean(reg1)
m2 <- mean(reg2)
# and so on for all reg...  #
m <- c(m1,m2,...) #

beta0 <- coefficients(fm.pc)[1]
for ( i in 2:k+1){
  for (j in 2:r){
    beta0 <- beta0 - coefficients(fm.pc)[i]*pc.data$load[j-1,i-1]*m[j-1]
  }
}

beta1 <- 0
for (i in 2:k+1){
  beta1 <- beta1 + coefficients(fm.pc)[i]*pc.data$load[1,i-1]
}

beta2 <- 0
for (i in 2:k+1){
  beta2 <- beta2 + coefficients(fm.pc)[i]*pc.data$load[2,i-1]
}

#
# and so on for all betas

c(beta0 = as.numeric(beta0),
  beta1 = as.numeric(beta1),
  beta2 = as.numeric(beta2), 
  beta3 = ...) #


# plotting our model in the original space (only in dimension 2): reg1 vs y
x <- seq(range(reg1), len=100)
plot(reg1, y, xlab='reg1', ylab='y', las=1) 
lines(x, beta0+beta1*reg1) # add the regressors that are function of reg1



## Diagnostic of residuals 
par(mfrow=c(2,2)) 
plot(fm.pc)
# * Top-Left: scatterplot of residuals against the fitted values 
# ($hat{y}$ on x-axis, $hat{varepsilon}$ on y-axis). 
# The red line corresponds to the general trend, namely the local average of the residuals 
# (to give you an idea if there are specific patterns in your dataset); 
# dashed grey line corresponds to the value 0. 
# Moreover, it points out some possible outliers, enumerated as the number of row 
# corresponding to the original dataset. 
#### We should see homogeneous distribution around 0 with no specific patterns and no particular shapes. 
# 
# * Top-Right: Normal QQ-plot of the quantiles. 
# I have the theoretical quantiles of the Gaussian distribution and the standardized 
# residuals on the y-axis. Again here you see outliers highlighted with numbers. 
#### The scatterplot of the data should be as close as possible to the straight line, 
#### if you see a tail you have the indexes of the extreme observations.
#
# * Bottom-Left: Scale-Location plot. 
# Square root of the standardized residuals against the fitted values. 
#### Here you should see homogeneous distribution with no patterns and no trends.
# 
# * Bottom-Right: Residuals vs Leverage. 
# It provides the standardized residuals against the leverage ($h_{ii}$). 
# You also have the isolines of the Cook's distance represented with red dashed lines. 
#### Using that you can identify the leverage points looking at those points that 
#### lies outside the dashed lines.

shapiro.test(residuals(fm.pc))
# High (> 0.1): there is no statistical evidence to reject H0 (I can assume Gaussianity of data)
# Very low (< 0.05): there is statistical evidence to reject H0 (I can NOT assume Gaussianity of data)
# (this test is a confirmation of the normal QQ-plot: check if there are some heavy tails)
























#### Linear Regression: ELASTIC NET solution ####

# [ frac{1-alpha}{2} |beta |_2^2 + alpha |beta |_1,  alpha in [0,1] ]
#
# - if $alpha=0$: Ridge regression
# - if $alpha=1$: Lasso regression



n     <- dim(data)[[1]]
y     <- data$...   #
reg1  <- data$...   #
reg2  <- data$...   #
r <- ... # number of regressors (beta0 included) #

fm <- lm(y ~ reg1 + reg2) #

# check collinearity: compute Variance Inflation Factor
vif(fm)
# if reg_i has vif_i>10, it's consider high (collinear)


# build matrix of predictors:
x <- model.matrix(y~reg1+reg2+...)[,-1] #


## if you know lambda:
lambda <- ... # bestlam.net #

# fitting net model 
fit.net <- glmnet(x, y, lambda = lambda, alpha = 0) # if alpha = 0: net

coef.net <- coef(fit.net) # beta coefficients
# yhat.lm <- cbind(rep(1,n), reg1, reg2)%*%coef(fm)      # LM fitted values 
yhat.en  <- cbind(rep(1,n), reg1, reg2)%*%coef.net    # net fitted values


# graphical representation: lm.net()    (and comparison with lm())
plot(reg1, yhat.en, type='l', lty=1, col=grey.colors(length(lambda)), lwd=2) 
points(reg1, y, pch=1, cex=.8)
# matlines(reg1, yhat.lm, type='l', lty=4, lwd=2, ylab='y',xlab='reg1') # comparison with lm
# legend("topleft",c("lm","net"),lty=c(4,1),col=c("black",grey.colors(length(lambda))), lwd = 2)


## if you do NOT know lambda:
lambda.c <- seq(0,10,0.01) #*
fit.net <- glmnet(x, y, lambda = lambda.c, alpha = 0)

# behavior of the coefficient as a function of log(lambda):
plot(fit.net, xvar='lambda',label=TRUE, col = rainbow(dim(x)[2])) 
legend('topright', dimnames(x)[[2]], col = rainbow(dim(x)[2]), lty=1, cex=1)

# Choice of the optimal lambda, e.g., via cross-validation:
cv.net <- cv.glmnet(x,y,lambda=lambda.c, alpha = 0, nfolds=10) # default: 10-fold CV
bestlam.net <- cv.net$lambda.min 
bestlam.net

## analogously:
# cv.net <- lm.net(y ~ reg1 + reg2, lambda = lambda.c)
# bestlam.net <- lambda.c[which.min(cv.net$GCV)]

# once you have the optimal lambda, you can re-run the model



#### Linear Regression: RIDGE solution ####

n     <- dim(data)[[1]]
y     <- data$...   #
reg1  <- data$...   #
reg2  <- data$...   #
r <- ... # number of regressors (beta0 included) #

fm <- lm(y ~ reg1 + reg2) #

# check collinearity: compute Variance Inflation Factor
vif(fm)
# if reg_i has vif_i>10, it's consider high (collinear)


# build matrix of predictors:
x <- model.matrix(y~reg1+reg2+...)[,-1] #


## if you know lambda (lambda is the penalization):
lambda <- ... # bestlam.ridge #

# fitting ridge model 
fit.ridge <- glmnet(x, y, lambda = lambda, alpha = 0) # if alpha = 0: RIDGE

coef.ridge <- coef(fit.ridge) # beta coefficients
# yhat.lm <- cbind(rep(1,n), reg1, reg2)%*%coef(fm)      # LM fitted values 
yhat.r  <- cbind(rep(1,n), reg1, reg2)%*%coef.ridge    # ridge fitted values


# graphical representation: lm.ridge()    (and comparison with lm())
plot(reg1, yhat.r, type='l', lty=1, col=grey.colors(length(lambda)), lwd=2) 
points(reg1, y, pch=1, cex=.8)
# matlines(reg1, yhat.lm, type='l', lty=4, lwd=2, ylab='y',xlab='reg1') # comparison with lm
# legend("topleft",c("lm","ridge"),lty=c(4,1),col=c("black",grey.colors(length(lambda))), lwd = 2)


## if you do NOT know lambda (lambda is the penalization):
lambda.c <- seq(..., ..., length=100)  #*
fit.ridge <- glmnet(x, y, lambda = lambda.c, alpha = 0)

# behavior of the coefficient as a function of log(lambda):
plot(fit.ridge, xvar='lambda',label=TRUE, col = rainbow(dim(x)[2])) 
legend('topright', dimnames(x)[[2]], col = rainbow(dim(x)[2]), lty=1, cex=1)

# Choice of the optimal lambda, e.g., via cross-validation:
cv.ridge <- cv.glmnet(x,y,lambda=lambda.c, alpha = 0, nfolds=10) # default: 10-fold CV
bestlam.ridge <- cv.ridge$lambda.min 
bestlam.ridge
#Coefficients of the model with best lambda:
coef.ridge <- predict(fit.ridge, s=bestlam.ridge, type = 'coefficients')[1:r,]
coef.ridge
## analogously:
# cv.ridge <- lm.ridge(y ~ reg1 + reg2, lambda = lambda.c)
# bestlam.ridge <- lambda.c[which.min(cv.ridge$GCV)]

# once you have the optimal lambda, you can re-run the model




#### Linear Regression: LASSO solution ####

n     <- dim(data)[[1]]
attach(data)
r <- ... # number of regressors (beta0 included) 

fm <- lm(price ~ two.bathrooms*. ,data ) 
summary(fm)
# check collinearity: compute Variance Inflation Factor
vif(fm)
# if reg_i has vif_i>10, it's consider high (collinear)

# build matrix of predictors:
x <- model.matrix(y~reg1+reg2+...)[,-1] #


## if you know lambda (lambda is the penalization):
lambda <- ... # bestlam.lasso #




# fitting lasso model 
fit.lasso <- glmnet(x, y, lambda = lambda, alpha = 1) # if alpha = 1: LASSO

coef.lasso <- coef(fit.lasso) # beta coefficients
coef.lasso

# yhat.lm <- cbind(rep(1,n), reg1, reg2)%*%coef(fm)      # LM fitted values 
yhat.l  <- cbind(rep(1,n), reg1, reg2)%*%coef.lasso      # lasso fitted values


# graphical representation: lm.lasso()    (and comparison with lm())
plot(reg1, yhat.l, type='l', lty=1, col=grey.colors(length(lambda)), lwd=2) 
points(reg1, y, pch=1, cex=.8)
# matlines(reg1, yhat.lm, type='l', lty=4, lwd=2, ylab='y',xlab='reg1') # comparison with lm
# legend("topleft",c("lm","lasso"),lty=c(4,1),col=c("black",grey.colors(length(lambda))), lwd = 2)






## if you do NOT know lambda (lambda is the penalization):
lambda.c <- seq(..., ..., length=100) #*
#if lambda is in a range 
lambda <- seq(0.01,1,by = 0.01)  #*
fit.lasso <- glmnet(x, y, lambda = lambda.c, alpha = 1)

# behavior of the coefficient as a function of log(lambda):
plot(fit.lasso, xvar='lambda',label=TRUE, col = rainbow(dim(x)[2])) 
legend('topright', dimnames(x)[[2]], col = rainbow(dim(x)[2]), lty=1, cex=1)

# Choice of the optimal lambda, e.g., via cross-validation:
cv.lasso <- cv.glmnet(x,y,lambda=lambda.c, alpha = 1, nfolds=10) # default: 10-fold CV
bestlam.lasso <- cv.lasso$lambda.min 
bestlam.lasso  #NEL GRAFICO GUARDO log(lambda)!
optimal.lasso <- cv.lasso$lambda.1se

#Regressori e coefficienti del modello con il miglior lambda:
coef.lasso <- predict(fit.lasso, s=bestlam.lasso, type = 'coefficients')[1:r,]
coef.lasso
selected =  coef.lasso[which(abs(coef.lasso)> 0.01)]



## analogously:
# cv.lasso <- lm.lasso(y ~ reg1 + reg2, lambda = lambda.c)
# bestlam.lasso <- lambda.c[which.min(cv.lasso$GCV)]

# once you have the optimal lambda, you can re-run the model with lambda known


# PREDICTION: 
new <- data.frame(reg1 = ..., reg2 = ..., reg3 = ..., reg4 = ..., reg5 = ..., reg6 = ...) #
predict(fit.lasso, as.matrix(new), s=bestlam.lasso, interval = "prediction", level = 0.95)

#PREDICTION CIPRIANI
new <- data.frame(
  footage = 30, 
  age = 5, 
  renovation = 5, 
  transport = 300, 
  center = 1000, 
  supermarket = 500, 
  park = 100, 
  two.bathrooms = FALSE
)

new_matrix <- model.matrix(~ two.bathrooms * ., new)[,-1]   # al posto di two.bathrooms * .  metti tuoi regressori 
predict(fit.lasso, newx= new_matrix , s=bestlam.lasso, type = 'response')



#### Linear Regression: Comparison btw Elastic, Ridge, Lasso, LM ####

# Compare coefficients estimates for LS, Ridge and Lasso
plot(0,0, pch = '', axes = F, xlab = '', ylab = expression(beta), 
     xlim=c(-1,4), ylim = range(c(coef.lasso[-1], coef.ridge[-1], coef.net[-1], coef(lm(y~x))[-1])))
points(rep(0, dim(x)[2]), coef(lm(y~x))[-1], col=rainbow(dim(x)[2]), pch=20) # lm
points(rep(1, dim(x)[2]), coef.ridge[-1], col=rainbow(dim(x)[2]), pch=20)    # ridge
points(rep(2, dim(x)[2]), coef.lasso[-1], col=rainbow(dim(x)[2]), pch=20)    # lasso
points(rep(3, dim(x)[2]), coef.net[-1],   col=rainbow(dim(x)[2]), pch=20)    # elastic net
abline(h=0, col='grey41', lty=1)
box()
axis(2)
axis(1, at=c(0,1,2,3), labels = c('LS', 'Ridge', 'Lasso', 'Elastic Net'))
legend('topright', dimnames(x)[[2]], col = rainbow(dim(x)[2]), pch=20, cex=1)





#### Subset Selection: Exhaustive ####

n <- dim(data)[[1]]
y <- data$...   #
data$y <- NULL  #


# Best Subset Selection (exhaustive search): 
# I look at all the possible combinations of regressors (2K possibilities, k=number of variables):
nvmax <- 8 # show the first nvmax combinations
regfit.full <- regsubsets(y~data, nvmax = nvmax, method = 'exhaustive') 
reg.summary <- summary(regfit.full)
reg.summary
# names(reg.summary) # all possible outcome of the summary

# plotting some parameters as a function of the number of variables
par(mfrow=c(1,3))
plot(reg.summary$rsq,xlab="Number of Variables",ylab="R-squared",type="b") 
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="b") 
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="b")
# choose the number of variables k such that elbows/ maximum of the R2 adj/...

k <- ... # which.max(reg.summary$adjr2) #
coef(regfit.full,k) # extract coefficient estimates

# # another graphical table of best subsets:
# par(mfrow = c(1,2)) 
# plot(regfit.full,scale="r2",main="Exhaustive search") 
# plot(regfit.full,scale="adjr2",main="Exhaustive search")




#### Subset Selection: Forward ####

n <- dim(data)[[1]]
y <- data$...   #
data$y <- NULL  #


# Best Subset Selection (Forward Stepwise search): 
# In this case, I start with only one variable (the one which have max pvalue).
# At each iteration, add the variable which max pvalue of the remaining. 
# Once the variable has been chosen, he cannot be removed.
nvmax <- 8 # show the first nvmax combinations
regfit.fwd <- regsubsets(y~data, nvmax = nvmax, method = 'forward') 
reg.summary <- summary(regfit.fwd)
reg.summary
# names(reg.summary) # all possible outcome of the summary

# plotting some parameters as a function of the number of variables
par(mfrow=c(1,3))
plot(reg.summary$rsq,xlab="Number of Variables",ylab="R-squared",type="b") 
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="b") 
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="b")
# choose the number of variables k such that elbows/ maximum of the R2 adj/...

k <- ... # which.max(reg.summary$adjr2) #
coef(regfit.fwd,k) # extract coefficient estimates

# # another graphical table of best subsets:
# par(mfrow = c(1,2)) 
# plot(regfit.fwd,scale="r2",main="Exhaustive search") 
# plot(regfit.fwd,scale="adjr2",main="Exhaustive search")



#### Subset Selection: Backward ####

n <- dim(data)[[1]]
y <- data$...   #
data$y <- NULL  #


# Best Subset Selection (Backward Stepwise search): 
# Using the method=“backward”, you start from the last row and then remove one-at-the-time 
# (I start with the model with all the covariates, and at each step I remove one of them):
# Once the variable has been removed, he cannot be added.
nvmax <- 8 # show the first nvmax combinations
regfit.bwd <- regsubsets(y~.,data, nvmax = nvmax, method = 'backward') 
reg.summary <- summary(regfit.bwd)
reg.summary
# names(reg.summary) # all possible outcome of the summary

# plotting some parameters as a function of the number of variables
par(mfrow=c(1,3))
plot(reg.summary$rsq,xlab="Number of Variables",ylab="R-squared",type="b") 
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="b") 
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="b")
# choose the number of variables k such that elbows/ maximum of the R2 adj/...

k <- ... # which.max(reg.summary$adjr2) #
coef(regfit.bwd,k) # extract coefficient estimates

# # another graphical table of best subsets:
# par(mfrow = c(1,2)) 
# plot(regfit.bwd,scale="r2",main="Exhaustive search") 
# plot(regfit.bwd,scale="adjr2",main="Exhaustive search")



#### k-fold cross validation ####

# nostra super funzione:
ATTENZIONE! togli #, era per evitare funzione
#my.subsetselection.cv <- function(Y, data, k.folds = 10, method = 'exhaustive', want.plot=T){
  # Performs 10-folds cross validation to find the optimal number
  # of variables to include in the subset selection model.
  #
  ## CALL:
  # k.cv <- subsetselection.cv(Y, data)
  #
  ## INPUT:
  # Y = vector of response variable.
  # data = dataframe of covariates.
  # k.fold = number of folds for Cross-Validation (default: k.fold=10).
  # method = method of subsect selection you want to use 
  #          possible methods: 'exhaustive', 'forward', 'backward', 'seqrep', 
  #          (default: method='exhaustive').
  # want.plot = logical input, set to T if you want a plot of cv-errors vs k,
  #           (default: want.plot=T).
  #
  ## OUTPUT:
  # k.cv= optimal number of variables that minimizes the root of mean cv-errors
  
  
  metodi = c('exhaustive', 'forward', 'backward', 'seqrep')
  if (sum(method == metodi)!=1){
    warning(paste(method), ' method not found, exaustive method has been used')
    method = 'exhaustive'
  }
  
  library(leaps) 
  
  n <- dim(data)[1]
  p <- dim(data)[2]
  data.all <- cbind(Y, data)
  
  set.seed(1)
  folds <- sample(1:k.folds,nrow(data),replace=TRUE) 
  
  # This is the loop that performs the estimation of the cross validation error:
  cv.errors <- matrix(NA,k.folds,p, dimnames=list(NULL, paste(1:p)))
  for(j in 1:k.folds){
    best.fit <- regsubsets(Y~.,data=data.all[folds!=j,],nvmax=p, method = method)
    for(i in 1:p){
      # funzione di merda:
      form <- as.formula('Y~.')
      mat <- model.matrix(form,data.all[folds==j,])
      coefi <- coef(best.fit,i)
      xvars <- names(coefi)
      pred <- mat[,xvars]%*%coefi
      
      cv.errors[j,i] <- mean( (Y[folds==j]-pred)^2 )
    }
  }
  
  root.mean.cv.errors <- sqrt(apply(cv.errors,2,mean)) # average over the columns
  
  k.cv <- which.min(root.mean.cv.errors) # minimum error
  
  if (want.plot==T){
    plot(root.mean.cv.errors,type='b', xlab = 'k')
    points(k.cv,root.mean.cv.errors[k.cv],
           col='red',pch=19)
  }
  
  return(as.numeric(k.cv))
}

k <- my.subsetselection.cv(Y, data, method = 'forward', want.plot=T)
k



#### Logistic Regression ####

## GLM regression:
# [ g(E[y|z])=beta_0 + beta_1z_1 + dots + beta_rz_r ]
# where $g()$ is the link function.
## Logit regression is a particular GLM regression in which $y sim Bernoulli(p)$, namely:
# [ g(p) = log(frac{p}{1-p}) ]
#
# other possible link functions are: 
# [ text{Poisson Regression:}  g(lambda = log(lambda)]
# [ text{Probit Regression: }  g(p) = Phi^-1(p),  where Phi: text{cumulative distr function of} N(0,1) ]

n     <- dim(data)[[1]]
y     <- data$...   #
reg1  <- data$...   #
reg2  <- data$...   #
r <- ... # number of regressors (beta0 included) #

y <- as.factor(y)

fit <- glm(y ~ reg1 + reg2, family = 'binomial') #
summary(fit)


## comparing residual deviance with that of the model without regressor (i.e. mean) 
# - only if 1 regressor
plot(reg1, as.numeric(y)-1)    # plotting original points
lines(seq(range(reg1), by=0.1), predict(fit, data.frame(reg1=seq(range(reg1),by=0.1)), type='response'))
# null model
Freq.tot <- table(y)[2]/ (table(y)[1] + table(y)[2]) 
abline(h = Freq.tot, col='blue', lty=2)
# abline(h = p, col='red', lty=2) # if you have a certain proportion of y1 wrt y0

## pointwise estimate for the proportion of y for specific regressors:
predict(fit, data.frame(reg1=...), type='response')

## pointwise estimate for the specific reg1 in which y=1 exceeded y=0:
p = 0.5
(log(p/1-p)-coefficients(fit)[1])/coefficients(fit)[2]

## pointwise estimate for the specific reg1 in which the proportion of y=1 wrt y=0 is p:
p = ... #*
(log(p/1-p)-coefficients(fit)[1])/coefficients(fit)[2]









#### Support Vector Machine ####

library(e1071)
data$GRUPPO[which(data$GRUPPO=="NOMEGRUPPO1")] <-0
data$GRUPPO[which(data$GRUPPO=="NOMEGRUPPO2")] <-1
data$GRUPPO <- as.factor(data$GRUPPO)


# kernel='radial' (Serve settare gamma!) se i dati non sono separabili con una retta (no linear)
# Aumentando il costo aumento i support points e diminuisco gli errori di misclassificazione, ottenendo però una regione più irregolare.
svmfit <- svm(GRUPPO~ ., data=data , kernel ='linear', cost =0.1) #, gamma = ..., , scale =FALSE) 
summary(svmfit)




plot(svmfit , data, col =c('salmon', 'light blue'), pch=19, asp=1)


# support vectors (are indicated with crosses)le croci nel plot)
svmfit$index


# Misclassification Error:
table<-table(true=data[ ,"GRUPPO"], pred=predict (svmfit ,
                                                  newdata =data[,]))
err<-table[1,2]+table[2,1]
#table<-table(true=train[ ,"GRUPPO"], pred=predict (svmfit , newdata =train[,])) #Sul Trining Set


# Costo tramite CV:
#par<- list(cost=c( ..., ..., ...), gamma=c(..., ..., ...) )  #Se non lineare
#par<- list(cost=c( ..., ..., ...))                           #Se lineare
train <- # training set
  tune.out <- tune(svm , y~., data=data[train ,], kernel ='...',  # in kernel 'linear' o 'radial'
                   ranges =par)
summary(tune.out)


#Best model CV:
svmfit<-tune.out$best.model




#Predizione:
new.datum <- data.frame(
  incidence = 60,
  tilt = 0)

ypred <- predict(bestmod,testdat)
#NON Guardare il primo 1, importa solo il valore sotto! (Vuol dire che il primo dato è classificato come valore sotto)
ypred
# si poteva notare anche dal grafico



#### Regression TREES #### (Lab_11)


attach(data)
n     <- dim(data)[1]

tree.data<- tree(NOMEVAR_Y~., data)
summary(tree.data)



# plot of the tree
plot(tree.data) 
text(tree.data,pretty=0)



## We can use cross validation to prune the tree optimally through the function cv.tree():
set.seed(...)
tree.data.cv <- cv.tree(tree.data)
# names(tree.data.cv)
# plot
plot(tree.data.cv$size,tree.data.cv$dev,type="b",xlab="size",ylab="misclass")
k <- ... # choose k that minimize misclass cost #*
# prune the tree
prune.data <- prune.tree(tree.data, best = k)
# plot the pruned tree
plot(prune.data) 
text(prune.data,pretty=0)





#### Classification TREES ####
attach(data)

#Se la dummy è da creare
y <- ifelse( VARY_Y <  ... ,"Classe1","Classe2")
y <- as.factor(y)
data$VAR_Y <- NULL
data <- data.frame(data, y)



tree.data<- tree(y~., data)
summary(tree.data)



# plot of the tree
plot(tree.data) 
text(tree.data,pretty=0)



## We can use cross validation to prune the tree optimally through the function cv.tree():
set.seed(...)
tree.data.cv <- cv.tree(tree.data,FUN=prune.misclass)
# names(tree.data.cv)
# plot
plot(tree.data.cv$size,tree.data.cv$dev,type="b",xlab="size",ylab="misclass")
#Guardare elbow



k <- ... # choose k that minimize misclass cost #*
# prune the tree
prune.data <- prune.misclass(tree.data, best = k)
# plot the pruned tree
plot(prune.data) 
text(prune.data,pretty=0)




#### FDR: False Discovery Rate 

allergy <- read.table('hatingalmonds.txt')
head(allergy)
dim(allergy)

noallergy <- read.table('lovingalmonds.txt')
head(noallergy)
dim(noallergy)

n1 <- dim(allergy)[1]
n2 <- dim(noallergy)[1]
p <- dim(noallergy)[2]

x.mean1 <- sapply(allergy, mean)
x.mean2 <- sapply(noallergy, mean)

p.hat <- (x.mean1*n1+x.mean2*n2)/(n1+n2)
x.var <- (p.hat*(1-p.hat))

# Test: H0.i: mu.i1 == mu.i2  vs H1.i: mu.i1 != mu.i2
# Asymptotic Z-test for the comparison of proportions (univariate and n=100 -> OK)

z.i <- (x.mean1-x.mean2)/sqrt(x.var*(1/n1+1/n2))
p.i <- ifelse(z.i<0, 2*pnorm(z.i), 2*(1-pnorm(z.i)))

which(p.i<.01)

# Bonferroni test
k <- 520

which(p.i*k<.01)

# or
p.Bf <- p.adjust(p.i, method='bonferroni')

which(p.Bf<.01)  

# Benjamini-Hockberg (control the false discovery rate)  
p.BH <- p.adjust(p.i, method='BH')

which(p.BH<.01)


quartz()
par(mfrow=c(1,3))
plot(p.i, main='Univariate')
abline(h=.01, lwd=2, col='red')

plot(p.Bf, main='Corrected - Bonferroni')
abline(h=.01, lwd=2, col='red')

plot(p.BH, main='Corrected - BH')
abline(h=.01, lwd=2, col='red')














##### LM standard, no relax ---------------
lmstd <- lm(visual ~ -1 + visual0 + time.f + treat.f:time.f, data = armd ) # METTERE PROPRIE VARIABILI
summary(lm1.form)

par(mar = c()) # per vedere matrice cov nei pazienti
plot(diag(x=***^2,nrow=30, ncol=30), main='Variance-covariance matrix of Y') ## x è standard deviation

plot(lm1.form$residuals) 
abline(h=0)
qqnorm(lm1.form$residuals)
qqline(lm1.form$residuals)
shapiro.test(lm1.form$residuals)


##### LM heteroscedastic and independent errors (different var only) ------
## gls() function allows the inclusion of dependency and heteroscedasticity

###### Option 1: VarIdent() --------------
# m+1 param, sigma_eps, e m delta (m = dimensione variabile su cui poniamo la varianza, cioe "regScelto")

lmmIDvar <- gls(y ~ -1 + reg1 + reg2 + reg1:reg2 # etc
                ,weights = varIdent(form = ~1|regScelto), 
                data = data)
summary(lmmIDvar)

plot(lmmIDvar$residuals) # difference from before is that now I know the exact variance in time t

lmmIDvar$modelStruct$varStruct
intervals(lmmIDvar, which = "var-cov")  ## 95% CI

# Visualization of Variance-covariance matrix of Y (first "xxx" observations)
par(mar = c()) # dim(regScelto), per quanti pazienti sono
plot(diag(x=c(1.000000^2*8.244094^2, 1.397600^2*8.244094^2, 1.664321^2*8.244094^2, 1.880852^2*8.244094^2), nrow=xxx, ncol=xxx) # guardare varstruct


###### Option 2: VarPower() ----------
# I have 2 param to estimate
# before I had sigma and 4 dealtas, now sigma and a delta (the increase is given from t as we can see from formula)

# uso update, oppure modello da 0 facendo gls
lmmPOWvar <- gls(y ~ -1 + reg1 + reg2 + reg1:reg2 # etc
                ,weights = varPower(form = ~regScelto), 
                data = data)
summary(lmmPOWvar)

lmmPOWvar <- update(lmmIDvar, weights = varPower(form = ~regScelto)) # Var. function; <delta, v_it>-group
summary(lmmPOWvar) 

lmmPOWvar$modelStruct$varStruct
intervals(lmmPOWvar, which = "var-cov")

# Visualization of Variance-covariance matrix of Y (first 30 observations) ## cambiare se serve numero osservazioni
par(mar = c())
plot(diag(x=c(4^(2*0.2519332)*5.974906^2, 12^(2*0.2519332)*5.974906^2, 24^(2*0.2519332)*5.974906^2, 52^(2*0.2519332)*5.974906^2), nrow=30, ncol=30),
    main='Variance-covariance matrix of Y - VarPower()')


# EXTRA - not useful, hopefully
## Residual analysis --we assess the fit of the model using residual plots. 

## raw residuals (of second model, the varPower)
plot(lmmPOWvar, resid(., type = "response") ~ fitted(.)) # Raw vs. fitted
# We observe an asymmetric pattern, with large positive (negative) residuals present mainly for small (large) fitted values.
# but it can be a consequence of the fact that raw residuals are intrinsically heteroscedastic and correlated.

plot(lmmPOWvar, resid(., type = "response") ~ time) # Raw vs. time (not shown)
bwplot(resid(lmmPOWvar) ~ time.f, pch = "|", data = dataset)

## Pearson residuals 
# now residual don't bend with the time, I made them unaffected by it
plot(lmmPOWvar, resid(., type = "pearson" ) ~ fitted(.)) # Pearson vs. fitted
plot(lmmPOWvar,resid(., type = "pearson") ~ time) 
bwplot( resid(lmmPOWvar, type = "pearson") ~ time.f, # Pearson vs. time.f
       pch = "|", data = armd)




##### LM heteroscedastic and dependent errors (correlazione negli errors) ---------
# nell'esempio la varianza dell'errore dipendeva dal tempo, all'esame potrebbe dipedere da qualcos'altro 
# per ora lascio tempo perchè è più capibile

## Variogram per time difference, it is as opposite of correlation, higher is variogram, lower is correlation!!!
# "subject" è su cosa si ripete il pattern uguale dell'errore mentre "time" è la variabile su cui l'errore non è più omoschedast. e indip.
Vg1 <- Variogram(lmmPOWvar, form = ~ time | subject) # subject e time potrebbero essere diversi
plot(Vg1, smooth = FALSE, xlab = "Time difference") # variogram per ogni possibile combinazione delle differenze nel tempo
# INTERPRETAZIONE: se vedo che la correlazione non resta invariata nel tempo allora è il caso di usare la correlazione non costante negli errors



###### Correlation 1: CorCompSym() (correlazione = rho in tutte le posizioni) -----
lm1.form <- formula(visual ~ -1 + visual0 + time.f + treat.f:time.f )
fm12.1 <- gls(lm1.form, weights = varPower(form = ~time), # prima riga dà i pesi diversi sulla diagonale, visto al capitolo prima
             correlation = corCompSymm(form = ~1|subject), # comando per la correlazione, cambiare subject in caso
             data = data)
summary(fm12.1)

intervals(fm12.1, which = "var-cov")
# The marginal variance-covariance structure, e correlation mat
fm12.1vcov <- getVarCov(fm12.1, individual = "...") ## Estimate of R_i = Delta'*C_i*Delta
print(cov2cor(fm12.1vcov), corr = TRUE, stdevs = FALSE)  ## Estimate of C_i (correlation matrix), dovrebbe avere 1 su diag e valore uguale su resto (rho)




###### Correlation 2: AR(1) (correlazione = rho^t, nel passare di t ) -----------
fm12.2 <- update(lm1.form, weights = varPower(form = ~time), 
                correlation = corAR1(form = ~tp|subject), ## correlazione AR, 1,rho,rho^2....
                data = data)
summary(fm12.2)

intervals(fm12.2, which = "var-cov")
fm12.2vcov <- getVarCov(fm12.2, individual = "+++++++++")  #Estimate of R_i, e.g. i=2
fm12.2cor <- cov2cor(fm12.2vcov)  #Estimate of C_i
print(fm12.2cor, digits = +++++++++++, corr = TRUE, stdevs = FALSE)




###### Correlation 3: general correlation structure (corr = all rhos are different) --------
# general is that all rhos are different maybe so rho12, rho13, rho21, etc
fm12.3 <- update(fm12.2, correlation = corSymm(form = ~tp|subject),  ## the variance function is still VarPower()
                data = data)
summary(fm12.3)

intervals(fm12.3, # 95% CIs for rho, delta, sigma
         which = "var-cov")

fm12.3vcov <- getVarCov(fm12.3, individual = "2")  ## Estimate of R_i (italic)
dimnames(fm12.3vcov) <- dnms
fm12.3vcov   # fm12.3vcov[1,1] = sigma_2 * Lambda[1]^2 = 5.737927^2*(4^0.2712624)^2

fm12.3cor <- cov2cor(fm12.3vcov)    ## Estimate of C_i
print(fm12.3cor, corr = TRUE, stdevs = FALSE)


### MODEL FIT DIAGNOSTIC STICAZZI, FINE DOCUMENTO FROM LM TO LMM












#### LMM (parte con anche RANDOM EFFECT (i.e. b piccole)) ####
## y_ij = z_ij*beta' + w_ij*b_i + eps_ij

###### Model 1.1 Random intercept, homoscedastic residuals ------
## simplest possible: y_ij = z_ij*beta' + b_i0 + epsij
# se pensi che le differenze tra i soggetti siano principalmente nell'intercetta 
# ES: c'è solo differenza nel il livello di base di una variabile
fm16.1mer <- lmer(y ~ reg0 + reg1 * treatment + (1|subject), # "(1|subject)" dice che mette effetti randomici sull'intercetta in base a subject
                 data = data)
summary(fm16.1mer)

confint(fm16.1mer,oldNames=TRUE) # è al 95 di default


## Var-Cov matrix of fixed-effects
vcovb <- vcov(fm16.1mer) 
vcovb
corb <- cov2cor(vcovb) 
nms <- abbreviate(names(fixef(fm16.1mer)), 5)
rownames(corb) <- nms
corb

## Var-Cov matrix of random-effects and errors
print(vc <- VarCorr(fm16.1mer), comp = c("Variance", "Std.Dev."))
sigma2_eps <- as.numeric(get_variance_residual(fm16.1mer))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(fm16.1mer))
sigma2_b

## Let's compute the conditional and marginal var-cov matrix of Y, sarebbero V=Lamb'*Di*Lamb e Ri=Lamb*C_i*Lamb
sgma <- summary(fm16.1mer)$sigma

A <- getME(fm16.1mer, "A") # A  --> N x n, A represents the D, bene o male, come se fosse Lamb*sqrt(Di)
I.n <- Diagonal(ncol(A)) # IN  --> n x n

## the CONDITIONAL variance-covariance matrix of Y (diagonal matrix), sarebbe varcov completa in caso chiedesse Y|b0
Sigmr = sgma^2 * (I.n)

## the MARGINAL variance-covariance matrix of Y (block-diagonal matrix)
V <- sgma^2 * (I.n + crossprod(A)) # V = s^2*(I_N+A*A) --> s^2*(I_N) is the error part, s^2*(A*A) is the random effect part



## PVRE
PVRE <- sigma2_b/(sigma2_b+sigma2_eps) # how big is the effect of the grouping on the data
PVRE 

# INTERPRETAZIONE: se alto è buono, il nostro modello coglie molta variabilità data dagli effetti casuali
#                  se basso no, variabilità modello data dagli error in gran parte, non ci possiamo fare niente
#                  low is a is a bad value, variability of the model is at most from the error                 


## DOTPLOT RANDOM EFFECT, visualization of the random intercepts with their 95% confidence intervals
# Random effects: b_0i for i=1,...,234
dotplot(ranef(fm16.1mer, condVar=T))
#È utile per identificare soggetti che si comportano in modo significativamente diverso rispetto alla media,
#data associated to the highest score -> il primo in alto
#data associated to the lowest score -> l'ultimo in basso


## PREDICTION 

# Prediction from mixed model on a test observation from a subject present in the training set:
test.data= data.frame(qualcosa che abbiamo nel dataset)

# 1) Without random effects ->  re.form=NA
predict_no_re <- predict(fm16.1mer, newdata = test.data, re.form=NA)
predict_no_re # (beta0 + beta1*test.data$reg1 + beta2*test.data$reg2 ...)
# 2) With random effects
predict_re <- predict(fm16.1mer, newdata = test.data)
predict_re # (beta0 + beta1*test.data$reg1 + beta2*test.data$reg2 ... + b0 )

re = ranef(fm16.1mer)[[1]]
re[row.names(re)==test.data$subject,] # it is our b_hat (coeff random intercept)


# Prediction from mixed model on a test observation from a subject not present in the training set:
test.data= data.frame(nuovo individuo)

# 1) Without random effects ->  re.form=NA
predict_no_re <- predict(fm16.1mer, newdata = test.data, re.form=NA)
predict_no_re # the same as before
# 2) With random effects
# it does not recognize the subject --> allow.new.levels = T
predict_re <- predict(fm16.1mer, newdata=test.data, allow.new.levels = T)
predict_re # the same as before, it uses the average of the random intercept, i.e. 0 -> NON CAMBIA NIENTE, b0=0



## DIAGNOSTIC plots 
# 1) Assessing Assumption on the within-group errors
x11()
plot(fm16.1mer)  ## Pearson and raw residuals are the same now

x11()
qqnorm(resid(fm16.1mer))
qqline(resid(fm16.1mer), col='red', lwd=2)

# 2) Assessing Assumption on the Random Effects
x11()
qqnorm(unlist(ranef(fm16.1mer)$subject), main='Normal Q-Q Plot - Random Effects on Intercept')
qqline(unlist(ranef(fm16.1mer)$subject), col='red', lwd=2)








######Model 1.2 Random intercept and slope CORRELATED and homoscedastic residuals ----------
# se pensi che le differenze tra i soggetti siano principalmente nell'intercetta e in un altra variabile, e queste due sono correlate tra loro
# ES: il livello di base della risposta e il tempo di cambiamento sono correlati (altezza iniziale bambino e tempo)

# due casi, le b_i correlate o meno, vediamo prima si e poi indipendenti
# scorrere giu al 2.2 per caso indipendenti

fm16.2mer <- lmer(y ~ reg1 + reg2 * treatment + (1+time|subject), ## b0 + b1*time , effetti casuali anche dipendenti dal tempo, per ogni soggetto
                 data = data)

summary(fm16.2mer)
# from voice 'Random effect' we can see their correlation
# there are them alone and correlated with time so in formula is like:
##   y_it = ... + b_0i + b_1i*time_it + eps_it
confint(fm16.2mer,oldNames=TRUE)


## Var-Cov mat of fixed
vcovb <- vcov(fm16.2mer) 
vcovb
corb <- cov2cor(vcovb) 
nms <- abbreviate(names(fixef(fm16.2mer)), 5)
rownames(corb) <- nms
corb


## Var-Cov matrix of random-effects and errors
# marginal again Z*D*Z' and cov matrix is sigmasq*R, 
print(vc <- VarCorr(fm16.2mer), comp = c("Variance", "Std.Dev."))
sigma2_eps <- as.numeric(get_variance_residual(fm16.1mer))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(fm16.1mer))
sigma2_b
sigma_b <- sqrt(sigma2_b)
sigma_b

# per capire i coefficienti (se ce ne sono negativi)
coefficients(fm16.2mer)

## Let's compute the conditional and marginal var-cov matrix of Y
sgma <- summary(fm16.2mer)$sigma

A <- getME(fm16.2mer, "A") # A : N*2 x n
I.n <- Diagonal(ncol(A)) # IN: n x n


## the CONDITIONAL variance-covariance matrix of Y (diagonal matrix)
Sigmr = sgma^2 * (I.n)
Sigmr[3:6, 3:6]  ## visualization of individual 2

## the MARGINAL variance-covariance matrix of Y (block-diagonal matrix)
V <- sgma^2 * (I.n + crossprod(A)) # V = s^2*(I_N+A*A)


## PVRE
# sigma2_b = Var(b0,b1) = sigma2_b0 + 2Cov(b0,b1)*mean(w) + sigma2_b1*mean(w^2)
sigma2_eps <- as.numeric(get_variance_residual(fm16.2mer))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(fm16.2mer)) 
sigma2_b

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE # 72% is very high!


## DOTPLOT: visualization of the random intercepts with their 95% confidence intervals
# Random effects: b_0i, b_1i for i=1,...,234
dotplot(ranef(fm16.2mer, condVar=T)) # xlim=c(-1,1)
dotplot(ranef(fm16.2mer, condVar=T),xlim=c(-1,1)) # zoom on graph, to see the second 


## DIAGNOSTIC plots 
# 1) Assessing Assumption on the within-group errors
x11()
plot(fm16.2mer)
x11()
qqnorm(resid(fm16.2mer))
qqline(resid(fm16.2mer), col='red', lwd=2)

# 2) Assessing Assumption on the Random Effects
x11()
qqnorm(unlist(ranef(fm16.2mer)$subject[,1]), main='Normal Q-Q Plot - Random Effects on Intercept')
qqline(unlist(ranef(fm16.2mer)$subject[,1]), col='red', lwd=2)
x11()
qqnorm(unlist(ranef(fm16.2mer)$subject[,2]), main='Normal Q-Q Plot - Random Effects on Slope')
qqline(unlist(ranef(fm16.2mer)$subject[,2]), col='red', lwd=2)

# INTERPR: if we observe that the correlation between d_11 and d_22 id very low, we fit a new model with a diagonal D matrix 





###### Model 1.3: random intercept and slope NOT CORRELATED, homoschedastic residuals  -------------------------
# se pensi che le differenze tra i soggetti siano principalmente nell'intercetta e in un altra variabile, e queste due non sono correlate tra loro
# ES: livello di base della risposta e il cambiamento della risposta nel tempo non sono correlati.


fm16.2dmer <- lmer(y ~ reg1 + reg2 * treatment + (1|subject) + (0 + time|subject), # divido le voci delle b_i, indipendenti
                  data = data) # control=lmerControl(optimizer="bobyqa",
                                #                   optCtrl=list(maxfun=2e5)))

summary(fm16.2dmer)
confint(fm16.2dmer,oldNames=TRUE)


## Var-Cov matrix of fixed-effects and errors
vcovb <- vcov(fm16.2dmer) 
vcovb
corb <- cov2cor(vcovb) 
nms <- abbreviate(names(fixef(fm16.2dmer)), 5)
rownames(corb) <- nms
corb

## Var-Cov matrix of random-effects and errors
print(vc <- VarCorr(fm16.2dmer), comp = c("Variance", "Std.Dev."))


## Let's compute the conditional and marginal var-cov matrix of Y
sgma <- summary(fm16.2dmer)$sigma

A <- getME(fm16.2dmer, "A") # A
I.n <- Diagonal(ncol(A)) # IN

## the CONDITIONAL variance-covariance matrix of Y (diagonal matrix)
Sigmr = sgma^2 * (I.n)

## the MARGINAL variance-covariance matrix of Y (block-diagonal matrix)
V <- sgma^2 * (I.n + crossprod(A)) # V = s^2*(I_N+A*A)



## PVRE
# sigma2_b = Var(b0,b1) = sigma2_b0 + 0 + sigma2_b1*mean(z^2)
sigma2_eps <- as.numeric(get_variance_residual(fm16.2dmer))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(fm16.2dmer)) + mean(armd$time^2)*as.numeric(get_variance_slope(fm16.2dmer)) # 54.07117 + 0.07935904*mean(armd$time^2) 
sigma2_b

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE # 72% is very high!


## DOTPLOT: visualization of the random intercepts with their 95% confidence intervals
# Random effects: b_0i, b_1i for i=1,...,234
dotplot(ranef(fm16.2mer, condVar=T))










###### Model 2.1 Random intercept, heteroscedastic residuals (varPower of time) -------

#Modello fm16.1: Quando si presume che la varianza residua sia costante 
#e che le differenze tra i soggetti siano solo sull'intercetta.
#Modello fm16.2: Quando si sospetta che la varianza residua cambi con il tempo (o un'altra variabile) 
#e che le differenze tra i soggetti sono ancora solo sull'intercetta.
#Modello fm16.3: Quando si sospetta che ci sia variazione tra i soggetti sia nell'intercetta che nella pendenza, 
#e che queste possano essere correlate.
#Modello fm16.4: Quando si sospetta che ci sia variazione tra i soggetti sia nell'intercetta che nella pendenza, 
#ma che queste non siano correlate.

## fixed-effects formula
lm2.form <- formula((y ~ reg1 + reg2 + treatment + treatment:reg2 )  #### ESEMPIO!!!
# LMM with homoscedastic residuals
fm16.1 <- lme(lm2.form, random = ~1|subject, data = data)
# update fm16.1 including heteroscedastic residuals
fm16.2 <- update(fm16.1,
                weights = varPower(form = ~ time), ## potrebbe anche esser "VarIdent()"
                data = data))

summary(fm16.2)

VarCorr(fm16.2)  


## MATRICI VARIANZE-COV

## var-cov matrix of the errors (i.e. of Y, conditional to the random effects), that are independent but heteroscedastic 
fm16.2ccov = getVarCov(fm16.2, type = "conditional",  individual = "2")
fm16.2ccov
plot(as.matrix(fm16.2ccov[[1]]), main = expression(paste('Conditional estimated Var-Cov matrix of ', Y[2])))

## var-cov matrix of Y_i, marginal
fm16.2cov = getVarCov(fm16.2, type = "marginal", individual = "2")
fm16.2cov 
plot(as.matrix(fm16.2cov[[1]]), main = expression(paste('Marginal estimated Var-Cov matrix of ', Y[2])))

## correlation matrix of Y_i
cov2cor(fm16.2cov[[1]])


## ANALYSIS OF RESIDUALS
# Default residual plot of conditional Pearson residuals
plot(fm16.2)
# Plots (and boxplots) of Pearson residuals per time and treatment, CAMBIARE I REGRESSORI!!!
plot(fm16.2, resid(., type = "pearson") ~ time | treat.f,
    id = 0.05)
bwplot(resid(fm16.2, type = "p") ~ time.f | treat.f, 
      panel = panel.bwplot, # User-defined panel (not shown)
      data = data)
# Normal Q-Q plots of Pearson residuals 
qqnorm(fm16.2, ~resid(.) | time.f) 


## ANALYSIS OF RANDOM EFFECTS -->>>>>> DA LAB, CAMBIARE COVARIATE
# Normal Q-Q plots of predicted random effects
qqnorm(fm16.2, ~ranef(.))  

## Computing predictions comparing population average predictions with patient-specific predictions
aug.Pred <- augPred(fm16.2,
                   primary = ~time, # Primary covariate
                   level = 0:1, # fixed/marginal (0) and subj.-spec.(1)
                   length.out = 2) # evaluated in two time instants (4 e 52 wks)
plot(aug.Pred, layout = c(4, 4, 1))









###### Model 2.2. random intercept and slope CORRELATED, heteroscedastic residuals (varPower of time) ---------------
#Modello fm16.3: Quando si sospetta che ci sia variazione tra i soggetti sia nell'intercetta che nella pendenza, 
#e che queste possano essere correlate.
fm16.3 <- update(fm16.2, ### scorrere su per copiarlo
                random = ~1 + time | subject,
                data = data)
summary(fm16.3)

getVarCov(fm16.3, individual = "2")  # D_i italic (i=2)
intervals(fm16.3, which = "var-cov")  # Estimate of theta_D, delta e sigma


###### Model 2.3. random intercept and slope NOT CORRELATED, heteroscedastic residuals (varPower of time) --------------
#Modello fm16.4: Quando si sospetta che ci sia variazione tra i soggetti sia nell'intercetta che nella pendenza, 
#ma che queste non siano correlate.
fm16.4 <- update(fm16.3,
                random = list(subject = pdDiag(~time)), # Diagonal D
                data = armd) 
summary(fm16.4) ## results suggest to remove the Treat and Time interaction

intervals(fm16.4)


















#### FDA (spiegazioni iniziali concetti) ####
# alcune cose non capite bene, provo a spiegare ciò che ho capito, in caso aggiorno le info dopo altri tde 

## WARNING:
# per FDA per curva in 3 dimensioni vedere lab "2024_FDA_smoothing.R", perchè forse non metterà mai, non voglio riempire l'indice inutilmente

## INDICE:
# 1. smoothing dataset con più curve (fourier basis, bSpline)
# 2. FPCA (per quando ho più curve)
# 3. smoothing per curve singole (bspline, locpoly)

# Fourier: dataset con funzioni circa sinusoidali o periodiche
# bspline: quasi sempre, per avere smoothing molto regolari, e buona anche a livello locale
# locpoly: buone stime locali, robuste a variazioni rapide, buona stima al contorno (diversamente da spline)



##### dataset con MOLTE curve --------
dataset <- .............
matplot(data_W,type='l',main='Argomento',xlab='X',ylab='Y') # plot curve assieme, forse serve il trasposto, fare attenzione !! -> t(data)

time <- 1:365 # dimensione basi esempio curva durante l'anno

# Scelta di nbasis, il giusto, valutare con la matrice varianza, ne vogliamo una significativa per catturare al meglio la varianza
# inolte dalla media osserviamo che non ci siano troppe oscillazione ma neanche sìche va a rimuovere le piccole oscillazioni significative

norder <- 4           # spline order (4th order polynomials)
degree <- 3    # spline degree  = norder-1
nbasis <- 15          # how many basis we want

basis <- create.bspline.basis(rangeval=c(1, 24),
                              nbasis=nbasis,
                              norder=norder)

data_W.fd <- Data2fd(y = data_W,argvals = time, basisobj = basis)  # se prima hai trasposto y= t(data)
plot.fd(data_W.fd)

data_W.fd$coefs[1:3,1] #esempio: primi 3 coefficienti ottenuti al girno 1

plot.fd(data_W.fd)
# mean
lines(mean.fd(data_W.fd),lwd=3)
# covariance
eval <- eval.fd(time,data_W.fd)
image.plot(time,time,(cov(t(eval))[1:365,]))




##### FPCA (x molte curve) ---------------------

# NEVER smooth too much your data if you later need to do a PCA!
# be cautious when smoothing, better not as pre-processing of other dimensional reduction, as PCA!!!
pca_W.1 <- pca.fd(data_W.fd,nharm=5,centerfns=TRUE)

# scree plot
par(mfrow=c(1,2))
plot(pca_W.1$values,xlab='j',ylab='Eigenvalues')
plot(cumsum(pca_W.1$values)/sum(pca_L$values),xlab='j',ylab='CPV',ylim=c(0.5,1))

# First three FPCs (first 3 eigenfunctions)
x11()
layout(cbind(1,2,3))
plot(pca_W.1$harmonics[1,],col=1,ylab='FPC1') # harmonics sarebbero i loadings nella pca normale
plot(pca_W.1$harmonics[2,],col=2,ylab='FPC2')
plot(pca_W.1$harmonics[3,],col=3,ylab='FPC3')


# PCA come perturbazione della media ---> UTILE per interpretazione della fPCA (se non capite chiedete a me in caso)
media <- mean.fd(data_L.fd)
plot(media,lwd=2,main='FPC1') # aggiustare per ylim = c(,) e inoltre cambiare indice "1" per altre PC
lines(media+pca_L$harmonic[1,]*sqrt(pca_L$values[1]), col=2)
lines(media-pca_L$harmonic[1,]*sqrt(pca_L$values[1]), col=3)

#c: interpret the retained principal components and discuss the results.
# Command of the library fda that automatically does these plots (con prime due, in caso con 3 fare harm=c(1,2,3) e par con c(1,3))
par(mfrow=c(1,2))
plot.pca.fd(pca_W.1, nx=100, pointplot=TRUE, harm=c(1,2), expand=0, cycle=FALSE)

# Scores
plot(pca_W.1$scores[,1],pca_W.1$scores[,2],xlab="Scores FPC1",ylab="Scores FPC2",lwd=2)   #plotta score 1,2
plot(pca_W.1$scores[,1],xlab="Scores FPC1",ylab="Scores FPC2",lwd=2)   #plotta score 1

points(pca_W.1$scores[...,1],pca_W.1$scores[...,2],col=2, lwd=4) # per evidenziare punti specifici, abbastanza inutile

layout(1)
matplot(eval,type='l')
lines(eval[,..],lwd=4, col=2) # per vedere la curva selezionata nello spazio degli scores







##### FDA Curva UNICA  -------

# Lavoriamo sia con rough data che con la curva vera (se la ho)

noisycurve <- read.table("..........",header=T)

Xobs0 <- noisycurve$X0  # forse nomidiversi, X0 sarebbero le Y delle osservazioni
abscissa <- noisycurve$Abscissa  # sarebbe la x delle osservazioni
NT <- length(abscissa) # number of observations

plot(noisycurve)
#plot(abscissa,Xobs0,xlab="t",ylab="observed data") ## uguale

truecurve <- read.table("truecurve.txt",header=T)  #### X0 valori y, X1 valori di y derivata, X2 valori di y derivata due volte
points(abscissa,truecurve$X0vera,type="l", col = "orange", lwd = 2)

# compute the central finite differences 
# OVVERO 
# approximation of the first derivative of the curve from the data
rappincX1 <- (Xobs0[3:NT]-Xobs0[1:(NT-2)])/(abscissa[3:NT]-abscissa[1:(NT-2)])
rappincX2 <- ((Xobs0[3:NT]-Xobs0[2:(NT-1)])/(abscissa[3:NT]-abscissa[2:(NT-1)])-(Xobs0[2:(NT-1)]-Xobs0[1:(NT-2)])/(abscissa[2:(NT-1)]-abscissa[1:(NT-2)]))*2/(abscissa[3:(NT)]-abscissa[1:(NT-2)])

# senza curva vera
plot(abscissa[2:(NT-1)],rappincX1,xlab="t",ylab="first differences x",type="l") # plot first derivative
points(abscissa,Xsp1bis,type='l',col="orange",lwd=3)
plot(abscissa[2:(NT-1)],rappincX2,xlab="t",ylab="second differences x",type="l",main = "2nd derivative")  # plot second derivative
points(truecurve$Abscissa,truecurve$X2vera,type='l',col="orange",lwd=3)

# con curva vera
par(mfrow=c(1,3))
plot(abscissa,Xobs0,xlab="t",ylab="observed data",main = "function")        
points(truecurve$Abscissa,#truecurve$X0vera,type='l',col="orange",lwd=3)
legend("topleft", legend = c("noisy data","true curve"), col = c("black", "orange"), lwd = c(1,2))
plot(abscissa[2:(NT-1)],rappincX1,xlab="t",ylab="first differences x",type="l",main = "1st derivative")   # plot first derivative
points(truecurve$Abscissa,truecurve$X1vera,type='l',col="orange",lwd=3)
plot(abscissa[2:(NT-1)],rappincX2,xlab="t",ylab="second differences x",type="l",main = "2nd derivative")  # plot second derivative
points(truecurve$Abscissa,truecurve$X2vera,type='l',col="orange",lwd=3)

##### plot basis e smoothed data --------------
m <- ....           # spline order -> how many parameter, one more then the degree, there is also the degree zero we can say, like intercept
degree <- m-1    # spline degree 
nbasis <- .....

basis <- create.bspline.basis(rangeval=c(0,...),  ##sostituisci a c(0,..) time 
                              nbasis=nbasis,     
                              norder=m)
#oppure
basis <- create.fourier.basis(rangeval=c(0,...),  ##sostituisci a c(0,..) con range(time) 
                              nbasis=nbasis)
plot(basis)  #plot of the basis

# plot of the smoothed data # Evaluate the basis on the grid of abscissa
## LEAST SQUARE (per truecurve quando non la ho)  
Xsp <- smooth.basis(argvals=abscissa, y=Xobs0, fdParobj=basis) #abscissa <- time
Xsp0bis <- eval.fd(abscissa, Xsp$fd) #  the curve smoothing the data

plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa,Xsp0bis ,type="l",col="red",lwd=2)

# inizio  uguale a sopra      sopra      sopra      sopra    sopra      sopra    sopra
# LEAST SQUARE (per truecurve quando non la ho) 
# Evaluate the basis on the grid of abscissa
basismat <- eval.basis(abscissa, basis)
est_coef = lsfit(basismat, Xobs0, intercept=FALSE)$coef # is only a least square fit
Xsp0 <- basismat %*% est_coef # my fit is basis matrix plus est
par(mfrow=c(1,1))
plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa,Xsp0 ,type="l",col="blue",lwd=2)
abline(v=basis$params)
# fine  uguale a    sopra      sopra      sopra      sopra    sopra      sopra    sopra


# to obtain the i-th derivative (argument Lfdobj= i )
basismat1<- eval.basis(abscissa, basis, Lfdobj=...) #inserisci valore derivata
head(basismat1)
Xsp1 <- basismat1 %*% est_coef

# alternative ready-to-use code for spline fitting
# use this one!!!
Xsp <- smooth.basis(argvals=abscissa, y=Xobs0, fdParobj=basis) # in input x, y (da punti curva da fittare), e basi
Xsp0bis <- eval.fd(abscissa, Xsp$fd) #  the curve smoothing the data
Xsp1bis <- eval.fd(abscissa, Xsp$fd, Lfd=1) # first derivative
Xsp2bis <- eval.fd(abscissa, Xsp$fd, Lfd=2) # second derivative
df <- Xsp$df   #  the degrees of freedom in the smoothing curve  
df             #  for regression splines the df are the number of basis



###### CROSS-VAL per le basi e lamba --------------

## PROBLEMA NUMERO BASI
# troppo poche, troppo smooth, rischio di togliere caratteristiche utili alla mia curva 
# troppo "tante", overfitta, prende troppo i miei punti e non ha una funzione regolare (smooth)
nbasis <- 6:30
abscissa <- 1:365 ## il tuo tempo
Xobs0 <- data$power  #sostituisci a power la variabile che ti serve
gcv <- numeric(length(nbasis))
for (i in 1:length(nbasis)){
  basis <- create.bspline.basis(range(abscissa), nbasis[i], ## m) mettere m solo se si è dato da capire, o in principio
                                # basis <- create.bspline.basis(c(0,1), nbasis[i])
                                # basis <- create.fourier.basis(c(0,1), nbasis[i])
                                gcv[i] <- smooth.basis(abscissa, Xobs0, basis)$gcv
}
par(mfrow=c(1,1))
plot(nbasis,gcv)
nbasis[which.min(gcv)]
abline(v = nbasis[which.min(gcv)], col = 2)


## per lambda quando uso comando -----> functionalPar <- fdPar(fdobj=basis, Lfdobj=4, lambda=)   
lambda <- 10^seq(-15,-5,by = 1)
gcv <- numeric(length(lambda))
for (i in 1:length(lambda)){
  functionalPar <- fdPar(fdobj=basis, Lfdobj=4, lambda=lambda[i])  
  gcv[i] <- smooth.basis(abscissa, Xobs0, functionalPar)$gcv
}
par(mfrow=c(1,1))
plot(log10(lambda),gcv)
lambda[which.min(gcv)]



###### Bias-Variance tradeoff ------

# Approximate pointwise confidence intervals 
# As in linear models, we can estimate the variance of x(t) as
# sigma^2*diag[phi*(phi'phi)^{-1}(phi)']
S <- basismat%*%solve(t(basismat)%*%basismat)%*%t(basismat) #projection operator 
sum(diag(S))
sigmahat <- sqrt(sum((Xsp0-Xobs0)^2)/(NT-df)) #estimate of sigma
lb <- Xsp0-qnorm(0.975)*sigmahat*sqrt(diag(S))
ub <- Xsp0+qnorm(0.975)*sigmahat*sqrt(diag(S))
plot(abscissa,Xsp0,type="l",col="blue",lwd=2,ylab="")
points(abscissa,lb,type="l",col="blue",lty="dashed")
points(abscissa,ub,type="l",col="blue",lty="dashed")

sigma <- sigmahat
nbasis <- 9:15
integrationinterval <- 11:90 # cambaire in base al range della vostra curva
bias <- rep(NA,len=length(nbasis))
var <- rep(NA,len=length(nbasis))
for (j in 1:length(nbasis)){
  basis <- create.bspline.basis(c(0,1), nbasis[j], m)
  basismat <- eval.basis(abscissa, basis)
  S <- basismat%*%solve(t(basismat)%*%basismat)%*%t(basismat)
  bias[j] <- sum((truecurve$X0vera-S%*%truecurve$X0vera)[integrationinterval])
  var[j] <- (sigma^2)*sum(diag(S[integrationinterval,integrationinterval]))
}
mse <- var+bias^2
par(mfrow=c(1,1))
plot(nbasis,bias^2,ylim=c(0,max(mse)),type="l",ylab="",main="Bias-Variance tradeoff")
points(nbasis,var,col="red",type="l")
points(nbasis,mse,col="green",type="l",lwd=3)
legend('topright', c("Bias","Var","MSE"), col=c("black","red","green"), 
       lty=1, cex=.5)

##### overfitting -> nbasis alto, oversmoothing -> nbasis basso --------------

##### Smoothing (bspline,locpoly) (da usare se da il lambda) -------

### BSPLINE smoothing
breaks <- abscissa # all abscissa points, just fewpoints, possiamo anche prendere la meta del mio range etc
basis <- create.bspline.basis(breaks, norder=m)
functionalPar <- fdPar(fdobj=basis, Lfdobj=4, lambda=)   
# Lfobj = 4 quasi sempre per i nostri scopi, per curve molto lisce che se la cavano con le forti oscillazioni

Xss <- smooth.basis(abscissa, Xobs0, functionalPar)
Xss0 <- eval.fd(abscissa, Xss$fd, Lfd=0) # derviatives 0,1,2
Xss1 <- eval.fd(abscissa, Xss$fd, Lfd=1)
Xss2 <- eval.fd(abscissa, Xss$fd, Lfd=2)

df <- Xss$df   #  the degrees of freedom in the smoothing curve
df
# if df is not an integer, trace of smoothing matrix, is subset of project operator, less then before
# PER CAPIRE: se aumento lambda vado a penalizzare di più la funziona e resto molto più regolare, i gradi di libertà di conseguenza DIMINUISCONO



### LOCPOLY smoothing

# PER CAPIRE: loc poly approssima con polinomi a livello locale
#             il locale è dato da moving window cioè un intervallo definito che si sposterà mano a mano sull'asse x fino a prendere tutti i punti
#             la moving window è di lunghezza BANDWITH, più è piccolo più approssimo con precisione quindi funzioni meno smooth, troppo largo anche non va bene!!

m <- 6           # order of the polynomial
degree <- m-1    # degree of the polynomial
bw <- 0.1 # bandwidth

Xsm0 <- locpoly(abscissa, Xobs0, degree=degree,          # Xobs0 sarebbero i punti della curva, i.e. il nostro dataset
                bandwidth=bw, gridsize=length(abscissa), 
                range.x=range(abscissa))
Xsm0 <- Xsm0$y

par(mfrow=c(1,1))
plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa,Xsm0 ,type="l",col="blue")

# per derivate aggiungere "drv"
Xsm1 <- locpoly(abscissa,Xobs0,drv=,degree=degree,bandwidth=bw,
                gridsize=length(abscissa), range.x=range(abscissa))
Xsm1 <- Xsm1$y





##### curves MONOTONE, smoothing (or positive but not really done) ---------

## usata se vediamo da "dataset con MOLTE curve" roba no sense tipo:
## se dataset parla di crescita delle persone e viene velocità negativa allora è impossibile, uso basi che saranno solo positive

variabile <- data$VARIABILE IN CONSIDERAZIONE 
nage <- length(variabile)
ageRng <- range(variabile)
nfine <-  BOH
agefine <- seq(ageRng[1], ageRng[2], length=nfine)

norder <- 6
nbasis <- nage - 2 + norder 
basis <- create.bspline.basis(rangeval = ageRng, nbasis = nbasis, 
                              norder = norder, breaks = variabile)
## Non ho ben capito ma serbe
Lfdobj <- 4          
lambda <- 10^(-0.5)  
cvecf <- matrix(0, nbasis, ncasef) # this is used as initial value for the numerical techniques
Wfd0 <- fd(coef = cvecf, basisobj = basis)
growfdPar <- fdPar(fdobj = Wfd0, Lfdobj = Lfdobj, lambda = lambda)

growthMon <- smooth.monotone(argvals = age, y = hgtf, WfdParobj = growfdPar)

fd <- growthMon$Wfd
beta <- growthMon$beta
yhatfd <- growthMon$yhatfd

velocfdUN <- deriv.fd(expr = yhatfd, Lfdobj = 1)
velocmeanfdUN <- mean.fd(velocfdUN)
accelfdUN <- deriv.fd(expr = yhatfd, Lfdobj = 2)
accelmeanfdUN <- mean.fd(accelfdUN)

# poi fare i plot














#### GEOSTAT: SPATIAL STAT ----------------
# runnare librerie e funzioni in LIBRARIES

## WARNING: conversione da lat e long ad UTM, preso palese da chat speriamo funzioni in caso faccia lo stronzo
install.packages("sf")
library(sf)
coords <- data.frame(longitude, latitude) # fai attach dei data per funzionare 
coords_sf <- st_as_sf(coords, coords = c("longitude", "latitude"), crs = 4326)  
utm_crs <- st_crs(32633)  # EPSG:32633 è UTM zona 33N WGS84
coords_utm <- st_transform(coords_sf, utm_crs)

##### Inizio lab -------------

# Define the sample coordinates
coordinates(data) <- c('x','y') # mettere SEMPRE questo, altrimenti si sminchia

# Bubble plot
bubble(data,'data$',do.log=TRUE,key.space='bottom')    # cambiare con variabile della spatial analisys

# histogram of Y variable
# we don't need gaussianity, but have it is always good
hist(data$, breaks=16, col="grey", main='Histogram', prob = TRUE, xlab = '*')

# IF NOT SO GAUSSIAN, better to transform to make everything more 'symmetric'
hist(log(data$), breaks=16, col="grey", main='Histogram of log(Zn)', prob = TRUE, xlab = 'log(Zn)')

xyplot(log() ~ sqrt(++++), as.data.frame(data)) #  sarebbe var da analizzare, e ++++ in base a cosa


##### Variogram ---------------
# SPIEGAZIONE: -- mostra come la differenza (o semivarianza) tra i valori di una variabile cambia con la distanza
#              -- dà informazioni sulla variabilità spaziale dei residui di un dataset
# ELEMENTI: 
# -- Range: valore della x quando converge variogram (a che distanza)
# -- Sill: valore della x quando converge variogram
# -- Nugget: da mettere quando viene specificato o si capisce, rappresenta una variabilità a distanze piccole, i.e. non regolarità del variogram vicino allo 0

# basic variogram 
xxx <- data$ # variabile su cui fare spatial
  svgm <- variogram(log(xxx) ~ 1, data) # 1 means stationary model
plot(svgm, main = 'Sample Variogram',alpha = c(0, 45, 90, 135), pch=19) # we see data which are stabilizing, voglio RANGE prima della fine del grafico (cut-off area)
# vedo in pratica le distanze secondo le diverse direzioni (alpha), vedo se c'è qualche direzione ideale e scelgo alpha

# COSTRUIAMO il variogram ---> guardo quello "basic" per poi costruire "a tentativi e ad occhio" il nostro
# # Recall: both spherical and exponential model have a linear behavior near the
#         origin but exponential model has a faster growth than the spherical one
# vgm(sill, model, range, nugget)

vgm(1, "Sph", 300, 0.5) # esempio ----> "sph" significa spherical, dato dal problema, gli altri li vedo io, al massimo dicono anche se nugget o no

# esempio finale 
v <- variogram(log(xxx) ~ 1, data)
v.fit <- fit.variogram(v, vgm(1, "Sph", 800, 1))
plot(v, v.fit, pch = 19)

# fitting method: non linear regression with minimization of weighted
# sum of squares error. final value of the minimum
attr(v.fit, 'SSErr')





##### ASSUMPTION del variogram -------
# assumption per lavorare con il variogram sono due:
# stazionario, plottando normalmente vedi se raggiunge l'asintoto prima del cut-off
# isotropo, con il seguente codice vedere se l'andamento del variogram lunga coordinata angolare è invariato 

v.dir <- variogram(log(xxx)~1,data,alpha=(0:3)*45) 
v.anis <- vgm(sill, "Sph", range, nugget, anis=c(45, 0.3)) ### da modificare i prima 4 elementi in base al problema 
print(plot(v.dir, v.anis, pch=19))





##### Kriging (i.e. prediction) ---------
# due tipi di kriging:
# ordinary -> Stationary Univariate Spatial Prediction
# universal -> Non-Stationary Univariate Spatial Prediction

###### ordinary (stationary) -----
## single new location 
s0.new=data.frame(x=++++++++++, y=++++++++++) # UTM coordinates, x,y are the name we gave to dataset before
coordinates(s0.new)=c('x','y')

g.tr <- gstat(formula = log(xxx) ~ 1, data = data, model = v.fit) # "v.fit" sarebbe il variogram scelto

predict(g.tr, s0.new) # stima e varianza della variabile spaziale d'interesse 
# Estimate the mean:use the argument 'BLUE'
predict(g.tr, s0.new, BLUE = TRUE) # caso stazionario: stima della mean sarà uguale in ogni punto!!!

# se abbiamo la grid del dataset (tipo la mappa), possiamo fare il plot di tutto
lz.ok <- predict(g.tr, data.grid, BLUE = FALSE)
spplot(lz.ok)


###### universal (stationary) -----
# mean costante è restrittivo, da prendere in considerazione che può variare nello spazio
# variogram dovrebbe venire in generale con sill più piccolo
# rivedo quindi variogram ma con dipendenza spaziale
data.gstat <- gstat(id = 'xxx', formula = log(xxx) ~ sqrt(dist), # giving sqrt of distance for mean (not anymore constant)
                    data = data, nmax = 50, model=v.fit, set = list(gls=1)) # gls = general least sqs
data.gstat
v.gls<-variogram(data.gstat)
plot(v.gls)
# different from before, first difference in in scale,  is lower

v.gls.fit <- fit.variogram(v.gls, vgm(1, "Sph", 800, 1)) # sill, range and nugget, try to play with them
plot(v.gls, v.gls.fit, pch = 19)
# Update gstat object with variogram model modified
data.gstat <- gstat(id = 'xxx', formula = log(xxx) ~ sqrt(dist),
                    data = data, nmax = 50, model=v.gls.fit, set = list(gls=1))

# riformulo anche il nuovo dato in dipendenza con la distanza
s0.vec <- as.vector(slot(s0.new,'coords'))
# calculate distance for everypoint of the "rhing" of our data which is responsable of the spatial analisys
# e.g. in lab the river increase the zinc, now we have general zinc as xxx, but general river will depend on new data
s0.dist <- min(rowSums(scale(*oggetto da cui vogliamo la distanza,s0.vec)^2))  
s0.new <- as.data.frame(c(s0.new,s0.dist))
names(s0.new) <- c('x','y','dist')
coordinates(s0.new) <- c('x','y')
s0.new <- as(s0.new, 'SpatialPointsDataFrame')

predict(data, s0.new)
predict(data, s0.new, BLUE = TRUE) # qui anche sarà diverso per ogni punto poichè la stima della media varia




### EXTRA, ricopiato pari pari da lab
# Is the drift important to explain the variability of the response variable?
# Let's compare the variogram of the data and of the residuals:
plot(v$dist,v$gamma,xlab='distance',ylab='semivariance',pch=19,col='skyblue1',ylim=c(0,0.8))
curve(v.f.est(x, C0=v.fit[2,2]+v.fit[1,2], cov.pars=rbind(c(v.fit[2,2], v.fit[2,3]),c(v.fit[1,2], v.fit[1,3])), cov.model = c("spherical","pure.nugget")), from = 0.0001, to = 1600,
      xlab = "distance", ylab = expression(gamma(h)),
      main = "Variogram model",add=TRUE,col='skyblue1',lwd=2, ylim=c(0,110))
points(v.gls$dist,v.gls$gamma,xlab='distance',ylab='semivariance',pch=19,col='steelblue',ylim=c(0,0.8))
curve(v.f.est(x, C0=v.gls.fit[2,2]+v.gls.fit[1,2], 
              cov.pars=rbind(c(v.gls.fit[2,2], v.gls.fit[2,3]),c(v.gls.fit[1,2], v.gls.fit[1,3])), cov.model = c("spherical","pure.nugget")), from = 0.0001, to = 1600,
      xlab = "distance", ylab = expression(gamma(h)),
      main = "Variogram model",add=TRUE,col='steelblue',lwd=2, ylim=c(0,110))
# upper variogram from initial
# lower variogram of residual -> less influential









