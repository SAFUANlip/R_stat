library(fda)

load <- read.table("2024_06_13/load.txt", h=TRUE)
load 

data <- t(load[, 1:25]) # we have to plot in hours domain, so need to transopse data

# a) Smooth the data using a Fourier basis with 13 basis functions. Provide a plot of the smoothed observations.

matplot(data, type='l',main='Kwat consumption', xlab='Hours', ylab='365 days')

nbasis <- 13
time <- seq(0,24, by=1)
basis <- create.fourier.basis(rangeval = c(0,24), nbasis = nbasis)
plot(basis)
data_W.fd <- Data2fd(y=data, argvals=time, basisobj=basis)

plot.fd(data_W.fd)
data_W.fd$coefs[1:3, 1] # 116.344769 -12.747130  -2.863551
data_W.fd$coefs[, 1]
# Same procedure:
Xsp <- smooth.basis(argvals=time, y=data, fdParobj=basis) # 
Xsp0 = eval.fd(time, Xsp$fd)
plot(Xsp) # same as previously
coef(Xsp$fd)[1:3, 1]

# b) Perform a functional principal component analysis on the smoothed observations. Which proportion of the total
# variance is explained by the second principal component? Considering dimensionality reduction, which number
# of principal components would you retain? Justify your answer.

pca_W <- pca.fd(data_W.fd, nharm=5, centerfns=TRUE)

# scree plot
# we had 13 basis function -> we will have 13 components
# pca.fd computes all the 13 eigenvalues,
plot(pca_W$values[1:35],xlab='j',ylab='Eigenvalues')
plot(cumsum(pca_W$values)[1:35]/sum(pca_W$values), xlab='j', ylab='CPV', ylim=c(0.8,1))


# we have 35 data, (features), so we can not plot more principal components
pca_W$values
cumsum(pca_W$values)[1:35]/sum(pca_W$values) # we have 13 PC, because we used basis of 13 function
# I would take first 4 components, because they are explains more than 99% of variance 

pca_W$values[2]/sum(pca_W$values) # 0.03984729
# second PC exaplins 0.9668994 - 0.9270522 = 0.0398472 part of variance (took from cumsum)

# c) Provide an interpretation of the first two principal components. Support your answer with a plot.

layout(cbind(1,2))
plot(pca_W$harmonics[1,],col=1,ylab='FPC1',ylim=c(0.0,0.4))
abline(h=0,lty=2)
plot(pca_W$harmonics[2,],col=2,ylab='FPC2',ylim=c(-0.5,0.5))

# Показывает форму главных компонент.
# Но не учитывает, насколько сильно каждая компонента варьирует относительно среднего.
# Может ввести в заблуждение: кривые выглядят сильно различающимися по масштабу, хотя они еще не "взвешены".


# plot of the FPCs as perturbation of the mean
media <- mean.fd(data_W.fd)

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

# Это стандартный способ интерпретации FPC: показывает, как средняя функция меняется, если отклониться на 1 стандартное отклонение вдоль FPC.
# Очень полезно: можно сказать, например, что FPC1 отвечает за общее смещение вверх/вниз, а FPC2 — за пик в определенные часы.

# First component explains general values of energy consumption during day (lower at night, higher during the day)
# while second explains high deviation among day and more like opposite to first component 



# d) Is the representation given by the first two principal components satisfying for distinguishing the working days
# from the holidays? Support your answer with a plot.

# By LDA
lda.loads <- lda(load$daytype ~ pca_W$scores[,1:5]) # using first 5 components 
lda.scores <- predict(lda.loads)$x # 1D projection
# Plot the LDA 1D scores
boxplot(lda.scores ~ load$daytype,
        main="LDA 1D projection",
        xlab="Day type", ylab="LDA score")

# By FDA
scores <- pca_W$scores
plot(scores[, 1], scores[, 2], col = ifelse(load$daytype == "Working day", "blue", "red"),
     pch = 19, xlab = "PC1", ylab = "PC2", main = "Scores of First Two Principal Components")
legend("topright", legend = c("Working day", "Holiday"), col = c("blue", "red"), pch = 19)

# so first two components satisfy for istinguishing the working days
# from the holidays

# e) Could you find a 1-dimensional representation of the data allowing for a better discrimination than the one
# given by the first principal component?

data_pca <- data.frame(
  pc1=scores[, 1],
  pc2=scores[, 2]
)

g <- 2 

i1 <- which(load$daytype=='Holiday')
i2 <- which(load$daytype=='Working day')

n1 <- length(i1)
n2 <- length(i2)
n <- n1+n2


m <-  colMeans(data_pca)
m1 <- colMeans(data_pca[i1,])
m2 <- colMeans(data_pca[i2,])


S1 <- cov(data_pca[i1,])
S2 <- cov(data_pca[i2,])
Sp  <- ((n1-1)*S1+(n2-1)*S2)/(n-g)

# covariance between groups (estimate) # should be here 1/(g-1)?
B <- 1/(g-1)*(cbind(m1 - m) %*% rbind(m1 - m) +
                cbind(m2 - m) %*% rbind(m2 - m)
)
B

# covariance within groups (estimate)
Sp

# how many coordinates?
g <- 2
p <- 2
s <- min(g-1,p)
s

# system on which we project the data (in PCA it's orhogonal basis, but here no)
# Matrix Sp^(-1/2)
val.Sp <- eigen(Sp)$val
vec.Sp <- eigen(Sp)$vec
invSp.2 <- 1/sqrt(val.Sp[1])*vec.Sp[,1]%*%t(vec.Sp[,1]) + 1/sqrt(val.Sp[2])*vec.Sp[,2]%*%t(vec.Sp[,2])
invSp.2

# ration variability between groups compared to variability within groups
# spectral decomposition of Sp^(-1/2) B Sp^(-1/2)
spec.dec <- eigen(invSp.2 %*% B %*% invSp.2)

# first canonical coordinate
a1 <- invSp.2 %*% spec.dec$vec[,1] # not exact eigen vector, but multiplyed by invSp
a1

# second canonical coordinate
a2 <- invSp.2 %*% spec.dec$vec[,2]
a2

cc1.data <- as.matrix(data_pca)%*%a1
cc2.data <- as.matrix(data_pca)%*%a2

coord.cc <- cbind(cc1.data,cc2.data)

plot.fisher.directions <- function() {
  plot(data_pca[, 1], data_pca[, 2], col = ifelse(load$daytype == "Working day", "blue", "red"),
       pch = 19, xlab = "PC1", ylab = "PC2", main = "Scores of First Two Principal Components", ylim=c(-50,50), xlim=c(-70, 70))
  legend("topright", legend = c("Working day", "Holiday"), col = c("blue", "red"), pch = 19)
  
  
  abline(h=0,v=0, col='grey35')
  
  arrows(x0=0, y0=0, x1=a1[1], y1=a1[2], length=.1)
  arrows(x0=0, y0=0, x1=a2[1], y1=a2[2], length=.1)
  
  text(a1[1], a1[2], 'a1', pos=2)
  text(a2[1], a2[2], 'a2',pos=3)
  
  arrows(x0=0, y0=0, x1=a1[1], y1=a1[2], length=.1)
  arrows(x0=0, y0=0, x1=a2[1], y1=a2[2], length=.1)
  
  abline(coef=c(0,(a1[2]/a1[1])), col='grey55',lty=2)
  abline(coef=c(0,(a2[2]/a2[1])), col='grey55',lty=2)
  
  points(cc1.data*a1[1]/(sum(a1^2)),cc1.data*a1[2]/(sum(a1^2)),col=ifelse(load$daytype == "Working day", "blue", "red"))
  points(cc2.data*a2[1]/(sum(a2^2)),cc2.data*a2[2]/(sum(a2^2)),col=ifelse(load$daytype == "Working day", "blue", "red"))
  
  #plot(cc1.iris, cc2.iris, main='Coordinate system of the canonical coordinates', xlab='first canonical coordinate', ylab='second canonical coordinate', pch=20, col=as.character(color.species))
  #legend('topleft', legend=levels(species.name), fill=c('red','green','blue'), cex=.7)
  #
  #cc.m1 <- c(m1%*%a1, m1%*%a2)
  #cc.m2 <- c(m2%*%a1, m2%*%a2)
  #cc.m3 <- c(m3%*%a1, m3%*%a2)
  #
  #points(cc.m1[1], cc.m1[2], pch=4,col='red' , lwd=2, cex=1.5)
  #points(cc.m2[1], cc.m2[2], pch=4,col='green' , lwd=2, cex=1.5)
  #points(cc.m3[1], cc.m3[2], pch=4,col='blue' , lwd=2, cex=1.5)
}
# as we see among first componet we have higher variability between - so better 
# classification
# if we don't have lot of groups - we more try to separete our groups, 
# (course of dimensonality - as bigger dimension - as easier sepearate groups)
# purpose of 

# and our a1, a2 vectors not orthogonal 
plot.fisher.directions()
a1 # so by a1, we can find better direction to discriminate our data 

plot(seq(-5,5))
points(cc1.data[i1], rep(0, length(cc1.data[i1])), pch = 16, col = 'blue')
points(cc1.data[i2], rep(0, length(cc1.data[i2])), pch = 16, col = 'red')





