library(fda)

load <- read.table("2024_06_13/load.txt", h=TRUE)
load 

matplot(load,type='l',main='Canadian temperature',xlab='Day',ylab='gigawats')

time <- 0:24

load_data_t <- as.matrix(load[, 1:25])  # 365 x 25
load_data_t <- t(load_data_t)

load_data_t_holiday <- as.matrix(load[load$daytype == "Holiday",][, 1:25])
load_data_t_holiday <- t(load_data_t_holiday)

load_data_t_working <- as.matrix(load[load$daytype == "Working day",][, 1:25])
load_data_t_working <- t(load_data_t_working)

matplot(load_data_t, type='l',main='overall Kwats',xlab='Day',ylab='gigawats', ylim=c(10,60))
matplot(load_data_t_holiday,type='l',main='holiday Kwats',xlab='Day',ylab='gigawats', ylim=c(10,60))
matplot(load_data_t_working,type='l',main='working day Kwats',xlab='Day',ylab='gigawats', ylim=c(10,60))


basis.1 <- create.fourier.basis(rangeval=c(0,24), nbasis=13)
data_W.fd.1 <- Data2fd(y = load_data_t, argvals = time, basisobj = basis.1)
plot.fd(data_W.fd.1, ylim=c(10,60), ylab='Kwats')



pca_W.1 <- pca.fd(data_W.fd.1, nharm=13, centerfns=TRUE)

# scree plot
# pca.fd computes all the 365 eigenvalues, but only the first 
# N-1=34 are non-null
plot(pca_W.1$values[1:35],xlab='j',ylab='Eigenvalues')
plot(cumsum(pca_W.1$values)[1:35]/sum(pca_W.1$values),xlab='j',ylab='CPV',ylim=c(0.8,1))
cumsum(pca_W.1$values)[1:35]/sum(pca_W.1$values)

layout(cbind(1,2))
plot(pca_W.1$harmonics[1,],col=1,ylab='FPC1',ylim=c(-0.6, 0.6))
abline(h=0,lty=2)
plot(pca_W.1$harmonics[2,],col=2,ylab='FPC2',ylim=c(-0.6, 0.6 ))

# plot of the FPCs as perturbation of the mean
media <- mean.fd(data_W.fd.1)

plot(media,lwd=2,ylim=c(10,60),ylab='temperature',main='FPC1')
lines(media+pca_W.1$harmonics[1,]*sqrt(pca_W.1$values[1]), col=2)
lines(media-pca_W.1$harmonics[1,]*sqrt(pca_W.1$values[1]), col=3)

plot(media,lwd=2,ylim=c(10,60),ylab='temperature',main='FPC2')
lines(media+pca_W.1$harmonics[2,]*sqrt(pca_W.1$values[2]), col=2)
lines(media-pca_W.1$harmonics[2,]*sqrt(pca_W.1$values[2]), col=3)
