#### Problem 4 - 18/06/2021 ####
#' The file power.txt reports the measurements of the electric power consumption 
#' in one household collected every day for one year. Considering a functional data 
#' analysis approach, answer to the following questions.
#' a) Perform a smoothing of the data using a Fourier basis. Choose the number of 
#'    basis functions using a generalized cross-validation (GCV) criterion. 
#'    Report the plot of the values of the GCV statistic as a function of the number 
#'    of basis functions, the number of basis functions chosen, a plot of the basis 
#'    system used and a plot of the smoothed data.
#' b) Compute an approximation of the first derivative of the curve from the data 
#'    and the first derivative of the smoothed curve obtained at point (a). 
#'    Provide a plot to compare the two and comment on the result.
#' c) Choose a number of basis functions that you deem appropriate to show the effect 
#'    of oversmoothing. Report the number of basis functions chosen, provide a plot 
#'    of the the smoothed data and comment the result.
#' d) Choose a number of basis functions that you deem appropriate to show the effect 
#'    of overfitting. Report the number of basis functions chosen, provide a plot of 
#'    the the smoothed data and comment the result.


power <- read.table("power.txt", h=TRUE) 
Xobs0 <- power$power

length(Xobs0)
abscissa <- 1:365

nbasis <- 2:100
gcv <- numeric(length(nbasis))
for (i in 1:length(nbasis)){
  basis <- create.fourier.basis(c(0,365), nbasis[i])
  gcv[i] <- smooth.basis(abscissa, Xobs0, basis)$gcv
}
par(mfrow=c(1,1))
plot(nbasis,gcv)
nbasis[which.min(gcv)]
abline(v = nbasis[which.min(gcv)], col = 2)

nbasis[which.min(gcv)]

basis14 <- create.fourier.basis(c(0,365), nbasis[which.min(gcv)])
plot(basis14)

Xss <- smooth.basis(argvals=abscissa, y=Xobs0, fdParobj=basis14)
Xss0 <- eval.fd(abscissa, Xss$fd, Lfd=0)

par(mfrow=c(1,1))
plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa, Xss0 ,type="l",col="blue",lwd=2)

# b)
NT <- length(abscissa)
rappincX1 <- (Xobs0[3:NT]-Xobs0[1:(NT-2)])/(abscissa[3:NT]-abscissa[1:(NT-2)])
Xss1 <- eval.fd(abscissa, Xss$fd, Lfd=1)

plot(abscissa[2:(NT-1)],rappincX1,xlab="t",ylab="first differences x",type="l",main = "1st derivative")
points(abscissa, Xss1,type='l',col="orange",lwd=3)
# from graphic we may see, that our model soes not overfiit the data (no undersoomthing or oversmoothing)
# and appropriately approximate first derivative


# second derivative
rappincX2 <- ((Xobs0[3:NT]-Xobs0[2:(NT-1)])/(abscissa[3:NT]-abscissa[2:(NT-1)])-(Xobs0[2:(NT-1)]-Xobs0[1:(NT-2)])/(abscissa[2:(NT-1)]-abscissa[1:(NT-2)]))*2/(abscissa[3:(NT)]-abscissa[1:(NT-2)])
Xss2 <- eval.fd(abscissa, Xss$fd, Lfd=2)

plot(abscissa[2:(NT-1)], rappincX2,xlab="t",ylab="second differences x",type="l",main = "2nd derivative")
points(abscissa, Xss2, type='l',col="orange",lwd=3)


# c) Choose a number of basis functions that you deem appropriate to show the effect 
# of oversmoothing. Report the number of basis functions chosen, provide a plot 
# of the the smoothed data and comment the result.

basis3 <- create.fourier.basis(c(0,365), 3)
plot(basis3)

Xss_b3 <- smooth.basis(argvals=abscissa, y=Xobs0, fdParobj=basis3)
Xss0_b3 <- eval.fd(abscissa, Xss_b3$fd, Lfd=0)

par(mfrow=c(1,1))
plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa, Xss0_b3 ,type="l",col="blue",lwd=2)
# as we seen approximate curve alsmot ignore low peaj at 210 day, and oversmoooth data

# d)
basis200 <- create.fourier.basis(c(0,365), 200)
plot(basis200)

Xss_b200 <- smooth.basis(argvals=abscissa, y=Xobs0, fdParobj=basis200)
Xss0_b200 <- eval.fd(abscissa, Xss_b200$fd, Lfd=0)

par(mfrow=c(1,1))
plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa, Xss0_b200 ,type="l",col="blue",lwd=2)
# here we see overfitted data, having very low variance

