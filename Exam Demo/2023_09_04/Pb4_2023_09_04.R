library(fda)

# The file izoard.txt contains altitude measurements taken at 200-meter intervals along the renowned 20-kilometer
# horizontal distance of the road ascent to the Col de l’Izoard in the French Alps. We are interested in recovering the
# continuous altitude profile as a smooth function of the horizontal distance, taking into account that the altitude
# measurements are subject to noise.

izoard <- read.table("2023_09_04/izoard.txt", h=TRUE)


plot(izoard$distance, izoard$altitude, xlab="ascent distance",ylab="altitude")

# a) Perform a penalized smoothing of the altitude measurements using a basis of cubic B-splines with breaks at
# each horizontal distance point, using a smoothing parameter lambda = 10^(-1). Report the number of splines used and
# the generalized cross validation (GCV) error.

m <- 4 # "using a basis of CUBIC B-splines", then we have to use norder = 4

nrow(izoard) # 101 points 
# nbasis = norder + number_of_knots
# number_of_knots = 101 - 2 (knot divide space)
# => 99 knots, norder = 99 + 4 = 103
nbasis <- 103

basis <- create.bspline.basis(rangeval=c(0,20), nbasis=nbasis, norder=m)
basis$nbasis

abscissa <- izoard$distance # seq(0,25,0.2)

# but, we can use "breaks" to automaticall indicate number of knots (nbasis)
breaks <- abscissa
basis_breaks <- create.bspline.basis(rangeval=c(0,25), breaks=breaks, norder=m)
basis_breaks$nbasis

functionalPar <- fdPar(fdobj=basis, lambda=0.1) # Lfdobj=m-2 penalise second derivative 

Xobs0 <- izoard$altitude
Xss <- smooth.basis(abscissa, Xobs0, functionalPar)

Xss0 <- eval.fd(abscissa, Xss$fd, Lfd=0) # zero derivative (fit to data)
Xss1 <- eval.fd(abscissa, Xss$fd, Lfd=1) # first derivative (fit to first derivative)
Xss2 <- eval.fd(abscissa, Xss$fd, Lfd=2)

gcv <- Xss$gcv  #  the value of the GCV statistic (Generalisd cross validation)
gcv # 888.7926

plot(abscissa,Xobs0, type = "l")
plot(basis)

# b) What is the approximate dimension of the space in which the fitted curve lives? Provide a plot of the fitted
# curve and and its first derivative.

df <- Xss$df   #  number of parameters (so dimension of space)
df  # 19.96405
cat("The fitted curve lives in a functional space of approximately", round(df,3), " dimensions.")

par(mfrow=c(1,2))
plot(abscissa, Xss0, type = "l", lwd = 2, col="red", ylab="fitted curve (RED)")
points(abscissa, Xobs0)
plot(abscissa, Xss1, type = "l", lwd = 2, col="blue", ylab="first derivative")

# c) Report the value of lambda minimizing the GCV error and the GCV error corresponding to that lambda, using a sequence
# of values in [10^-1, 10^3], with a step of 0.5, for the grid search. Fit again the smoothed curve with this lambda value.

# generalized cross-validation
lambda <- 10^seq(-1,3, by = 0.5)
gcv <- numeric(length(lambda)) # GCV - general cross validation 
for (i in 1:length(lambda)){
  functionalPar <- fdPar(fdobj=basis, Lfdobj=m-2, lambda=lambda[i])  
  gcv[i] <- smooth.basis(abscissa, Xobs0, functionalPar)$gcv
}
par(mfrow=c(1,1))
plot(log10(lambda),gcv)
lambda[which.min(gcv)] # 3.162278
abline(v = log10(lambda[which.min(gcv)]), col = 2)

opt_lambda <- lambda[which.min(gcv)]
cat("Optimal lambda:", opt_lambda, "\nGCV error:", min(gcv), "\n")

# Optimal lambda: 3.162278 
# GCV error: 842.0294 

# Refit
functionalPar_refit <- fdPar(fdobj=basis, Lfdobj=m-2, lambda=3.162278)
Xss_refit <- smooth.basis(abscissa, Xobs0, functionalPar_refit)

Xss0_refit <- eval.fd(abscissa, Xss_refit$fd, Lfd=0) # zero derivative (fit to data)
Xss1_refit <- eval.fd(abscissa, Xss_refit$fd, Lfd=1) # first derivative (fit to first derivative)
Xss2_refit <- eval.fd(abscissa, Xss_refit$fd, Lfd=2)

par(mfrow=c(1,2))
plot(abscissa, Xss0_refit, type = "l", lwd = 2, col="red", ylab="fitted curve (RED)")
points(abscissa, Xobs0)
plot(abscissa, Xss1_refit, type = "l", lwd = 2, col="blue", ylab="first derivative")

# d) Calculate the slope at the steepest point of the ascent.
max_slope <- max(Xss1_refit)
max_index <- which.max(Xss1_refit)
max_distance <- abscissa[max_index]
max_distance # 17

