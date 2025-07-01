library(fda)


# The file stelvio.txt contains altitude measurements recorded at 200-meter intervals along the well-known 25-
# kilometer ascent of the Passo dello Stelvio in the Italian Alps. Our goal is to reconstruct the continuous altitude
# profile as a smooth function of horizontal distance, considering that these altitude measurements may be a↵ected
# by noise.

stelvio <- read.table("2024_09_06/stelvio.txt", h=TRUE) 
stelvio

plot(stelvio$distance, stelvio$altitude, xlab="ascent distance",ylab="altitude")


help(create.bspline.basis)
# from help
# norder:	
# an integer specifying the order of b-splines,
# which is one higher than their degree. The default of 4 gives CUBIC SPLINES.

# a) Apply penalized smoothing to the altitude data using a basis of cubic B-splines, with knots placed at each
# horizontal distance point, penalizing the second-order derivative and using a smoothing parameter of lambda = 1.
# Report the number of splines used and the generalized cross-validation (GCV) error.

m <- 4 # "using a basis of CUBIC B-splines", then we have to use norder = 4

nrow(stelvio) # 126 points 
# nbasis = norder + number_of_knots
# number_of_knots = 126 - 2 (knot divide space)
# => 124 knots, norder = 124 + 4 = 128
nbasis <- 128

basis <- create.bspline.basis(rangeval=c(0,25), nbasis=nbasis, norder=m)
basis$nbasis

abscissa <- stelvio$distance # seq(0,25,0.2)

# but, we can use "breaks" to automaticall indicate number of knots (nbasis)
breaks <- abscissa
basis_breaks <- create.bspline.basis(rangeval=c(0,25), breaks=breaks, norder=m)
basis_breaks$nbasis

functionalPar <- fdPar(fdobj=basis, Lfdobj=m-2, lambda=1) # Lfdobj=m-2 penalise second derivative 

Xobs0 <- stelvio$altitude
Xss <- smooth.basis(abscissa, Xobs0, functionalPar)

Xss0 <- eval.fd(abscissa, Xss$fd, Lfd=0) # zero derivative (fit to data)
Xss1 <- eval.fd(abscissa, Xss$fd, Lfd=1) # first derivative (fit to first derivative)
Xss2 <- eval.fd(abscissa, Xss$fd, Lfd=2)

gcv <- Xss$gcv  #  the value of the GCV statistic (Generalisd cross validation)
gcv # 1490.361

plot(abscissa,Xobs0, type = "l")
plot(basis)

# b) Estimate the approximate dimension of the space in which the fitted curve lives. Provide a plot of the fitted
# curve along with its first derivative.

df <- Xss$df   #  number of parameters (so dimension of space)
df  # 14.31518
cat("The fitted curve lives in a functional space of approximately", round(df,3), " dimensions.")

par(mfrow=c(1,2))
plot(abscissa, Xss0, type = "l", lwd = 2, col="red", ylab="fitted curve (RED)")
points(abscissa, Xobs0)
plot(abscissa, Xss1, type = "l", lwd = 2, col="blue", ylab="first derivative")

# c) Determine the value of lambda that minimizes the GCV error, and report the corresponding GCV error. Use a grid
# search with log10(lambda) values ranging from -1 to 3 in increments of 0.5. Refit the smoothed curve using this
# optimal lambda value.

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
max_distance # 11.2
