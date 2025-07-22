# One-way ANOVA -------------------------------------------------------------------------------
# Example: Dataset Chicken Weights (p=1, g=6): (6 groups and 71 observations)
# Data from an experiment to measure and compare the effectiveness
# of various feed supplements on the growth rate of chickens. 
# (71 observations)

# ANOVA ------------------------------------------------------------------------

help(chickwts)

head(chickwts)
dim(chickwts)

summary(chickwts)

attach(chickwts)

chickwts

bartlett.test(weight, feed) # Bartlett test of homogeneity of variances

fit <- aov(weight ~ feed)
summary(fit)

# SStreat - variability between groups
# SSres - variability withing groups
### How to read the summary:
#              Df   Sum Sq      Mean Sq      F value     Pr(>F)    
# treat      (g-1) SStreat  SStreat/(g-1)  Fstatistic  p-value [H0: tau.i=0 for every i]
# Residuals  (n-g) SSres     SSres/(n-g)                    
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# g - number of groups
# n - number of observations

#             Df Sum Sq Mean Sq F value   Pr(>F)    
# feed         5 231129   46226   15.37 5.94e-10 ***
# Residuals   65 195556    3009                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Number of groups g = 5 + 1 = 6
length(unique(feed)) # 6

# Number of observations: n - g = 65 => n = 65 + 6 = 71 
length(feed) # 71

SStreat <- 231129
SSres   <- 195556

## Value of F-statistic --------------------------------------------------------
# F0 =  (1/(g-1) * SStreat) / (1/(n-g) * SSres) ~ F(g-1, n-g)
F0 <- (1/(6-1) * SStreat) / (1/(71-6) * SSres)
F0 # 15.36479 - F value from summary

## p-value ---------------------------------------------------------------------
# We reject H0 at level alpha if F0 > F_1-alpha_(g-1, n-g) (quantile level of 1-alpha)
alpha <- 0.05
qf(1-alpha, 6-1, 71-6) # 2.356028, our F0 bigger than this quantile

p_value <- 1 - pf(F0, 6-1, 71-6)
p_value # 5.93648e-10, lower than 0.05 => can reject

# MANOVA (I could not compute F-stat for it)------------------------------------

attach(iris)

species.name <- factor(Species, labels=c('setosa','versicolor','virginica'))
iris4        <- iris[,1:4] # variables

# perform MANOVA - to check if there are difference
# but to find which response to difference - we use ANOVA test

# Model: X.ij = mu + tau.i + eps.ij; eps.ij ~ N_p(0, Sigma), X.ij, mu, tau.i and eps.ij in R^4
# Test:
# H0: tau.1 = tau.2 = tau.3  = (0,0,0,0)'
# H1: (H0)^c
# that is
# H0: The membership to an iris species hasn't any significant effect on the mean
#     of X.ij (in any direction of R^4) 
# H1: There exists at least one direction in R^4 along which at least two species
#     have some feature significantly different

# This is a case of one-way MANOVA: four variables (Sepal.Length, Sepal.Width, 
# Petal.Length, Petal.Width) observed over g=3 levels (setosa, versicolor, virginica)

fit <- manova(as.matrix(iris4) ~ species.name) # "Pillai", "Wilks", "Hotelling-Lawley", "Roy" -
# - it's or matricies with differen coefficients [1, -1], [1, 0, -1] 
summary.manova(fit)

#                Df Pillai approx F num Df den Df    Pr(>F)    
# species.name   2 1.1919   53.466      8    290 < 2.2e-16 ***
# Residuals    147                                            
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Exact tests (for Wilks statistic) for p<=2 or g<=3 already implemented in R
# Note: If we use Wilks statistic, since g=3 the test is exact (cf. JW p. 300) 

# Reject the test, i.e., we have statistical evidence to state that
# the factor "Species" has an effect on the mean features 
# of the flowers.
# Who's the responsible for this?

# Via ANOVA: for each of the p=4 variables we perform an ANOVA test
#            to verify if the membership to a group has influence
#            on the mean of the variable (we explore separately the
#            4 axes directions in R^4)
summary.aov(fit)

# Each of the 4 variables is significantly influenced by the  
# factor species.
# Note: this analysis does NOT say: 
#       a) which group differ
#       b) with respect to which variables the groups in (a) differ
# => As for the ANOVA, we build confidence intervals (many more!)

fit <- manova(as.matrix(iris[,1:4]) ~ iris$Species)
summary_fit <- summary(fit, test = "Wilks")
summary_fit

# Access E and H matrices
manova_model <- manova(as.matrix(iris[,1:4]) ~ iris$Species)
H <- crossprod(fitted(manova_model))   # H = hypothesis SSCP
E <- crossprod(residuals(manova_model)) # E = error SSCP

# Wilks' Lambda:
wilks_lambda <- det(E) / det(E + H)
wilks_lambda

# Degrees of freedom:
g <- length(unique(iris$Species))  # 3
p <- ncol(iris[,1:4])              # 4
n <- nrow(iris)                    # 150
v <- n - g                         # residual degrees of freedom

# Approximated F-statistic and p-value can be computed using Bartlett’s approximation or other approximations




