library(nlmeU) ## --> for the dataset
library(nlme)
library(insight)


co2 <- read.table("2024_09_06/co2.txt", h=TRUE)

# 
lme1 <- lme(co2 ~ 1 + purchases + heating + flights, random = ~1|as.factor(IDCity), data=co2)
summary(lme1)
#                         Value Std.Error   DF   t-value p-value
# (Intercept)           4.429619 1.1410674 2967   3.88200   1e-04
# purchases             1.990057 0.0311294 2967  63.92864   0e+00
# heating              -2.942302 0.0639885 2967 -45.98172   0e+00
# flights               10.029173 0.1355581 2967  73.98429   0e+00
# betta1 = 1.990057 ~ 1.990

# pvre:
var_b <- get_variance_random(lme1)
var_eps <- get_variance_residual(lme1)
var_b # 39.01042
var_eps # 2.917392
PVRE <- var_b/(var_b+var_eps) # we can take them from summary as ^2 of StdDev
PVRE # 0.9304187 pretty high pvre, there is some random effect 

# b) On top of M1, fit now a model M2, introducing heteroscedastic residuals eps_ij ~ N (0, sigma_ij^2 ) with
# sigma_ij= sigma · |purchases_ij |^delta
# for individual j 2 {1, . . . , ni}, for city i 2 IDCity.
# Estimate delta and the PVRE for M2.

lme2 <- lme(co2 ~ 1 + purchases + heating + flights,
            random = ~1|as.factor(IDCity),
            weights = varPower(form = ~purchases),
            data=co2)
summary(lme2)

# Variance function:
#  Structure: Power of variance covariate
# Formula: ~purchases 
# Parameter estimates:
#   power 
# 2.007418 = delta

var_b2 <- get_variance_random(lme2)
var_eps2 <- get_variance_residual(lme2)
var_b2 # 39.37599
var_eps2 # 1.011572
PVRE2 <- var_b2/(var_b2+var_eps2) # we can take them from summary as ^2 of StdDev
PVRE2 # 0.9749534 almost didn't change, so probably there are no reason to use heteroscedastic residuals
PVRE # 0.9304187

# Perform a likelihood ratio test to compare M1 and M2. Which model would you choose?
anova(lme1, lme2) # p-value < 0.05 so we can reject H0 of zero not importance of heteroscedastic

# d) On top of the selected model, net of the impact of fixed effect covariates, which are the IDCity associated with
# the highest CO2 emissions?

# NET OF - за вычетом 

ranef(summary(lme2)) 
# based on value of betta_0_i
# highest emission in city with id 28, because it has biggest valuse of coefficent 
