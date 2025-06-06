library(MASS)
library(car)
library(rgl)
library(glmnet) # to use LASSO

## Pb3 of 09/02/2022 exam -----------------------------------------------------------------------
# The file wine.txt reports the data on the alcohol content in 179 bottles of wine. 
# For the alcohol content consider a linear model, accounting for the sugar content 
# of grapes, and for type of wine ('Red', 'Rose', 'White'):
#   alcoholg = b0,g + b1,g*sugar + eps,
# with eps ??? N(0, sigma^2) and g the grouping structure induced by the type of wine.
# a) Estimate the parameters of the model ({b0,g, b1,g, sigma}). Verify the model assumptions, 
#    reporting any plot you consider important.
# b) Perform two statistical tests - each at level 1% - to verify if
#     - there is a significant dependence of the mean alcohol content on the type of wine;
#     - there is a significant dependence of the mean alcohol content on the sugar content.
# c) Based on tests (b) or any other test deemed relevant, reduce the model and report 
#    the updated model parameters.
# d) Build a prediction interval at 99% for a new bottle of red wine made with grapes 
#    with 20 g of sugar.

wine <- read.table("wine.txt", h=TRUE)

plot(wine)

# a)
lm_intercept <- lm(alcohol ~ type + type:sugar, data = wine)
summary(lm_intercept)

# how to cumpute sigma by hand
sqrt(sum(lm_intercept$residuals^2)/lm_intercept$df.residual) 


# ASSUMPTIONS -------------------------------------------------------------------
# Assumptions required for estimation: 
# - Residuals have zero mean and are homoscedastic: 
# The residuals vs fitted diagnostic plot shows that the residuals are evenly distributed on both sides of the zero line and show no specific
# mean or variance pattern against the fitted values
# Assumptions required for inference: 
# - Residuals are normally distributed: The residuals Q-Q plot shows that the empirical quantiles
# are close to normal quantiles, except for the left tail which seems a bit heavy

fm <- lm(alcohol ~ -1 + type + type:sugar, data = wine)
summary(fm)

fm$coefficients

shapiro.test(fm$residuals)

par(mfrow=c(2,2)); plot(fm)

# b) loook like ANOVA
alpha <- 0.01


# 2. the variable G; 
# so we need to check, are there significant of group and significance of sugar
# groups presneted in all variabels, sugar only in last 3 
AG <- rbind(c(1,0,0,0,0,0),
            c(0,1,0,0,0,0),
            c(0,0,1,0,0,0),
            c(0,0,0,1,0,0),
            c(0,0,0,0,1,0),
            c(0,0,0,0,0,1))
bG <- c(0,0,0,0,0,0)
linearHypothesis(fm, AG, bG, level = alpha) # so there are signiifcance of group

As <- rbind(
            c(0,0,0,1,0,0),
            c(0,0,0,0,1,0),
            c(0,0,0,0,0,1))
bs <- c(0,0,0)
linearHypothesis(fm, As, bs) # so there are signiifcance of sugar in model

fit.aov <- aov(wine$alcohol ~ wine$type)
summary(fit.aov) # so all three groups have different alchogol means

# c) find something, to reduce the model
Ac <- rbind(
  c(0,0,1,0,0,0), c(0,1,0,0,0,0), c(1,0,0,0,0,0))
bc <- c(0,0,0)
linearHypothesis(fm, Ac, bc) # 

summary(fm) # as we see from summary, there are no enoug statistic, to say, that
# white type of importance (on level 1% we can not reject H0 bett0.g (for any g) == 0)

fm2 <- lm(alcohol ~ type:sugar, data = wine)
summary(fm2)

# d)
new_data <- data.frame(type="Red", sugar=20)

predict(fm2, new_data, interval="prediction", level = 1-alpha)



