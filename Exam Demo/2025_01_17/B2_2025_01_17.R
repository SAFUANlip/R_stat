library(nlmeU) ## --> for the dataset
library(nlme)
library(insight)
library(lme4)
library(MASS)

# The dataset crop yields.txt contains data regarding wheat crop yields in tons per hectare (tons/ha) across 100
# agricultural areas in Italy, each identified by area id and categorized by their respective province province id.
# For each area, the dataset includes the following variables: average temperature, cumulated mm of rainfall,
# average number of sunny days per month, average soil quality index, and average fertilizer usage. Additionally,
# the dataset includes a categorical variable irrigation indicating whether the region has irrigation systems in place
# (coded as 1 for regions with irrigation, 0 for regions without).
# All continuous variables have been standardized to have a mean of 0 and a standard deviation of 1.

crop_yields <- read.table("2025_01_17/crop_yield.txt", h=TRUE)

# a) Fit the following linear regression model M0:
#   crop yield= ω0,k + ω1 temperature + ω2 rainfall + ω3 sunny + ω4 soil quality + ω5 fertilizer + ε (1)
#   where ε → N (0, ϑ2) and k represents the grouping factor induced by irrigation.
#   Estimate the parameters of the model using Ordinary Least Squares (OLS) and assess the assumptions underlying
#   the model.

lm0 <- lm(formula = yield ~ -1 + as.factor(irrigation) + temperature + rainfall + sunny + soil_quality + fertilizer, 
          data=crop_yields)

summary(lm0)

par(mfrow=c(2,2)); plot(lm0)

# plot to confirm that residuals are normal
qqnorm(lm0$residuals) 
qqline(resid(lm0), col='red', lwd=2)

plot(lm0, which = 1) # plot to confirm that residuals are normal
plot(lm0, which = 2) # plot showing homoscedasticity 

# 100 observations
# coefficeints:
# temperature + rainfall + sunny + soil_quality + fertilizer (5)
# as.factor(irrigation) (2 - Yes or No) 
# + sigma 

# ASSUMPTIONS -------------------------------------------------------------------
# Assumptions required for estimation: 
# - Residuals have zero mean and are homoscedastic: 
# The residuals vs fitted diagnostic plot shows that the residuals are evenly distributed on both sides of the zero line and show no specific
# mean or variance pattern against the fitted values
# Assumptions required for inference: 
# - Residuals are normally distributed: The residuals Q-Q plot shows that the empirical quantiles
# are close to normal quantiles, except for the left tail which seems a bit heavy

# to check this assumtpion we can run shapiro test (to check normality), 
# and look on graphics of model 
shapiro.test(lm0$residuals) # Can not reject H0 of nresiduals normality

# b) Test whether we can affrm with 99% confidence that temperature has a negative effect on crop yields. Additionally,
# provide a 95% confidence interval for the mean difference in crop yields between areas with and without
# irrigation.

confint(lm0, level=0.99)
# temperature  -0.18994090 -0.01027311 
# temperature has negative coefficient and it's confidence interval does not contain 0, so we 
# can affirm, that temperature has negative effect on crop yields

# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#  as.factor(irrigation)No   3.92772    0.06423  61.150  < 2e-16 ***
#  as.factor(irrigation)Yes  4.37581    0.06281  69.666  < 2e-16 ***
#  temperature              -0.10011    0.03416  -2.930  0.00426 **  
#  rainfall                  0.24840    0.05355   4.639 1.14e-05 ***
#  sunny                     0.14573    0.03454   4.219 5.70e-05 ***
#  soil_quality              0.29861    0.03558   8.392 5.10e-13 ***
#  fertilizer                0.24552    0.03603   6.814 9.32e-10 ***
  
# p-value of temperature = 0.00426 (0.004) 

# 95% confidence interval for the mean difference in crop yields between areas with and without
# irrigation 

lm0_b0 <- lm(formula = yield ~ as.factor(irrigation) + temperature + rainfall + sunny + soil_quality + fertilizer, 
          data=crop_yields) # in that case we have common intercept and additional factor for irrigation Yes
# this factor plays role of delta between two groups, so we can use if to compare means in two groups 
summary(lm0_b0)
confint(lm0_b0, parm="as.factor(irrigation)Yes", level = 0.95)
# [0.3066912 0.5894834] ~ [0.307, 0.589]

# c) Update M0 by introducing a compound-symmetry correlation structure, with the province as a grouping factor
# (model M1). Report the 99% confidence interval for the parameters ϖ and ϑ associated with the compound
# symmetry.
gls1 <- gls(yield ~ -1 + as.factor(irrigation) + temperature + rainfall + sunny + soil_quality + fertilizer, 
            correlation = corCompSymm(form =~1|as.factor(province_id)),
            data=crop_yields)
summary(gls1)

# Correlation Structure: Compound symmetry
# Formula: ~1 | as.factor(province_id) 
# Parameter estimate(s):
#   Rho 
# 0.660084 (0.660)

# Residual standard error: 0.3373466 (0.337 - Sigma)
# Degrees of freedom: 100 total; 93 residual
intervals(gls1, level = 0.99)
intervals(gls1, which = "var-cov", level = 0.99)

# Rho: [0.389, 0.841]
# Sigma: [0.247, 0.460]

# d) Update M0 by incorporating a random intercept based on the province grouping factor (model M2). Compare
# the two models. What do you observe? Comment.

lmm2 <- lme(yield ~ as.factor(irrigation) + temperature + rainfall + sunny + soil_quality + fertilizer,
            random = ~1|as.factor(province_id),
            data=crop_yields)

summary(lmm2)
get_variance_random(lmm2) # 0.07511935, sqrt() = 0.2740791 (sigma_b)
get_variance_residual(lmm2) # 0.03868337, sqrt() = 0.1966809 (sigma)

anova(gls1, lmm2)
# Model df      AIC      BIC    logLik
# gls1     1  9 52.88858 75.68198 -17.44429
# lmm2     2  9 52.88858 75.68198 -17.44429
# Models have same AIC, BIC values, so they are have same preformance 

