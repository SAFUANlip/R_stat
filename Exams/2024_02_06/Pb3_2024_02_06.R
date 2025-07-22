library(nlmeU) ## --> for the dataset
library(nlme)
library(insight)
library(lme4)
library(MASS)

# The dataset asthma.txt contains data regarding asthma prevalence across the 110 Italian provinces, identified by
# province id and grouped by their regional code region id.
# For each province, the dataset includes the following variables: average age, air pollution index, average
# number of sunny days per month, average tobacco consumption, average household income, average education
# level, and asthma prevalence rate, standardized per 10,000 residents. Additionally, the dataset includes a categorical
# variable urban indicating whether the province is categorized as urban or not.
# All numerical variables have been scaled to have a mean 0 and a standard deviation 1.

asthma <- read.table("2024_02_06/asthma.txt", h=TRUE)

# a) Implement the following linear regression model M0:
#   asthma= betta0,k + betta1 age + betta2 pollution + betta3 sunny + betta4 income + betta5 education + eps (1)
#  with ✏ ⇠ N (0, simga2) and k the grouping variable induced by urban.
#  Report the estimates of the parameters of the model fitted with Ordinary Least Squares and verify the model
#  assumptions.

lm_model <-lm(asthma ~ -1 + as.factor(urban) + age + pollution + sunny + income + education, data=asthma)
summary(lm_model)

par(mfrow=c(2,2)); plot(lm_model)

# plot to confirm that residuals are normal
qqnorm(lm_model$residuals) 
qqline(resid(lm_model), col='red', lwd=2)

plot(lm_model, which = 1) # plot to confirm that residuals are normal
plot(lm_model, which = 2) # plot showing homoscedasticity 

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

shapiro.test(lm_model$residuals) # so we can not reject H0 of residuals Normality
# W = 0.98402, p-value = 0.2129

# b) Can we affrm at 90% confidence level that the age has a positive effect on asthma prevalence? Additionally,
# provide an 95% confidence interval for the mean difference between the asthma prevalence in an urban province
# and in a non-urban one

confint(lm_model, level=0.9) # we can not affirm, that age has positive effect, because it's CI intersect 0
# age    -0.4681881  5.710121

lm_model_mean <-lm(asthma ~ as.factor(urban) + age + pollution + sunny + income + education, data=asthma)
summary(lm_model_mean)
confint(lm_model_mean, parm="as.factor(urban)Yes", level=0.95)
#                         2.5 %   97.5 %
# as.factor(urban)Yes -1.555065 13.28899
# looks like urban also do not have effect

# c) After having reduced the model M0, if appropriate, update it by introducing a compound-Symmetry Correlation
# Structure using the region as a grouping factor (model M1). Provide a 99% confidence interval for the
# parameters rho and sigma of the compound symmetry.

# step-by step removing coefficients:
lm_model_1 <-lm(asthma ~ -1 + as.factor(urban) + age + pollution + income + education, data=asthma)
summary(lm_model_1)

lm_model_2 <-lm(asthma ~ -1 + as.factor(urban) + pollution + income + education, data=asthma)
summary(lm_model_2)

gls1 <- gls(asthma ~ -1 + as.factor(urban) + pollution + income + education, 
            correlation = corCompSymm(form =~1|as.factor(region_id)),
            data=asthma)
summary(gls1)

# Correlation Structure: Compound symmetry
# Formula: ~1 | as.factor(region_id) 
# Parameter estimate(s):
#  Rho 
# 0.771199 

# Residual standard error: 20.39315 
# Degrees of freedom: 110 total; 106 residual
intervals(gls1, level = 0.99)
intervals(gls1, which = "var-cov", level = 0.99)

# Correlation structure:
#  lower     est.     upper
# Rho 0.5548358 0.771199 0.8993105 ~ [0.555, 0.899]
#
# Residual standard error:
#  lower     est.    upper 
# 14.78759 20.39315 28.12362  ~ [14.788, 28.124]

# d) From the possibly reduced version of the model M0, update it now by introducing a random intercept related
# to the regional grouping factor (model M2). What do you observe? Provide the estimate of the standard
# deviation of the random intercept along with the one of the error term.

lme2 <- lme(asthma ~ -1 + as.factor(urban) + pollution + income + education, 
            random = ~1|as.factor(region_id),
            data=asthma)
summary(lme2)
# AIC      BIC    logLik
# 867.5849 886.1627 -426.7925

# Random effects:
#   Formula: ~1 | as.factor(region_id)
# (Intercept) Residual
# StdDev:    18.00971 9.809624
sqrt(get_variance_random(lme2)) # 18.00971 
sqrt(get_variance_residual(lme2)) # 9.809624

anova(gls1, lme2) # lme not better 

#       Model df      AIC      BIC    logLik
# gls1     1  7 867.5849 886.1627 -426.7925
# lme2     2  7 867.5849 886.1627 -426.7925

