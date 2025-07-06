library(nlmeU) ## --> for the dataset
library(nlme)
library(insight)
library(lme4)

# We are studying the effect of temperature on the growth rate of tomatoes. The file tomatoes.txt contains data
# regarding 2089 greenhouses growing 200 di↵erent species of tomatoes. Specifically it reports:
#  • the average temperature in the greenhouse during the growth season: temp (the variable is centred)
#  • the tomato species that is cultivated, encoded as an integer variable: species
#  • the average yield per crop cycle in kg/m2: yield
# a) Implement the following linear regression model M0:

# yield= betta0,i + betta1 temp + eps 

tomatoes <- read.table("2024_06_13/tomatoes.txt", h=TRUE)

lm_model <- lm(yield ~ -1 + as.factor(species) + temp, data=tomatoes)
summary(lm_model)

coefficients(lm_model)[2] # betta_0_2 = 16.06652 
coefficients(lm_model)[201] # betta_1 = 0.5106887


# b) Consider now the model M1:
# yield_i = betta_0 + b_i*1_ni + betta1*temp_i + eps_i 
# Fit the model and report the estimate of sigma_b. Without performing any model comparison, in your opinion, what
# is the advantage of M1 over M0?

lme_model <- lme(yield ~ 1 + temp, random = ~1|as.factor(species), data=tomatoes)
summary(lme_model)
# Random effects:
#   Formula: ~1 | as.factor(species)
# (Intercept) Residual
# StdDev:   0.9563481 2.113192 
# sigma_b = 0.9563481 ~ 0.956
sqrt(get_variance_random(lme_model)) # 0.9563481 - same value 

# M1 have less params than M0, so bigger degree of freedom, and will not overfit the data

# c) A farmer tells you: ”High temperatures generally favour tomatoes growth but this effect is more or less
# pronounced depending on the species that is considered”. Propose and fit an update M2 of M1 to account for
# the effect described by the farmer. Is there a species for which we estimate that the temperature has a negative
# effect?

lme_model2 <- lme(yield ~ 1 + temp, random = ~1+temp|as.factor(species), data=tomatoes)
summary(lme_model2)

ranef(lme_model2) # as we can see from coefficents of random effect, there are
# species, that have negative effect from temperature

anova(lme_model, lme_model2)
# model 2 better, due to p-value < 0.05 (and we reject H0 of equal zero rzndom effects)
# also AIC, BIC values lower for second model
# and Model 2 consider how temeprature affect on growth for each species group

#   Model       df      AIC      BIC    logLik   Test  L.Ratio        p-value
# lme_model      1  4 9293.869 9316.443 -4642.934                        
# lme_model2     2  6 9243.082 9276.943 -4615.541 1 vs 2 54.78688  <.0001
