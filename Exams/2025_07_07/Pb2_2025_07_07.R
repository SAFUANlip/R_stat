library(nlmeU) ## --> for the dataset
library(nlme)
library(insight)

# A tech company is analyzing how much time users spend watching their newly released Functional Training workout
# series, which is available in 10 di!erent language versions on their streaming platform.
# Data has been collected from 1000 user accounts, with 100 users for each language. The goal is to predict the
# number of hours a user will spend watching the workout series, denoted by the variable Views, using the following
# predictors:

# • Premium account → {0, 1}: indicates whether the user has a premium (ad-free) subscription.
# • Laptop time: average number of hours per day the user spends on a laptop.
# • Phone time: average number of hours per day the user spends on a phone.
# • Social connections: number of friends the user has on the platform.
# • Fitness level → {0, 1}: user’s self-declared fitness level (0 = beginner, 1 = advanced).

# a) Fit a linear model, referred to as M0, assuming independent and identically distributed observations and no
# interaction terms. In this model, Views is predicted using the variables Premium account, a linear combination
# of device usage defined as (Laptop time + 1/2 Phone time), Social connections, and Fitness level.
# Report the estimated coe”cients for (Laptop time + 1/2 Phone time) and Social connections.
# Then, test whether there is su”cient evidence at the 1% significance level to claim that Social connections
# has a negative e!ect on Views

training <- read.table("FunctionalTraining.txt", h=TRUE)

training_lin <- data.frame(training,
                           lin = (training$Laptop_time + training$Phone_time/2)
)

lm_model <- lm(Views ~  Premium_account + lin + Social_connections + Fitness_level,
               data=training_lin)

summary(lm_model)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         61.5958     3.8464  16.014  < 2e-16 ***
#   Premium_account      1.7411     0.3693   4.714 2.77e-06 ***
#   lin                  2.9026     0.1754  16.546  < 2e-16 *** # 2.9026  ~  2.903 - asking coefficient
#   Social_connections  -0.3069     0.1878  -1.634    0.103     # -0.3069 ~ -0.307 - asking coefficient
# Fitness_level        4.8114     0.4607  10.443  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 5.724 on 995 degrees of freedom
# Multiple R-squared:  0.2966,	Adjusted R-squared:  0.2938 
# F-statistic: 104.9 on 4 and 995 DF,  p-value: < 2.2e-16

# p-value of test that Social_connections has negative effect

confint(lm_model, "Social_connections", level=0.99) # looks like it was usless

# b) Based on model M0, compute a 95% prediction interval [lower, upper] for
# Views for a beginner user (fitness level = 0) watching the English version of the series,
# who has a premium account, spends on average 5 hours daily on a laptop
# and 2 hours on a phone, and has 10 friends on the platform.

new <- data.frame(Fitness_level = 0, Language= "English", Premium_account=1,
                  Laptop_time=5, Phone_time=2, Social_connections=10,
                  lin=5+2/2)
# predict(lm_model, new, interval="confidence", level=1-0.05)  # доверительный интервал для E[Y | X]
predict(lm_model, new, interval="prediction", level=1-0.05) # prediction interval

#      fit      lwr      upr
# 1 77.68303 65.83125 89.53481 ~ [77.683, 89.535]

# c) Extend model M0 to a new model M1, accounting for the language in which the content is viewed. Specifically,
# introduce a random e!ect modelling a direct influence on the number of hours spent watching the series.
# Report the total number of parameters to be estimated in M1.

lme_model <- lme(Views ~  Premium_account + lin + Social_connections + Fitness_level,
                 random = ~1|as.factor(Language),
                 data=training_lin)

summary(lme_model)

# (Intercept)        61.90778 2.2472068 986 27.54877       0
# Premium_account     2.03250 0.1322207 986 15.37202       0
# lin                 2.97136 0.0627136 986 47.37982       0
# Social_connections -0.34609 0.0670218 986 -5.16377       0
# Fitness_level       4.93703 0.1642169 986 30.06406       0

# 5 constant params 

# Random effects:
# Formula: ~1 | as.factor(Language)
#         (Intercept) Residual
# StdDev:    5.627941 2.037135

# Two additional params:
# Random intercept - sigma_b
# Residual - eps

# Overall 7 params

ranef(lme_model) # this does not account as params 
# (I don't know why, even if they came from same distribution,
# formed by sigma_b, but they are still different for each group)

# d) Report the residual standard error from model M1. Also, identify the language group associated with the
# highest expected Views, after adjusting for all other covariates.

#         (Intercept) Residual
# StdDev:    5.627941 2.037135 ~ 2.037

# Highest expected Views
ranef(lme_model)

#             (Intercept)
# Arabic      -1.6485792
# Chinese     -1.1704567
# English      4.3189551
# French      -0.8943009
# German      -4.1812968
# Hindi        4.3754002
# Japanese     4.9906480
# Portuguese  -7.1843094
# Russian     -7.8770762
# Spanish      9.2710160 # Spanish have highest expected views

# wrong approach:
lme_model2 <- lme(Views ~  Premium_account + lin + Social_connections + Fitness_level,
                  random = ~1 + as.factor(Fitness_level)|as.factor(Language),
                  data=training_lin)


summary(lme_model2)

# M2 model
# Model that we had to submit (with varIdent heteroscedscity):
lme_model3 <- lme(Views ~  Premium_account + lin + Social_connections + Fitness_level,
                  random = ~1|as.factor(Language),
                  weights = varIdent(form =~1|as.factor(Fitness_level)),
                  data=training_lin)

summary(lme_model3)

anova(lme_model, lme_model2)
anova(lme_model, lme_model3)

#            Model  df  AIC      BIC    logLik     Test  L.Ratio   p-value
# lme_model      1  7 4344.617 4378.936 -2165.308                        
# lme_model3     2  8 3430.953 3470.175 -1707.476 1 vs 2 915.6639  <.0001
# So model with heteroscedscity residuals better than simple model with random efffect
