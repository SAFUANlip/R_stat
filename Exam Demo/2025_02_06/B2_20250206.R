library(nlmeU) ## --> for the dataset
library(nlme)
library(insight)
library(lme4)

# We are investigating how sugar intake a!ects individuals’ perceived energy levels. The file sugar energy.txt
# contains data from 2101 individuals following 200 different dietary patterns. Specifically, it reports:
#  • the average daily sugar intake in grams: sugar (this variable is centered)
#  • the dietary background of the individual, encoded as an integer variable: diet
#  • the self-reported average energy level on a scale from -10 to 10: energy

sugar_energy <-read.table("2025_02_06/sugar.txt", h=TRUE)

# a) Implement the following linear regression model M0:
# energy= ω0,i + ω1 sugar + ε
# where ε → N (0, ϑ2), and i in {1, . . . , 200} represents the dietary background.
# Report the estimates of ω0,2 and ω1.

length(unique(sugar_energy$diet))

lm_model <- lm(energy ~ -1 + as.factor(diet) + sugar, data = sugar_energy)
summary(lm_model)

lm_model$coefficients[2] # betta_0_2 = 1.071348
lm_model$coefficients[201] # betta_1 = 0.5116741

# b) Consider now the model M1:
# energyi = ω0 + bi1ni + ω1 sugari + ωi 
# with ωi → N (0, ϑ2Ini ), bi → N (0, ϑ2b ) and ni the number of individuals following dietary pattern i.
# Fit the model and report the estimate of ϑb. Without performing any model comparison, what advantage does
# M1 have over M0, in your opinion?

lme_model <- lme(energy ~ 1 + sugar, random = ~1|as.factor(diet), data = sugar_energy)
summary(lme_model)

sigma2 <- get_variance_random(lme_model)
sqrt(sigma2) # sigma_b = 0.9410168

VarCorr(lme_model)
# Advantage of M1 model that it take into account randomness in each group individually
# so take into account difference of groups


# c) A nutritionist suggests: ”Sugar intake generally boosts energy levels, but the magnitude of this effect varies
# depending on the individual’s dietary background.” Propose and fit an updated model M2 based on M1 to
# account for this observation. Is there a dietary pattern for which sugar intake appears to have a negative effect
# on energy levels?

lme_model22 <- lme(energy ~ 1 + sugar, random = ~1+sugar|as.factor(diet), data = sugar_energy)
summary(lme_model22)

n_obs <- nobs(lme_model22)          # 2101
n_fixef <- length(fixef(lme_model22))  # 2
df_resid <- n_obs - n_fixef         # приблизительная оценка
df_resid

summary(lme_model22)$tTable

random_effects <- ranef(lme_model22)

head(random_effects)


lme_model2 <- lme(energy ~ 1 + sugar:as.factor(diet), random = ~1|as.factor(diet), data = sugar_energy)
summary(lme_model2)
# yes, there are diets, where sugar may have negitive effect (based on value of ceofficients)

# d) Comment on whether M1 or M2 better explains the data, supporting your answer with an appropriate test.
anova(lme_model, lme_model2, lme_model22) # 22 - model with random intercept and slope has only 6 params 
# so the model 2 better, because p-value is lower than 0.05 and we can reject H0 of not importance diet on sugar

# Description of difference between standard model/intercept/slope -------------

# number of params: 
# betta0 - common intercept
# betta1 - coefficient for sugar
# sigma_b = sigma*sqrt(d11) - random intercept 
# sigma = for residuals
# 4 params
# energy_i = betta0 + betta1*sugar + b0_i + eps # (b0_i ~ N(0, sigma_b^2))
lme_model <- lme(energy ~ 1 + sugar, random = ~1|as.factor(diet), data = sugar_energy)
summary(lme_model)
head(ranef(lme_model)) # b0_i - 200 for each group

# number of params:
# betta0 - common intercept
# betta1-betta200 - coefficient for sugar with each group (200 params)
# sigma_b = sigma*sqrt(d11) - random intercept 
# sigma = for residuals
# 203 params
# energy_i = betta0 + betta1_i*sugar + b0_i + eps  # (b0_i ~ N(0, sigma_b^2))
lme_model2 <- lme(energy ~ 1 + sugar:as.factor(diet), random = ~1|as.factor(diet), data = sugar_energy)
summary(lme_model2)

# number of params:
# betta0 - common intercept
# betta1 - coefficient for sugar
# sigma_b1 = sigma*sqrt(d11) - random intercept 
# sigma_b2 = sigma*sqrt(d22) - random slope
# sigma_b12 = sigma*sqrt(d12) - correlation of sigma_b1 and sigma_b2
# sigma = for residuals
# 6 params
lme_model22 <- lme(energy ~ 1 + sugar, random = ~1+sugar|as.factor(diet), data = sugar_energy)
summary(lme_model22)

VarCorr(lme_model22) 
#              Variance   StdDev    Corr 
# (Intercept) 0.87548453 0.9356733 (Intr)  sigma_b1
# sugar       0.08755064 0.2958896 0.03    sigma_b2
# Residual    3.74370333 1.9348652         sigma

# Num of Prarmeters ------------------------------------------------------------
# how many parameters estimated - don't forget about sigma and d11
# Num of fixed params
n_fixed <- length(fixef(lme_model22))

# Num of params from random effect (look at correlation matrix - sigma_b1, sigma_b2, sigma_12):
# (here we have two random effect, so we also will have correlation between them)
re_structures <- VarCorr(lme_model22)
# Number of non-residual rows
n_re <- nrow(re_structures) - 1

# 
# If we have 2 random effect (intercept + slope), then matrix 2x2 symmetric → 3 params
n_variance_params <- n_re * (n_re + 1) / 2 # if n_re = 1 -> 1 param

# And residual sigma^2
n_sigma <- 1

# Common number of params
n_total <- n_fixed + n_variance_params + n_sigma

cat("Total number of estimated parameters:", n_total, "\n")

