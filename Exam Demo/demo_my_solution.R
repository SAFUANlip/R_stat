library(MASS)
library(car)
library(rgl)
library(glmnet) # to use LASSO
library(nlme)

data <- read.table("asthma.txt")
# if our urban been numerical variabel, 0/1, then R will use it as numeric, not factor
# but we expected to use factor
lm <- lm(asthma ~ urban + age + pollution + sunny + tobacco + income + education, data = data)
summary(lm)

par(mfrow=c(2,2)); plot(lm)

data_new  <- data[-92, ] # as example how to remove outlier (if it will be outlier)

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
shapiro.test(lm$residuals)

# Can we affirm at 90% confidence level that the age has a positive effect on asthma prevalence?
# ANSWER: 
# The estimate of the coefficient associated to age is positive and the p-value of the significance test is below 0.1 so we
# can affirm that the age has a positive effect at the 90%
# confidence level

# Additionally,
# provide an 95% confidence interval for the mean difference between the asthma prevalence in an urban province
# and in a non-urban one

summary(lm)
confint(lm, parm="urbanYes", level = 0.95)
confint(lm, level = 0.95)

# After having reduced the model M0, if appropriate
# Reduce model step by step, removing by one unsignificant feature (with highest p-value)

lm2 <- lm(asthma ~ urban + age + pollution + sunny + tobacco + income, data = data)
summary(lm2) # removed education

lm3 <- lm(asthma ~ urban + pollution + sunny + tobacco + income, data = data)
summary(lm3) # removed age
# now everything significant 


# Update it by introducing a compound-Symmetry Correlation Structure using 
# the region as a grouping factor (model M1).
fitS <- gls(asthma ~ urban + pollution + sunny + income + tobacco,
            correlation = corCompSymm(form = ~1| region_id), data = data)

summary(fitS)

plot(fitS)

# Provide a 99% confidence interval for the
# parameters ρ and σ of the compound symmetry.

intervals(fitS, which = "var-cov", level = 0.99)

# From the possibly reduced version of the model M0, update it now by introducing a random intercept related
# to the regional grouping factor (model M2). What do you observe? Provide the estimate of the standard
# deviation of the random intercept along with the one of the error term.
library(lme4)

# Fit the mixed effects model
fitM2 <- lme(asthma ~ urban + pollution + sunny + income + tobacco,
            random = ~1|region_id,
            data = data)

summary(fitM2) # we don't see any signifficant difference between lsat two models 

# Random effects:
#  Formula: ~1 | region_id
#           (Intercept) Residual
# StdDev:    17.48762 3.426692
# 17.48762 - estimate of the standard deviation of the random intercept
# 3.426692 - standart deviation one of the error term


# My solution from source ------------------------------------------------------

data <- read.table("asthma.txt", h=TRUE)
data$province_id.f <- as.factor(data$province_id)
data$region_id.f <- as.factor(data$region_id)
data$urban <- as.factor(data$urban)
str(data)

lm <- lm(asthma~ -1 + urban + age + pollution + sunny + tobacco + income + education, data=data)
summary(lm)

# we check normality assumptions of residuals 
# means of residuals should be around zero
# also there should not be any patterns in residuals/fitted values 
qqnorm(resid(lm))
qqline(resid(lm), col='red', lwd=2)
shapiro.test(lm$residuals)

par(mfrow=c(2,2))
plot(lm)

# b)
# Can we affirm at 90% confidence level that the age has a positive effect on asthma prevalence? Additionally,
# provide an 95% confidence interval for the mean difference between the asthma prevalence in an urban province
# and in a non-urban one

confint(lm, level=0.9) # CI for Age does not contain 0, and coefficient of age positive,
# so we can affirm this hypothesis

confint(lm, parm="urbanYes", level = 0.95)

cfr.t <- qt(1-0.05/(2), 110-1)
data[data$urban == "Yes"]
delta_mean <- mean(data[data$urban == "Yes",]$asthma - data[data$urban == "No",]$asthma)

D.cov <- cov(as.matrix(data[data$urban == "Yes",]$asthma - data[data$urban == "No",]$asthma))
D.inv <- solve(D.cov)

c(delta_mean - cfr.t*sqrt(D.cov[1,1]/110),
  delta_mean,
  delta_mean + cfr.t*sqrt(D.cov[1,1]/110))

coef_diff <- coef(lm)["urbanYes"] - coef(lm)["urbanNo"]

# Получаем ковариационную матрицу коэффициентов
vcov_mat <- vcov(lm)

# Стандартная ошибка разности
se_diff <- sqrt(vcov_mat["urbanYes", "urbanYes"] +
                  vcov_mat["urbanNo", "urbanNo"] -
                  2 * vcov_mat["urbanYes", "urbanNo"])

# 95% доверительный интервал (двусторонний)
t_crit <- qt(0.975, df = df.residual(lm))
ci_low <- coef_diff - t_crit * se_diff
ci_high <- coef_diff + t_crit * se_diff

c("diff" = coef_diff, "lower" = ci_low, "upper" = ci_high)

# c)
# After having reduced the model M0, if appropriate, update it by introducing a compound-Symmetry Correlation
# Structure using the region as a grouping factor (model M1). Provide a 99% confidence interval for the
# parameters ρ and σ of the compound symmetry.

# reduce model 
lm2 <- lm(asthma~ -1 + urban + age + pollution + sunny + tobacco + income, data=data)
summary(lm2)
lm3 <- lm(asthma~ -1 + urban + pollution + sunny + tobacco + income, data=data)
summary(lm3) # reduced model

# compound-Symmetry Correlation
lm3_anova <- gls(asthma~ -1 + urban + pollution + sunny + tobacco + income,
            data=data)
glm3 <- gls(asthma~ -1 + urban + pollution + sunny + tobacco + income,
            correlation = corCompSymm(form = ~1|region_id.f), 
            data=data)

summary(glm3)
# Rho = 0.9630235
# sigma = 17.82019

intervals(glm3, which = "var-cov", level = 0.99)
# doesnt contain zero => there are effect
# Rho               0.9110261 0.9630235 0.9850941
# Residuals (sigma) 11.87700 17.82019 26.73732 

anova(lm3_anova, glm3) # new model better 

# d)
# From the possibly reduced version of the model M0,
# update it now by introducing a random intercept related
# to the regional grouping factor (model M2). What do you observe? 

llm3 <- lme(asthma~ -1 + urban + pollution + sunny + tobacco + income,
            random= ~1|region_id.f,
            data=data)

summary(llm3)
# StdDev:
# Intercpet: 17.48762
# Residuals: 3.426692

anova(lm3_anova, llm3) # the are efect of random intercept
anova(glm3, llm3) # but both models with random intercept and compound-Symmetry Correlation
# have same results, according to AIC, BIC 

