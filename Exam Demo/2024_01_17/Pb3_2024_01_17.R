library(nlmeU) ## --> for the dataset
library(nlme)
library(insight)

storage <- read.table("2024_01_17/StorageCentres.txt", h=TRUE)

# The file StorageCentres.txt contains data regarding 40 storage centres (id storage centre 2 {1, ..., 40}) located
# in Regione Lombardia. For each storage centre, information about whether it is located within a radius of 15 km
# from a city (rad less 15 city 2 {0, 1}) and its size measured in m2 (size 2 R) are available. Moreover, the
# semestral costs [ke] for maintenance (costs 2 R) are provided at 5 di↵erent time points (time 2 {1, ..., 5}),
# starting the 1st semester of 2021 up to the 1st semester of 2023.

lm_model <- lm(costs ~ 1 + as.factor(time) + costs0 + growth:as.factor(time) + rad_less_15_city + size,
               data=storage)
summary(lm_model)

# Residual standard error: 5.444 on 187 degrees of freedom
# Multiple R-squared:  0.9991,	Adjusted R-squared:  0.9991 
# F-statistic: 1.801e+04 on 12 and 187 DF,  p-value: < 2.2e-16
# sigma = 5.444

AIC(lm_model) # 1259.965

# b) Provide the plot of the standardized residuals and comment on it. Do you believe that the hypothesis of
# homoscedastic residuals is satisfied?
#   In your opinion, what factors or aspects does M0 fail to consider? Support your reasoning with appropriate
# plots (e.g. boxplots).

par(mfrow=c(2,2)); plot(lm_model)

# plot to confirm that residuals are normal
qqnorm(lm_model$residuals) 
qqline(resid(lm_model), col='red', lwd=2)

plot(lm_model, which = 1) # plot to confirm that residuals are normal
plot(lm_model, which = 2) # plot showing homoscedasticity,


shapiro.test(lm_model$residuals) # So we reject H0 of residuals normality

colori = rainbow(4)
boxplot(lm_model$residuals ~ storage$time, col=colori,
        xlab='Time.f', ylab='Residuals')  ## -> the variance of th observations increases in time

# By common plots of lml_model there are no heteroscedascity
# but plot of residuals among time group, shows heteroscedascity of residuals 

# c) Implement a model M1 such that the independent error terms have heterogeneous variances; in particular,
# assume ✏it ⇠ N (0, sigma^2_it) with
# sigma_it = sigma · |timeit|^delta

gls_model1 <- gls(
  costs ~ 1 + as.factor(time) + costs0 + growth:as.factor(time) + rad_less_15_city + size,
  weights = varPower(form = ~time),
  data=storage
)

summary(gls_model1)

# Variance function:
#   Structure: Power of variance covariate
# Formula: ~time 
# Parameter estimates:
#   power 
# 0.8899608 - delta

# d) Implement a model M2 with the same within-group heteroscedasticity structure of M1, but with Heteroscedastic
# Autoregressive (AR1) Residual Errors. Provide a 95% confidence interval for RHO (but in summary it will be Phi) 
# in the matrix of the correlation structure.
# Comment on whether M1 or M2 is better, supporting your answer with a test.

gls_model2 <- gls(
  costs ~ 1 + as.factor(time) + costs0 + growth:as.factor(time) + rad_less_15_city + size,
  weights = varPower(form = ~time),
  correlation = corAR1(form = ~time|id_storage_centre), # коррелируем по time внтури групп, заданных id_storage_centre
  data=storage
)
summary(gls_model2)
intervals(gls_model2, which = "var-cov", level = 0.95)

# Approximate 95% confidence intervals
#
# Correlation structure:
#  lower       est.     upper
# Phi -0.2216134 -0.05239013 0.1198964 - it's rho from question
#
# Variance function:
#  lower      est.    upper
# power 0.7044107 0.8847498 1.065089
#
# Residual standard error:
#  lower     est.    upper 
# 1.559689 1.909049 2.336662

anova(gls_model1, gls_model2) # we can not reject H0 of non importance of AR1 effect
# also maybe seen from CI for Phi, so model 1 better

