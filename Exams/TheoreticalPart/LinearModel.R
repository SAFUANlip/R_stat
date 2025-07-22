library(MASS)
library(car)
library(rgl)


# F-test -----------------------------------------------------------------------
storage <- read.table("2024_01_17/StorageCentres.txt", h=TRUE)

lm_model <- lm(costs ~ 1 + size + costs0, data=storage)
r <- 2
n <- nrow(storage)

y_mean <- mean(storage$costs)

RSS <- sum(lm_model$residuals^2)
ESS <- sum((lm_model$fitted.values - y_mean)^2)

F_stat <- (ESS/r)/(RSS/(n-r-1))
F_stat

summary(lm_model)$fstatistic

p_value <- 1 - pf(F_stat, r, n-r-1)
p_value

# we reject if F_stat > F(1-alpha, r, n-r-1) = qf(1-alpha, r, n-r-1)
qf(1-0.05, r, n-(2+1)) # 3.041753

vif(lm_model)



# Assumptions underlying model -------------------------------------------------
crop_yields <- read.table("2025_01_17/crop_yield.txt", h=TRUE)

lm0 <- lm(formula = yield ~ -1 + as.factor(irrigation) + temperature + rainfall + sunny + soil_quality + fertilizer, 
          data=crop_yields)

summary(lm0)

par(mfrow=c(2,2)); plot(lm0)

# plot to confirm that residuals are normal
# We want to see residuals with:
# 1. Zero mean values
# 2. No specific shapes, only sphere/ellips (we would like a random cloud with roughly zero mean)
# 3. Residual have to be normal, if we want make inference on model
qqnorm(lm0$residuals) 
qqline(resid(lm0), col='red', lwd=2)

plot(lm0, which = 1) # plot to confirm that residuals are normal
plot(lm0, which = 2) # plot showing homoscedasticity 
shapiro.test(lm0$residuals)

# Cook's distance and failed assumptions ---------------------------------------

# As example when it doesn't work
par(mfrow=c(2,2)); plot(lm_model) # not zero mean value, 
# and we see patterns in Residuals vs Fitted plot
# With Cook's distance we may see influential points 
shapiro.test(lm_model$residuals) # not normal


# General outputs --------------------------------------------------------------

plot(cars, xlab='Speed', ylab='Stopping distance', las=1)

n          <- dim(cars)[[1]]
distance   <- cars$dist
speed1     <- cars$speed
speed2     <- cars$speed^2

# Model:
# distance = beta_0 + beta_1 * speed + beta_2 * speed^2 + Eps
# (linear in the parameters!)

# Assumptions:
# 1) Parameter estimation: E(Eps) = 0  and  Var(Eps) = sigma^2 
# 2) Inference:            Eps ~ N(0, sigma^2)

fm <- lm(distance ~ speed1 + speed2)

summary(fm)

# Call:
#   lm(formula = distance ~ speed1 + speed2)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -28.720  -9.184  -3.188   4.628  45.152 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)  2.47014   14.81716   0.167    0.868
# speed1       0.91329    2.03422   0.449    0.656 - we also can see high varaince of first varaibel
# speed2       0.09996    0.06597   1.515    0.136
# 
# Residual standard error: 15.18 on 47 degrees of freedom
# Multiple R-squared:  0.6673,	Adjusted R-squared:  0.6532 
# F-statistic: 47.14 on 2 and 47 DF,  p-value: 5.852e-12 - overall model signifficant 
# But each coefficenit individually not significant, but their combination is 

fitted(fm)        # y hat (predicted)
residuals(fm)     # eps hat (residuals)

distance - fitted(fm)  - residuals(fm) # residuals = y_true - y_predict

coefficients(fm)  # beta_i (intercept - b0)
vcov(fm)          # cov(beta_i)

fm$rank # order of the model [r+1] (r - number of features, +1 - b0)
fm$df   # degrees of freedom of the residuals [n-(r+1)]


hatvalues(fm) # h_ii (or sometimes called "leverage effect")
# how far observation from other observations 
# They quantify:
# 1) How far is the i-th observation from the other ones in the features space
# 2) The influence of the i-th observation on the fit (can be seen as the
# derivative dyhat_i / dy_i)

# 0 <= hii <= 1 (if hii around 1, then var(eps_i) = 0 => eps_i = 0 (since E(eps_i) = 0))
# so when hii big, it's bad, because it doesn't show anything 


# In AIM notes - look on page 121 for standartised residuals and 
# page 114 - S^2 estimate of sigma^2

# way to work with slight distortion if residuals 
rstandard(fm) # standardized residuals: eps_j / sqrt(s^2*(1-h_ii))

sum(residuals(fm)^2)/fm$df  # S^2 estimate of sigma^2

residuals(fm)/sqrt(sum(residuals(fm)^2)/fm$df*(1-hatvalues(fm))) - rstandard(fm)
# correct with formula 

plot(cars, xlab='Speed', ylab='Stopping distance', las=1, xlim=c(0,30), ylim=c(-5,130))
x <- seq(0,30,by=0.1)
b <- coef(fm)
lines(x, b[1]+b[2]*x+b[3]*x^2)

# Inference on the parameters --------------------------------------------------

# Assumption: Eps ~ N(0, sigma^2) -  normality assumtion on residuals
# Test (Fisher): (F-test)
#   H0: (beta1, beta2) == (0, 0) vs H1: (beta1, beta2) != (0, 0)
linearHypothesis(fm, rbind(c(0,1,0), c(0,0,1)), c(0,0)) # C-matrix 
# beta0 * 0 + beta1 * 1 + beta2 * 0 = 0
# beta0 * 0 + beta1 * 0 + beta2 * 1 = 0
# это проверяерт чтобы хотя бы один был не равен 0, поэтому только одно p-value
summary(fm) # p-value < 0.05, so we can say that at least variable significant

p <- 2  # number of tested coefficients
r <- 2  # number of regressors

# Collinearity problem from plots ----------------------------------------------

# Confidence region:
# center: point estimate
c(coefficients(fm)[2], coefficients(fm)[3]) 
# Direction of the axes?

eigen(vcov(fm)[2:3,2:3])$vectors # vcov(fm) - cov(beta_i)
eigen(vcov(fm)[2:3,2:3])$values

plot(coefficients(fm)[2], coefficients(fm)[3], xlim = c(-6,6), ylim = c(-6,6), asp=1, xlab='beta1', ylab='beta2')
# 1. High varaince among one of the component (Flat ellipse):
ellipse(coefficients(fm)[2:3], vcov(fm)[2:3,2:3], sqrt(p*qf(1-0.05,p,n-(r+1)))) 
# 2. High varaince among one of the component (Big CI):
ellipse(coefficients(fm)[2:3], rbind(c(10, 0.1),                                
                                     c(0.1, 1)), sqrt(p*qf(1-0.05,p,n-(r+1))), col='red') # 
# 3. High correlation among features (high variance among one of the component):
ellipse(coefficients(fm)[2:3], rbind(c(0.6, 0.9),                                
                                     c(0.9, 0.5)), sqrt(p*qf(1-0.05,p,n-(r+1))), col='green') # 
# ! ALL these examples shows high collinearity of features
abline(v=0)
abline(h=0)

vcov(fm)[2:3,2:3] # High varaince among one first feature
eigen(vcov(fm)[2:3,2:3])
eigen(rbind(c(10.5, 0.9),
            c(0.9, 0.5))) # Т.е при высоком variacne вдоль одной переменной мы имеем коллинеарность

# Note: colinearity! (because ellipse flat) (our features dependent, especially in case when we look only on some train sample)
# ellipse doesn't contain  (0,0), so we reject H0, but if we will project on beta1 or beta2,
# then we get that it contain 0, so independatly this variabels don't have effect on target, but together
# they are

# Bonferroni intervals (level 95%)
Bf <- rbind(
  beta1=c(coefficients(fm)[2]-sqrt(vcov(fm)[2,2])*qt(1-0.05/(2*p), n-(r+1)),
          coefficients(fm)[2]+sqrt(vcov(fm)[2,2])*qt(1-0.05/(2*p), n-(r+1))),
  beta2=c(coefficients(fm)[3]-sqrt(vcov(fm)[3,3])*qt(1-0.05/(2*p), n-(r+1)),
          coefficients(fm)[3]+sqrt(vcov(fm)[3,3])*qt(1-0.05/(2*p), n-(r+1)))
)
Bf

# or (only for intervals on beta)
confint(fm, level= 1-0.05/p)[2:3,]  # Bonferroni correction!

# Note: confint() returns the confidence intervals one-at-a-time;
# to have a global level 95% we need to include a correction

### Test: (restricted model) - ограниченная модель 
# H0: (beta0+beta2, beta1) == (0,0) vs H1: (beta0+beta2, beta1) != (0,0)
C <- rbind(c(1,0,1), c(0,1,0))
linearHypothesis(fm, C, c(0,0))

# Collinearity problem from values ---------------------------------------------
# 1. We can see it from correlation among features:
cor(speed1, speed2) # even if speed2 = speed1^2 
# in our limited data, speed2 can presented as linear tranformation of speed1

# 2. From VIF coefficient (as it inflates the variance of the coeﬃcient, with respect,
# to the situation in which all the regressors zi are orthogonal!)
#  >5, or >10 =>Collinearity
vif(fm) # 24.61489 24.61489 

# 3. High varaince of one of the variabel
data_speed <- data.frame(speed1=speed1, speed2=speed2)
vcov(fm)[2:3,2:3]
sqrt(4.1380528) # 2.03422 - Std. Error from model summary

# 🔺 Большая дисперсия признака	  🔻 Уменьшает Var(β)
# 🔺 Высокая корреляция признаков	🔺 Увеличивает Var(β)

cor(data_speed)
summary(fm)

vcov(fm) # high variance of intercept doesn't tell anything about the collinearity
#              (Intercept)    speed1     speed2
# (Intercept) 219.5483705 -28.9523122  0.872858710
# speed1      -28.9523122   4.1380528 -0.131439753
# speed2        0.8728587  -0.1314398  0.004351805
