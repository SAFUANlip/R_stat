
#______________ Applied Statistics 2024/2025 _________________
####         Mixed-effects Models Module - Lab 2          ####


#### ARMD example ####
#_____________________

library(nlmeU)
library(corrplot)
library(nlme)
library(lattice)
library(plot.matrix)
library(lme4)
library(insight)

rm(list=ls())
graphics.off()

# d11 - what was estimated previously from R (for deviation of intercept)

# Topics: ####
#  LINEAR MIXED MODELS WITH HOMOSCEDASTIC RESIDUALS
#   1. Linear Models with random intercept 
#   2. Linear Models with random intercept + slope 
#      2.A general structure of D
#      2.B diagonal D 
#--> Interpretation of random effects and PVRE
#--> Diagnostic
#--> Models comparison 
#--> Tests for the random components
#
#  LINEAR MIXED MODELS WITH HETEROSCEDASTIC RESIDUALS (VarPower())
#   1. Linear Models with random intercept (q=0) 
#   2. Linear Models with random intercept + slope (q=1)
#      2.1 general structure of D
#      2.2 diagonal D  



data(armd, package = "nlmeU") # Age-Related Macular Degeneration
rlims <- c(-4.5, 4.5)
xlims <- c(0, 90)
xlimc <- c("4", "12", "24", "52wks")


## Linear Mixed-Effects models (LMM)

## Two main R packages: 
## 1. 'lme4' with lmer() --> it does not handle heteroscedastic residuals
## 2. 'nlme' with lme()  --> it handles heteroscedastic residuals

## --> We will mainly use lme() function in nlme package for LMM models;
## --> We will shift to lmer() function in lme4 package for LMM models with homoscedastic residuals (this script)
##     only for some visualizations



#### LINEAR MIXED MODELS WITH HOMOSCEDASTIC RESIDUALS ####

#___________________________________________________________________
#### Model 1. Random intercept, homoscedastic residuals (M16.1) ####

# We now treat time as a numeric variable (as assessed in the last lab)
lm2.form <- formula(visual ~ visual0 + time + treat.f + treat.f:time)


fm16.1 <- lme(lm2.form, random = ~1|subject, data = armd) 
# By default, lme() assumes independent residual errors with a constant variance, sigma^2.
# The default REML estimation is used --> to change it to the ML estimation, put method="ML" 

summary(fm16.1)
# sigma*sqrt(d11) = 8.98 (standard deviation of the random intercepts)
# sigma = 8.63           (residual standard deviation)
VarCorr(fm16.1)
# d11 = (8.98/sigma)^2


# print out the estimated fixed-effects table
printCoefmat(summary(fm16.1)$tTable, has.Pvalue = TRUE, P.values = TRUE) 

# to get the confidence intervals of all the estimated parameters
intervals(fm16.1)

# Number of parameters ---------------------------------------------------------
# how many parameters estimated - don't forget about sigma and d11
# Num of fixed params
n_fixed <- length(fixef(fm16.1))

# Num of variance params
# Каждая уникальная случайная группа и ее ковариационная матрица
re_structures <- VarCorr(fm16.1)
re_structures
n_random <- attr(re_structures, "sc")  # residual std dev
n_variance_params <- nrow(re_structures) - 1  # минус строка residual

# Всегда 1 параметр для остаточной дисперсии (sigma)
n_sigma <- 1 # (we work with homoscedastic, so there i only one sigma)

# Общее число параметров
n_total <- n_fixed + n_variance_params + n_sigma

cat("Total number of estimated parameters:", n_total, "\n")

# Var-Cov matrix of
# - fixed effects
# - random effects
# - of Y (conditional and marginal)

# Var-Cov matrix of fixed-effects
vcovb <- vcov(fm16.1) 
vcovb
# and Correlation of fixed effects
corb <- cov2cor(vcovb) 
nms <- abbreviate(names(fixef(fm16.1)), 5)
rownames(corb) <- nms
corb


# Var-Cov matrix of random-effects and residuals (what we observe in the summary)
print(vc <- VarCorr(fm16.1), comp = c("Variance", "Std.Dev."))
VarCorr(fm16.1)
var_eps = as.numeric(vc[2,1])
var_eps

sd_eps <- summary(fm16.1)$sigma
sd_eps

var_b = as.numeric(vc[1,1])
var_b


# Let's compute the conditional and marginal var-cov matrix of Y
#---------------------------------------------------------------#
# --> Conditional variance-covariance matrix of Y given b (diagonal matrix) 
#     sigma^2 * Ri for the second subject (type='conditional')

getVarCov(fm16.1,                     
          type = "conditional",       # sigma^2 * R_i = 74.434 * I4
          individual = "2")   
# diagonal matrix 
# Conditioned to the random effects b_i --> var-cov of the errors are independent and homoscedastic

# Conditional 
# Это остаточная дисперсия:
# Var(Yit∣bi)=σ^2*Ri (Ri - ковариационная матрица ошибок (как в прошлом семинаре))

# we extract sigma^2 * Ri for patients i=2,3,4
sR = getVarCov(fm16.1, type = "conditional", individual = 2:4)
# and we plot them
plot(as.matrix(bdiag(sR$`2`, sR$`3`, sR$`4`)),
     main = 'Conditional estimated Var-Cov matrix of Y given b')

# Marginal
# Var(Yit∣bi) = Zi*D*Zi^T + σ^2*Ri = σ^2*Vi
# Zi*D*Zi^T - ковариация, порождённая случайными эффектами (например, интерцептом).
# σ^2*Ri - остаточная ковариация

# --> the marginal variance-covariance matrix of Y (block-diagonal matrix)
#     sigma^2 * V_i for the second subject (type='marginal')
(sVi = getVarCov(fm16.1,                       
                 type = "marginal", # sigma^2 * V_i: sigma^2*d11 extra-diagonal and sigma^2(1+d11) on main diagonal
                 individual = "2"))
(cov2cor(sVi[[1]]))               # Corr(sigma^2 * V_i) 

# we extract sigma^2 * V_i for patients i=2,3,4
sV <- getVarCov(fm16.1, type = "marginal", individual = 2:4) 
# visualization 
plot(as.matrix(bdiag(sV$`2`, sV$`3`, sV$`4`)), #-> V is a block-diagional matrix, the marginal var-cov matrix
     main = 'Marginal estimated Var-Cov matrix of Y')


sVi
sR
VarCorr(fm16.1)[1,1]

tau2 <- as.numeric(VarCorr(fm16.1)[1,1])  # это d11
J <- matrix(1, nrow = 4, ncol =4)
sR_matrix <- diag(74.434, 4,4)
V_manual <- sR_matrix + tau2 * J

V_manual
sVi
# Vi - marginal variance-covariance matrix 
# we got same result as previous, but with different model

# PVRE 
#------#
# i.e. the Percentage of Variance explained by the Random Effect (PVRE).
# This is also called the intraclass correlation (ICC),
# because it is also an estimate of the within group correlation.

VarCorr(fm16.1)
var_b
var_eps

get_variance_residual(fm16.1)
get_variance_random(fm16.1)

PVRE <- var_b/(var_b+var_eps) # 80.60829/(80.60829+74.43401)
PVRE # it is high!
# because we have repeated measurments,


# Visualization of confidence intervals
#--------------------------------------#
intervals(fm16.1)

# fixed effects
library(sjPlot)
plot_model(fm16.1)

# random effects
## visualization of the random intercepts with their 95% confidence intervals
# Random effects: b_0i for i=1,...,234
re = ranef(fm16.1)
dat = data.frame(x= row.names(re),y=re[,attr(re,'effectName')])
# The dotplot shows the point and interval estimates for the random effects
# ordered
dotplot(reorder(x,y)~y,data=dat)
# not ordered
plot(ranef(fm16.1))

#_____________________________________________________________________________
# nicer visualization of the random effects with lmer() function
# formulation with lmer() for including a random intercept at subject level
fm16.1mer <- lmer(visual ~ visual0 + time * treat.f + (1|subject), data = armd)
# we can highligh which are significantly different from the mean (0)
# visualization of the random intercepts with their intervals
dotplot(ranef(fm16.1mer)) 

#install.packages('TMB', type = 'source')
plot_model(fm16.1mer, type='re') #--> positive (blu) and negative (red) effect
#_____________________________________________________________________________
# So, if there at least someone, who doesn't contain 0 (in CI), then we have to take
# into account random effect for patients, then we have to use patients specifica model (use random effect)



#Test for (variance of) random intercept
#--------------------------------------#
# H0 is "variance of a random effect is 0 
#       in an LMM with a known correlation structure of the tested random effect and
#       independent and identically distributed random errors"
library(RLRsim)    
exactRLRT(fm16.1)        # M16.1 (alternative model) 
# The function simulates values of the REML-based LRtest statistic.

# The p-value of the REML-based LR test,
# estimated from 10,000 simulations (the default), 
# --> pval clearly indicates the importance of including the random intercepts into the model, 
#     needed to adjust for the correlation between visual acuity measurements.

# Because we test a random effect in model M16.1,
# which contains only a single random effect (only the intercept), 
# we use the abbreviated form of the function call, with model M16.1 as the only argument. 


# Diagnostic plots 
#----------------#
# 1) Assessing Assumption on the within-group errors
plot(fm16.1)  # Pearson and raw residuals are the same now 
# (no scale is applied since we are dealing with homogeneous variance)

qqnorm(resid(fm16.1)) # normality of the residuals
qqline(resid(fm16.1), col='red', lwd=2)

# 2) Assessing Assumption on the Random Effects
qqnorm(fm16.1, ~ranef(.), main='Normal Q-Q Plot - Random Effects on Intercept')



#_______________________________________________________________________________
#### Model 2: random intercept + slope and homoscedastic residuals  (M16.2) ####

# random slope 

##### Model 2.A: general D (M16.2A) #####
#---------------------------------------#

fm16.2A <- lme(lm2.form, random = ~1 + time | subject, data = armd) # time - is slope
# previously we had ~1 | subject

summary(fm16.2A)
intervals(fm16.2A)

# Var-Cov matrix of random-effects and errors
print(vc <- VarCorr(fm16.2A), comp = c("Variance", "Std.Dev."))
sigma <- summary(fm16.2A)$sigma


# Let's compute the conditional and marginal var-cov matrix of Y
#---------------------------------------------------------------#
# the conditional variance-covariance matrix of Y (diagonal matrix)
getVarCov(fm16.2A,                     
          type = "conditional",       # sigma^2 * R_i
          individual = "2")   

# we extract sigma^2 * R_i for patients i=2,3,4
sR = getVarCov(fm16.2A, type = "conditional", individual = 2:4)
# and we plot them
plot(as.matrix(bdiag(sR$`2`, sR$`3`, sR$`4`)), 
     main = 'Conditional estimated Var-Cov matrix of Y given b')

# the marginal estimated variance-covariance matrix of Y (block-diagonal matrix)
(sVi = getVarCov(fm16.2A,                       
                 type = "marginal",      # sigma^2 * V_i
                 individual = "2"))
(cov2cor(sVi[[1]]))                     # Corr(sigma^2 * V_i) 

# we extract sigma^2 * V_i for patients i=2,3,4
sV <- getVarCov(fm16.2A, type = "marginal", individual = 2:4)
# and we plot them
plot(as.matrix(bdiag(sV$`2`, sV$`3`, sV$`4`)), 
     main = 'Marginal estimated Var-Cov matrix of Y')


sR
sVi
VarCorr(fm16.2A) #[1,1]

# PVRE 
#-----#
# In this case the variance the mean random effect variance of the model is given by
# var_b = Var(b0,b1) = sigma2_b0 +  sigma2_b1 * mean(w^2) + 2 * Cov(b0,b1) * mean(w) 
# remember that Cov(b0,b1) = Cor(b0,b1) * sd(b0) * sd(b1)
# See equation (10) in Johnson (2014), Methods in Ecology and Evolution, 5(9), 944-946.

var_eps <- as.numeric(vc[3,1])
var_eps
var_b <- as.numeric(vc[1,1]) + as.numeric(vc[2,1])*mean(armd$time^2) + 
  + 2*as.numeric(vc[2,3])*as.numeric(vc[1,2])*as.numeric(vc[2,2])*mean(armd$time) 
var_b

get_variance_residual(fm16.2A)
get_variance_random(fm16.2A)

# Сначала извлечь матрицу D
D <- matrix(as.numeric(vc[1:2, 1:2]), nrow = 2)  # если 2 случайных эффекта
# Среднее значение t
t_bar <- mean(armd$time)
# Вектор Z: [1, t]
Z <- matrix(c(1, t_bar), ncol = 1)
# Посчитать var_b
var_b <- t(Z) %*% D %*% Z
var_b


PVRE <- var_b/(var_b+var_eps)
PVRE # very high!



# Visualization 
#-------------#
# visualization of the random intercepts & slopes with their 95% confidence intervals
# Random effects: b_0i, b_1i for i=1,...,234
plot(ranef(fm16.2A))


# as before, we can fit better plots through lmer()
fm16.2Amer <- lmer(visual ~ visual0 + time * treat.f + (1+time|subject), data = armd, 
                   control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
dotplot(ranef(fm16.2Amer))

# for a better visualization
plot_model(fm16.2Amer, type='re') #--> positive (blu) and negative (red) effect



# Comparing models 
#-----------------#
anova(fm16.1, fm16.2A)
# The p-value for the test is essentially zero -> we prefer fm16.2A



# Diagnostic plots
#-----------------#
# 1) Assessing Assumption on the within-group errors
plot(fm16.2A)

# we still assume, that our residuals and Intercept & Slope have normal distribution
qqnorm(resid(fm16.2A))
qqline(resid(fm16.2A), col='red', lwd=2) # 

# 2) Assessing Assumption on the Random Effects
qqnorm(fm16.2A, ~ranef(.), main='Normal Q-Q Plot - Random Effects on Intercept & Slope')



intervals(fm16.2A, which = "var-cov")
# We observed that the 95% CI for correlation between sigma^2*d_11 and sigma^2*d_22 id contains 0 --> 
# --> they can be uncorrelated
# --> we fit a new model with a diagonal D matrix 



#_______________________________________________________________________________
##### Model 2.B: diagonal D (M16.2B) ######
#-------------------------------------------------------------------------------#

fm16.2B <- lme(lm2.form, random = list(subject = pdDiag(~1+time)), data = armd) 
# Diagonal D (diagonal positive-definite matrix)

intervals(fm16.2B)                       # 95% CI for betas, sigma  

summary(fm16.2B)


# Var-Cov matrix of random-effects and residuals
print(vc <- VarCorr(fm16.2B), comp = c("Variance", "Std.Dev."))


sigma <- summary(fm16.2B)$sigma
sigma


# Let's compute the conditional and marginal var-cov matrix of Y
#---------------------------------------------------------------#
# the conditional variance-covariance matrix of Y (diagonal matrix)
getVarCov(fm16.2B,                     
          type = "conditional",       # sigma^2 * R_i
          individual = "2")   
# Conditioned to the random effects b_i, we observe the var-cov of the errors
# that are independent and homoscedastic

# we extract sigma^2 * R_i for patients i=2,3,4
sR = getVarCov(fm16.2B, type = "conditional", individual = 2:4)
# and we plot them
plot(as.matrix(bdiag(sR$`2`, sR$`3`, sR$`4`)), 
     main = 'Conditional estimated Var-Cov matrix of Y given b')

# the marginal variance-covariance matrix of Y (block-diagonal matrix)
(sVi = getVarCov(fm16.2B,                       
                 type = "marginal",      # sigma^2 * V_i
                 individual = "2"))
(cov2cor(sVi[[1]]))                     # Corr(sigma^2 * V_i) 

# we extract sigma^2 * V_i for patients i=2,3,4
sV <- getVarCov(fm16.2B, type = "marginal", individual = 2:4)
# and we plot it
plot(as.matrix(bdiag(sV$`2`, sV$`3`, sV$`4`)), 
     main = 'Marginal estimated Var-Cov matrix of Y')


# PVRE

#calc_PVRE <- function(model) {
#  vc <- VarCorr(model)
#  var_all <- as.numeric(vc[, "Variance"])
#  var_eps <- var_all[length(var_all)]          # Последняя — всегда residual
#  var_b <- sum(var_all[-length(var_all)])      # Остальные — random effects
#  PVRE <- var_b / (var_b + var_eps)
#  return(PVRE)
#}


#-----#
# In this case the variance of random effects represents the mean random 
# effect variance of the model and is given by
# var_b = Var(b0,b1) = sigma2_b0 + 0 + sigma2_b1*mean(z^2)
# See equation (10) in Johnson (2014), Methods in Ecology and Evolution, 5(9), 944-946.
vc
var_eps <- as.numeric(vc[3,1])
var_eps
var_b <- as.numeric(vc[1,1]) + mean(armd$time^2)*as.numeric(vc[2,1]) # 54.07117 + 0.07935904*mean(armd$time^2) 
var_b

vc

get_variance_random(fm16.2B)
get_variance_residual(fm16.2B)

PVRE <- var_b/(var_b+var_eps)
PVRE # is very high!

VarCorr(fm16.2B)


# Visualization
#--------------#
# visualization of the random intercepts with their 95% confidence intervals
# Random effects: b_0i, b_1i for i=1,...,234
plot(ranef(fm16.2B))

# for a better visualization
fm16.2Bmer <- lmer(visual ~ visual0 + time * treat.f + (1|subject) + (0 + time|subject),
                   data = armd, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
dotplot(ranef(fm16.2Bmer))


# Comparing models
#--------------#
anova(fm16.2A, fm16.2B)



#### LINEAR MIXED MODELS WITH HETEROSCEDASTIC RESIDUALS ####

##### Model 1. Random intercept, heteroscedastic residuals (varPower of time) #####

## fixed-effects formula
lm2.form <- formula(visual ~ visual0 + time + treat.f + treat.f:time ) 

# LMM with homoscedastic residuals
fm16.1 <- lme(lm2.form, random = ~1|subject, data = armd)

# update fm16.1 including heteroscedastic residuals
fm16.2 <- update(fm16.1,
                 weights = varPower(form = ~ time), 
                 data = armd)

summary(fm16.2)

VarCorr(fm16.2)  

## var-cov matrix of the errors (i.e. of Y, conditional to the random effects), that are independent but heteroscedastic 
fm16.2ccov = getVarCov(fm16.2, type = "conditional",  individual = "2")
fm16.2ccov

plot(as.matrix(fm16.2ccov[[1]]), main = expression(paste('Conditional estimated Var-Cov matrix of ', Y[2])))

## var-cov matrix of Y_i
fm16.2cov = getVarCov(fm16.2, type = "marginal", individual = "2")
fm16.2cov # (90.479 = 31.103 + 59.37555; 121.440 = 62.062 + 59.37555; ...)
plot(as.matrix(fm16.2cov[[1]]), main = expression(paste('Marginal estimated Var-Cov matrix of ', Y[2])))

# var-cov matrix of y_i is the same for each subject i, 
# except for the number of observations, ranging from 1 to 4

## correlation matrix of Y_i
cov2cor(fm16.2cov[[1]])


### Analysis of Residuals ###

# Default residual plot of conditional Pearson residuals
plot(fm16.2)

# Plots (and boxplots) of Pearson residuals per time and treatment
plot(fm16.2, resid(., type = "pearson") ~ time | treat.f,
     id = 0.05)
bwplot(resid(fm16.2, type = "p") ~ time.f | treat.f, 
       panel = panel.bwplot, # User-defined panel (not shown)
       data = armd)
# Despite standardization, the variability of the residuals seems to vary a bit.

# Normal Q-Q plots of Pearson residuals 
qqnorm(fm16.2, ~resid(.) | time.f) 


### Analysis of Random Effects ###

# Normal Q-Q plots of predicted random effects
qqnorm(fm16.2, ~ranef(.))  

## Computing predictions comparing population average predictions with patient-specific predictions

aug.Pred <- augPred(fm16.2,
                    primary = ~time, # Primary covariate
                    level = 0:1, # fixed/marginal (0) and subj.-spec.(1)
                    length.out = 2) # evaluated in two time instants (4 e 52 wks)

plot(aug.Pred, layout = c(4, 4, 1))
# we can see, that for each patients we have differnet residuals and it's better fit the data
# so there we have to take into account random effect 

##### Model 2.1. random intercept + slope (correlated), heteroscedastic residuals (varPower of time) #####

fm16.3 <- update(fm16.2,
                 random = ~1 + time | subject,
                 data = armd)
summary(fm16.3)

getVarCov(fm16.3, individual = "2")  # D_i italic (i=2)



##### Model 2.2. random intercept + slope independent, heteroscedastic residuals (varPower of time) #####

fm16.4 <- update(fm16.3,
                 random = list(subject = pdDiag(~time)), # Diagonal D
                 data = armd) 
summary(fm16.4) ## results suggest to remove the Treat and Time interaction

getVarCov(fm16.4, individual = "2")  # D_i italic (i=2)

anova(fm16.4, fm16.3)  # We test if d_12 = 0 --> we can simplify the D structure in diagonal

qqnorm(fm16.4, ~ranef(.)) # to be interpreted with caution since it might not reflect the real unknown distribution

plot(fm16.4, resid(., type = "pearson") ~ time | treat.f,
     id = 0.05)
bwplot(resid(fm16.4, type = "p") ~ time.f | treat.f, 
       panel = panel.bwplot, # User-defined panel (not shown)
       data = armd)

## We make predictions comparing population average predictions with patient specific predictions
aug.Pred <- augPred(fm16.4,
                    primary = ~time, # Primary covariate
                    level = 0:1, # Marginal(0) and subj.-spec.(1)
                    length.out = 2) # Evaluated in two time instants (4 e 52 wks)

plot(aug.Pred, layout = c(4, 4, 1), columns = 2) 
# here we also see, that random also slope also have an effect


