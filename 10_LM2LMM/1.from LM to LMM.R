
#______________ Applied Statistics 2024/2025 _________________
####         Mixed-effects Models Module - Lab 1          ####
#-------------------------------------------------------------

# Topics: ####
#   0. Recap on Linear Models Formulation  
#   1. Linear Models with homoscedastic and independent errors
#   2. Linear Models with heteroscedastic and independent errors
#      2.1 VarFixed()
#      2.2 VarIdent()
#      2.3 VarPower()
#   3. Linear Models with heteroscedastic and dependent errors
#      3.1 corCompSymm()
#      3.2 corAR(1)
#      3.3 corSymm()
#   4. Hypotheses tests about the fixed effects


rm(list=ls())
graphics.off()

library(nlmeU) ## --> for the dataset
library(nlme)  ## --> for models implementation

library(corrplot)
library(lattice)
library(plot.matrix)

data(armd) # Age-Related Macular Degeneration: dataset of interest
data(armd0) # Age-Related Macular Degeneration: dataset for visualization
help(armd0)

# The ARMD data arise from a randomized multi-center clinical trial 
# comparing an experimental treatment (interferon-alpha) versus placebo 
# for patients diagnosed with ARMD. Patients with ARMD progressively lose vision.
# The dataset contains information about 234 subjects, for which the visual level is measured up to 4 times.
# We are mainly interested in the effect of treatment on the visual acuity measurements.

## The ARMD0 dataset contains the same information, but with an extra row for each patient relative 
## to the measurement at time 0. ARMD0 contains 240 subjects.  


## Visual-acuity profiles for selected patients --> we visualize some of the trends
armd0.subset <- subset(armd0, as.numeric(subject) %in% seq(1, 240, 5)) # one each 5 patients

xy1 <- xyplot(visual ~ time | treat.f,   
              groups = subject,
              data = armd0.subset,
              type = "l", lty = 1)
update(xy1, xlab = "Time (in weeks)",
       ylab = "Visual acuity",
       grid = "h")
## We observe a decreasing trend in time, on average, but patients have very different trends.
## Also, Active patients have on average lower values of the response.


## sample means across time and treatment
flst <- list(armd$time.f, armd$treat.f)
tMn <- tapply(armd$visual, flst, FUN = mean)
tMn

## We confirm what we observe in the plot

## Box-plots for visual acuity by treatment and time
bw1 <- bwplot(visual ~ time.f | treat.f,
              data = armd0)
xlims <- c("Base", "4\nwks", "12\nwks", "24\nwks", "52\nwks")
update(bw1, xlim = xlims, pch = "|")


##______________________________________________________________________________
### 0. Recap on Linear Models Formulation ####

# When we specify the Mean Structure using a Model Formula
# y (dependent variable) ~ x1 + x2 +... (mean structure of the model)
# (i.e. y = beta_0 + beta_1 * x1 + beta_2 * x2 + ... + eps )

# Operators used when specifying an R formula:
# + (Essential)(существенный)         Separates terms in the formula
# : (Essential)                       Separates predictors in interaction terms
# * (Non essential)                   Used to keep the formula short
# functions I() and update()          Used to keep the formula short

# y ~ x1                  # Univariate linear regression 
# formula(y ~ x1)         # ... equivalent specification

# y ~ 1 + x1              # Explicit indication for inclusion of an intercept (default) # короче добавление betta0
# y ~ -1 + x1             # Exclusion of the intercept using term -1 # убрать betta0 из модели 

# Employing nonessential operators
# y ~ f1*f2             means     y ~ f1 + f2 + f1:f2       # ANOVA with two-way interaction
# y ~ (f1+f2+f3)^2      means     y ~ f1 + f2 + f3 + 
#                                   f1:f2 + f1:f3 + f2:f3   # Up to 2nd order interactions

# Composite terms:
# -> Formulae syntax can be extended through mathematical functions, e.g.
# y ~ sqrt(x1) + x2     # Square root transformation of x1
# log(y) ~ x1 + x2      # log transform for y

# -> the functions I() and update():
# form2 = y ~ I(x1 + x2/100)   # I() function for to explicitate the arithmetic (vs default) meaning 
# update(form2, . ~ . + x3)    # x3 predictor added to form2 # добавить x3 в модель 
# update(form2, . ~ . -1)      # Intercept omitted from form2 # убрать betta0 из модели 



##______________________________________________________________________________
### 1. Linear Models with homogeneous and independent errors ####
### we start by considering all observations as independent, with homogeneous variance

# LM: VISUAL_it = b_0t + b1 * VISUAL_0i + b_2t * TREAT_i + e_it

# b_0t, and b_2t denote the timepoint-specific intercept, 
# baseline visual acuity effect, and timepoint-specific treatment effect.
# Thus, the model assumes a time-dependent treatment effect,
# with the time variable being treated as a factor.

# in first part we will modify A - matrix (correspond to covariance)
# in second part we will modify C - matrix (correspond to collinearity)

# To obtain timepoint-specific intercepts at 4,12,24 and 52 weeks, 
# the overall intercept is removed from the model by specifying the -1 term.

lm6.1 <- lm(visual ~ -1 + visual0 + time.f + treat.f:time.f, data = armd )
summary(lm6.1)

# variance-covariance matrix of Y  --> it is a diagonal matrix with a value of 12.38^2
par(mar = c(4,4,4,4))
plot(diag(x=12.38^2,nrow=12, ncol=12), main='Variance-covariance matrix of Y')

## residual analysis
plot(lm6.1$residuals) # they seem quite homoscedastic
abline(h=0)

qqnorm(lm6.1$residuals) # shapiro test sensitive to high number of observations, so if we have many value - then more probably p-valu < 0.05
qqline(lm6.1$residuals) # so looking on qqplot we can say, the there are something unusual with variability 

shapiro.test(lm6.1$residuals)

## But we know that observations are not independent 
## and that the variance of the visual measurements increases in time

## let's color the residuals relative to different patients
colori =rainbow(length(unique(armd$subject)))
num_sub = table(armd$subject)
colori2 = rep(colori, num_sub)
plot(lm6.1$residuals, col=colori2)
abline(h=0)   ## --> not very informative

boxplot(lm6.1$residuals ~ armd$subject, col=colori,
        xlab='Subjects', ylab='Residuals', main ='Distribution of residuals across patients')  ## --> informative!
# bocplot on residuals - here we see, that we have some groups 

## let's color the residuals relative to different time instants
set.seed(1)
colori =rainbow(4)
colori2 = colori[armd$tp] # associate to each one of the 4 time instants a color
plot(lm6.1$residuals, col=colori2, ylab='residuals')
abline(h=0)
legend(650, -25, legend=c("time 4wks", "time 12wks", "time 24wks", "time 52wks"),
       col=colori, lty=1, cex=0.8)
# we can see, that time 52 weeks have higher variability 

## Note: we observe that red points are the closest to 0, purple ones are the farthest
## We expect the residuals to be heterogeneous across different time instants observations


boxplot(lm6.1$residuals ~ armd$time.f, col=colori,
        xlab='Time.f', ylab='Residuals')  ## -> the variance of th observations increases in time


# The model does not take into account the correlation 
# between the visual acuity observations obtained from the same subject. 
# It also does not take into account the heterogeneous variability
# present at different time points. Thus, it should not be used as a basis for inference.


##______________________________________________________________________________
### 2. Linear models with heteroscedastic and independent errors ####

## gls() function allows the inclusion of heteroscedasticity (and dependency)
## gls(model, 
##     data, 
##     subset,                           # optional
##     na.action,                        # optional
##     weights = varFunc(form=formula),  # Focus of 2.
##     control = glsControl()            # a list of control values to replace the default ones
##                                         e.g., for changing number of iterations, ...              
## )

## weights
## NB. The default value of the weights argument is NULL --> LM with homoscedastic residual errors
## weights can be given directly as a one-sided formula

## We know that variance increases in time --> we model the variance as a function of time
## We have different possibilities

## varFunc Class (Chapter 8.2)
?varClasses

##_varClass___________parameters___________________Group
## varFixed()         value                        known weights
## varIdent()         value, form, fixed           <delta>-group
## varExp()           value, form, fixed           <delta>-group, <delta,mu>-group, <mu>-group
## varPower()         value, form, fixed           <delta>-group, <delta,mu>-group, <mu>-group
## varConstPower()    const, power, form, fixed    <delta>-group, <delta,mu>-group, <mu>-group

# same model of before, using gls()
fm6.1 <- gls(visual ~ -1 + visual0 + time.f + treat.f:time.f, data = armd)

#### 2.1 Option 1: VarFixed() ####

# we suppose that the variance is proportional to the time

fm9.0 <- gls(visual ~ -1 + visual0 + time.f + treat.f:time.f, 
             weights = varFixed(value = ~time), # Var.function; lambda_i(v_i) (v_i is known and observable)
             data = armd)
# NB. the variance covariate needs to be continuous: if we put time.f, it doesn't work!

summary(fm9.0) # even if p-value of coefficient > 0.05 - we can not just remove it 


anova(fm6.1, fm9.0) # we can compare the models to see which one is better (lower AIC) (Akaike information criterion)

# Visualization of variance-covariance matrix of Y (12 observations, 3 patients)
par(mar = c(4,4,4,4))
plot(diag(x=c(  4 * 3.222976^2, # v_i * sigma^2 # sigma - residuals standart error
               12 * 3.222976^2, 
               24 * 3.222976^2, 
               52 * 3.222976^2), nrow=12, ncol=12),
     main='Variance-covariance matrix of Y - VarIdent() - Model 9.0')


# another example
# we suppose that the variance is proportional to 1/visual0 
fm9.0b <- gls(visual ~ -1 + visual0 + time.f + treat.f:time.f, 
              weights = varFixed(value = ~I(1/visual0)), data = armd)
summary(fm9.0b)

anova(fm6.1, fm9.0, fm9.0b)



#### 2.1 Option 2: VarIdent() ####

fm9.1 <- gls(visual ~ -1 + visual0 + time.f + treat.f:time.f,  # the same as before
             weights = varIdent(form = ~1|time.f), # Var. function; <delta, stratum>-group
             data = armd)
summary(fm9.1)

plot(fm9.1$residuals)  # Paramters estiamtes - our sigmas, which inreases a

fm9.1$modelStruct$varStruct
intervals(fm9.1, which = "var-cov")  ## 95% CI

# Visualization of Variance-covariance matrix of Y (12 observations, 3 patients)
par(mar = c(4,4,4,4))
plot(diag(x=c(1.000000^2*8.244094^2,  # delta_t^2 * sigma^2
              1.397600^2*8.244094^2, 
              1.664321^2*8.244094^2, 
              1.880852^2*8.244094^2), nrow=12, ncol=12),
     main='Variance-covariance matrix of Y - VarIdent()  - Model 9.1')

## To formally test the hypothesis that the variances are timepoint specific, 
## we apply the anova() function. 

## The anova() function will take the model objects as arguments, and return an ANOVA testing 
## whether the more complex model is significantly better at capturing the data than the simpler model.

# H0 - delta2-4 = 1 
anova(fm9.1, fm6.1)  ## fm6.1 C fm9.1



#### 2.1 Option 3: VarPower() ####

## Now that we know the variance is increasing in time, we try a more parsimonious model

fm9.2 <- update(fm9.1, weights = varPower(form = ~time)) # Var. function; <delta>-group
## Notice continuous-time variable "time" rather than to the factor "time.f"
##        if you set time.f, there would be an error
summary(fm9.2)

fm9.2$modelStruct$varStruct
intervals(fm9.2, which = "var-cov")


# Visualization of Variance-covariance matrix of Y (12 observations, 3 patients)
par(mar = c(4,4,4,4))
plot(diag(x=c(4^(2*0.2519332)*5.974906^2,  # TIME_it^{2*delta} * sigma^2
              12^(2*0.2519332)*5.974906^2, # we multiply by 2, because of it computed sigma_it, and we need sigma_it^2
              24^(2*0.2519332)*5.974906^2, 
              52^(2*0.2519332)*5.974906^2), nrow=12, ncol=12),
     main='Variance-covariance matrix of Y - VarPower() - Model 9.2')



# Test of the variance structure: power of time vs. timepoint-specific variances
anova(fm9.2, fm9.1) # fm9.2 C fm9.1 # so we can not reject H0 that delta = 0

AIC(fm9.2, fm9.1)  # --> fm9.2 is better in terms of AIC and parsimony!


## Alternatively: 
## - with delta = [delta_1, delta_2], strata = treatment group treat.f  
fm9.3 <- update(fm9.1,                            
                weights = varPower(form = ~time|treat.f))   # <delta>-group
summary(fm9.3)

anova(fm9.2, fm9.3) # fm9.2 C fm9.3 # our fm9. models trying to find best transformation of time


## Residual analysis -- we assess the fit of the model using residual plots. 

## raw residuals 
plot(fm9.2, resid(., type = "response") ~ fitted(.)) # Raw vs. fitted
# We observe an asymmetric pattern, with large positive (negative) residuals 
# present mainly for small (large) fitted values.
# But it can be a consequence of the fact that raw residuals are intrinsically 
# heteroscedastic and correlated.

plot(fm9.2, resid(., type = "response") ~ time) # Raw vs. time (not shown)
bwplot(resid(fm9.2) ~ time.f, pch = "|", data = armd)
# The box-and-whiskers plots clearly show an increasing variance of the residuals.

## Pearson residuals [ ^eps_i/sqrt(Var(y_i)) ]
## Pearson residuals are obtained from the raw residuals by dividing the latter by an
## estimate of the appropriate residual standard deviation, so they should be more homoscedastic

plot(fm9.2, resid(., type = "pearson" ) ~ fitted(.)) # Pearson vs. fitted
plot(fm9.2,resid(., type = "pearson") ~ time) 
bwplot( resid(fm9.2, type = "pearson") ~ time.f, # Pearson vs. time.f
        pch = "|", data = armd)
## this plot illustrate the effect of scaling: the variance of the residuals is virtually constant.


##______________________________________________________________________________
### 3. Linear models with heteroscedastic and dependent errors ####

## We now modify the model, so that the visual acuity measurements,
## obtained for the same individual, are allowed to be correlated.

## We can estimate the semivariogram to calculate correlation coefficients between Pearson
## residuals for every pair of timepoints, separately. 

# The semivariogram function can be defined as the complement of the correlation function.

## Variogram per time difference (time = 4,12,24,52)
Vg1 <- Variogram(fm9.2, form = ~ time | subject)
Vg1
plot(Vg1, smooth = FALSE, xlab = "Time difference",ylim=c(0,0.7))

## Variogram per time lag (tp = 1,2,3,4), i.e., the absolute difference between two position indexes
Vg2 <- Variogram(fm9.2, form = ~tp | subject)
Vg2
plot(Vg2, smooth = FALSE, xlab = "Time Lag",ylim=c(0,0.7))

## From these two plots we see that correlation decreases (maybe inreases) with time lag/difference
## Therefore, a  correlation structure like, e.g., a compound symmetry, will most likely not fit the data well. 
## A more appropriate structure might be, e.g., an autoregressive process of order 1 AR(1). 

## Nevertheless, for illustrative purposes, we consider a model with a compound symmetry
## correlation structure.

#### 3.1 Option 1: corCompSymm() - Compound-Symmetry Correlation Structure ####

fm12.1 <- gls(visual ~ -1 + visual0 + time.f + treat.f:time.f, 
              weights = varPower(form = ~time),
              correlation = corCompSymm(form = ~1|subject), 
              # correlation on what happen between first and last,
              # same as correlation between second and third... and others
              data = armd)
summary(fm12.1)

intervals(fm12.1, which = "var-cov") # if contaoin 0, then probably rpho = 0, and there are no correlation effect
# With the estimates of rho, sigma and delta we can estimate the var-cov matrix

## The marginal variance-covariance structure
## Estimate of R_i (italic)
fm12.1vcov <- getVarCov(fm12.1, individual = "2")  # e.g. i=2
nms <- c("4wks", "12wks", "24wks", "52wks")
dnms <- list(nms, nms)       # Dimnames created
dimnames(fm12.1vcov) <- dnms # Dimnames assigned
print(fm12.1vcov)

## on the diagonal we have (5.981515^2)*TIME^(2*0.2598167)
## out of the diagonal we have (5.981515^2)*TIME_1^(0.2598167)*TIME_2^(0.2598167)*rho

## Visualization of R_i (italic)
R_i = fm12.1vcov
R = matrix(0, nrow=28, ncol=28) # example for 7 subjects with all the observations
for(i in 0:6){
  R[(i*4+1):(i*4+4),(i*4+1):(i*4+4)] = R_i
}
plot(R)

## The compound-symmetry correlation structure: 
## Estimate of C_i
print(cov2cor(fm12.1vcov), corr = TRUE, stdevs = FALSE)  ## Estimate of C_i (correlation matrix)

## Visualization of C_i
C = matrix(0, nrow=28, ncol=28) # example for 7 subjects with all the observations
for(i in 0:6){
  C[(i*4+1):(i*4+4),(i*4+1):(i*4+4)] = cov2cor(fm12.1vcov)
}
plot(C)

## Test of independence vs. compound-symmetry correlation structure
anova(fm9.2, fm12.1) # M9.2 C M12.1 # may try with fm9.0

# The result of the LR test is clearly statistically significant, indicating
# the importance of the adjustment for the correlation in modeling the data


#### 3.1 Option 2: corAR(1) -  Heteroscedastic Autoregressive Residual Errors ####

fm12.2 <- update(fm9.2, 
                 correlation = corAR1(form = ~tp|subject),  # correlation goes down among time 
                 # (as far, as less correlation with first observation)
                 data = armd)
# Notice that (form = ~1 | subject) would lead to a mistake
# as form = ~1|group means "use of the order of the observations in the group as the position index
# and for some subjects, intermittent measurements may be missing
summary(fm12.2)
intervals(fm12.2, which = "var-cov")

## The marginal variance-covariance structure: 
## Estimate of R_i (italic)
fm12.2vcov <- getVarCov(fm12.2, individual = "2")  # e.g. i=2
dimnames(fm12.2vcov) <- dnms
fm12.2vcov

## on the diagonal we have (6.356295^2)*TIME^(2*0.2311874)
## out of the diagonal we have (6.35629^2)*4^(0.2311874)*12^(0.2311874)*0.6573069
##                             (6.35629^2)*4^(0.2311874)*24^(0.2311874)*0.6573069^2...


## Estimate of C_i (italic)
fm12.2cor <- cov2cor(fm12.2vcov)  
print(fm12.2cor, digits = 2, 
      corr = TRUE, stdevs = FALSE)

# Compound-symmetry vs. autoregressive correlation (nonnested models)
anova(fm12.1, fm12.2)
## we prefer AR(1)


#### 3.1 Option 3: corSymm() - General correlation matrix for Residual Errors ####

fm12.3 <- update(fm12.2, 
                 correlation = corSymm(form = ~tp|subject),  ## the variance function is still VarPower()
                 data = armd)
summary(fm12.3)

intervals(fm12.3, # 95% CIs for rho, delta, sigma
          which = "var-cov")

## Estimate of R_i (italic)
fm12.3vcov <- getVarCov(fm12.3, individual = "2")  
dimnames(fm12.3vcov) <- dnms
fm12.3vcov   # fm12.3vcov[1,1] = sigma_2 * Lambda[1]^2 = 5.737927^2*(4^0.2712624)^2

## Estimate of C_i
fm12.3cor <- cov2cor(fm12.3vcov)    
print(fm12.3cor, corr = TRUE, stdevs = FALSE)
# the correlation decreases for visual acuity measurements more distant in time


## Autoregressive of order 1 vs. a general correlation structure
anova(fm12.2, fm12.3) # M12.2 C M12.3 --> we prefer M12.3 


## Model-Fit Diagnostics

# (a) Plots (and boxplots) of raw residuals
panel.bwxplot0 <- function(x,y, subscripts, ...){
  panel.grid(h = -1)
  panel.stripplot(x, y, col = "grey", ...)
  panel.bwplot(x, y, pch = "|", ...)
}
bwplot(resid(fm12.3) ~ time.f | treat.f, 
       panel = panel.bwxplot0,
       ylab = "Residuals", data = armd)
# The box-and-whiskers plots clearly show an increasing variance of the residuals with timepoint. 
# This reflects the heteroscedasticity.


# (b) Plots of Pearson residuals vs. fitted values
# Pearson residuals are obtained from the raw residuals by dividing the latter by an
# estimate of the appropriate residual standard deviation, so they should be more homoscedastic

plot(fm12.3) 
# Due to the correlation of the residuals corresponding to the measurements obtained
# for the same patient at different timepoints, the plot reveals a pattern, with a few
# large, positive residuals in the upper-left part and a few negative ones in the lower-right part.

## We therefore decide to visualize the residuals for each time instants
plot(fm12.3, 
     resid(., type = "p") ~ fitted(.) | time.f)
stdres.plot <-
  plot(fm12.3, resid(., type = "p") ~ jitter(time) | treat.f,
       id = 0.01, adj = c(-0.3, 0.5 ), grid = FALSE)
plot(update(stdres.plot, # Fig. 12.4
            xlim = c(-5,59), ylim = c(-4.9, 4.9), grid = "h"))
# The four scatterplots show a somewhat more balanced pattern.



#_______________________________________________________________________________
### 4. Hypotheses tests about the fixed effects ####

# We now start from the model fm12.3, keeping the same variance-covariance structure,
# but we modify the mean structure
# fm12.3 was
# VISUAL_it = b_0t + b_1 * VISUAL0_i + b_2t * TREAT_i + eps_it
# visual ~ -1 + visual0 + time.f + treat.f:time.f 

##### model fm12.3a #####
# (we add a general intercept; equivalent to 12.3 but refitted with 'ML') 
# VISUAL_it = b_0 + b_0t + b_1*VISUAL0_i + b_2*TREAT_i + b_2t*TREAT_i + eps_it
lm1a.form <- formula (visual ~ visual0 + time.f + treat.f + time.f:treat.f)   
fm12.3a <- update(fm12.3, lm1a.form,          
                  method = "ML", data = armd)

##### model fm12.4 #####
# (we remove time dependent intercept and we leave general intercept 
#  & we change time from factor to continuous variable)
# VISUAL_it = b_0 + b_1*VISUAL0_i + b_2*TIME_t + b_3*TREAT_i + b_4 * TIME_t * TREAT_i + eps_it
lm2.form <- formula(visual ~ visual0 + time + treat.f + treat.f:time)
fm12.4 <- update(fm12.3, lm2.form,            
                 method = "ML", data = armd)

##### model fm12.5 #####
# (we remove from fm12.4 the interaction between time and treatment)
# VISUAL_it = b_0 + b_1 * VISUAL0_i + b_2 * TIME_t + b_3 * TREAT_i + eps_it
lm3.form <-  update(lm2.form, . ~ . - treat.f:time)     
fm12.5 <- update(fm12.3, lm3.form,            # fm12.5 <- fm2.3
                 method = "ML", data = armd)


##### comparison #####
anova(fm12.3a, fm12.4, fm12.5)

summary(fm12.5)
# final goal - decide that treatment is affective or not, so in our case treatments doesn't look effective 


