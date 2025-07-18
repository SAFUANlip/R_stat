
#______________ Applied Statistics 2024/2025 _________________
####         Mixed-effects Models Module - Lab 2          ####


#### Students and Schools example ####
#_____________________________________

rm(list=ls())
graphics.off()

# Topics: ####
#   - Linear Mixed Models with Random Intercept
#   - Linear Mixed Models with Random Intercept + Random Slope
#   - Inference
#   - Assessing Assumptions
#   - Prediction


library(ggplot2)
library(insight)
library(lattice)
library(lme4)


rm(list=ls())
graphics.off()

# Dataset school - Student achievement

# We have data from 1000 pupils who attend 50 different primary schools. 
# Students are tested in mathematics with a standardized test across schools. 
# The response variable is the achievement test score (numeric). 
# We have two explanatory variables at the student level: 
#   - pupil gender (1 = male, 0 = female)
#   - a scale centered in 0 for pupil socioeconomic status, pupil escs.
# Moreover, we know the anonymous school identification number. 


school= read.table('school.txt', header=T)
school$gender= as.factor(school$gender)
school$school_id= as.factor(school$school_id)
head(school)
str(school)

# We look at achievement scores for students. 
# The source of dependency is due to students attending the same primary school.

# For our mixed model we'll look at the effects for gender and socioeconomic status (escs) 
# on scholastic achievement,
# taking into account the source of dependency given to the hierarchical structure. 

summary(school)
sd(school$achiev)

sqrt(var(school$achiev))

# Achievement variability in primary schools

x11()
ggplot(data=school, aes(x=as.factor(school_id), y=achiev, fill=as.factor(school_id))) +
  geom_boxplot() +
  labs(x='Primary School', y='Achievement') +
  ggtitle('Boxplot of achievements among primary schools') +
  theme_minimal() +
  theme(axis.text=element_text(size=rel(1.15)),axis.title=element_text(size=rel(1.5)),
        plot.title = element_text(face="bold", size=rel(1.75)), legend.text = element_text(size=rel(1.15)),
        legend.position = 'none')
# we can see that there are a lot of variability among schools 



#### Linear Model ####

# We start with a standard linear regression model, neglecting the dependence structure

# MODEL: achiev_i = beta_0 + beta_1*gender_i+ beta_2*escs_i + eps_i
# eps_i ~ N(0, sigma2_eps)

# lm1 <- lm(achiev ~ 1+gender+escs, data=school)

lm1 = lm(achiev ~ gender + escs, data = school)
summary(lm1)

plot(school$escs,school$achiev, col='blue')
abline(9.91880,1.86976, col='green', lw=4)          # females
abline(9.91880 -0.68298,1.86976, col='orange', lw=4)  # males

plot(lm1$residuals)

boxplot(lm1$residuals ~ school$school_id, col='orange', xlab='School ID', ylab='Residuals')
## residuals differ a lot across schools


#-----------------------------#
# Linear Mixed Effects Models #
#-----------------------------#
# We now take into account the clustering at primary school --> dependency among students within the same school

# MODEL: achiev_ij = beta_0 + beta_1*gender_ij + beta_2*escs_ij + b_i + eps_ij
# eps_ij ~ N(0, sigma2_eps)
# b_i ~ N(0, sigma2_b) - random intercept by schools 

lmm1_lme <- lme(achiev ~ 1 + gender + escs, random = ~1|school_id, data = school)
summary(lmm1_lme)

lmm1 = lmer(achiev ~ gender + escs + (1|school_id), 
                      data = school)
summary(lmm1)


# Fixed Effects and 95% CIs --------------------------------------------------------------

confint(lmm1, oldNames=TRUE)
intervals(lmm1_lme)
fixef(lmm1)

# The fixed effects tell us there is a negative effect of being male on achievement, 
# and on average, students with higher escs are associated to higher achievement scores.


# Variance components ------------------------------------------------------------
# One thing that's new compared to the standard regression output is the estimated 
# variance/standard deviation of the school effect.
# This tells us how much, on average, achievement bounces around as we move from school to school. 
# In other words, even after making a prediction based on student covariates, each school has its
# own unique deviation, and that value (in terms of the standard deviation) is the estimated 
# average deviation across schools. 

print(vc <- VarCorr(lmm1), comp = c("Variance", "Std.Dev."))
help(get_variance)

get_variance(lmm1_lme) # also work with lme models

sigma2_eps <- as.numeric(get_variance_residual(lmm1))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(lmm1))
sigma2_b

# Another way to interpret the variance output is to note percentage of the student variance out 
# of the total, i.e. the Percentage of Variance explained by the Random Effect (PVRE).
# This is also called the intraclass correlation (ICC), because it is also an estimate of the within 
# cluster correlation.
PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE

# PVRE = 41.8% is very high!

# Random effects: b_0i --------------------------------------------------------------

ranef(lmm1)

# The dotplot shows the point and interval estimates for the random effects, 
# ordering them and highlighting which are significantly different from the mean (0)

x11()
dotplot(ranef(lmm1))

# Random intercepts and fixed slopes: (beta_0+b_0i, beta_1, beta_2)
coef(lmm1)
head(coef(lmm1)$school_id)


# Let's plot all the regression lines
## FEMALES
x11()
par(mfrow=c(1,2))
plot(school$escs[school$gender==0], school$achiev[school$gender==0],col='blue',
     xlab='escs', ylab='achievement',ylim=c(-5,30),main='Data and regression lines for females')
abline(10.02507,1.96618, col='red', lw=6)          

for(i in 1:50){
  abline(coef(lmm1)$school_id[i,1], coef(lmm1)$school_id[i,3])
}

## MALES
plot(school$escs[school$gender==1], school$achiev[school$gender==1],col='blue',
     xlab='escs', ylab='achievement',ylim=c(-5,30),main='Data and regression lines for males')
abline(10.02507-0.91180,1.96618, col='red', lw=6)  

for(i in 1:50){
  abline(coef(lmm1)$school_id[i,1] + coef(lmm1)$school_id[i,2], coef(lmm1)$school_id[i,3])
}

# 

# Diagnostic plots ------------------------------------------------------------

# 1) Assessing Assumption on the within-group errors
x11()
plot(lmm1)

x11()
qqnorm(resid(lmm1))
qqline(resid(lmm1), col='red', lwd=2)

# 2) Assessing Assumption on the Random Effects
x11()
qqnorm(unlist(ranef(lmm1)$school_id), main='Normal Q-Q Plot - Random Effects for Primary School')
qqline(unlist(ranef(lmm1)$school_id), col='red', lwd=2)



# Prediction --------------------------------------------------------------------

# Let's now examine standard predictions vs. cluster-specific predictions.
# As with most R models, we can use the predict function on the model object.

# Prediction from regression model
predict_lm <- predict(lm1)
head(predict_lm) # see our prediction

# Prediction from mixed model:
# 1) Without random effects ->  re.form=NA
predict_no_re <- predict(lmm1, re.form=NA)
head(predict_no_re) # same predictions
# 2) With random effects
predict_re <- predict(lmm1)     
head(predict_re)


# Prediction from mixed model on a test observation from a subject present in the training set:
test.data = data.frame(school_id= '50', gender = as.factor(1), escs = 3)

ranef(lmm1)[[1]]

# 1) Without random effects ->  re.form=NA
predict_no_re <- predict(lmm1, newdata = test.data, re.form=NA)
predict_no_re # (9.94471 - 0.74963*1 + 1.86878*test.data$escs )

# 2) With random effects
predict_re <- predict(lmm1, newdata = test.data)
predict_re # (9.94471 - 0.74963*1 + 1.86878*test.data$escs +  0.8234367)

# where 0.8234367 comes from the random intercept vector and corresponds to the school_id 50
re = ranef(lmm1)[[1]]
re[row.names(re)==test.data$school_id,]

# NEW SCHOOL (NEW GROUP)
## --> remember to allow new levels in the RE if any!!!
your_new_data = data.frame(school_id= '51', gender = factor(1), escs = 3)

# 1) Without random effects ->  re.form=NA
predict(lmm1, newdata = your_new_data, re.form=NA)

# 2) With random effects
predict(lmm1, newdata = your_new_data)
# it does not recognize the subject --> allow.new.levels = T
predict(lmm1, newdata = your_new_data, allow.new.levels = TRUE) # assuming of random intercept = 0
# the same as before, it uses the average of the random intercept, i.e. 0



## Scenario Analysis ------------------------------------------------------------

# Let's imagine to observe three new students with the same personal characteristics 
# but enrolled in different schools,
# two of them are observed and one is new

new_student1 = data.frame(gender=as.factor(1), escs=0.7, school_id=32) # observed school
new_student2 = data.frame(gender=as.factor(1), escs=0.7, school_id=11) # observed school
new_student3= data.frame(gender=as.factor(1), escs=0.7, school_id=53) # new school

predict(lmm1, new_student1, re.form=NA)
predict(lmm1, new_student1)

predict(lmm1, new_student2, re.form=NA)
predict(lmm1, new_student2)

predict(lmm1, new_student3, re.form=NA)
predict(lmm1, new_student3, allow.new.levels = T)

summary(lmm1)
# but we can cluster schools, and then find to whicj cluster our new school belongs 
# and use information from this school for prediction

#### Linear Mixed Model with Random Intercept & Slope --------------------------

graphics.off()

## We now consider the possibility that the association between escs and student achievements differs across schools.
## We include a random slope for the escs to model this additional source of heterogeneity. 

lmm2_lme <- lme(achiev ~ 1 + escs + gender, random=~1+escs|school_id, data=school)
summary(lmm2_lme)

# b_0i - intercpet of scools
# b_1i - slope of escs
# MODEL:  achiev_ij = beta_0 + b_0i + (beta_1 + b_1i)*escs_i + eps_i --> homoscedastic residuals 

# eps_i ~ N(0, sigma2_eps)
# Random effects: b_i ~ N(0, Sigma) (b_0i - random intercept, b_1i - random slope)

# To allow both the intercept, represented by 1, and the slope, represented by escs,
# to vary by student we can add the term:
#   - (1+escs|school_id)
# or, in alternative, without 1
#   - (escs|school_id)

lmm2 = lmer(achiev ~ gender + escs + (1 + escs|school_id), 
                data = school)
summary(lmm2)

confint(lmm2, oldNames=TRUE)



# Variance components
#--------------------
# In this case the variance of random sigma2_R effects represents the mean random 
# effect variance of the model and is given by
# sigma2_b = Var(b0,b1) = sigma2_b0 + 2Cov(b0,b1)*mean(w) + sigma2_b1*mean(w^2)
# See equation (10) in Johnson (2014), Methods in Ecology and Evolution, 5(9), 944-946.

print(vc <- VarCorr(lmm2), comp = c("Variance", "Std.Dev."))

sigma2_eps <- as.numeric(get_variance_residual(lmm2))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(lmm2))  ## it automatically computes Var(b0,b1)
# 4.3228 + 2 * 0.164 * 2.0791 * 1.6451 * mean(school$escs, na.rm=T) + 2.7063*mean(school$escs^2, na.rm=T)
sigma2_b

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE

# PVRE = 56%

# Estimates of fixed and random effects
#--------------------------------------

# Fixed effects: (beta_0, beta_1, beta_2)
fixef(lmm2)

# Random effects: (b_0i, b_1i) for i=1,...,200
ranef(lmm2)
head(ranef(lmm2)$school_id)

x11()
dotplot(ranef(lmm2))

# Random intercepts and slopes: (beta_0+b_0i, beta_1, beta_2+b_2i)
coef(lmm2)
head(coef(lmm2)$school)



# Lines Visualization
#---------------------


# Let's plot all the regression lines
## FEMALES
x11()
par(mfrow=c(1,2))
plot(school$escs[school$gender==0], school$achiev[school$gender==0],col='blue',
     xlab='escs', ylab='achievement',ylim=c(-5,30),main='Data and regression lines for females')
abline(10.0546535,1.6790886, col='red', lw=6)          

for(i in 1:50){
  abline(coef(lmm2)$school_id[i,1], coef(lmm2)$school_id[i,3])
}

## MALES
plot(school$escs[school$gender==1], school$achiev[school$gender==1],col='blue',
     xlab='escs', ylab='achievement',ylim=c(-5,30),main='Data and regression lines for males')
abline(10.02507-0.91180,1.96618, col='red', lw=6)  

for(i in 1:50){
  abline(coef(lmm2)$school_id[i,1] + coef(lmm2)$school_id[i,2], coef(lmm2)$school_id[i,3])
}


# Diagnostic plots 
#--------------------
# 1) Assessing Assumption on the within-group errors
x11()
plot(lmm2)

x11()
qqnorm(resid(lmm2))
qqline(resid(lmm2), col='red', lwd=2)


# 2) Assessing Assumption on the Random Effects
x11()
par(mfrow=c(1,2))
qqnorm(unlist(ranef(lmm2)$school_id[1]), main='Normal Q-Q Plot - Random Effects on Intercept')
qqline(unlist(ranef(lmm2)$school_id[1]), col='red', lwd=2)
qqnorm(unlist(ranef(lmm2)$school_id[2]), main='Normal Q-Q Plot - Random Effects on escs')
qqline(unlist(ranef(lmm2)$school_id[2]), col='red', lwd=2)

x11()
plot(unlist(ranef(lmm2)$school_id[2]),unlist(ranef(lmm2)$school_id[1]),
     ylab=expression(paste('Intercept  ', b['0i'])),
     xlab=expression(paste('escs  ', b['1i'])), col='dodgerblue2',
     main='Scatterplot of estimated random effects')
abline(v=0,h=0)
# Alternative plot(ranef(lmm2))


# Comparing models
#------------------
# The anova function, when given two or more arguments representing fitted models,
# produces likelihood ratio tests comparing the models.
anova(lmm1, lmm2)

# The p-value for the test is essentially zero -> we prefer lmm2

anova(lmm1_lme, lmm2_lme)
graphics.off()


