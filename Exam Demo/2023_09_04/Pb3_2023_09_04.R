library(nlmeU) ## --> for the dataset
library(nlme)
library(insight)

# We aim to investigate the factors influencing customer satisfaction at FashionFancyClothes ready-to-wear shops.
# The file satisfaction.txt comprises satisfaction scores from 400 FashionFancyClothes customers, along with
# their total purchase amounts, membership duration, age, and preferred store location. We want to analyze how
# these factors influence customer satisfaction using a linear model of the form:

# score = betta_0 + betta_1 purch amount + betta_2 memb duration + betta_3 age + eps

# eps ~ N(0, sigma^2)

# a) Fit the model and report the estimates of the unknown parameters.

satisfaction <- read.table("2023_09_04/satisfaction.txt", h=TRUE)

lm_model <- lm(score ~ 1 + purch_amount  + memb_duration + age,
               data=satisfaction)

summary(lm_model)

# Call:
#   lm(formula = score ~ 1 + purch_amount + memb_duration + age, 
#      data = satisfaction)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.48147 -0.41134 -0.00233  0.36039  1.63951 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    1.0818153  0.1157442   9.347   <2e-16 ***
#   purch_amount   0.0144580  0.0006375  22.681   <2e-16 ***
#   memb_duration  0.0097201  0.0058901   1.650   0.0997 .  
# age           -0.0052748  0.0026943  -1.958   0.0510 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.5649 on 396 degrees of freedom
# Multiple R-squared:  0.8404,	Adjusted R-squared:  0.8392 
# F-statistic: 695.2 on 3 and 396 DF,  p-value: < 2.2e-16

# b) State and verify the model assumptions.

# ASSUMPTIONS -------------------------------------------------------------------
# Assumptions required for estimation: 
# - Residuals have zero mean and are homoscedastic: 
# The residuals vs fitted diagnostic plot shows that the residuals are evenly distributed on both sides of the zero line and show no specific
# mean or variance pattern against the fitted values
# Assumptions required for inference: 
# - Residuals are normally distributed: The residuals Q-Q plot shows that the empirical quantiles
# are close to normal quantiles, except for the left tail which seems a bit heavy

par(mfrow=c(2,2))
plot(lm_model)

shapiro.test(lm_model$residuals)
# Shapiro-Wilk normality test
# 
# data:  lm_model$residuals
# W = 0.99422, p-value = 0.1348 
# can not reject H0 of residuals normality

plot(lm_model$residuals) 
# also don't see any specific patterns and shapes of residuals vs fitted values and among all residuals

# c) Perform a test of level 5% to verify whether memb duration and age have an effect on the satisfaction score.

confint(lm_model, parm = c("memb_duration", "age"), level=1-0.05/2)
#                     1.25 %      98.75 %
# memb_duration -0.003532262 0.0229725154
# age           -0.011336808 0.0007872555
# So we can not say, that there are signifficant effect of age and memb_duration
# as CI contains zero

# d) Perform any other statistical tests that you consider useful to reduce the model, and update the estimates of
# its parameters.
# Lets step by step remove params

# first remove memb_duration as it has higer p-value
lm_model2 <- lm(score ~ 1 + purch_amount + age,
               data=satisfaction)

summary(lm_model2)

# now remove age
lm_model3 <- lm(score ~ 1 + purch_amount,
                data=satisfaction)

summary(lm_model3)

# e) Let’s now add to the model found in (d) the variable store, added as a random intercept. Fit the new model
# and report the PVRE index.

lme_model <- lme(score ~ 1 + purch_amount,
                 random =  ~1|as.factor(store),
                 data=satisfaction)

summary(lme_model)

var_b <- get_variance_random(lme_model)
var_eps <- get_variance_residual(lme_model)
var_b # 0.06575383
var_eps # 0.2590417

PVRE <- var_b/(var_b+var_eps) # we can take them from summary as ^2 of StdDev
PVRE # 0.2024468 

# f) Report the dot plot of the estimated random intercepts. Ignoring the effect of the fixed effect covariates, which
# store is associated to the highest score?

plot(ranef(lme_model))

max(ranef(lme_model)) # store F
