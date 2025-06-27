library(nlmeU) ## --> for the dataset
library(nlme)
library(insight)


# The file plants mood.txt contains data from 2000 residents of high-rise apartment buildings across 20 different
# urban zones in Southeast Asia, identified by the variable zone id. Participants took part in a behavioral ecology
# project called Green Within Walls, which examined how indoor plant ownership a!ects daily well-being in dense
# urban environments. Each resident logged daily mood scores via an app and completed surveys on their indoor
# environments.

# Consider the following linear mixed-e!ects model:
#   moodi = ω01ni + ω1 plantsi + ω2 lighti + ω3 sociali + bi_1ni + εi 
# plants_mood <- read.table("plants_mood.txt", h=TRUE)

# for i → zone id, with εi = N (0, ϑ2Ini), bi = N (0, ϑ2b), and ni the number of participants in zone i.

# a) Fit model M1, estimate ω1. Is there a significant effect of the number
# of plants on the well-being? Compute the PVRE

# bi_1ni - means random intercept 

plants_mood <- read.table("2025_06_12/plants_mood.txt", h=TRUE)

summary(lm(mood ~ zone_id, data = plants_mood))

lme_model <- lme(mood ~ plants + light + social, random =
                ~1|as.factor(zone_id), data = plants_mood)

summary(lme_model)

# betta1 = plants = 0.057

var_b <- get_variance_random(lme_model)
var_eps <- get_variance_residual(lme_model)
var_b
var_eps



PVRE <- var_b/(var_b+var_eps) # we can take them from summary as ^2 of StdDev
PVRE # 0.3337571 - high value (hihg percent of variance eplained by groups)


# b) Fit an extended model M2, allowing heteroscedastic residuals: 
# εij = N (0,ϑ2ij) with
                                                                         
# ϑij= sigma*|socialij|^delta
# for individual j → {1, . . . , ni}, in zone i → zone id.
# Estimate delta for model M2.

lme_model2 <- lme(mood ~ plants + light + social, random =
                    ~1|as.factor(zone_id), 
                  weights = varPower(form = ~social) ,data = plants_mood)

summary(lme_model2)
# Variance function:
# Structure: Power of variance covariate
# Formula: ~social 
# Parameter estimates:
#  power 
# 0.5026533 = delta

# c) Should M2 be preferred over M1? Support your answer with a test.
anova(lme_model, lme_model2) # so model with heteroscedastic residuals better
# due to we rejected H0 of not importance of heteroscedastic

# Model df      AIC      BIC    logLik   Test           L.Ratio p-value
# lme_model      1  6 6642.329 6675.923 -3315.165                        
# lme_model2     2  7 6573.207 6612.399 -3279.604 1 vs 2 71.12235  <.0001
# Report the value of the test statistic: 71.122 - L.Ratio 

# d) Estimate (using M2) the mood of a person having 5 plants,
# 12 hours of natural light exposure and 5 hours of in-person interaction per day.
test_data = data.frame(zone_id= '21', plants=5, light = 12, social=5)
predict(lme_model2, test_data, level = FALSE) 
# level = FALSE - for new group, TRUE - for existing group

var_b2 <- get_variance_random(lme_model2)
var_eps2 <- get_variance_residual(lme_model2)
sqrt(var_eps2)
