## Problem 1 -------------------------------------------------------------------
# The file Pb4.txt reports the number Y (expressed in thousands of units)
# of vehicles registered annually in three countries of the European Union
# (France, Germany and Italy) during a reference period of 10 years.
# Recent economic models describe the behavior of this variable according
# to the model:
# Y | (X = x, G = g) = beta0.g + beta1.g * x^2 + eps
# with eps ~ N (0, sigma^2), x = 1, 2, ... , 10 (year) and
# g = France, Germany, Italy (EU country).
# (a) With the least squares method, estimate the 7 parameters of the model.
# (b) Using appropriate statistical tests, state if you deem necessary to
#     include into the model:
#     1. the variable x^2;
#     2. the variable G;
#     3. the effect of the variable G on the coefficient that multiplies
#        the regressor x^2;
#     4. the effect of the variable G on the intercept.
# (c) Once identified the "best model", build three prediction intervals
#     for the number of vehicles registered in the three countries 
#     during the eleventh year, so that the three new observations
#     will fall simultaneously within the respective ranges with 95%
#     of probability.

pb4  <- read.table('Pb4.txt')
pb4

# looks like we have to use dummy variables to indicate each group (dummy - binary, so we need two dummy var)

x <- c(1:10)
pb4 <- cbind(pb4, x)

pb4new <- data.frame(
  y = c(pb4$Francia, pb4$Germania, pb4$Italia),
  x = rep(c(1:10), 3),
  dummy_France = rep(c(1, 0, 0), c(10,10,10)),
  dummy_Germany = rep(c(0, 1, 0), c(10,10,10))
)

pb4new_full <- data.frame(
  y = c(pb4$Francia, pb4$Germania, pb4$Italia),
  x = rep(c(1:10), 3),
  dummy_France = rep(c(1, 0, 0), c(10,10,10)),
  dummy_Germany = rep(c(0, 1, 0), c(10,10,10)),
  dummy_Italy = rep(c(0, 0, 1), c(10,10,10))
)

fit_pb4 <- lm(y ~ dummy_France + dummy_Germany + I(x^2) + I(x^2):dummy_France + I(x^2):dummy_Germany, data=pb4new)
summary(fit_pb4)

fit_pb4_full <- lm(y ~ -1 + dummy_France + dummy_Germany + dummy_Italy+ I(x^2):dummy_France + I(x^2):dummy_Germany + I(x^2):dummy_Italy, data=pb4new_full)
summary(fit_pb4_full)

# b) 
# 1. the variable x^2; H0 - all bettai with x^2 = 0, H1 - at least one bettai of x^2 != 0
AQ <- rbind(c(0,0,0,1,0,0), c(0,0,0,0,1,0), c(0,0,0,0,0,1))
bQ <- c(0,0,0)
linearHypothesis(fit_pb4, AQ, bQ) # so we can not say, that qadratic terms = 0
# there is statistic evidence to state that coefficients of x^2 varaibels is different from 0

# 2. the variable G; 
AG <- rbind(c(0,1,0,0,0,0),
            c(0,0,1,0,0,1),
            c(0,0,0,0,1,0),
            c(0,0,0,0,0,1))
bG <- c(0,0,0,0)
linearHypothesis(fit_pb4, AG, bG)

AG_full <- rbind(
  c(1, -1,  0,  0,  0,  0),  # beta0_France - beta0_Germany = 0
  c(1,  0, -1,  0,  0,  0),  # beta0_France - beta0_Italy    = 0
  c(0,  0,  0,  1, -1,  0),  # beta1_France - beta1_Germany = 0
  c(0,  0,  0,  1,  0, -1)   # beta1_France - beta1_Italy    = 0
  # we should not put this two, because they linearry dependent from previous, so we already check assumtions
  # c(0,  1,  -1,  0,  0, 0),   # beta0_Germany - beta0_Italy    = 0
  # c(0,  0,  0,  0,  1, -1)   # beta1_Germany - beta1_Italy    = 0
)
bG_full <- c(0, 0, 0, 0)
linearHypothesis(fit_pb4_full, AG_full, bG_full) # т.е мы проверили что все пары коэффициентов одинаковы

# 3. the effect of the variable G on the coefficient that multiplies
#        the regressor x^2;

AGQ <- rbind(
            c(0,0,0,0,1,0),
            c(0,0,0,0,0,1))
bGQ <- c(0,0)
linearHypothesis(fit_pb4, AGQ, bGQ)

AGQ_full <- rbind(
  c(0,0,0,1,-1,0),
  c(0,0,0,1,0,-1))
bGQ_full <- c(0,0)
linearHypothesis(fit_pb4_full, AGQ_full, bGQ_full)

# 4. the effect of the variable G on the intercept.

AGI <- rbind(
  c(0,1,0,0,0,0),
  c(0,0,1,0,0,0))
bGI <- c(0,0)
linearHypothesis(fit_pb4, AGI, bGI)

AGI_full <- rbind(
  c(1,-1,0,0,0,0),
  c(1,0,-1,0,0,0))
bGI_full <- c(0,0)
linearHypothesis(fit_pb4_full, AGI_full, bGI_full) # so, there are no effect of group on intercept

# c) so, if no effect of group on intercept => 
fit_pb4_best <- lm(y ~ I(x^2) + I(x^2):dummy_France + I(x^2):dummy_Germany, data=pb4new)
summary(fit_pb4_best)

new_data <- data.frame(x = c(11,11,11), dummy_France=c(1,0,0), dummy_Germany=c(0,1,0)) # so it automatically make x^2
predict(fit_pb4_best, new_data, interval="prediction")

fit_pb4_best_full <- lm(y ~ -1 + I(x^2):dummy_France + I(x^2):dummy_Germany + I(x^2):dummy_Italy, data=pb4new_full)
summary(fit_pb4_best_full)

new_data_full <- data.frame(x = c(11,11,11), dummy_France=c(1,0,0), dummy_Germany=c(0,1,0), dummy_Italy=c(0,0,1)) # so it automatically make x^2
predict(fit_pb4_best, new_data_full, interval="prediction")


## Problem 3 -----------------------------------------------------------------------------------
# The file people.txt records the tons of waste collected monthly
# in the city of Santander since January 2009 (t = 1) until May 2011
# (t = 29). Assuming a model of the type:
#   Waste = A + B * t  + C * (1-cos(2pi / 12 * t)) + eps
# with eps ~ N(0, sigma^2) and identifying the contribution of the residents
# with the first two factors, and that of the tourists with the third
# addendum, answer the following questions.
# a) Estimate the parameters of the model.
# b) On the basis of the model (a), is there statistical evidence of an
#    increase attributable to residents?
# c) On the basis of the model (a), is there statistical evidence of a
#    significant contribution by tourists?
# d) The University of Cantabria considered that the GROWTH attributable to
#    residents is quantifiable in an increase of 10 tons per month.
#    Can you deny this statement?
# e) Based on the test (b), (c) and (d) propose a possible reduced and/or 
#    constrained model and estimate its parameters.
# f) On the basis of model (e), provide three pointwise forecasts for the
#    waste that will be collected in June 2011, for the waste that will be
#    collected in June 2011 due to residents and that which will be collected
#    in June 2011 due to the tourists.

people <- read.table("people.txt", h=TRUE)

plot(people)

fit_people <- lm(rifiuti ~ mese + I(1-cos(2*pi/12*mese)), data=people)
summary(fit_people)

# a)
fit_people$coefficients

t <- c(1:30)
pred <- c(fit_people$coefficients[1] + t + (1-cos(2*pi/12*t)))
plot(pred)

# b)
par(mfrow=c(2,2))
plot(fit_people) # I don't see, that residuals increase among time

# but question was about residents (not residuals)
# lets check that betta1 != 0
A_residents <- rbind(c(0,1,0))
b_residents <- c(0)
linearHypothesis(fit_people, A_residents, b_residents)
summary(fit_people)
# coefficient of betta1 (time) positive and we have enough statistical evidence
# that it != 0, there for we can say that number of residents increases

# c) effect of tourists maybe noticet if we look on preiodic part of our model
A_tourists <- rbind(c(0,0,1))
b_tourists <- c(0)
linearHypothesis(fit_people, A_tourists, b_tourists)
# So we see statistical evidence of a significant contribution by tourists

# d) increase by 10 tons each month => betta1 = 10?
A_10 <- rbind(c(0,1,0)) # H0: betta1 = 10, H1: betta1 != 10
b_10 <- c(10)
linearHypothesis(fit_people, A_10, b_10) # so we can not reject H0, and 
# increasing goes by 10

# e) 

fit_people_e <- lm(rifiuti ~ I(10*mese) + I(1-cos(2*pi/12*mese)), data=people)
summary(fit_people_e)

# f) On the basis of model (e), provide three pointwise forecasts for the
#    waste that will be collected in June 2011, for the waste that will be
#    collected in June 2011 due to residents and that which will be collected
#    in June 2011 due to the tourists.

# by default will be collected 
fit_people_e$coefficients[1]

# by residents
fit_people_e$coefficients[2] * 30*10

# by tourists
fit_people_e$coefficients[3] * (1-cos(2*pi/12*30))


# correct (by GPT, seminar solution differnet) point e)
people$t_residents <- 10 * people$mese

fit_people_e <- lm(rifiuti ~ t_residents + I(1 - cos(2 * pi / 12 * mese)), data = people)
summary(fit_people_e)

# correct (by GPT, seminar solution differnet) point f)
t_pred <- 30
A_hat <- fit_people_e$coefficients[1]
C_hat <- fit_people_e$coefficients[3]

seasonal_component <- 1 - cos(2 * pi / 12 * t_pred)

# f1. Total waste forecast
Y_total <- A_hat + 10 * t_pred + C_hat * seasonal_component

# f2. Waste from residents
Y_residents <- 10 * t_pred

# f3. Waste from tourists
Y_tourists <- C_hat * seasonal_component

# Print all
cat("Total waste (June 2011):", Y_total, "\n")
cat("Residents' contribution:", Y_residents, "\n")
cat("Tourists' contribution:", Y_tourists, "\n")

#_______________________________________________________________________________
##### Problem 4 of 4/7/2007
#####-------------------------
# At the Tenaris steel mills, the relationship between length [m] and
# Temperature [?C] of some steel bars that will be sold to Pirelli
# is under study (the data are contained in tenaris.txt file). The relation
# is hypothesized of the kind:
#   L = L0 + C* T + D  * T ^ 2 + eps
# with L the length of the bar, T the temperature of the bar, L0 the length 
# of the bar at 0 ?C, C the coefficient of of linear thermal expansion, D
# the coefficient of quadratic thermal expansion and eps a measurement error
# of zero mean.
# Answer the following questions using appropriate statistical arguments:
# a) Estimate the parameters L0, C, D and the variance of error eps.
# b) Based on the analysis of residuals, do you think that there are the
#    conditions to make inference on the coefficients based on a Gaussian
#    model? (In case of Yes proceed to step (c); in case of negative answer
#    identify the problem, remove it and return to point (a))
# c) Do you think that the model explains the possible dependence between 
#    the temperature T and the length L?
# d) do you deem plausible to consider that the length of the bars at 0 ?C
#    is equal to 2?
# E) do you think that you can eliminate from the model the quadratic term?




#_______________________________________________________________________________
##### Problem 4 of 09/09/2009
#####--------------------------------------
# A zoologist is studying the temporal evolution of the height of a new breed
# of goat (dataset goat.txt). Using an exponential growth model for the i-th 
# individual of the following type: 
# h_i = A + B (1 - exp(-t_i)) + C eps_i, 
# with: 
# h_i = the height of the individual [cm],
# t_i = the age of the individual [years], 
# eps_i = a random term distributed according to a standard normal distribution, 
# A and B = parameters exclusively dependent on the gender of the individual, 
# C = a parameter  equal for the whole population.
# a) Estimate with the least squares method the 5 parameters of the model.
# b) On the basis of model (a), is there statistical evidence that the mean 
#    height at birth (t=0) is different between males and females?
# c) On the basis of model (a), is there statistical evidence that the mean 
#    height at adulthood (t=+inf) is different between males and females?
# d) On the basis of tests (b) and (c), implement an appropriate reduced model and 
#    estimate its parameters.
# e) On the basis of the reduced model (d), provide confidence intervals of global 
#    confidence 90% for the mean height at birth and at adulthood of males and females.


goat <- read.table('goat.txt', h=TRUE)

# я не понял как можно было догодаться до этой модели (может по числу параметров...)
fit <- lm(height ~ gender + I(1 - exp(-age)) + I(1 - exp(-age)):gender, data=goat)
summary(fit)

# a)
fit$coefficients

# b) when t = 0, we have height ~ gender + eps, so we have to check if there are. significance
# of this coefficent
A0 <- rbind(
  c(0,1,0,0)
  )
b0 <- c(0)
linearHypothesis(fit, A0, b0) # so we can not reject H0 of betta - gender  = 0, that means it unsignifficant feature

# c) I(1 - exp(-age)) - goes to one, I(1 - exp(-age)):gender - will depend on gender, so we have to check this and gender coefficeinte
Ainf <- rbind(
  c(0,1,0,1)
)
binf <- c(0)
linearHypothesis(fit, Ainf, binf) #


# d) impement reduced model (we can ignore gender, due to it don't reject H0 of zero coefficient)
fit_reduced <- lm(height ~I(1 - exp(-age)) + I(1 - exp(-age)):gender, data=goat)
summary(fit_reduced)

# e)
alpha <- 0.1
k <- 4

new_obs_young_male <- data.frame(
  age = 0,
  gender="male"
)

new_obs_young_female <- data.frame(
  age = 0,
  gender="female"
)

new_obs_adult_male <- data.frame(
  age = 10,
  gender="male"
)

new_obs_adult_female <- data.frame(
  age = 10,
  gender="female"
)

predict(fit, new_obs_young_male,  interval="confidence", level=1-alpha/k)
predict(fit, new_obs_young_female,  interval="confidence", level=1-alpha/k)
predict(fit, new_obs_adult_male,  interval="confidence", level=1-alpha/k)
predict(fit, new_obs_adult_female,  interval="confidence", level=1-alpha/k)

# solution of point e) (I changed C matrix)
C <- rbind(
  c(1, 0, 0),  # t=0, young female     
  c(1, 1, 0),  # t=0, adult female   
  c(1, 0, 1),  # t=inf, young male
  c(1, 1, 1)   # t=inf, adult male
)

mu_hat <- C %*% coef(fit_reduced)
se <- sqrt(diag(C %*% vcov(fit_reduced) %*% t(C)))

qt_val <- qt(1 - 0.10 / (2*4), df = n - length(fit_reduced$coefficients))  # делим на 2 для двустороннего

CI <- cbind(mu_hat - qt_val * se,
            mu_hat + qt_val * se)

CI # last column = 0 means female => so we make CI for females, zero in second columns means young,
# because when age = 0, we get 0 coefficient
# .......... SMTH .........
Bf <- rbind(
  c((C %*% coefficients(fit.red))[1] - sqrt((C %*% vcov(fit.red) %*% t(C))[1,1]) * qt(1 - 0.10/6, n-3),
    (C %*% coefficients(fit.red))[1] + sqrt((C %*% vcov(fit.red) %*% t(C))[1,1]) * qt(1 - 0.10/6, n-3)),
  c((C %*% coefficients(fit.red))[2] - sqrt((C %*% vcov(fit.red) %*% t(C))[2,2]) * qt(1 - 0.10/6, n-3),
    (C %*% coefficients(fit.red))[2] + sqrt((C %*% vcov(fit.red) %*% t(C))[2,2]) * qt(1 - 0.10/6, n-3)),
  c((C %*% coefficients(fit.red))[3] - sqrt((C %*% vcov(fit.red) %*% t(C))[3,3]) * qt(1 - 0.10/6, n-3),
    (C %*% coefficients(fit.red))[3] + sqrt((C %*% vcov(fit.red) %*% t(C))[3,3]) * qt(1 - 0.10/6, n-3))
)
Bf

