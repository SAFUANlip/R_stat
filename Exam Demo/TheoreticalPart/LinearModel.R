library(MASS)
library(car)
library(rgl)

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
