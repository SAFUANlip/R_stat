library(car)
library(MVN)


# Additional exercise on mean differences between independent Gaussian: --------
# The Galician Food Association has launched an award for the Best Pulpo a la Gallega (meaning 
# Galician-style octopus). As part of the challenge, two tasters are sent to evaluate the 
# 30 finalist octopus dishes in A Coruna and the 30 finalist octopus dishes in Pontevedra. 
# Files "acoruna.txt" and "pontevedra.txt" collect the evaluations on each of the finalist 
# dishes given by the tasters in A Coruna and Pontevedra, respectively. Assume the evaluations 
# on different dishes to be independent, and the evaluations of the two tasters on the same 
# octopus dish to come from a bivariate Gaussian distribution. (двумерное гауссово распредление)
# a) Perform a statistical test of level 99\% to verify if the mean evaluations in the two 
#    cities differ. State and verify the model assumptions. 
# b) Interpret the results of the test at point (a) through two Bonferroni intervals of
#    global level 99% for appropriate differences in the mean. Comment the result. 
# c) Is there statistical evidence to state that, at level 99%, the average evaluations of
#    A Coruna's octopus dishes are in mean higher than those of Pontevedra's octopus dishes? 
#
#    [by average evaluation of a octopus dishes is meant the one obtained by averaging the 
#    evaluations of the two tasters on that dishes]

pontevedra <- read.table('pontevedra.txt', header = T)
acoruna <- read.table('acoruna.txt', header = T)

# As we see they are really MVN, even if in acoruna T2 not normal by test
mvn(pontevedra)
mvn(acoruna)

qqnorm(acoruna$T2)

pairs(pontevedra, pch=19, main='Dataset pontevedra')
pairs(acoruna, pch=19, main='Dataset acoruna')


# we compute the sample of differences
D <- data.frame(
  T1 = pontevedra$T1 - acoruna$T1,
  T2  = pontevedra$T2 - acoruna$T2
) 
D

# Scatter plot of the dataset of differences with the four quadrants
plot(D, asp=1, pch=19, main='Dataset of Differences')
abline(h=0, v=0, col='grey35')
points(0,0, pch=19, col='grey35')

# Now we can proceed as we already know, but working on D

# T2 Hotelling Test 
# H0: delta == delta.0 vs H1: delta != delta.0
# with delta.0 = c(0,0)

delta.0 = c(0,0)
n <- dim(D)[1]
p <- dim(D)[2]

D.mean <- sapply(D, mean)
D.cov <- cov(D)
D.inv <- solve(D.cov)

# reject at 0.01, don't reject at 0.001
# alpha = 0.05 Есть ≤5% шанс, что мы ошиблись, отклонив H₀
# alpha = 0.01 Более строго: только если есть ≤1% вероятность ошибки
# alpha = 0.001 Есть ≤0.1% шанс, что мы ошиблись, отклонив H₀
alpha <- 0.01

T2 <- n*t(D.mean - delta.0)%*%D.inv%*%(D.mean - delta.0)

quntile_fisher <- (n-1)*p/(n-p)*qf(1-alpha, p, n-p)

T2 < quntile_fisher # so we can reject H0, of equal deltas (so we have different means)

# p-value
1 - pf(T2*(n-p)/((n-1)*p), p, n-p)

# Ellipsoidal confidence region with confidence level 95%
plot(D, asp=1, pch=1, main='Dataset of the Differences', ylim=c(-4,4))
# plottin ellipse, don't forget to divide cov/n
ellipse(center=D.mean, shape=D.cov/n, radius=sqrt(quntile_fisher), lwd=2)

# Adding delta.0 and the quadrants
points(delta.0[1], delta.0[2], pch=16, col='grey35', cex=1.5)
abline(h=delta.0[1], v=delta.0[2], col='grey35')

# Ellipsoidal confidence region with confidence level 99%
ellipse(
  center = D.mean,
  shape = D.cov / n,
  radius = sqrt((n - 1) * p / (n - p) * qf(1 - alpha, p, n - p)),
  lty = 2,
  col = 'grey',
  lwd = 2
)

# Simultaneous T2 intervals in the direction of DBOD and DSS
IC.T2.T1 <-
  c(D.mean[1] - sqrt(quntile_fisher * D.cov[1, 1] / n),
    D.mean[1],
    D.mean[1] + sqrt(quntile_fisher * D.cov[1, 1] / n))

IC.T2.T2  <-
  c(D.mean[2] - sqrt(quntile_fisher * D.cov[2, 2] / n),
    D.mean[2],
    D.mean[2] + sqrt(quntile_fisher * D.cov[2, 2] / n))

T2.sim <- rbind(IC.T2.T1, IC.T2.T2)
dimnames(T2.sim)[[2]] <- c('inf', 'center', 'sup')
T2.sim

# Plot of the simultaneous T2 intervals
plot.simT2 <- function() {
  plot(D, asp=1, pch=1, main='Dataset of the Differences',ylim=c(-4,4))
  ellipse(center=D.mean, shape=D.cov/n, radius=sqrt(quntile_fisher), lwd=2, col='grey')
  abline(v = T2.sim[1,1], col='red', lwd=1, lty=2)
  abline(v = T2.sim[1,3], col='red', lwd=1, lty=2)
  abline(h = T2.sim[2,1], col='red', lwd=1, lty=2)
  abline(h = T2.sim[2,3], col='red', lwd=1, lty=2)
  
  points(delta.0[1], delta.0[2], pch=16, col='grey35', cex=1.5)
  abline(h=delta.0[1], v=delta.0[2], col='grey35')
  
  segments(IC.T2.T1[1], 0, IC.T2.T1[3], 0, lty=1, lwd=2, col='blue')
  segments(0, IC.T2.T2[1], 0, IC.T2.T2[3], lty=1,lwd=2, col='red')
}
plot.simT2()

# T1 interval doesn't contain zero, so we can reject H0 in this direction

# b)
### Bonferroni intervals
k <- p  # Number of Bonferroni intervals
cfr.t <- qt(1-alpha/(2*k), n-1) # Student quantile

# Bonferroni confidence intervals in the direction of DBOD and DSS
IC.BF.T1 <- c(D.mean[1] - cfr.t*sqrt(D.cov[1,1]/n),
                D.mean[1],
                D.mean[1] + cfr.t*sqrt(D.cov[1,1]/n))
IC.BF.T2  <- c(D.mean[2] - cfr.t*sqrt(D.cov[2,2]/n),
                D.mean[2],
                D.mean[2] + cfr.t*sqrt(D.cov[2,2]/n))
# Bonferroni region defined by the cartesian product of the Bf intervals
Bf <- rbind(IC.BF.T1, IC.BF.T2)
dimnames(Bf)[[2]] <- c('inf','center','sup')
Bf

# by Bonferroni correction we see, that we can reject H0 by T1 direction on level 0.01
# (So in this direction we see significnat differnece in data means)
# Bonferroni intervals in the direction of T2 and T1
abline(v = Bf[1,1], col='green', lwd=1, lty=2)
abline(v = Bf[1,3], col='green', lwd=1, lty=2)
abline(h = Bf[2,1], col='green', lwd=1, lty=2)
abline(h = Bf[2,3], col='green', lwd=1, lty=2)
segments(IC.BF.T1[1], 0, IC.BF.T1[3], 0, lty=1, lwd=2, col='green')
segments(0, IC.BF.T2[1], 0, IC.BF.T2[3], lty=1, lwd=2, col='green')

# c) as we need test mean (of both features as one) we will compare one-dimension variables
# so it will be t-test (instead of T^2)

# so the average price in acoruna greater than average price in pontevedra
t.test((acoruna$T1 + acoruna$T2)/2, (pontevedra$T1 + pontevedra$T2)/2, alternative = "greater")


# Pb 1 of 10/02/10 -------------------------------------------------------------
##### Pb 1 of 10/02/10
# The file exchange.txt collects the daily exchange rates dollar/euro and
# pound/euro of Jan 2010. Assume that the 30 daily increments are independent
# realization of a bivariate Gaussian distribution.
# a) Is there statistical evidence to state that during January the exchange 
#    rate has changed in mean?
# b) Using the Bonferroni inequality, provide four confidence intervals of
#    global confidence 90% for the mean of the increments and for their 
#    variances.

exchange <- read.table("exchange.txt", header=TRUE)
dim(exchange)


incremetC <- matrix(0, 30, 2, byrow=T)

for (i in 1:30){
  incremetC[i,1] = exchange[i+1,1] - exchange[i,1]
  incremetC[i,2] = exchange[i+1,2] - exchange[i,2]
}

exchange_increment <- data.frame(
  dollar = incremetC[,1],
  pound  = incremetC[,2]
) 

mvn(exchange_increment) # doesn't seem MVN

n <- dim(exchange)[1]
p <- dim(exchange)[2]
alpha <- 0.05

M <- sapply(exchange_increment, mean) # sample mean
M
S <- cov(exchange_increment) # covariance matrix
S

S.inv <- solve(S)

delta.0 = c(0,0)
T2 <- n*t(M-delta.0)%*%S.inv%*%(M-delta.0)

quntile_fisher <- (n-1)*p/(n-p)*qf(1-alpha, p, n-p) 

T2 < quntile_fisher # so we can reject H0, of equal deltas (so we have different means)

# p-value (So we can reject H0 of equal inrements)
1 - pf(T2*(n-p)/((n-1)*p), p, n-p)

matplot(t(exchange_increment), type='l', lty = 1)

# b) Bonferroni CI

### Bonferroni intervals
k <- p  # Number of Bonferroni intervals
cfr.t <- qt(1-alpha/(2*k), n-1) # Student quantile

# Bonferroni confidence intervals in the direction of DBOD and DSS
IC.BF.dollar <- c(M[1] - cfr.t*sqrt(S[1,1]/n),
              M[1],
              M[1] + cfr.t*sqrt(S[1,1]/n))
IC.BF.pound  <- c(M[2] - cfr.t*sqrt(S[2,2]/n),
               M[2],
               M[2] + cfr.t*sqrt(S[2,2]/n))
# Bonferroni region defined by the cartesian product of the Bf intervals
Bf <- rbind(IC.BF.dollar, IC.BF.pound)
dimnames(Bf)[[2]] <- c('inf','center','sup')
Bf

# Bonferroni CI contain 0, so we can not reject H0

# For variance:
IC.var <- matrix(NA, nrow=p, ncol=3)
rownames(IC.var) <- colnames(exchange_increment)
colnames(IC.var) <- c("inf", "center", "sup")

for (j in 1:p) {
  var_j <- S[j,j]
  IC.var[j, ] <- c(
    (n - 1) * var_j / qchisq(1 - alpha / (2 * k), df = n - 1),
    var_j,
    (n - 1) * var_j / qchisq(alpha / (2 * k), df = n - 1)
  )
}
IC.var

# ------------------------------------------------------------------------------
##### Pb 3 of 10/02/10
# An association of Milanese technologists collected in the file mobile.txt
# the prices of iphone and Nokia 5800 recorded for 100 shops in the province
# of Milan
# a) Build three simultaneous T2 intervals with global confidence 90%
#    for the mean of the two prices and for their difference
# b) Nokia states that - at a worldwide level - Nokia 5800 costs in mean
#    one third of the price for an iphone. Based on the data, can we
#    deny this statement for the province of Milan?

mobile <- read.table("mobile.txt", header = TRUE)
matplot(mobile)

alpha <- 0.1

shapiro.test(mobile$Nokia5800)
shapiro.test(mobile$iphone)

n <- dim(mobile)[1]
p <- dim(mobile)[2]

M <- sapply(mobile, mean) # sample mean
M
S <- cov(mobile) # covariance matrix
S
S.inv <- solve(S)

quntile_fisher <- (n-1)*p/(n-p)*qf(1-alpha, p, n-p)

# p-value (So we can reject H0 of equal inrements)
1 - pf(T2*(n-p)/((n-1)*p), p, n-p)

IC.Nokia <-
  c(M[1] - sqrt(quntile_fisher * S[1, 1] / n),
    M[1],
    M[1] + sqrt(quntile_fisher * S[1, 1] / n))

IC.iphone  <-
  c(M[2] - sqrt(quntile_fisher * S[2, 2] / n),
    M[2],
    M[2] + sqrt(quntile_fisher * S[2, 2] / n))

a <- c(-1, 1)
IC.diff  <-
  c(M %*% a - sqrt(quntile_fisher * t(a) %*% S %*% a / n),
    M %*% a,
    M %*% a + sqrt(quntile_fisher * t(a) %*% S %*% a / n))

T2.sim <- rbind(IC.Nokia, IC.iphone, IC.diff)
dimnames(T2.sim)[[2]] <- c('inf', 'center', 'sup')

T2.sim

# b)

mobile_x3 <- data.frame(
  Nokia5800_3 = mobile$Nokia5800*3,
  iphone = mobile$iphone
)

t.test(mobile_x3$Nokia5800_3, mobile_x3$iphone)

# Pb 3 of 24/09/10 -------------------------------------------------------------
##### Pb 3 of 24/09/10
# The PoliPharma has given a drug to 10 students in order to increase the levels
# of Matina and decrease the level of Fisina in their blood. In the files before.txt
# and after.txt the levels of Matina, Fisina, Chimina and Elettrina are reported
# for 10 students, before and after the administration of the drug.
# a) Having introduced and tested the appropriate hypotheses of Gaussianity,
#    perform a test of level 1% to prove the existence of an effect of the 
#    drug on the mean levels of the four enzymes.
# b) Provide four simultaneous T2 intervals for the mean of the four
#    increments
# c) Perform a test of level 1% to confirm/deny what stated by the Polipharma,
#    that is: the ingestion of the drug causes a mean increment of 2 units of
#    the Matina, a decrease of 1 unit in the Fisina and a mean increment in the
#    Chimina equal to the mean decrease in the Elettrina

before <- read.table("before.txt")
after <- read.table("after.txt")

matplot(before, type='l')
matplot(after, type='l')

alpha <- 0.01

diff_df <- after - before
matplot(diff_df, type='l')
M

delta <- c(0,0,0,0)

mvn(diff_df) # each of variable Normal

n <- dim(diff_df)[1]
p <- dim(diff_df)[2]

quantile_fisher <- (n-1)*p/(n-p)*qf(1-alpha, p, n-p)

M <- sapply(diff_df, mean)
S <- cov(diff_df)
S.inv <- solve(S)

T2 <- n*t(M-delta)%*%S.inv%*%(M-delta)
T2 < quantile_fisher

T2
quantile_fisher


p_value <- 1 - pf(T2*(n-p)/((n-1)*p), p, n-p)
p_value # can not reject H0 on level 0.01 then we can not say, 
# that there are any difference before and after

p_value

# b)

IC.matina <-
  c(M[1] - sqrt(quantile_fisher * S[1, 1] / n),
    M[1],
    M[1] + sqrt(quantile_fisher * S[1, 1] / n))
IC.matina

IC.fisina <-
  c(M[2] - sqrt(quantile_fisher * S[2, 2] / n),
    M[2],
    M[2] + sqrt(quantile_fisher * S[2, 2] / n))
IC.fisina

IC.chimina <-
  c(M[3] - sqrt(quantile_fisher * S[3, 3] / n),
    M[3],
    M[3] + sqrt(quantile_fisher * S[3, 3] / n))
IC.chimina

IC.elettrina <-
  c(M[4] - sqrt(quantile_fisher * S[4, 4] / n),
    M[4],
    M[4] + sqrt(quantile_fisher * S[4, 4] / n))
IC.elettrina

T2.sim <- rbind(IC.matina, IC.fisina, IC.chimina, IC.elettrina)
dimnames(T2.sim)[[2]] <- c('inf', 'center', 'sup')

T2.sim

plot(1:nrow(T2.sim), T2.sim[, "center"], ylim = range(T2.sim),
     xaxt = "n", xlab = "Variables", ylab = "Value",
     main = "Simultaneous Confidence Intervals",
     pch = 16, col = "blue")

# Добавляем интервалы
for (i in 1:nrow(T2.sim)) {
  segments(i, T2.sim[i, "inf"], i, T2.sim[i, "sup"], lwd = 2)
}

# Ось X с названиями переменных
axis(1, at = 1:nrow(T2.sim), labels = rownames(T2.sim))

# c) 
# H0: delta1 = 2, delta2 = -1, delta3 = - delta4
# i.e.,
# H0: delta1 = 2, delta2 = -1, delta3 + delta4 = 0
p.c <- 3 # 3 delta => 3 features

diff_df
delta.c <- c(2, -1, 0)
C <- matrix(
  c(1, 0, 0, 0,
    0, 1, 0, 0,
    0, 0, 1, 1),
      3, 4, byrow=T)

S.c <- (C%*%S%*%t(C))
S.inv.c <- solve(C%*%S%*%t(C))
S.inv.c
M.c <- C%*%M
M.c

quantile_fisher.c <- (n-1)*p.c/(n-p.c)*qf(1-alpha, p.c, n-p.c)
T2.c <- n*t(M.c-delta.c)%*%(S.inv.c)%*%(M.c-delta.c)
T2.c < quantile_fisher.c

p_value.c <- 1-pf(T2.c*(n-p.c)/((n-1)*p.c), p.c, n-p.c)
p_value.c

quantile_fisher.c

cbind( "Inf"= M.c-sqrt(quantile_fisher.c*diag(S.c)/n) , M.c, Sup=M.c+sqrt(quantile_fisher.c*diag(S.c)/n) )
