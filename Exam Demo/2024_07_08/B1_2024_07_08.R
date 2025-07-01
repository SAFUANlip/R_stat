library(MVN)
library(car)
library(heplots)

# We want to investigate how di↵erent exercise routines and meditation a↵ect various mental health indicators. The
# dataset wellbeing.txt contains measurements of two mental health indicators:
#   • stress which corresponds to the perceived stress level (on a scale from 0 to 100), that should be kept low;
#  • happiness which corresponds to the happiness index (on a scale from 0 to 100), that should be kept high;
# of 200 randomly selected individuals, along with the indication of following a specific exercise routine or not
# (exercise) and practicing meditation or not (meditation).


wellbeing <- read.table("2024_07_08/wellbeing.txt", h=TRUE)

# as we have to factor meditation and exercises - need to ues two-way MANOVA

meditation   <- factor(wellbeing$meditation) # Treat.1
exercise   <- factor(wellbeing$exercise) # Treat.2

med_exer <- factor(paste(meditation, exercise, sep=''))
med_exer

wellbeing_data <- wellbeing[, 1:2]

# a) Do exercise routines and meditation have a significant e↵ect on the mental health indicators? Support your
# answer with a MANOVA model.

fit <- manova(as.matrix(wellbeing_data) ~ meditation + exercise + meditation:exercise)
summary(fit)

#                       Df   Pillai approx F num Df den Df    Pr(>F)    
#  meditation            1 0.252087   32.863      2    195 5.017e-13 ***
#  exercise              1 0.168289   19.728      2    195 1.575e-08 ***
#  meditation:exercise   1 0.003558    0.348      2    195    0.7065    
#  Residuals           196                                           

# as we see, we oave effect of each factor, but their intercation does not
# because p-value > 0.05 and we could not reject H0 of zero gamma_ij

# b) Specify and verify the assumptions of the model introduced in a).
# Each group should be MVN and with homogenity of covariance matricies

Ps <- c(mvn(wellbeing_data[med_exer==levels(med_exer)[1],],)$multivariateNormality$`p value`,
        mvn(wellbeing_data[med_exer==levels(med_exer)[2],],)$multivariateNormality$`p value`,
        mvn(wellbeing_data[med_exer==levels(med_exer)[3],],)$multivariateNormality$`p value`,
        mvn(wellbeing_data[med_exer==levels(med_exer)[4],],)$multivariateNormality$`p value`)
Ps # all values > 0.05 (So we can not reject H0 of normlaity assumptions)

S1 <-  cov(wellbeing_data[med_exer==levels(med_exer)[1],])
S2 <-  cov(wellbeing_data[med_exer==levels(med_exer)[2],])
S3 <-  cov(wellbeing_data[med_exer==levels(med_exer)[3],])
S4 <-  cov(wellbeing_data[med_exer==levels(med_exer)[4],])

par(mfrow=c(1,4))
image(S1, col=heat.colors(100),main='Cov. S1', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3,S4), (0:100)/100, na.rm=TRUE))
image(S2, col=heat.colors(100),main='Cov. S2', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3,S4), (0:100)/100, na.rm=TRUE))
image(S3, col=heat.colors(100),main='Cov. S3', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3,S4), (0:100)/100, na.rm=TRUE))
image(S4, col=heat.colors(100),main='Cov. S4', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3,S4), (0:100)/100, na.rm=TRUE))


summary(boxM(wellbeing_data, med_exer))
# p-value > 0.05, we can not reject H0 of equal covariance matricies

# c) After fitting the model in (a), would you now propose a di↵erent one? Give reasons for your choice.
# I would propose simpler model, without interaction effect, due to it does not have effect

fit2 <- manova(as.matrix(wellbeing_data) ~ meditation + exercise)
summary(fit2)
#               Df  Pillai approx F num Df den Df    Pr(>F)    
#  meditation   1 0.25175   32.973      2    196 4.534e-13 ***
#  exercise     1 0.16781   19.762      2    196 1.519e-08 ***
#  Residuals  197                                             
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# d) Provide Bonferroni intervals (global level 95%) for the effects of exercise routines and meditation on the mental
# health indicators. How would you describe the effect of exercise routines and meditation on the mental health
# indicators?
sum(med_exer == "FALSETRUE")
sum(med_exer == "FALSEFALSE")
sum(med_exer == "TRUEFALSE")
sum(med_exer == "TRUETRUE")


alpha <- 0.05
g <- 2 # number of levels of first factor
b <- 2 # number of levels of second factor
p <- 2 # num of features which means we compare 
n <- 50
N <- n*g*b # 200

W <- summary.manova(fit2)$SS$Residuals

# how many comparisons?
k <- p*g*(g-1)/2 + p*b*(b-1)/2
# because we have: g levels on the first factor on p components
#                  b levels on the second factor on p components
k

qT <- qt(1 - alpha/(2*k), g*b*n-g-b+1) # g*b*n-g-b+1 = 197 degree of freedom from manova
# the degrees of freedom of the residuals on the additive model are
# g*b*n-g-b+1
sum(meditation=='TRUE') # 100, 
sum(meditation=='FALSE') # 100
# so divide by 1/100 + 1/100

m_med_true  <- sapply(wellbeing_data[meditation=='TRUE',], mean)
m_med_false  <- sapply(wellbeing_data[meditation=='FALSE',], mean)
inf_med <- m_med_true-m_med_false - qT * sqrt(diag(W)/(g*b*n-g-b+1) * (1/100+1/100))
sup_med <- m_med_true-m_med_false + qT * sqrt(diag(W)/(g*b*n-g-b+1) * (1/100+1/100))

inf_med
sup_med


sum(exercise=='TRUE')
sum(exercise=='FALSE') 

m_exe_true  <- sapply(wellbeing_data[exercise=='TRUE',],mean)
m_exe_false  <- sapply(wellbeing_data[exercise=='FALSE',],mean)
inf_exe <- m_exe_true-m_exe_false - qT * sqrt(diag(W)/(g*b*n-g-b+1) * (1/100+1/100))
sup_exe <- m_exe_true-m_exe_false + qT * sqrt(diag(W)/(g*b*n-g-b+1) * (1/100+1/100))

inf_exe
sup_exe

IC2   <- list(med_true_false=cbind(inf_med, sup_med), exe_true_false=cbind(inf_exe, sup_exe))
IC2

# $med_true_false
#             inf_med   sup_med
# stress    -21.402099 -9.129901 # mediation have negative effect on stree, so when you meditate - strees decreases (that good)
# happiness  -3.619189  9.139189 # but it doesn have effect on happiness (CI contain 0, and we can not reject H0 that groups have equal means)

# $exe_true_false
#             inf_exe   sup_exe
# stress    -4.350099  7.922099 # exercise does not have effect on stress
# happiness  8.048811 20.807189 # But have effect on happines, when you do exercise - you become happy
