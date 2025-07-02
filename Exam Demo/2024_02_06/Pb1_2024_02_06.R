library(MVN)
library(car)
library(heplots)

# We want to investigate how vegetarian diet and vitamin intake a↵ect various health indicators. The dataset
# diet.txt contains measurements of two health indicators:
#  • pressure which corresponds to the systolic blood pressure (in mmHg), that should be kept low;
#  • cholesterol which corresponds to the Low-Density Lipoprotein (LDL) cholesterol concentration (in mg/dL),
# that should also be kept low,
# of 200 randomly selected individuals, along with the indication of a vegetarian diet or not (vegetarian) and a
# vitamin intake or not (vitamin).

diet <- read.table("2024_02_06/diet.txt", h=TRUE)

sum(diet["vegetarian"] == FALSE)
sum(diet["vegetarian"] == TRUE)
sum(diet["vitamin"] == FALSE)
sum(diet["vitamin"] == TRUE)

vitamin   <- factor(diet$vitamin) # Treat.1
veg   <- factor(diet$vegetarian) # Treat.2

veg_vit <- factor(paste(veg, vitamin, sep=''))
veg_vit

diet_data <- diet[, 1:2]

# a) Do dietary habits have a significant e↵ect on the health indicators? Support your answer with a MANOVA
# model.
fit <- manova(as.matrix(diet_data) ~ vitamin + veg + vitamin:veg)
summary(fit)

# Df   Pillai approx F num Df den Df    Pr(>F)    
#   vitamin       1 0.304601   42.707      2    195 4.148e-16 ***
#   veg           1 0.076833    8.115      2    195 0.0004119 ***
#   vitamin:veg   1 0.004914    0.482      2    195 0.6185741    
# Residuals   196                                              
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# as we see doetary habbits have effect on th health idicators
# Due to p-value of vegetarian and vitamin < 0.05 and we can reject H0, that they don't have effect

# b) Specify and verify the assumptions of the model introduced in a).
# Assumptions: each group is MVN, and have same covariance structure (Homogenity)

Ps <- c(mvn(diet_data[veg_vit==levels(veg_vit)[1],],)$multivariateNormality$`p value`,
        mvn(diet_data[veg_vit==levels(veg_vit)[2],],)$multivariateNormality$`p value`,
        mvn(diet_data[veg_vit==levels(veg_vit)[3],],)$multivariateNormality$`p value`,
        mvn(diet_data[veg_vit==levels(veg_vit)[4],],)$multivariateNormality$`p value`)
Ps # all values > 0.05 (So we can not reject H0 of normlaity assumptions)

S1 <-  cov(diet_data[veg_vit==levels(veg_vit)[1],])
S2 <-  cov(diet_data[veg_vit==levels(veg_vit)[2],])
S3 <-  cov(diet_data[veg_vit==levels(veg_vit)[3],])
S4 <-  cov(diet_data[veg_vit==levels(veg_vit)[4],])

par(mfrow=c(1,4))
image(S1, col=heat.colors(100),main='Cov. S1', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3,S4), (0:100)/100, na.rm=TRUE))
image(S2, col=heat.colors(100),main='Cov. S2', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3,S4), (0:100)/100, na.rm=TRUE))
image(S3, col=heat.colors(100),main='Cov. S3', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3,S4), (0:100)/100, na.rm=TRUE))
image(S4, col=heat.colors(100),main='Cov. S4', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3,S4), (0:100)/100, na.rm=TRUE))


summary(boxM(diet_data, veg_vit))
# p-value > 0.05, we can not reject H0 of equal covariance matricies

# c) After fitting the model in (a), would you now propose a di↵erent one? Give reasons for your choice.
# as we seen from summary of model, there are no intercation effectof vegetarien and vitamine diets
# so we can simplfy model
fit2 <- manova(as.matrix(diet_data) ~ vitamin + veg)
summary(fit)

# d) Provide Bonferroni intervals (global level 95%) for the e↵ects of the vegetarian diet and the vitamin intake on
# the health indicators. How would you describe the e↵ect of the vegetarian diet and vitamin intake on the health
# indicators?

alpha <- 0.05
g <- 2 # number of levels of first factor
b <- 2 # number of levels of second factor
p <- 2 # num of features which means we compare 
n <- 100 # each group has 100 examples 
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
sum(veg == TRUE) # 100, 
sum(veg == FALSE) # 100
# so divide by 1/100 + 1/100

m_veg_true  <- sapply(diet_data[veg=='TRUE',], mean)
m_veg_false  <- sapply(diet_data[veg=='FALSE',], mean)
inf_veg <- m_veg_true-m_veg_false - qT * sqrt(diag(W)/(g*b*n-g-b+1) * (1/100+1/100))
sup_veg <- m_veg_true-m_veg_false + qT * sqrt(diag(W)/(g*b*n-g-b+1) * (1/100+1/100))

inf_veg
sup_veg

sum(vitamin=='TRUE')
sum(vitamin=='FALSE') 

m_vitamin_true  <- sapply(diet_data[vitamin=='TRUE',],mean)
m_vitamin_false  <- sapply(diet_data[vitamin=='FALSE',],mean)
inf_vitamin <- m_vitamin_true-m_vitamin_false - qT * sqrt(diag(W)/(g*b*n-g-b+1) * (1/100+1/100))
sup_vitamin <- m_vitamin_true-m_vitamin_false + qT * sqrt(diag(W)/(g*b*n-g-b+1) * (1/100+1/100))

inf_vitamin
sup_vitamin

IC2   <- list(veg_true_false=cbind(inf_veg, sup_veg), vitamin_true_false=cbind(inf_vitamin, sup_vitamin))
IC2

# $veg_true_false
# inf_veg   sup_veg
# pressure     -3.561176  1.301176 # veg no effect on pressure
# cholesterol -15.379225 -5.718775 # but have negative effect on cholesterol (reduce it - so it's good)
# 
# $vitamin_true_false
# inf_vitamin sup_vitamin
# pressure     -13.043176   -8.180824 # vitamin has negative effect on pressure 
# cholesterol   -2.877225    6.783225 # but don't have effect on cholesterol






