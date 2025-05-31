library(MASS)
library(MVN)

#_______________________________________________________________________________
##### Problem 2 of 1/07/2009
#####--------------------------
# An art historian requires your help to identify a criterion of classification 
# to discriminate the sculptures created by Gian Lorenzo Bernini from those of 
# other contemporary sculptors, based on the weight [tons] and height [m] 
# of 100 sculptures of undoubted attribution (sculptures.txt files). Taking into
# account that Bernini's sculptures are about 25% of the sculptures which have 
# to be classified and that the purpose of the historian is to minimize the expected
# number of misclassifications:
# a) build two classifiers C1 and C2, respectively, assuming for C1 that the data
#    come from two normal populations with equal covariance matrix, and for C2 that
#    the data come from two normal populations with different covariance matrix;
# b) estimate by cross-validation the AER of the two classifiers and comment their
#    values;
# c) how will be classified by the two classifiers a 2 meter high and 4 tons heavy
#    statue?

sculpt <- read.table('sculptures.txt', header=T)
head(sculpt)

sum(sculpt$Autore == "Altro")
sum(sculpt$Autore == "Bernini")

altro_index <- which(sculpt$Autore == "Altro")
bernini_index <- which(sculpt$Autore == "Bernini")

sculpt_data <-sculpt[,1:2]

# look on assumptions of normality and homogenity ------------------------------

# normality in upgrading Altro (for LDA and QDA)
mvn_uni_normal_altro <- mvn(data = sculpt_data[altro_index,], multivariatePlot = "qq", univariateTest = "SW", mvnTest = "royston")
print(mvn_uni_normal_altro$multivariateNormality)
print(mvn_uni_normal_altro$univariateNormality)

# normality in upgrading Bernini (for LDA and QDA)
mvn_uni_normal_bernini <- mvn(data = sculpt_data[bernini_index,], multivariatePlot = "qq", univariateTest = "SW", mvnTest = "royston")
print(mvn_uni_normal_bernini$multivariateNormality)
print(mvn_uni_normal_bernini$univariateNormality)

# homogenity (For LDA)
S_altro <-  cov(sculpt_data[altro_index,])
S_bernini <-  cov(sculpt_data[bernini_index,])

# No coefficient of the covariance matrix should differ from more than a factor ~10 across groups
cat("Maximum factor of features across group", max(abs(S_altro/S_bernini)), "Has to be less than ~10")


# a) - C1 (LDA), C2 (QDA) ------------------------------------------------------
# 25% - Bernini's => 75% Altro, prior will be 0.25 and 0.75

lda.sculpt <- lda(sculpt_data, sculpt$Autore, prior=c(75, 25)/100)
lda.sculpt

qda.sculpt <- qda(sculpt_data, sculpt$Autore, prior=c(75, 25)/100)
qda.sculpt

# b) leave-one-out cross validation --------------------------------------------

# LDA
lda_predicted_labels <- vector(length = dim(sculpt)[1])

for(i in 1:dim(sculpt)[1]){
  LdaCV.i <- lda(sculpt_data[-i,], sculpt$Autore[-i], prior=c(75, 25)/100)
  pred <- predict(LdaCV.i, sculpt_data[i,])$class
  lda_predicted_labels[i] <- as.character(pred)
}

lda_predicted_labels

# Confusion matrix components
TP_lda <- sum(lda_predicted_labels == "Altro" & sculpt$Autore == "Altro")
FP_lda <- sum(lda_predicted_labels == "Altro" & sculpt$Autore == "Bernini")
FN_lda <- sum(lda_predicted_labels == "Bernini" & sculpt$Autore == "Altro")
TN_lda <- sum(lda_predicted_labels == "Bernini" & sculpt$Autore == "Bernini")

# QDA
qda_predicted_labels <- vector(length = dim(sculpt)[1])

for(i in 1:dim(sculpt)[1]){
  QdaCV.i <- qda(sculpt_data[-i,], sculpt$Autore[-i], prior=c(75, 25)/100)
  pred <- predict(QdaCV.i, sculpt_data[i,])$class
  qda_predicted_labels[i] <- as.character(pred)
}

qda_predicted_labels

# Confusion matrix components
TP_qda <- sum(qda_predicted_labels == "Altro" & sculpt$Autore == "Altro")
FP_qda <- sum(qda_predicted_labels == "Altro" & sculpt$Autore == "Bernini")
FN_qda <- sum(qda_predicted_labels == "Bernini" & sculpt$Autore == "Altro")
TN_qda <- sum(qda_predicted_labels == "Bernini" & sculpt$Autore == "Bernini")

AER_lda <- 0.75*FN_lda/50 + 0.25*FP_lda/50
AER_qda <- 0.75*FN_qda/50 + 0.25*FP_qda/50

cat("AER lda (C1): ", AER_lda, "AER qda (C2): ", AER_qda)


# b) CV from solution ----------------------------------------------------------

# LDA
LdaCV.s <- lda(sculpt[,1:2], autore, prior=c(0.25, 0.75), CV=T)
table(class.true=autore, class.assignedCV=LdaCV.s$class)

AER.CV.l <- 45/50*0.25+3/50*0.75
AER.CV.l

# QDA
QdaCV.s <- qda(sculpt[,1:2], autore, prior=c(0.25, 0.75), CV=T)
table(class.true=autore, class.assignedCV=QdaCV.s$class)

AER.CV.q <- 20/50*0.25+6/50*0.75
AER.CV.q

# c) new observation -----------------------------------------------------------
new_obs <- c(2, 4)

prediction_lda <- predict(lda.sculpt, new_obs)
prediction_qda <- predict(qda.sculpt, new_obs)

prediction_lda$class
prediction_qda$class


#_______________________________________________________________________________
##### Problem 2 of 16/07/2010
#####--------------------------
# A young Portuguese engineer wants to build a machine able to automatically
# distinguish between two species of sardines (the Atlantic sardines and the
# Iberian sardine) on the basis of the length [cm] of the sardine. To
# do this, it aims to build a classifier based on the measurements of 500 
# Atlantic sardines and 500 Iberian sardine (sardine.txt file).
# a) Perform two tests to verify the hypothesis of normality of the two 
#    populations, a test for equality of means, a test for equality of 
#    variances.
# b) On the basis of the previous tests and knowing that 75% of fished sardines
#    belong to the Atlantic species while 25% to the Iberian species, build a
#    classifier that minimizes the number of misclassified sardines and report
#    the parameters.
# c) Estimate the AER of the classifier analytically using the estimated 
#    probability densities.

sardine <- read.table('sardine.txt', header=T)
head(sardine)

# a) normality test - (shapiro for p=1), ---------------------------------------
# mean equality - anova, t.test,
# homogenity - var.test, bartlett.test
help(t.test) 

Atalantica_data <- sardine$Atlantica
Iberica_data <- sardine$Iberica

help(shapiro.test)
normality_test_Atalantica <- shapiro.test(Atalantica_data)
normality_test_Iberica <- shapiro.test(Iberica_data)

# Normality 
normality_test_Atalantica$p.value # p-value > 0.05 can not reject H0 of normality
normality_test_Iberica$p.value # p-value > 0.05 can not reject H0 of normality

data_all <- c(Atalantica_data, Iberica_data)
labels_all <- vector(length = 1000)

for (i in 1:length(labels_all)){
  if (i <= 500){
    labels_all[i] <- "Atlantica"
  }
  else{
    labels_all[i] <- "Iberica"
  }
}

Atalantica_index <- which(labels_all == "Atlantica")
Iberica_index <- which(labels_all == "Iberica")


# equality of variances 
bartlett.test(data_all ~ labels_all) # p-value > 0.05 => so we can not reject H0 of equal variances
var.test(data_all ~ labels_all) # p-value > 0.05 => so we can not reject H0 of equal variances

# equality of means
t.test(data_all ~ labels_all) # p-value < 0.05, so we can reject H0 of equal means 
fit.aov <- aov(data_all ~ labels_all) # p-value < 0.05, so we can reject H0 of equal means

# variacne equal, normal distributed -> can perform LDA, for QDA (don't need variance equality)
# For FDA don't need normality 


# b) If asks minimise misclassification -> it have to be LDA or QDA ------------

lda_model <- lda(as.matrix(data_all), labels_all, prior=c(75, 25)/100)
lda_model

# Projection of LD1 ------------------------------------------------------------
# 1. Get direction, where we have to project our data, to maximize 
# variability between/variability within - ? as in FDA?
w <- lda_model$scaling[, 1] # solve(S) %*% (M1 - M0)

# 2. Проецируем все точки
lda_proj <- as.vector(as.matrix(data_all) %*% w)

# 3. Получаем проекции по группам
proj_atlantica <- lda_proj[Atalantica_index]
proj_iberica <- lda_proj[Iberica_index]

# 4. Оценим среднее и дисперсию на проекции
M_atlantica_proj <- mean(proj_atlantica)
M_iberica_proj <- mean(proj_iberica)

S_atlantica_proj <- cov(as.matrix(proj_atlantica))
S_iberica_proj <- cov(as.matrix(proj_iberica))

S_proj <- ((500-1) * S_atlantica_proj + (500-1) * S_iberica_proj) / (500 + 500 - 2)  # т.к. на проекции — одно измерение

# 5. Plot graphs
x <- seq(min(lda_proj), max(lda_proj), length.out = 500)

par(mfrow = c(2,1))

# Gaussian
plot(
  x,
  0.75 * dnorm(x, M_atlantica_proj, sqrt(S_proj)),
  type = 'l',
  col = 'blue',
  ylab = expression(paste('estimated ', p[i] * f[i], '(x)')),
  main = 'LDA (projection)'
)
lines(x, 0.25 * dnorm(x, M_iberica_proj, sqrt(S_proj)), col = 'red')
points(proj_atlantica, rep(0, length(proj_atlantica)), col = 'blue', pch = 16)
points(proj_iberica, rep(0, length(proj_iberica)), col = 'red', pch = 16)
legend("topright", legend=c("Atlantica", "Iberica"), col=c("blue", "red"), lty=1)

# posterior probability 
posterior_atlantica <- 0.75 * dnorm(x, M_atlantica_proj, sqrt(S_proj)) / (
  0.75 * dnorm(x, M_atlantica_proj, sqrt(S_proj)) + 0.25 * dnorm(x, M_iberica_proj, sqrt(S_proj))
)
posterior_iberica <- 1 - posterior_atlantica

plot(x, posterior_atlantica, type = 'l', col = 'blue', ylab = 'estimated posterior')
lines(x, posterior_iberica, col = 'red')
legend("topright", legend=c("P(Class 0 | x)", "P(Class 1 | x)"), col=c("blue", "red"), lty=1)

par(mfrow = c(1,1))

# c) 
predictions <- predict(lda_model, as.matrix(data_all))$class
predictions

# Confusion matrix components
TP_lda <- sum(predictions == "Atlantica" & labels_all == "Atlantica")
FP_lda <- sum(predictions == "Atlantica" & labels_all == "Iberica")
FN_lda <- sum(predictions == "Iberica" & labels_all == "Atlantica")
TN_lda <- sum(predictions == "Iberica" & labels_all == "Iberica")

AER <- 0.75 * FN_lda/500 + 0.25 * FP_lda/500
  
AER  
