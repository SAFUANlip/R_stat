library(MVN)
library(car)
library(heplots)
library(MASS)

data <- read.table("greenhouse.txt", h=TRUE)

data_value <- data[,1:2]

auto   <- factor(data$auto_watering) # Treat.1
air   <- factor(data$air_purifier) # Treat.2

ExAd <- factor(paste(auto, air, sep=''))

fit <- manova(as.matrix(data_value) ~ auto + air + auto:air)
summary.manova(fit)

Ps <- c(mvn(data_value[ExAd==levels(ExAd)[1],],)$multivariateNormality$`p value`,
        mvn(data_value[ExAd==levels(ExAd)[2],],)$multivariateNormality$`p value`,
        mvn(data_value[ExAd==levels(ExAd)[3],],)$multivariateNormality$`p value`,
        mvn(data_value[ExAd==levels(ExAd)[4],],)$multivariateNormality$`p value`)
Ps

S1 <-  cov(data_value[ ExAd==levels(ExAd)[1],])
S2 <-  cov(data_value[ ExAd==levels(ExAd)[2],])
S3 <-  cov(data_value[ ExAd==levels(ExAd)[3],])
S4 <-  cov(data_value[ ExAd==levels(ExAd)[4],])

par(mfrow=c(1,4))
image(S1, col=heat.colors(100),main='Cov. S1', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3,S4), (0:100)/100, na.rm=TRUE))
image(S2, col=heat.colors(100),main='Cov. S2', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3,S4), (0:100)/100, na.rm=TRUE))
image(S3, col=heat.colors(100),main='Cov. S3', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3,S4), (0:100)/100, na.rm=TRUE))
image(S4, col=heat.colors(100),main='Cov. S4', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3,S4), (0:100)/100, na.rm=TRUE))

summary(boxM(data_value, ExAd))

lda.model <- lda(data_value, air)
lda.model

# LDA
lda_predicted_labels <- vector(length = dim(data_value)[1])

for(i in 1:dim(data_value)[1]){
  LdaCV.i <- lda(data_value[-i,], air[-i])
  pred <- predict(LdaCV.i, data_value[i,])$class
  lda_predicted_labels[i] <- as.character(pred)
}

lda_predicted_labels
TP_lda <- sum(lda_predicted_labels == "TRUE" & air == "TRUE")
FP_lda <- sum(lda_predicted_labels == "TRUE" & air == "FALSE")
FN_lda <- sum(lda_predicted_labels == "FALSE" & air == "TRUE")
TN_lda <- sum(lda_predicted_labels == "FALSE" & air == "FALSE")

(FP_lda + FN_lda)/(FP_lda + FN_lda + TN_lda + TP_lda) 
