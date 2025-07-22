library(readxl)
library(dplyr)
library(openxlsx)
library(MVN)
library(car)
library(heplots)
library(ggplot2)
library(reshape2)
library(MASS)
library(tidyverse)
library(lubridate)


# We aim to investigate how air purification systems and automated watering influence two physiological indicators
# of plant health in smart greenhouses. The dataset greenhouse.txt contains measurements of:
#   • chlorophyll, the average chlorophyll concentration in leaves, measured in µg/cm2;
#   • growth, the average daily height increase of plants, measured in cm/day.
# The data come from 200 greenhouse units, each growing the same species of lettuce under controlled conditions.
# Each unit is tagged with two binary variables:
#   • air purifier, indicating whether the unit was equipped with an air purification system (1 = yes, 0 = no);
#   • auto watering, indicating whether automated watering was used (1 = yes, 0 = no).

greenhouse <- read.table("greenhouse.txt", h=TRUE)

# a) Do air purification and automated watering systems have a significant effect on the two plant health indicators?
# Support your answer using a MANOVA model.

AW   <- factor(greenhouse$auto_watering) # Treat.1
AP   <- factor(greenhouse$air_purifier) # Treat.2

AW_AP <- factor(paste(AW, AP, sep=''))
AW_AP

greenhouse_data <- greenhouse[, 1:2]

# Two-ways MANOVA
# Model with interaction (complete model): 
fit <- manova(as.matrix(greenhouse_data) ~ AW + AP + AW:AP)
summary.manova(fit)

# Do air purification and automated watering systems have a significant effect on the two plant health indicators?
# - They both have significant dependent effects


# b) Clearly state the assumptions of the MANOVA model used in (a), and assess their validity using appropriate
# diagnostics.

# Each group should be Multivariate Normal, with same covariance sturcture 
# and homogeneity of the covariance (qualitatively)
Ps <- c(mvn(greenhouse_data[AW_AP==levels(AW_AP)[1],],)$multivariateNormality$`p value`,
        mvn(greenhouse_data[AW_AP==levels(AW_AP)[2],],)$multivariateNormality$`p value`,
        mvn(greenhouse_data[AW_AP==levels(AW_AP)[3],],)$multivariateNormality$`p value`,
        mvn(greenhouse_data[AW_AP==levels(AW_AP)[4],],)$multivariateNormality$`p value`)
Ps

S1 <-  cov(greenhouse_data[AW_AP==levels(AW_AP)[1],])
S2 <-  cov(greenhouse_data[AW_AP==levels(AW_AP)[2],])
S3 <-  cov(greenhouse_data[AW_AP==levels(AW_AP)[3],])
S4 <-  cov(greenhouse_data[AW_AP==levels(AW_AP)[4],])

summary(boxM(greenhouse_data, AW_AP))

# c) Based on your analysis in (a) and (b), would you consider modifying the model? Explain.
# Due to our model has two effects and I already applied two-way version of MANOVA,
# confirming its assumptions (point b), I do not modify the model


# d) Build an appropriate discriminant classifier to predict whether the lettuces of a specific greenhouse were grown
# using an air purification system, based on the clorophyll concentration and the average daily growth. Similarly,
# develop a classifier to predict whether automated watering was used. Report the estimate of the actual error
# rate with leave-one-out cross validation for the two classifiers built.

# Due to data MVN and homogeneity - I will use LDA model
lda.air_purifier <- lda(greenhouse_data, AP, CV=TRUE)
lda.auto_watering <- lda(greenhouse_data, AW, CV=TRUE)

lda.air_purifier$class
AERCV.air_purifier   <- sum(lda.air_purifier$class != AP)/length(AP)
AERCV.air_purifier

# From scratch
predictions.air_purifier <- vector(length = dim(greenhouse_data)[1])
for(i in 1:dim(greenhouse_data)[1]){
  LdaCV.i <- lda(greenhouse_data[-i,], AP[-i])
  pred <- predict(LdaCV.i, greenhouse_data[i,])$class
  predictions.air_purifier[i] <- as.character(pred)
}

sum(predictions.air_purifier != AP)/length(AP)



AERCV.auto_watering  <- sum(lda.auto_watering$class != AW)/length(AW)
AERCV.auto_watering

# From scratch
predictions.auto_watering <- vector(length = dim(greenhouse_data)[1])
for(i in 1:dim(greenhouse_data)[1]){
  LdaCV.i <- lda(greenhouse_data[-i,], AW[-i])
  pred <- predict(LdaCV.i, greenhouse_data[i,])$class
  predictions.auto_watering[i] <- as.character(pred)
}

sum(predictions.auto_watering != AW)/length(AW) # strange, here I have 0.445 instead of 0.440


# e) Given the MANOVA results, would you consider a different approach for predicting whether both air purification
# and automated watering were used?
# I would be take into account interaction of effects, to do so I would predict not 2 classes, but 4
# using AW_AP - which take into account interactions between groups 

lda.AW_AP <- lda(greenhouse_data, AW_AP, CV=TRUE)
sum(lda.AW_AP$class != AW_AP)/length(AW_AP)

lda.AW_AP
