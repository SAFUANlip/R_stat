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

# We aim to explore the impact of different music preferences on emotional well-being. The dataset music.txt
# contains measurements of two key indicators:
#   • relaxation, representing a relaxation score (ranging from 0 to 100), where higher values indicate greater
# relaxation;
#   • motivation, representing a motivation score (ranging from 0 to 100), where higher values indicate greater
# motivation.
# The dataset consists of data from 200 randomly selected individuals, along with information on whether they
# regularly listen to classical music (classical) and whether they listen to upbeat music (upbeat).

music <- read.table("2025_02_06/music.txt", h=TRUE)

classical   <- factor(music$classical) # Treat.1
upbeat   <- factor(music$upbeat) # Treat.2

cls_up <- factor(paste(classical, upbeat, sep=''))
cls_up

music_data <- music[, 1:2]

# a) Do classical and upbeat music significantly influence emotional well-being indicators?
# Justify your answer using a MANOVA model.

fit <- manova(as.matrix(music_data) ~ classical + upbeat + classical:upbeat)
summary.manova(fit)
# so type of music separately have effect on well-being, but their 
# common intercation doesn't have effect (as p-value > 0.05)

# b) Identify and check the assumptions of the model introduced in (a).
# we need check MVN and homogenity 
Ps <- c(mvn(music_data[cls_up==levels(cls_up)[1],],)$multivariateNormality$`p value`,
        mvn(music_data[cls_up==levels(cls_up)[2],],)$multivariateNormality$`p value`,
        mvn(music_data[cls_up==levels(cls_up)[3],],)$multivariateNormality$`p value`,
        mvn(music_data[cls_up==levels(cls_up)[4],],)$multivariateNormality$`p value`)
Ps # all values > 0.05 (So we can not reject H0 of normlaity assumptions)

S1 <-  cov(music_data[cls_up==levels(cls_up)[1],])
S2 <-  cov(music_data[cls_up==levels(cls_up)[2],])
S3 <-  cov(music_data[cls_up==levels(cls_up)[3],])
S4 <-  cov(music_data[cls_up==levels(cls_up)[4],])

summary(boxM(music_data, cls_up)) # also same covariance structure

# c) Based on the results from (a), would you propose a different model? Explain your reasoning.
# Due to our model has two effects but interaction of effects doesn't have effect
# I would leave only effects separetly  (classical + upbeat)

fit2 <- manova(as.matrix(music_data) ~ classical + upbeat)
summary.manova(fit2)

# d) Compute Bonferroni-adjusted confidence intervals (global level 95%) 
# for the effects of classical and upbeat music
# on emotional well-being. How would you interpret these effects
# on relaxation and motivation?

sum(cls_up == "FALSETRUE")
sum(cls_up == "FALSEFALSE")
sum(cls_up == "TRUEFALSE")
sum(cls_up == "TRUETRUE")


alpha <- 0.05
g <- 2 # number of levels of first factor
b <- 2 # number of levels of second factor
p <- 2 # num of features which means we compare 
n <- 50 # number of samples in each group
N <- n*g*b # 20

W <- summary.manova(fit2)$SS$Residuals

# how many comparisons?
k <- p*g*(g-1)/2 + p*b*(b-1)/2
# because we have: g levels on the first factor on p components
#                  b levels on the second factor on p components
k

qT <- qt(1 - alpha/(2*k), g*b*n-g-b+1) # g*b*n-g-b+1 = 197 degree of freedom from manova
# the degrees of freedom of the residuals on the additive model are
# g*b*n-g-b+1
sum(classical=='TRUE') # 100, 
sum(classical=='FALSE') # 100
# so divide by 1/100 + 1/100

m_cls_true  <- sapply(music_data[classical=='TRUE',], mean)
m_cls_false  <- sapply(music_data[classical=='FALSE',], mean)
inf_classical <- m_cls_true-m_cls_false - qT * sqrt(diag(W)/(g*b*n-g-b+1) * (1/100+1/100))
sup_classical <- m_cls_true-m_cls_false + qT * sqrt(diag(W)/(g*b*n-g-b+1) * (1/100+1/100))

inf_classical
sup_classical


sum(upbeat=='TRUE')
sum(upbeat=='FALSE') 

m_up_true  <- sapply(music_data[upbeat=='TRUE',],mean)
m_up_false  <- sapply(music_data[upbeat=='FALSE',],mean)
inf_up <- m_up_true-m_up_false - qT * sqrt(diag(W)/(g*b*n-g-b+1) * (1/100+1/100))
sup_up <- m_up_true-m_up_false + qT * sqrt(diag(W)/(g*b*n-g-b+1) * (1/100+1/100))

inf_up
sup_up

IC2   <- list(classical_true_false=cbind(inf_classical, sup_classical), up_true_false=cbind(inf_up, sup_up))
IC2
# As we see CI for classical music for motivation contain zero 
# => there are no effect of classical music on motivation
# but have effect on relaxation
#
# Also for upbeat (cotainig zero in CI) => no effect of upbeat on relaxation
# but have effect on motivation

# what if we had intercation eeffect
alpha <- 0.05
g <- 4  # теперь 4 группы (все комбинации classical + upbeat)
p <- 2  # две переменные (relaxation, motivation)
n <- 50 # по 50 наблюдений в каждой группе
N <- g * n

# Вычисляем количество попарных сравнений
k <- p * choose(g, 2)  # = 2 * 6 = 12

# Остаточная степень свободы:
df_resid <- N - g  # т.к. сравниваются средние 4 групп

fit_full <- manova(as.matrix(music_data) ~ classical + upbeat + classical:upbeat)


# Матрица остаточной дисперсии
W <- summary.manova(fit_full)$SS$Residuals

# Квантиль с поправкой Бонферрони
qT <- qt(1 - alpha / (2 * k), df_resid)

# Средние по 4 группам
g1 <- colMeans(music_data[classical=="FALSE" & upbeat=="FALSE",])
g2 <- colMeans(music_data[classical=="FALSE" & upbeat=="TRUE",])
g3 <- colMeans(music_data[classical=="TRUE" &  upbeat=="FALSE",])
g4 <- colMeans(music_data[classical=="TRUE" &  upbeat=="TRUE",])

group_means <- list(g1=g1, g2=g2, g3=g3, g4=g4)
group_names <- c("FF", "FT", "TF", "TT")

# Функция для расчёта CI на разность двух групп
get_CI <- function(m1, m2, W, qT, df_resid, n1, n2) {
  diff <- m1 - m2
  se <- sqrt(diag(W)/df_resid * (1/n1 + 1/n2))
  inf <- diff - qT * se
  sup <- diff + qT * se
  return(cbind(inf, sup))
}

# Расчёт CI для всех пар групп
CI_list <- list()
pair_labels <- combn(1:4, 2)

for (i in 1:ncol(pair_labels)) {
  idx1 <- pair_labels[1, i]
  idx2 <- pair_labels[2, i]
  name <- paste0(group_names[idx1], "_vs_", group_names[idx2])
  CI_list[[name]] <- get_CI(group_means[[idx1]], group_means[[idx2]], W, qT, df_resid, n, n)
}

CI_list

