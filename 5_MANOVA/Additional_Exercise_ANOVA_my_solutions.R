library(MVN)
library(car)
library(heplots)

## Additional exercise 1 ------------------------------------------------------------------------
#
# During the austral summer three species of penguins nest in the Antarctic 
# Peninsula: Chinstrap, Adelie and Gentoo.
# Some biologists of the Artowski basis measured the weight [kg] of 90 adults:
# 15 males and 15 females for each of the three species (penguins.txt file). 
# a) By using an ANOVA model with two factors, claim if gender and / or species
#    belonging significantly affect the weight. 
# b) Using an appropriate model (possibly reduced), provide estimates (global
#    90% confidence) of means and variances of the groups identified at point
#    (a).


# one feature and two factors => Two way Anova
penguins <- read.table("penguins.txt", h=TRUE)
penguins

species <- factor(penguins$species)
gender <- factor(penguins$gender)

species_gender <- factor(paste(species, gender))

weight <- penguins$weight

g <- length(levels(species))
b <- length(levels(gender))
n <- dim(penguins)[1]/(b*g) # balanced experiment (15 observations for each group)

M           <- mean(penguins$weight) # overall mean
Mgender      <- tapply(penguins$weight, gender, mean) # mean per gender
Mspecies       <- tapply(penguins$weight, species, mean) # mean per species
Mspecies_gender <- tapply(penguins$weight, species_gender, mean) # mean per gender x species type

species_gender

# check noramlity assumption
shapiro.test(weight[species_gender == levels(species_gender)[1]])
shapiro.test(weight[species_gender == levels(species_gender)[2]])
shapiro.test(weight[species_gender == levels(species_gender)[3]])
shapiro.test(weight[species_gender == levels(species_gender)[4]])
shapiro.test(weight[species_gender == levels(species_gender)[5]])
shapiro.test(weight[species_gender == levels(species_gender)[6]])

# 2) homogeneity of variances
bartlett.test(weight, species_gender)

plot(species_gender, penguins$weight, col=rainbow(5)[2:5], ylim=c(0,10),xlab='')
par(mfrow=c(1,1)) # probably we will notice difference among species 

fit.aov2.int <- aov(weight ~ species + gender + species:gender)
summary.aov(fit.aov2.int)

# We don't see strong effect of interaction of two factor (gamma_ij = 0)
# So let's remove this interaction from model

fit.aov2 <- aov(weight ~ species + gender)
summary.aov(fit.aov2)

# As we see, we also don't see strong evidence of gender effect betta_i = 0

fit.aov2_sp <- aov(weight ~ species) # So now its like one-way anova
summary.aov(fit.aov2_sp)

# check assumptions for species 
# check noramlity assumption
shapiro.test(weight[species == levels(species)[1]])
shapiro.test(weight[species == levels(species)[2]])
shapiro.test(weight[species == levels(species)[3]])

# 2) homogeneity of variances
bartlett.test(weight, species)

# Confidence Intervals ---------------------------------------------------------
# b)
DF <- fit.aov2_sp$df
summary(fit.aov2_sp)

20.97/DF # Mean Sq

var1 <- var(weight[species == levels(species)[1]])
var2 <- var(weight[species == levels(species)[2]])
var3 <- var(weight[species == levels(species)[3]])

Spooled <- ((30-1)*var1 + (30-1)*var2 + (30-1)*var3)/(90 - 3)
Spooled
k <- 4

cfr.t <- qt(1-alpha/(2*k), DF) # Student quantile

IC.BF.Adelia <- c(Mspecies[1] - cfr.t*sqrt(Spooled/30),
                  Mspecies[1],
                  Mspecies[1] + cfr.t*sqrt(Spooled/30))
IC.BF.Chinstrap  <- c(Mspecies[2] - cfr.t*sqrt(Spooled/30),
                  Mspecies[2],
                  Mspecies[2] + cfr.t*sqrt(Spooled/30))
IC.BF.Gentoo  <- c(Mspecies[3] - cfr.t*sqrt(Spooled/30),
                      Mspecies[3],
                      Mspecies[3] + cfr.t*sqrt(Spooled/30))
# Bonferroni region defined by the cartesian product of the Bf intervals
Bf_mean <- rbind(IC.BF.Adelia, IC.BF.Chinstrap, IC.BF.Gentoo)
dimnames(Bf_mean)[[2]] <- c('inf','center','sup')
Bf_mean

 
IC.var <- matrix(NA, nrow=1, ncol=3)
rownames(IC.var) <- "var"
colnames(IC.var) <- c("inf", "center", "sup")

IC.var[1, ] <- c(
  DF * Spooled / qchisq(1 - alpha / (2 * k), df = DF),
  Spooled,
  DF * Spooled / qchisq(alpha / (2 * k), df = DF)
)
IC.var

# 
### question b)
DF <- fit.aov2_sp$df # (b*n)*g - g = (2*15)*3 - 3 = 90 - 3 = 87
Spooled <- sum(fit.aov2_sp$res^2)/DF
Spooled

means <- as.vector(tapply(penguins$weight, penguins$species, mean))
names(means) <- levels(species)
means

alpha <- 0.10
k     <- 4 # g + 1 = 4 (g Conf Int for the means and 1 for the variance)
BF    <- rbind(cbind(means - sqrt(Spooled / 30) * qt(1 - alpha / (2*k), DF), 
                     means + sqrt(Spooled / 30) * qt(1 - alpha / (2*k), DF)),
               c(Spooled * DF / qchisq(1 - alpha/(2*k), DF), 
                 Spooled * DF / qchisq(alpha/(2*k), DF)))
rownames(BF)[4] <- 'Var.'
BF


# Problem 1 of 09/07/08 --------------------------------------------------------
##### 
##### (questions b and c)


# The client.txt dataset contains data on 150 customers of
# PoliBank. For each customer we are given age [years], money invested
# at low risk [thousands of euros] (safemoney variable) and money invested
# at high risk [thousands of euros] (riskymoney variable).
# [a) Using only the variable age, cluster customers in three groups and
#     describe them in terms of age. Use a hierarchical agglomerative 
#     algorithm based on Euclidean distance and single linkage. Report
#     cophenetic coefficient and the size of the clusters.
#  b) Introducing the appropriate assumptions about distributions of the 
#     variables safemoney and riskymoney within the three groups, perform
#     a MANOVA to see if there is statistical evidence of a difference 
#     in the joint distributions of safemoney and riskymoney variables 
#     in the three groups.
#  c) Comment the result of MANOVA by means of suitable Bonferroni 
#     intervals with global confidence 90%.

client <- read.table('client_class.txt', header=T)  # Clustering already performed at 
                                                    # point a), labels inserted as 1st 
                                                    # column

client_data <- data.frame(
  safemoney = client$safemoney,
  riskymoney = client$riskymoney
)
age <- client$age

# cehck MVN
mvn(client_data[client$age == "adult",])
mvn(client_data[client$age == "young",])
mvn(client_data[client$age == "old",])

Sadult <-  cov(client_data[client$age == "adult",])
Syoung <-  cov(client_data[client$age == "young",])
Sold <- cov(client_data[client$age == "old",])

# covariance with same structure  
boxM(client_data, age)

# b) MANOVA 

fit <- manova(as.matrix(client_data) ~ age)
summary.manova(fit)

summary.aov(fit)

# c) BI
alpha <- 0.1
g <- length(unique(client$age)) # two groups
p <- ncol(client_data) # number of features
n <- nrow(client_data)

k <- p * g * (g - 1) / 2
qT <- qt(1 - alpha / (2 * k), n - g)

W <- summary.manova(fit)$SS$Residuals
m  <- sapply(client_data, mean)         # estimates mu
m_adult <- sapply(client_data[client$age == "adult",], mean)    # estimates mu.0 = mu + tau.0
m_young <- sapply(client_data[client$age == "young",], mean)    # estimates mu.1 = mu + tau.1
m_old <- sapply(client_data[client$age == "old",], mean)



inf_adult_young <- m_adult - m_young - qT*sqrt((1/sum(client$age == "adult") + 1/sum(client$age == "young"))*diag(W)/(n-g))
sup_adult_young <- m_adult - m_young + qT*sqrt((1/sum(client$age == "adult") + 1/sum(client$age == "young"))*diag(W)/(n-g))

inf_adult_old <- m_adult - m_old - qT*sqrt((1/sum(client$age == "adult") + 1/sum(client$age == "old"))*diag(W)/(n-g))
sup_adult_old <- m_adult - m_old + qT*sqrt((1/sum(client$age == "adult") + 1/sum(client$age == "old"))*diag(W)/(n-g))

inf_young_old <- m_old - m_young - qT*sqrt((1/sum(client$age == "old") + 1/sum(client$age == "young"))*diag(W)/(n-g))
sup_young_old <- m_old - m_young + qT*sqrt((1/sum(client$age == "old") + 1/sum(client$age == "young"))*diag(W)/(n-g))


CI <- list(adult_young    = cbind(inf_adult_young, m_adult - m_young, sup_adult_young),
           setosa_virginica     = cbind(inf_adult_old, m_adult - m_old, sup_adult_old),
           versicolor_virginica = cbind(inf_young_old, m_old - m_young, sup_young_old))
CI

plot(age, client_data$safemoney, col=rainbow(5)[2:5], ylim=c(0,10),xlab='')

##### Problem 3 of 29/06/11 ----------------------------------------------------
# Juan de los Euros, a known driver of Santander, has measured the duration of
# his recent trips to / from the airport (file time.txt).
# a) Having fitted a two-factor ANOVA additive model (center-aero / aero-center,
#    Weekday / weekend) provide point estimated of the means and the variances  
#    of the four possible types of travel.
# b) On the basis of the model (a) perform a 90% test to test the significance
#    of the factor-center aero / aero-center.
# c) On the basis of the model (a) perform a 90% test to test the significance
#    of the factor weekday / weekend.
# d) Based on the tests (b) and (c) propose a possible reduced model and re-
#    estimate point-wise - coherently with the reduced model - the means and
#    variances of the four possible types of travel.

time <- read.table("time.txt", h=TRUE)
durata <- time$durata
AR <- factor(time$AR)
FF <- factor(time$FF)

# a)

fit.aov.add <- aov(durata ~ AR+FF)
summary.aov(fit.aov.add)

# I had to compute differently
mean_ac_feriale <- mean(durata[time$AR == "aero_centro" & time$FF == "feriale"])
mean_ac_festivo <- mean(durata[time$AR == "aero_centro" & time$FF == "festivo"])
mean_ca_feriale <- mean(durata[time$AR == "centro_aero" & time$FF == "feriale"])
mean_ca_festivo <- mean(durata[time$AR == "centro_aero" & time$FF == "festivo"])

Spooled <- 88.95/17

shapiro.test(durata[time$AR == "aero_centro" & time$FF == "feriale"])
shapiro.test(durata[time$AR == "aero_centro" & time$FF == "festivo"])
shapiro.test(durata[time$AR == "centro_aero" & time$FF == "feriale"])
shapiro.test(durata[time$AR == "centro_aero" & time$FF == "festivo"])

bartlett.test(durata, factor(paste(AR, FF)))

# b)
fit.aov.direction <- aov(durata ~ AR)
summary.aov(fit.aov.direction) # there are no significant difference in means among direction

# c) 
fit.aov.weekend <- aov(durata ~ FF)
summary.aov(fit.aov.weekend) # so there are significant difference in means among days

# d)

##### Problem 1 of 12/02/08 ----------------------------------------------------
# In a medical research center, concentrations of interferon gamma-6 and 
# Interferon gamma-7 were measured in the blood of some patients who had
# been infected by Human Papilloma Virus (PV.txt file). The experiment
# included patients that have different medical profiles in terms
# Relapse and HPV-Clearance:
# Relapse: extinct infection (A) or ongoing infection (B);
# HPV-Clearance: extinct we aim to find out if Relapse and Clearance factors
# have an effect on interferon gamma-6 and interferon gamma-7 distributions.
# a) Introduce an appropriate statistical model that justifies the use of
#    MANOVA as a tool for the analysis of these data.
# b) Perform a test to verify the interaction between the factors.
# c) Identify the factor or factors that generate effects statistically
#    significant.
# c) Construct Bonferroni confidence intervals with global coverage 90% 
#    that clarify the conclusions drawn at point (c).

# Two way-manova
PV <- read.table("PV.txt", h=TRUE)








