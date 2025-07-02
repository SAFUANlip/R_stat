# LMM -------------------------
library(corrplot)
library(lattice)
library(plot.matrix)
library(nlmeU)
library(nlme)
library(lme4)
library(insight)

#1) random intercept----------------
rm(list=ls())
graphics.off()
data <- read.table('tomatoes.txt',header=T)
head(data)
fm = lm(yield ~ -1 + as.factor(species) + temp, data)
summary(fm)
beta <- coefficients(fm)
beta
beta[2]
beta[201]

#b)

fm16.1mer <- lmer(yield ~ temp + (1|species),   data = data)

print(vc <- VarCorr(fm16.2mer), comp = c("Variance", "Std.Dev."))

sigma2_b <- as.numeric(get_variance_random(fm16.1mer))
sigma_b <- sqrt(sigma2_b)
sigma_b

#c)

fm16.2mer <- lmer(yield ~ temp + (1+temp|species), 
                  data = data)
coefficients(fm16.1mer)
# species 62 has that the temperature has a negative effect

## DOTPLOT: visualization of the random intercepts with their 95% confidence intervals
# Random effects: b_0i, b_1i for i=1,...,234
dotplot(ranef(fm16.2mer, condVar=T)) # xlim=c(-1,1)
dotplot(ranef(fm16.2mer, condVar=T),xlim=c(-1,1)) # zoom on graph, to see the second 


#d)
anova(fm16.1mer,fm16.2mer)
## PVRE
# sigma2_b = Var(b0,b1) = sigma2_b0 + 2Cov(b0,b1)*mean(w) + sigma2_b1*mean(w^2)
sigma2_eps <- as.numeric(get_variance_residual(fm16.1mer))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(fm16.1mer)) 
sigma2_b

PVRE1 <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE1 #  0.1699943 
## PVRE
# sigma2_b = Var(b0,b1) = sigma2_b0 + 2Cov(b0,b1)*mean(w) + sigma2_b1*mean(w^2)
sigma2_eps <- as.numeric(get_variance_residual(fm16.2mer))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(fm16.2mer)) 
sigma2_b

PVRE2 <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE2 # 0.2390488

#from PVRE is better M2






#2) dependence of the response on more categorical variables, report model and its parametrization, verify the assumption, estimated standard deviation, pointwise estimate and a prediction interval of level 95% of a new datum---------------

#a)
rm(list=ls())
graphics.off()
data <- read.table('beachVolley.txt',header=T)
head(data)
data$shower <- as.factor(data$shower)
data$environment <- as.factor(data$environment)
data$sand.color <- as.factor(data$sand.color)

#NON è LMM!!!!!!!!!!!!!!1
fm <- lm(price ~ ., data = data)
summary(fm)








#3) LM with an average stronzo, CorCompSymm, Random intercept, comparison two models-------------------
rm(list=ls())
graphics.off()
data <- read.table('Lakes_pollution.txt',header=T)
head(data)
fm <- lm(DO ~ 1+ depth + mercury_conc + ph + turbidity + wastewater_discharge ,data ) 
summary(fm)
fm2 <- lm(DO ~ 1+ depth  + ph + turbidity + wastewater_discharge ,data ) 
anova(fm,fm2)
#we keep fm2 without mercury_conc


beta <- coefficients(fm2)
beta
sigmasq <- sum(residuals(fm2)^2)/fm2$df # estimate of sigma^2
sigmasq
#1.482333
par(mfrow=c(2,2))
plot(fm)
shapiro.test(residuals(fm2))
dev.off()
#Diagnostic Plots
#Residuals vs Fitted:  points appear randomly distributed around the horizontal line with no pattern, the assumptions of linearity and homoscedasticity are respected.
#Scale-Location:  points are homogeneously distributed, there are no heteroscedasticity issues.

#b)
#What is the average increase of DO due to increment of 3 NTU in turbidity? Is that significant? Report the
#mean difference of DO between the locations with wastewater discharge with respect to the ones that are not discharged.
# y= b0 + b1*d + b2*(t+3)
# y= b0 + b1*d + b2*(t)
# difference is b2*3
beta[4]*3
#9.001631
# is significant in confront of our beta


# Report the mean difference of DO between the locations with wastewater discharge with respect to the ones that are not discharged.
# the mean difference is Beta$wastewater_dischargeYes
#1.8970702 

#c) 
###### Correlation 1: CorCompSym() (correlazione = rho in tutte le posizioni)
lm1.form <- formula(DO ~ 1+ depth  + ph + turbidity + wastewater_discharge  )
fm12.1 <- gls(lm1.form, # prima riga dà i pesi diversi sulla diagonale, visto al capitolo prima
              correlation = corCompSymm(form = ~1|italian_lakes), # comando per la correlazione, cambiare subject in caso
              data = data)
summary(fm12.1)

#rho
intervals(fm12.1, which = "var-cov",alpha=0.95)
#         lower       est.     upper
#Rho -0.0495982 0.03929671 0.2165529
#This interval contains 0, indicating that the correlation might not be significantly different from 0
#so they are indipendent

#sigma
#Residual standard error:
#  lower     est.    upper 
#1.072828 1.218486 1.383920 
#This interval does not contain 0 and suggests eteroschedasticity.  
#the correlation structure might be weak 


#d) random intercept, report PVRE
fm16.1mer <- lmer(DO ~ 1+ depth  + ph + turbidity + wastewater_discharge  + (1|italian_lakes), # "(1|subject)" dice che mette effetti randomici sull'intercetta in base a subject
                  data = data)
sigma2_eps <- as.numeric(get_variance_residual(fm16.1mer))
sigma2_b <- as.numeric(get_variance_random(fm16.1mer))
PVRE <- sigma2_b/(sigma2_b+sigma2_eps) # how big is the effect of the grouping on the data
PVRE
#0.03924727 -> is a bad value, variability of the model is at most from the error                 
# se basso no, variabilità modello data dagli error in gran parte, non ci possiamo fare niente

#PVRE (Proportion of Variance Explained) di 0.700168 indica che il modello spiega circa il 70% della varianza nei dati.
#Questo è un buon risultato, suggerendo che il modello ha una buona capacità predittiva e che le variabili incluse nel modello 
#sono rilevanti per spiegare la variabilità della variabile dipendente.
# In english: The PVRE (Proportion of Variance Explained) of 0.700168 indicates that the model explains about 70% of the variance in the data. This is a good result, suggesting that the model has strong predictive power and that the variables included in the model are relevant for explaining the variability of the dependent variable.

#bonus: comparison between CorCompSymm and random intercept
# random intercept more complex in general
#AIC and BIC: Lower values indicate a better model fit, but they also penalize model complexity. The model with the lower AIC or BIC is generally preferred.
AIC(fm12.1)
AIC(fm16.1mer)
BIC(fm12.1)
BIC(fm16.1mer)
# fm12.1 is a little bit better
# If similar, it is vetter CorCompSymm (fm12.1)

#e)
dotplot(ranef(fm16.1mer, condVar=T))
#Lake Levico



























#4) 2023-06-16 DBSCAN --------------

rm(list=ls())
graphics.off()
data <- read.table('molecules.txt',header=T)
head(data)
# a) Which clustering methods discussed in class are suitable for this case? Provide a precise justification for your answer.

#Hierarchical clustering and DBSCAN are the most suitable methods for this case, where the number of clusters are unknown .
#Hierarchical clustering is useful for its flexibility and ability to visualize data at different levels, while DBSCAN is advantageous for handling complex structures and noise within the data. 

# b)

n <- dim(data)[1]
p <- dim(data)[2]


# setting distance and linkage:
#If small differences or presence of zeros in the chemical data are crucial: Canberra Distance.
#If the data is normalized or differences are uniformly important: Euclidean Distance.
distance <- 'euclidean' # manhattan, canberra
linkages <- c('single', 'average', 'complete', 'ward.D2')

# distance matrix:
data.dist <- dist(data, method=distance)
# plot:
image(1:n,1:n, as.matrix(data.dist), main=paste('metrics: ', distance), asp=1, xlab='', ylab='')

# perform hierarchical clustering:
data.s <- hclust(data.dist, method=linkages[1])
data.a <- hclust(data.dist, method=linkages[2])
data.c <- hclust(data.dist, method=linkages[3])
data.w <- hclust(data.dist, method=linkages[4])

# plot dendograms:
par(mfrow=c(2,2))
plot(data.s, main=paste(distance, ' - ', linkages[1]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
plot(data.a, main=paste(distance, ' - ', linkages[2]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
plot(data.c, main=paste(distance, ' - ', linkages[3]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
plot(data.w, main=paste(distance, ' - ', linkages[4]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
# note: if you want to set dendogram ylim equal:
#         add: ylim = c(0,VALOREMAX), 
#         take out hang=-0.1, 
#         take out labels=F, 
#         add leaflab='none'

# select the best linkage and best k:
k <- 3
linkage <- 'ward.D2' # average, complete, ward.D2
data.hc <- data.w #s, a, c, w
par(mfrow=c(1,1))
plot(data.hc, main=paste(distance, ' - ', linkage), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.hc, k=k)

# cut dendogram:
clusters <- cutree(data.hc, k=k)
table(clusters)
#clusters
#1  2  3 
#30 30 30 

#c)
data <- data.frame(data)
data <- as.matrix(data)
data <- as.dist(data)
minPts <- 3
# Plot of the distances to the minPts-1 nearest neighbor
kNNdistplot(data, minPts = minPts)
# Taking eps = 0.05 seems to be a good threshold
abline(h = 0.05, col = "red", lty = 2)
kNNdistplot(dati, k = 3)

# Run the dbscan
dbs <- dbscan(data, eps = 0.05, minPts = 3)
dbs
# 0  1  2  3  4  5  6  7  8 
#29  6  5 15  6 14  7  3  5 

#it's not good, i should take the elbow point where the slope of the curve changes sharply.
# value of eps = 0.1 
abline(h = 0.1, col = "red", lty = 2)
dbs2 <- dbscan(data, eps = 0.1, minPts = 3)
dbs2
#3 clusters of
# 0  1  2 
#6 56 28 



#d) minPts = 10 and eps = 0.15
minpts <- 10
kNNdistplot(data, minPts = minpts)
abline(h = 0.15, col = "red", lty = 2)

# Run the dbscan
dbs <- dbscan(data, eps = 0.15, minPts = minpts)
dbs

# 0  1  2  3 
#9 23 22 36


# Suggest a quantitative method for comparing the quality of the clustering results obtained from this DBSCAN
#run and the hierarchical clustering conducted in b)

library(cluster)

sil <- silhouette(dbs$cluster, dist(data))
summary(sil)
mean(sil[, "sil_width"])
#0.3451817

sil <- silhouette(clusters, dist(data))
summary(sil)
mean(sil[, "sil_width"])
#0.5633844

#Values close to 1 indicate that the object is well clustered.
#Values close to 0 indicate that the object lies between two cluste


#hierarchical is better



#e)Is there a way to visualize the molecules in a two-dimensional plot?
#Dimension of the space in which i want to represent the data
k <- 3  #* 

# setting distance:
#*
distance <- 'euclidean' # manhattan, canberra
# distance matrix:
data.dist <- dist(data, method=distance)


data.map <- cmdscale(data.dist, k=k)

# Create a color vector for clusters:
colors <- c("red", "blue", "green")  # Assign more colors if you have more clusters
cluster_colors <- colors[as.factor(clusters)]  # Assign colors based on clusters

# Plot the MDS results with colors according to clusters:
plot(data.map[, 1], data.map[, 2], col = cluster_colors, pch = 16, asp = 1, 
     main = "MDS of Data Colored by Cluster", xlab = "Dimension 1", ylab = "Dimension 2")

# Optionally, add cluster labels:
text(data.map[, 1], data.map[, 2], labels = clusters, col = cluster_colors, cex = 0.75, pos = 3)
# Alternatively, if you want to label with molecule names and color by clusters:
text(data.map[, 1], data.map[, 2], labels = colnames(as.matrix(data.dist)), 
     col = cluster_colors, cex = 0.75, pos = 3)






































#5) #inspecting with plot of residuals against each variable, propose a new model I(age^2), mean difference in average monthly and confidence interval,why use penalized regression method, lasso and choose of lambda ------------------------------
rm(list=ls())
graphics.off()
data <- read.table('expenditure.txt',header=T)
head(data)
fit <- lm(avg_exp  ~ 1 + income + age  + perc_taxes  + as.factor(owns_house) , data)
summary(fit)


par(mfrow=c(2,2))
plot(fit)
shapiro.test(residuals(fit))
vif(fit)
dev.off()
# homoschedasticity ok
# normality no
# new model???

#b)
par(mfrow=c(1,1))
plot(data$age, fit$residuals) 
# fai anche per il resto delle variabili
# noto un comportamento che sale e scend, provo con age^2

fit2 <- lm(avg_exp ~ income+age+I(age^2)+perc_taxes+owns_house, data)
summary(fit2)
plot((data$age)^2,residuals(fit2))
#ok
par(mfrow=c(2,2))
plot(fit2)
shapiro.test(residuals(fit2))
#super ok


#c)mean difference in average monthly expenses between employees who own a house and 
#those who rent? Provide a 95% confidence interval.

#mean difference in average (y con - y senza = beta$owns_house_yes)
coefficients(fit2)[6]

#95% confidence interval tra own_house e NO_own_house 
alpha <- 0.05
confint(fit2, level= 1-alpha)[6,]  #



#d) why we should use a penalized regression method? 
vif(fit)
#high collinearity for 2 variable->we use lasso



#e)we choose LASSO!
set.seed(20230707)
y     <- data$avg_exp
x <- model.matrix(avg_exp ~ income+age+perc_taxes+owns_house, data)[,-1]
lambda.c <- 10^seq(10,-2,length.out=100) 

#if lambda is in a range 
fit.lasso <- glmnet(x, y, lambda = lambda.c, alpha = 1)
par(mfrow=c(1,1))
# behavior of the coefficient as a function of log(lambda):
plot(fit.lasso, xvar='lambda',label=TRUE, col = rainbow(dim(x)[2])) 
legend('topright', dimnames(x)[[2]], col = rainbow(dim(x)[2]), lty=1, cex=1)

# Choice of the optimal lambda, e.g., via cross-validation:
cv.lasso <- cv.glmnet(x,y,lambda=lambda.c, alpha = 1, nfolds=10) # default: 10-fold CV
optlam.lasso <- cv.lasso$lambda.1se #opt (meno parametri) #minimizza la cross validation
bestlam.lasso <- cv.lasso$lambda.min #best #minimizza min squared error
bestlam.lasso  #NEL GRAFICO GUARDO log(lambda)!

# minimizza cross validation -> optlam.lasso
# minimizza cross validation e model complexity -> bestlam.lasso


#Regressori e coefficienti del modello con il miglior lambda:
r<-#numero di coeff
  coef.lasso <- predict(fit.lasso, s=bestlam.lasso, type = 'coefficients')[,]
#in cv.lasso$cvm ci sono gli MSE provare con which per trovare quello associato
coef.lasso
plot(cv.lasso)
coef.lasso <- predict(fit.lasso, s=optlam.lasso, type = 'coefficients')[,]
#in cv.lasso$cvm ci sono gli MSE provare con which per trovare quello associato
coef.lasso
plot(cv.lasso)
#same number of parameter we prefer best lambda




#6) what factors or aspects does M0 fail to consider? see dependent and eteroschedasticity through boxplot, --------------
# model where independent error terms have heterogeneous variances ovvero sigma_eps is st sigma= sigma*|time|^lambda, ------
# Heteroscedastic Autoregressive (AR1) Residual Errors and a 95% confidence interval for  correlation structure -------------------------------
rm(list=ls())
graphics.off()
data <- read.table('StorageCentres.txt',header=T)
head(data)


fit <- lm( costs ~ 1 + time + costs0 + time:growth + rad_less_15_city + size , data=data )
summary(fit)

beta <- coefficients(fit)
beta

sigmasq <- sum(residuals(fit)^2)/fit$df # estimate of sigma^2
sigmasq

AIC(fit)


#b)Provide the plot of the standardized residuals
par(mfrow=c(2,2))
plot(fit)

shapiro.test(residuals(fit))

dev.off()
# dipendent error and homoschedasticity
# see with a boxplot
par(mfrow=c(1,1))
boxplot( fit$residuals  ~ data$time )
# there is variability through different times, 

#c) Model such that the independent error terms have heterogeneous variances, and  sigma_eps is such that sigma= sigma*|time|^lambda
lmm <- gls( costs ~ time + costs0 + growth:time + rad_less_15_city + size,
            weights = varPower(form = ~time), 
            data = data)
summary(lmm)
#Report the estimates of  δ and the AIC.
#δ -> Parameter estimates: power 
#0.88779 
AIC(lmm)


#d) model with Heteroscedastic Autoregressive (AR1) Residual Errors.
#Provide a 95% confidence interval for rho in the matrix of the correlation structure.
fit2 <- gls(costs ~ time + costs0 + growth:time + rad_less_15_city + size,
            weights = varPower(form = ~time), 
            correlation = corAR1(form = ~ 1 | id_storage_centre), #1 or time ## correlazione AR, 1,rho,rho^2....
            data = data)
summary(fit2)

intervals(fit2, which = "var-cov")
#Correlation structure:
#lower        est.     upper
#Phi -0.1999194 -0.04387526 0.1143395


#e) comparison two models
anova(lmm,fit2)
# two models do not differ too much therefore we can use first model since its simpler





















#2)PCA with categorical ------------
rm(list=ls())
graphics.off()
data <- read.table('asteroids.txt',header=T)
head(data)
dataor <- data
data <- data.frame(data[,1:8])
n <- dim(data)[1]
p <- dim(data)[2]

data.originalmean <- sapply(data, mean)
data.originalsd <- sapply(data, sd)

# checking variability: if there are some variables with a very larger variance, 
# they could driven the analysis of Principal Components:

boxplot(scale(x=data, center = FALSE, scale = FALSE), las = 2, col = 'gold')  ##boxplot dati originali
boxplot(scale(x=data, center = TRUE, scale = FALSE), las = 2, col = 'gold')  ##boxplot dati centrati (mu=0, tengono la stessa varianza)
boxplot(scale(x=data, center = TRUE, scale = TRUE), las = 2, col = 'gold')   ##boxplot dati standardizzati (mu=0, deviazione standard a 1)

# i want to standardize data:
data <- scale(data)
data <- data.frame(data)

# performing PCA:
pc.data <- princomp(data, scores=T)
summary(pc.data)


#b)
load.data <- pc.data$loadings

# plotting first 2 loadings
par(mfrow = c(2,1))
for(i in 1:2) barplot(load.data[,i], ylim = c(-1, 1), main = paste('PC', i))

### Scatterplot of the data along the first two principal components 
par(mfrow = c(1,1))
biplot(pc.data)

#inspection with categorical plot wrt first two pc
head(pc.data$scores)
fac <- as.factor(dataor$Type)
plot(pc.data$scores[,1:2], col = fac)
legend('topright',legend=levels(fac), fill=1:6)

# we notice that along the first component there are differences on the type of asteroids.
# a lower pc1 corrisponds to carbonaceous, pc1 medium metallic, pc1 high silicate

#c)
#  For asteroids of the metallic variety, construct a 95% confidence region for the mean of the vector whose components are the first two PC
#id1 <- which(dataor$Type=="metallic")  
#data <- data[id1,]
dataor <- read.table('asteroids.txt',header=T)

head(dataor)
pc.data.new <- data.frame(pc.data$scores[,1:2])
pc.data.new$V3 <- dataor$Type
head(pc.data.new)
names(pc.data.new)
pc.data.new <- data.frame(pc.data.new[which(pc.data.new$V3 == "metallic"),])
pc.data.new <- data.frame(pc.data.new[,1:2])

n <- dim(pc.data.new)[1]
p <- dim(pc.data.new)[2]
data.mean <- sapply(pc.data.new, mean)  # mean
data.cov <- cov(pc.data.new)            # var/cov matrix
data.invcov <- solve(data.cov)   # inverse of var/cov matrix

alpha <- 0.05 
cfr.chisq <- qchisq(1-alpha,p)
# Characterize the ellipse:
# Axes directions:
eigen(data.cov)$vectors
# Center:
data.mean
# Radius of the ellipse:
r <- sqrt(cfr.chisq)
# Length of the semi-axes:
r*sqrt(eigen(data.cov)$values)
# Plot
par(mfrow = c(1, 1))
plot(pc.data.new, asp = 1, col='forestgreen', pch=19)
points(data.mean[1], data.mean[2], pch = 4, cex = 1.5, lwd = 2)
ellipse(center=data.mean, shape=data.cov, radius=sqrt(cfr.chisq), col = 'black', lty = 2, center.pch = 4)


# normality assumption is verified?
mvn(pc.data.new)
shapiro.test(pc.data.new$Comp.1)
# COMP 1 not normal














