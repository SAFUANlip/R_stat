library(sp)           ## Data management
library(lattice)      ## Data management
library(geoR)         ## Geostatistics
library(gstat)

# One of the most relevant consequences of the eruption of volcan 
# Eyjafjoll (in Iceland), in 2010, is the contamination by fluoride. 
# The latter is due to the deposit on the ground of the ash released
# in the atmosphere during the eruption.
# The file "fluoruro.txt" reports the coordinates of 50 measurement sites
# s_i, i=1,...,50, the corresponding concentrations of fluoride (ppm) F(s_i)
# and the distance D.s_i of each site s_i from the crater of the volcano.
# Denoting by delta a zero-mean, second-order stationary and isotropic random
# field:
# a) Estimate two empirical variograms, assuming the following models:
#    F(s_i)=beta_0+delta(s_i) and 
#    F(s_i)=beta_0+beta_1*D.s_i+delta(s_i). 
#    Choose the most appropriate model for the observations.
# b) Fit to the empirical variogram at point (a), a Gaussian model
#    without nugget, via weighted least squares. Use as initial parameters: 
#    sill=100, range=0.02. Report the estimates of sill and range.
# c) Fit to the empirical variogram chosen at point (a), a spherical model
#    without nugget, via weighted least squares. Report the estimates of sill 
#    and range.
# d) Compare the variograms estimated at points (b) and (c), with the empirical
#    variogram at point (a). Given that the ash deposition is known to be
#    a very regular phenomenon, which variogram model is the most appropriate?
# e) Based on model (d), estimate the concentration of fluoride due to the eruption
#    in the city of Raufarhofn (s0 = (0.3; 0.24), D.s0 = 0.1970) 
# f) Based on model (d), estimate the concentration of fluoride at the same
#    location, due to a possible new eruption of equivalent intensity, independent 
#    of that of 2010.

fluoruro <- read.table("fluoruro.txt", h=TRUE)

## Define the sample coordinates (transform Table to SpatialPointsDataFrame)
coordinates(fluoruro) <- c('X', 'Y')

# isotropic random field - no dependence on direction, 
# so whenever we look, it always will be same variogram
plot(fluoruro)
bubble(fluoruro, 'Conc.ppm',do.log=TRUE)

hist(fluoruro$Conc.ppm, breaks=16, col="grey", main='Histogram of Conc.ppm', prob = TRUE, xlab = 'Conc.ppm')
# our target - Conc.ppm (fluoride)
# under gaussianity it works better
# under assymetry - it also work better 
# our data already look gaussian and symmetric, so don't need log
# log - helped make more symmetric 

# F(s_i)=beta_0+delta(s_i) 
# (beta_0 - comman intercept ~ 1 (only intercept),
# delta(s_i) - local fluctuations, addede automaticall to the model)
v1 <- variogram(Conc.ppm ~ 1, fluoruro)
plot(v1)

# F(s_i)=beta_0+beta_1*D.s_i+delta(s_i)
v2 <- variogram(Conc.ppm ~ 1 + D, fluoruro) # Conc.ppm ~ D (same model)
plot(v2)

# I would prefer second model, because we may see that lim(gamm(h)/||h||^2) -> 0
# while in first one it's not

# b) Fit to the empirical variogram at point (a), a Gaussian model
#    without nugget, via weighted least squares. Use as initial parameters: 
#    sill=100, range=0.02. Report the estimates of sill and range.
vgm()

# WEIGHTS during fitting -------------------------------------------------------
# how can we choose weights? argument fit.method in fit.variogram
# fit.method = 1 : w = N_j
# fit.method = 2 : w = N_j/gamma(h_j)^2
# fit.method = 6 : w = 1
# fit.method = 7 : w = N_j/h_j^2
# Nj — количество пар точек, попавших в лаг hj (имеющих расстояние "близкое" к hj)
# Например, если лаг 0.1 включает все точки, находящиеся на расстоянии от 0.05 до 0.15, и таких пар 35, то Nj = 35


v2.fit <- fit.variogram(v2, vgm(100, "Gau", 0.02), fit.method = 7) # 7 - by default (Weighted least squares) 
# I did't indicate nugget, so model don't use it
v2.fit # here we see psill and range (estimated)
plot(v2, v2.fit)

# c) Fit to the empirical variogram chosen at point (a), a spherical model
#    without nugget, via weighted least squares. Report the estimates of sill 
#    and range.
v2.fit_sph <- fit.variogram(v2, vgm(100, "Sph", 0.02), fit.method = 7) # 7 - by defauls 
# I did't indicate nugget, so model don't use it
v2.fit_sph # here we see psill and range (estimated)
plot(v2, v2.fit_sph)

dev.off()

# Final comparison plot
plot.new()
plot(v2$dist, v2$gamma, pch = 19, main = "Empirical and Fitted Variograms",
     xlab = "Distance", ylab = "Semivariance", ylim = c(0, max(v2$gamma)*1.2))

d.seq <- seq(0, max(v2$dist), length.out = 100)
gamma1 <- variogramLine(v2.fit, dist_vector = d.seq)$gamma
lines(d.seq, gamma1, col = "blue", lwd = 2)

gamma2 <- variogramLine(v2.fit_sph, dist_vector = d.seq)$gamma
lines(d.seq, gamma2, col = "red", lwd = 2)

legend("bottomright", legend = c("Model Gaussian", "Model Spherical"),
       col = c("blue", "red"), lwd = 2, bty = "n")


# d) Compare the variograms estimated at points (b) and (c), with the empirical
#    variogram at point (a). Given that the ash deposition is known to be
#    a very regular phenomenon, which variogram model is the most appropriate?

# variogram with Gaussian method better fit first points and look smoother

# Модель	      Поведение у 0 (гладкость)	                Подходит при…
# Spherical	     Линейный рост у начала	                 Процесс с конечной корреляцией, не гладкий
# Exponential	 Острый изгиб у нуля, быстро растёт	       Нерегулярный, шумный процесс
# Gaussian	       Очень плавный старт	                 Гладкий процесс, высокая корреляция
# very regular phenomenon - Gaussian 

# e) Based on model (d), estimate the concentration of fluoride due to the eruption
#    in the city of Raufarhofn (s0 = (0.3; 0.24), D.s0 = 0.1970)

s0.new=data.frame(X=0.3, Y=0.24, D=0.1970) # UTM coordinates
coordinates(s0.new)=c('X','Y')

g.tr <- gstat(formula = Conc.ppm ~ 1 + D, data = fluoruro, model = v2.fit)
predict(g.tr, s0.new)
# var1.var - MSE of prediction
# CI 
# z <- qnorm(0.975)  # ≈ 1.96
# lower <- 50.80604 - z * sqrt(98.66119)
# upper <- 50.80604 + z * sqrt(98.66119)

# f) Based on model (d), estimate the concentration of fluoride at the same
#    location, due to a possible new eruption of equivalent intensity, independent 
#    of that of 2010. (so predict mean value)
predict(g.tr, s0.new, BLUE = TRUE)

# Что означает BLUE = TRUE?
# Это означает, что ты просишь оценку линейного тренда в точке s0.new, а не полное кригинговое предсказание.
# То есть предсказание для детерминированной части модели 
# E[F(s)]=β0+β1D(s), а не для полной случайной величины (because E[δ(s)] = 0)
# F(s)=β0+β1D(s)+δ(s) 
# Такое предсказание называется GLS (Generalized Least Squares) оценкой тренда 
# — это BLUE (Best Linear Unbiased Estimator) при наличии коррелированных ошибок.

# Почему дисперсия здесь гораздо меньше?
#  Потому что:
#  var1.var = 3.077244 — это дисперсия оценки тренда (то есть неопределённость в параметрах модели тренда).
# В обычном kriging, дисперсия (var1.var ≈ 98) включает также вариацию  
# из-за сто́хастического поля δ(s), а не только неопределённость тренда.




# ------------------------------------------------------------------------------
# The file radioville.txt reports the information on 158 control units
# in the area around the nuclear power plant of Radioville.
# At each site, available data consist of: radioactivity levels [Bq],
# longitude [°N], latitude [°W] and type of soil [urban/vegetation].
# Denoting by s_i the i-th site, by R the radioactivity level,
# by eps a weakly stationary random field and by D a dummy 
# urban/vegetation:

# a) estimate the parameters of the linear model 
#    R(s_i) = beta_0 + beta_1 D(s_i) + eps(s_i) 
#    assuming for eps a spherical variogram without nugget, estimated
#    via weighted least squares;
# b) estimate the parameters of the linear model 
#    R(s_i) = beta_0 + beta_1 D(s_i) + eps(s_i) 
#    assuming for eps a spherical variogram with nugget, estimated
#    via weighted least squares;
# c) choose the best variogram model by comparing the fitted model 
#    with the corresponding empirical variograms (report qualitative
#    plots and the estimated variogram parameters)
# d) on the basis of model (c), predict the radioactivity level at the
#    parking lot of the shopping centre of Radioville (lon = 78.59, 
#    lat = 35.34), and in the park of Radioville (lon = 77.6, 
#    lat = 34.99);
# e) estimate variance of prediction error at the same locations
#    as at point d).

radioville <- read.table("radioville.txt", h=TRUE)

# a) 
# firstly we need empirical variogram, and then we may fit our models on it
coordinates(radioville) <- c('Long', 'Lat')

#proj4string(radioville) <- CRS("+proj=longlat +datum=WGS84")  # indicate, that it long-lat

# Transform to UTM
#radioville_utm <- spTransform(radioville, CRS("+proj=utm +zone=17 +datum=WGS84"))

v <- variogram(Bq ~ 1 + D, radioville)
plot(v)

hist(radioville$Bq, breaks=16, col="grey", main='Histogram of Conc.ppm', prob = TRUE, xlab = 'Conc.ppm')

v1.fit <- fit.variogram(v, vgm(1, "Sph", 0.7), fit.method = 7) # 7 - by default (Weighted least squares) 
# I did't indicate nugget, so model don't use it
v1.fit # here we see psill and range (estimated)
plot(v, v1.fit)

v2.fit <- fit.variogram(v, vgm(1, "Sph", 0.7, nugget = 0.1), fit.method = 7) # 7 - by default (Weighted least squares) 
v2.fit # here we see psill and range (estimated)
plot(v, v2.fit)

# Clear device again
dev.off()

# Final comparison plot
plot.new()
plot(v$dist, v$gamma, pch = 19, main = "Empirical and Fitted Variograms",
     xlab = "Distance", ylab = "Semivariance", ylim = c(0, max(v$gamma)*1.2))

d.seq <- seq(0, max(v$dist), length.out = 100)
gamma1 <- variogramLine(v1.fit, dist_vector = d.seq)$gamma
lines(d.seq, gamma1, col = "blue", lwd = 2)

gamma2 <- variogramLine(v2.fit, dist_vector = d.seq)$gamma
lines(d.seq, gamma2, col = "red", lwd = 2)

legend("bottomright", legend = c("Model Spherical", "Model Spherical + nugget"),
       col = c("blue", "red"), lwd = 2, bty = "n")

# I will use seconf model, because it better estimatse first observations, 
# and take into account nugget effect

# d) on the basis of model (c), predict the radioactivity level at the
#    parking lot of the shopping centre of Radioville (lon = 78.59, 
#    lat = 35.34), and in the park of Radioville (lon = 77.6, 
#    lat = 34.99);

s1.new=data.frame(Long=78.59, Lat=35.34, D="U") # UTM coordinates
coordinates(s1.new)=c('Long','Lat')

s2.new=data.frame(Long=77.6, Lat=34.99, D="V")
coordinates(s2.new)=c('Long','Lat')

g.tr <- gstat(formula = Bq ~ 1 + D, data = radioville, model = v2.fit)

predict(g.tr, s1.new)
predict(g.tr, s2.new)



