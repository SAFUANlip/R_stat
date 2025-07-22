library(sp)           ## Data management
library(lattice)      ## Data management
library(geoR)         ## Geostatistics
library(gstat)        ## Geostatistics
library(nlme)

# Particulate Matter (PM) refers to tiny particles or droplets in the air that can be inhaled into the lungs and may
# cause serious health problems. PM10 specifically refers to inhalable particles with diameters that are generally
# 10 micrometers and smaller. These particles can come from various sources, including vehicle exhaust, industrial
# emissions, construction activities, and natural sources such as dust storms and wildfires.
# The Lombardia region is interested in modeling the distribution of PM10 concentration over its entire territory
# to assess air quality and potential health risks.
# The file pm10.txt contains measurements of daily maximum concentration z of PM10 [µg/m3] recorded on July
# 14th, 2023, within 160 locations in Lombardia. The locations sk (k = 1, ..., 160) are expressed in UTM coordinates
# (variables x and y). Additionally, an ”urbanization measure” of the area surrounding the location through the
# numerical variable urban ranging from 0 to 3 is provided. The variable urban is built as follow: urban = 3 inside
# an agglomeration, urban = 2 in an urbanized plain, urban = 1 in an low-urbanized plain and urban = 0 in an a
# mountain or forest area.
# For all questions, 

# For all questions, delta(sk) is a 2nd order stationary residual with an Exponential model with Nugget for the spatial
# dependence structure.

pm10 <- read.table("2024_02_06/pm10.txt", h=TRUE)
coordinates(pm10) <- c('x', 'y')

# a) Consider for the PM10 concentration z(sk ) the following stationary model:
#   z(sk ) = betta_0 + delta_(sk ) for k = 1, ..., 180,
#   Fitting the model via Generalized Least Squares, estimate the range and the sill of delta_(sk ), along with betta_0.

v <- variogram(pm10 ~  1, pm10)
plot(v)
# params: psill = NA, model, range = NA, nugget
v.fit <- fit.variogram(v, vgm(0.3, "Exp", 30000, 0.15), fit.method = 7) # 7 - by default (Weighted least squares) 
v.fit # here we see psill and range (estimated)
#    model     psill    range
# 1   Nug  0.1281670    0.000
# 2   Sph 0.1724577 15802.19
# psill = 0.1281670 + 0.1724577 = 0.3006247
# range = 15802.19
plot(v, v.fit)

g.tr <- gstat(formula = pm10 ~  1, data = pm10, model = v.fit)
g.tr

s0.new=data.frame(x=0.0, y=0.0, urban = 0, optional = TRUE) # UTM coordinates
coordinates(s0.new)=c('x','y')

predict(g.tr, s0.new, BLUE = TRUE) # 2.155857 = betta_0 

# b) Now, consider the following modification of the previous model:
# z(sk ) = betta_0 + betta_1 urbank + delta(sk ) for k = 1, ..., 180,
# Fitting the model via Generalized Least Squares, estimate the range and the sill of delta(sk ),
# along with betta_0 and betta_1.

v2 <- variogram(pm10 ~ 1 + urban, pm10)
plot(v2)
# params: psill = NA, model, range = NA, nugget
v2.fit <- fit.variogram(v2, vgm(0.006, "Exp", 30000, 0.003), fit.method = 7) # 7 - by default (Weighted least squares) 
v2.fit # here we see psill and range (estimated)
#    model     psill    range
# 1   Nug  0.001138761    0.000
# 2   Sph 0.004857698 7012.38
# psill = 0.001138761 + 0.004857698 = 0.005996459 ~ 0.006
# range = 7012.38
plot(v2, v2.fit)

g2.tr <- gstat(formula = pm10 ~ 1 + urban, data = pm10, model = v2.fit)
g2.tr

s1.new=data.frame(x=0.0, y=0.0, urban = 0, optional = TRUE) # UTM coordinates
coordinates(s1.new)=c('x','y')
predict(g2.tr, s1.new, BLUE = TRUE) # 1.952211 = betta_0

s2.new=data.frame(x=0.0, y=0.0, urban = 1, optional = TRUE) # UTM coordinates
coordinates(s2.new)=c('x','y')
predict(g2.tr, s2.new, BLUE = TRUE) # 2.570913 = betta_0 + betta_1
# betta_1 = 2.570913 - 1.952211 = 0.618702 ~ 0.619

s3.new=data.frame(x=0.0, y=0.0, urban = 2, optional = TRUE) # UTM coordinates
coordinates(s3.new)=c('x','y')
predict(g2.tr, s3.new, BLUE = TRUE) # 3.189615 = betta_0 + betta_1 * 2
# betta_1 = (3.189615 - 1.952211)/2 = 0.618702 ~ 0.619

# c) Finally, consider the model below:
#   z(sk ) = betta_0,j + delta(sk ) for k = 1, ..., 180,
#   where j 2 [0. . . 3] is the grouping variable induced by the 
# type of surrounding area (as defined in the variable urban). 
# Fitting the model via Generalized Least Squares, 
# estimate the range and the sill of delta(sk ), along with betta_0,j

summary(lm(pm10 ~ -1 + as.factor(urban), pm10))

v3 <- variogram(pm10 ~ -1 + as.factor(urban), pm10)
plot(v3)
# params: psill = NA, model, range = NA, nugget
v3.fit <- fit.variogram(v3, vgm(0.006, "Exp", 30000, 0.003), fit.method = 7) # 7 - by default (Weighted least squares) 
v3.fit # here we see psill and range (estimated)
#    model     psill    range
# 1   Nug  0.001508530    0.000
# 2   Sph 0.004441755 7494.11
# psill = 0.001508530 + 0.004441755 = 0.005950285 ~ 0.006
# range = 7494.11
plot(v3, v3.fit)

g3.tr <- gstat(formula = pm10 ~ -1 + as.factor(urban), data = pm10, model = v3.fit)
g3.tr

s4.new=data.frame(x=0.0, y=0.0, urban = 0, optional = TRUE) # UTM coordinates
coordinates(s4.new)=c('x','y')
predict(g3.tr, s4.new, BLUE = TRUE) # 1.953394 = betta_0_0

s5.new=data.frame(x=0.0, y=0.0, urban = 1, optional = TRUE) # UTM coordinates
coordinates(s5.new)=c('x','y')
predict(g3.tr, s5.new, BLUE = TRUE) # 2.562675 = betta_0_1

s6.new=data.frame(x=0.0, y=0.0, urban = 2, optional = TRUE) # UTM coordinates
coordinates(s6.new)=c('x','y')
predict(g3.tr, s6.new, BLUE = TRUE) # 3.177759 = betta_0_2

s7.new=data.frame(x=0.0, y=0.0, urban = 3, optional = TRUE) # UTM coordinates
coordinates(s7.new)=c('x','y')
predict(g3.tr, s7.new, BLUE = TRUE) # 3.824466 = betta_0_3

# d) Which of the three models would you keep? Justify and comment on your choice.
plot.new()
plot(v2$dist, v2$gamma, pch = 19, main = "Empirical and Fitted Variograms",
     xlab = "Distance", ylab = "Semivariance", ylim = c(0, max(v2$gamma)*1.2))

d.seq <- seq(0, max(v2$dist), length.out = 100)
gamma1 <- variogramLine(v2.fit, dist_vector = d.seq)$gamma
lines(d.seq, gamma1, col = "blue", lwd = 2)

gamma2 <- variogramLine(v3.fit, dist_vector = d.seq)$gamma
lines(d.seq, gamma2, col = "red", lwd = 2)

legend("bottomright", legend = c("Common urban factor", "Individual intercept"),
       col = c("blue", "red"), lwd = 2, bty = "n")

# I would prefer model 3, because it little bit smoother, because has higher range than model 2
# (while their sill is equal)
# Model 1 worse, because have much higher sill and not smooth enough

# e) Using the selected model, provide a point prediction for the maximum concentration of PM10 on July 14th,
# 2023 at the location (x=514961, y=5034538), corresponding to the Duomo di Milano.


s8.new=data.frame(x=514961, y=5034538, urban = 3, optional = TRUE) # UTM coordinates
coordinates(s8.new)=c('x','y')
predict(g3.tr, s8.new, BLUE = FALSE) # 3.827962
predict(g2.tr, s8.new, BLUE = FALSE) # 3.827962





