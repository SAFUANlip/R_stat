library(sp)           ## Data management
library(lattice)      ## Data management
library(geoR)         ## Geostatistics
library(gstat)        ## Geostatistics
library(nlme)


# The file temperature.txt contains the yearly average air temperature (y(si),
# in degrees Celsius) at 60 locations si in the Alpine region surrounding the Aletsch Glacier.
# The dataset includes the UTM coordinates (x, y) of the
# locations si, whether the measurement site is north-oriented (orient = N),
# south-oriented (orient = S) or either
# east or west-oriented (orient = EW), and the elevation of the location e(si) (in kilometers).
# Consider the following model for air temperature estimation:

# y(si) = ω0,g + ω1,g· e(si) + ε(si)

# where ε(si) is a 2nd order stationary residual with an exponential
# variogram without nugget e!ect, and g = 0, 1, 2
# represents the grouping induced by the variable orient
# (g = 0 for orient = N, g = 1 for orient = EW and g = 2 for orient = S).

# a) In Eq. 2, assume ω1,g = ω1 for g = 0, 1, 2 and fit the model. Report a plot of the fitted variogram, initialising
# the variogram fit with the model vgm(0.2, "Exp", 1000). Indicate the estimate of the range and the sill.

temperature <- read.table("2025_01_17/temperature.txt", h=TRUE)
coordinates(temperature) <- c('x', 'y')

v1 <- variogram(temperature ~ -1 + as.factor(orientation) + elevation, data=temperature)
v1
plot(v1)

v1.fit <- fit.variogram(v1, vgm(0.2, "Exp", 1000), fit.method = 7) # 7 - by default (Weighted least squares) 
v1.fit # here we see psill and range (estimated)
plot(v1, v1.fit)

#    model    psill    range
# 1   Exp   0.767535 196.8432
#            0.768.  196.8432

# NOTE, when we use Exp variogram, range will not coincide with plot
# we have to multiply it by 3
# so in our case it will be 196.8432 * 3 = 590.5296 now it closer to the value,
# that we see on plot

# b) Using the model fitted in part (a), estimate the yearly average air temperature y→(s0) at the Jungfraujoch
# station which is located at s0 = (5140000, 427000), west-oriented, at an altitude of 3450m.

summary(lm(temperature ~ -1 + as.factor(orientation) + elevation, data = temperature))

g.tr <- gstat(formula = temperature ~ -1 + as.factor(orientation) + elevation, data = temperature, model = v1.fit)
summary(g.tr)

s0.new=data.frame(x=5140000, y=427000, orientation="EW", elevation=3.450, optional=TRUE) # UTM coordinates
coordinates(s0.new)=c('x','y')

predict(g.tr, s0.new, BLUE = FALSE) # var1.pred = -10.78355 ~ -10.784

# c) According to the model, what is the expected difference of temperature between a north and a south-exposed
# slope, all other things being equal?

s1.new=data.frame(x=0.0, y=0.0, orientation="S", elevation=0.0, optional=TRUE) # South b_0_2
coordinates(s1.new)=c('x','y')
predict(g.tr, s1.new, BLUE = TRUE) # var1.pred = 6.466195

s2.new=data.frame(x=0.0, y=0.0, orientation="N", elevation=0.0, optional=TRUE) # North b_0_0
coordinates(s2.new)=c('x','y')
predict(g.tr, s2.new, BLUE = TRUE) # var1.pred = -7.42604

# Expected difference between south and north = 6.466195 - (-7.42604) = 13.892 celsious

# d) Relaxing the assumption made in a) fit the model indicated by Eq. 2. Indicate the estimate of the sill, fitting
# the variogram with the same initialisation as in a). Should this model be preferred to the first one? Justify
# your answer.

v2 <- variogram(temperature ~ -1 + as.factor(orientation) + elevation:as.factor(orientation), data=temperature)
v2
plot(v2)

v2.fit <- fit.variogram(v2, vgm(0.2, "Exp", 1000), fit.method = 7) # 7 - by default (Weighted least squares) 
v2.fit # here we see psill and range (estimated)
plot(v2, v2.fit)

# model     psill   range
# 1   Exp 0.2340876 803.079
#          ~0.234

# Second model should be more preferable due it it takes into account difference of
# temperature depending on elevation among regions 
# Also model d) has lower psill, which may say about better spatial variation
# and bigger range may say about more smoother variogram 








