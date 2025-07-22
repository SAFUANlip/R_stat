library(sp)           ## Data management
library(lattice)      ## Data management
library(geoR)         ## Geostatistics
library(gstat)        ## Geostatistics
library(nlme)

# A study is conducted to analyze the impact of environmental conditions, seasonal variation, and geographic location
# on the growth rate of trees in a forest reserve between May and July 2023. The dataset treegrowth.txt reports the
# daily growth measurements of trees in di!erent parts of the forest. It includes the UTM geographical coordinates
# s_i of the trees, a categorical variable season indicating if the measurement was taken in early or late summer
# (early for May–June and late for July), the recorded average daily soil moisture moisturei (in percentage), and
# the observed growth rate y(si) [mm/day]. Consider the following model:

# y(si) = b0,j + b1moisturei + ω(si) 
# where ω(si) represents stationary residuals with spherical variogram with nugget, and j = 0, 1 corresponds to the
# grouping induced by the variable season (j = 0 for early, j = 1 for late).

treegrowth <- read.table("treegrowth.txt", h=TRUE)

## Define the sample coordinates (transform Table to SpatialPointsDataFrame)
coordinates(treegrowth) <- c('x', 'y')

# a) Report a plot of the fitted variogram, initialising the variogram fit with the model vgm(0.1, "Sph", 1000,
# 0.1). Indicate the estimate of the range and the sill.

v1 <- variogram(growth ~ -1 + as.factor(season) + moisture, treegrowth)
v1
plot(v1)

v1.fit <- fit.variogram(v1, vgm(0.1, "Sph", 1000, 0.1), fit.method = 7) # 7 - by default (Weighted least squares) 
v1.fit # here we see psill and range (estimated)
plot(v1, v1.fit)
# Range - 1161.639
# sill SUM OF sill from Nugget and Spherical part 0.09237614+0.08322185 = 0.175598

# b) Estimate the parameters b0,0, b0,1 
# and b1 using the generalized least squares method.
# lm(growth ~ -1 + as.factor(season) + moisture, data = treegrowth)

g.tr <- gstat(formula = growth ~ -1 + as.factor(season) + moisture, data = treegrowth, model = v1.fit)
summary(g.tr)

s0.new=data.frame(x=0.0, y=0.0, moisture=0.0, season=FALSE) # UTM coordinates
coordinates(s0.new)=c('x','y')

predict(g.tr, s0.new, BLUE = TRUE) # var1.pred = 1.117702 - b_0_0

s1.new=data.frame(x=0.0, y=0.0, moisture=0.0, season=TRUE) # UTM coordinates
coordinates(s1.new)=c('x','y')

predict(g.tr, s1.new, BLUE = TRUE) # var1.pred = 0.9080086 - b_0_1

s2.new=data.frame(x=0.0, y=0.0, moisture=1.0, season=FALSE) # UTM coordinates
coordinates(s2.new)=c('x','y')

predict(g.tr, s2.new, BLUE = TRUE) # var1.pred = 1.12994
# here we predict as b_0_0 + b_1 = 1.12994 =>
# b_1 = 1.12994 - 1.117702 = 0.012238

# c) Using the model, provide an estimate of the total growth expected for a tree during July assuming a constant
# soil moisture of 25%.

sc.new=data.frame(x=0.0, y=0.0, moisture=25.0, season=TRUE) 
coordinates(sc.new)=c('x','y')

predict(g.tr, sc.new, BLUE = TRUE) # var1.pred = 1.213959
1.213959 * 31 # Question was about total growth during July (so all days in July)
# there for I had to multiply by 31 (because our data reports DAILY growth)

# d) Due to their environment, trees can be categorized as growing in canopy-covered areas (canopy=TRUE) or in
# open areas (canopy=FALSE). Modify the model in Eq. (1), including this categorical effect, as follows:
#        y(si) = b0,j,k + b1,k moisturei + ω(si)
# where ω(si) represents stationary residuals with exponential variogram without nugget, and k is the grouping
# induced by the variable canopy.

# summary(lm(growth ~ -1 + as.factor(season):as.factor(canopy) + moisture:as.factor(canopy), treegrowth))
v2 <- variogram(growth ~ -1 + as.factor(season):as.factor(canopy) + moisture:as.factor(canopy), treegrowth)
v2
plot(v2)

v2.fit <- fit.variogram(v2, vgm(0.1, "Exp", 3000), fit.method = 7) # 7 - by default (Weighted least squares) 
# I didт't indicate nugget, so model don't use it
v2.fit # here we see psill and range (estimated)
plot(v2, v2.fit)

# just to check that I used correct model structure
# lm(growth ~ -1 + as.factor(season):as.factor(canopy) + moisture:as.factor(canopy), data = treegrowth)


g2.tr <- gstat(formula = growth ~ -1 + as.factor(season):as.factor(canopy) + moisture:as.factor(canopy), data = treegrowth, model = v2.fit)
summary(g2.tr)

s4.new=data.frame(x=0.0, y=0.0, moisture=0.0, season=FALSE, canopy=FALSE) # UTM coordinates
coordinates(s4.new)=c('x','y')
predict(g2.tr, s4.new, BLUE = TRUE) # var1.pred = 0.6261478 = b_0_0_0


s5.new=data.frame(x=0.0, y=0.0, moisture=0.0, season=FALSE, canopy=TRUE) # UTM coordinates
coordinates(s5.new)=c('x','y')
predict(g2.tr, s5.new, BLUE = TRUE) # var1.pred = 0.8614567 = b_0_0_1

s6.new=data.frame(x=0.0, y=0.0, moisture=1.0, season=FALSE, canopy=TRUE) # UTM coordinates
coordinates(s6.new)=c('x','y')
predict(g2.tr, s6.new, BLUE = TRUE) # var1.pred = 0.8901377 = b_0_0_1 + b_1_1
# b_1_1 = 0.8901377 - 0.8614567 = 0.028681


s7.new=data.frame(x=0.0, y=0.0, moisture=1.0, season=FALSE, canopy=FALSE) # UTM coordinates
coordinates(s7.new)=c('x','y')
predict(g2.tr, s7.new, BLUE = TRUE) # var1.pred = 0.637564 = b_0_0_0 + b_1_0
# b_1_0 = 0.637564 - 0.6261478 = 0.0114162

# How would you describe the effect of the canopy variable (during early summer)?
# It has positive effect, вue to coefficent > 0, so canopy helps trees growth 
# also Coefficents with canopy (TRUE) has higher value, so it has better effect for
# trees growth, compared to open-airs

