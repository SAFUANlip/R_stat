library(sp)           ## Data management
library(lattice)      ## Data management
library(geoR)         ## Geostatistics
library(gstat)

# A study is conducted to analyze the spatial distribution of uranium concentration in the Tim Mersoi Basin in
# northern Niger. The dataset uranium.txt reports the uranium concentration at various locations within the study
# area. It includes the UTM geographical coordinates s of the sampling locations, a categorical variable rock type
# indicating the type of rock (granite, sandstone, or shale), the depth of the sample depth (in meters), and the
# measured uranium concentration y(s) [ppm]. Consider the following model:

# y(s) = b0,j + b1depth + delta(s)

uranium_source <- read.table("2024_07_08/uranium.txt", h=TRUE)
uranium <- read.table("2024_07_08/uranium.txt", h=TRUE)
coordinates(uranium) <- c('x', 'y')

# a) Report a plot of the fitted variogram, initialising the variogram fit with 
# the model vgm(200000, "Sph", 2000, 100000). Indicate the estimate of the range and the sill.

# summary(lm(concentration ~ -1 + as.factor(rock_type) + depth, uranium)) # just to check required params

v <- variogram(concentration ~ -1 + as.factor(rock_type) + depth, uranium)
plot(v)
# params: psill = NA, model, range = NA, nugget
v.fit <- fit.variogram(v, vgm(200000, "Sph", 2000, 100000), fit.method = 7) # 7 - by default (Weighted least squares) 
v.fit # here we see psill and range (estimated)
#    model     psill    range
# 1   Nug  23264.89    0.000
# 2   Sph 120319.89 1990.503
# psill = 23264.89 + 120319.89 = 143584.8
# range = 1990.503
plot(v, v.fit)

# b) Provide an estimate of the mean uranium concentration at the surface of a sandstone rock type area.
g.tr <- gstat(formula = concentration ~ -1 + as.factor(rock_type) + depth, data = uranium, model = v.fit)
g.tr

# at the surface, so depth = 0
s0_sand.new=data.frame(x=0.0, y=0.0, depth = 0, rock_type = "sandstone") # UTM coordinates
coordinates(s0_sand.new)=c('x','y')

predict(g.tr, s0_sand.new, BLUE = TRUE) # 2619.905 = betta_0_1 

predict(g.tr, s0_sand.new, BLUE = TRUE)

# c) Independently of the position, by which quantity the uranium concentration increases when the sample is taken
# 1m lower?

s0_sand_1m.new=data.frame(x=0.0, y=0.0, depth = 1, rock_type = "sandstone") # UTM coordinates
coordinates(s0_sand_1m.new)=c('x','y')

predict(g.tr, s0_sand_1m.new, BLUE = TRUE) # betta_0_1 + betta_1 = 2622.912 
# betta_1 = 2622.912 - 2619.905 = 3.007

# My wrong approach, but gave same result
uranium_1m <- read.table("2024_07_08/uranium.txt", h=TRUE)
uranium_1m$depth = uranium_1m$depth + 1
coordinates(uranium_1m) <- c('x', 'y')

predict_val_original <- predict(g.tr, uranium, BLUE = TRUE)$var1.pred
predict_val_1m <- predict(g.tr, uranium_1m, BLUE = TRUE)$var1.pred

mean(predict_val_original) # 2218.65
mean(predict_val_1m) # 2221.658 incresed by 3.008 (on average)

# d) Consider a new location with coordinates s0 = (687000, 2234000), which is in a sandstone rock type area. Which
# depth must we reach to find an uranium concentration of at least 3000 ppm at that location?

s0.new=data.frame(x=687000.0, y=2234000.0, depth = 88, rock_type = "sandstone") # UTM coordinates
coordinates(s0.new)=c('x','y')

predict(g.tr, s0.new, BLUE = FALSE) 
# [using universal kriging]
#          coordinates var1.pred var1.var
# 1 (687000, 2234000)  3000.454 72441.66 
# just by experiments, depth = 88

# e) Consider now the model update:
#   y(s) = b0,j + b1,j depth + delta(s) (1)
# Indicate the estimate of the sill, fitting the variogram with the same initialisation as in a). Should this model
# be preferred to the first one? Justify your answer
v2 <- variogram(concentration ~ -1 + as.factor(rock_type) + depth:as.factor(rock_type), uranium)
plot(v2)

v2.fit <- fit.variogram(v2, vgm(200000, "Sph", 2000, 100000), fit.method = 7) # 7 - by default (Weighted least squares) 
v2.fit # here we see psill and range (estimated)
plot(v2, v2.fit)

#   model     psill    range
# 1   Nug  23311.97    0.000
# 2   Sph 119587.48 2031.484
# I would take second model, as it has little bit smoother variogram 
# sill = 23311.97 + 119587.48 = 142899.5

# Final comparison plot
plot.new()
plot(v$dist, v$gamma, pch = 19, main = "Empirical and Fitted Variograms",
     xlab = "Distance", ylab = "Semivariance", ylim = c(0, max(v$gamma)*1.2))

d.seq <- seq(0, max(v$dist), length.out = 100)
gamma1 <- variogramLine(v.fit, dist_vector = d.seq)$gamma
lines(d.seq, gamma1, col = "blue", lwd = 2)

gamma2 <- variogramLine(v2.fit, dist_vector = d.seq)$gamma
lines(d.seq, gamma2, col = "red", lwd = 2)

legend("bottomright", legend = c("Model Spherical", "Model Spherical + facto in dept"),
       col = c("blue", "red"), lwd = 2, bty = "n")

